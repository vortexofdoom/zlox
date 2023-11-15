const std = @import("std");
const vm = @import("vm.zig");
const InterpretError = vm.InterpretError;
const Scanner = @import("scanner.zig");
const chunk_ = @import("chunk.zig");
const Value = @import("value.zig").Value;
const object = @import("object.zig");
const copyString = object.copyString;
const Obj = object.Obj;
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;
const ObjType = object.ObjType;
const Op = chunk_.Op;
const Chunk = chunk_.Chunk;
const scanToken = Scanner.scanToken;
const Token = Scanner.Token;
const TT = Scanner.TokenType;
const CompileError = @import("error.zig").CompileError;

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
};

const ParseFn = *const fn (bool) anyerror!void;
const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precendence,
};

const Local = struct {
    name: []const u8 = "",
    depth: isize = -1,
    is_captured: bool = false,
};

const Upvalue = struct {
    index: u8 = 0,
    is_local: bool = false,
};

const FunctionType = enum {
    function,
    script,
};

const Compiler = struct {
    enclosing: ?*Compiler = null,
    function: ?*ObjFunction = null,
    type: FunctionType = .script,
    locals: [256]Local = .{Local{}} ** 256,
    local_count: u32 = 0,
    upvalues: [256]Upvalue = .{Upvalue{}} ** 256,
    scope_depth: u32 = 0,

    fn init(compiler: *Compiler, ty: FunctionType) !void {
        compiler.enclosing = current;
        compiler.function = null;
        compiler.type = ty;
        compiler.local_count = 0;
        compiler.scope_depth = 0;

        var fun = try ObjFunction.new();
        compiler.function = fun;
        current = compiler;
        if (ty != FunctionType.script) {
            compiler.function.?.name = try copyString(parser.previous.str);
        }
        var local = &compiler.locals[compiler.local_count];
        compiler.local_count += 1;
        local.depth = 0;
        local.name = "";
        local.is_captured = false;
    }

    fn resolveLocal(self: *Compiler, name: []const u8) ?u8 {
        var i = self.local_count - 1;
        while (i < std.math.maxInt(u32)) : (i -%= 1) {
            const local = &self.locals[i];
            if (identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    errorPrev("Can't read local variable in its own initializer.");
                }
                return @truncate(i);
            }
        }

        return null;
    }

    fn resolveUpvalue(self: *Compiler, name: []const u8) ?u8 {
        if (self.enclosing) |enclosing| {
            if (enclosing.resolveLocal(name)) |local| {
                enclosing.locals[local].is_captured = true;
                return self.addUpvalue(local, true);
            }

            if (enclosing.resolveUpvalue(name)) |uv| {
                return self.addUpvalue(uv, false);
            }
        }
        return null;
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) u8 {
        const uv_count = self.function.?.upvalue_count;

        // If we have already saved this upvalue, return it
        //for (0..uv_count) |i| {
        for (self.upvalues[0..uv_count], 0..) |*uv, i| {
            //const uv = &self.upvalues[i];
            if (uv.index == index and uv.is_local == is_local) {
                return @truncate(i);
            }
        }

        if (uv_count == 256) {
            errorPrev("Too many closure variables in function.");
            return 0;
        }

        // Otherwise we add a new one
        self.upvalues[uv_count].is_local = is_local;
        self.upvalues[uv_count].index = index;
        self.function.?.upvalue_count += 1;
        return @truncate(uv_count);
    }
};

fn currentChunk() *Chunk {
    return &current.?.function.?.chunk;
}

var parser = Parser{
    .current = undefined,
    .previous = undefined,
    .had_error = false,
    .panic_mode = false,
};

pub var current: ?*Compiler = null;

const Precendence = enum {
    NONE,
    ASSIGNMENT,
    OR,
    AND,
    EQUALITY,
    COMPARISON,
    TERM,
    FACTOR,
    UNARY,
    CALL,
    PRIMARY,
};

inline fn makeRule(comptime prefix: ?ParseFn, comptime infix: ?ParseFn, comptime precedence: Precendence) ParseRule {
    return .{
        .prefix = prefix,
        .infix = infix,
        .precedence = precedence,
    };
}

// zig fmt: off
const rules = [_]ParseRule{
    makeRule(grouping,  call,   .CALL), // LEFT_PAREN
    makeRule(null,      null,   .NONE), // RIGHT_PAREN
    makeRule(null,      null,   .NONE), // LEFT_BRACE
    makeRule(null,      null,   .NONE), // RIGHT_BRACE
    makeRule(null,      null,   .NONE), // COMMA
    makeRule(null,      dot,   .CALL), // DOT
    makeRule(unary,     binary, .TERM), // MINUS
    makeRule(null,      binary, .TERM), // PLUS
    makeRule(null,      null,   .NONE), // SEMICOLON
    makeRule(null,      binary, .FACTOR), // SLASH
    makeRule(null,      binary, .FACTOR), // STAR
    makeRule(unary,     null,   .NONE), // BANG
    makeRule(null,      binary, .EQUALITY), // BANG_EQUAL
    makeRule(null,      null,   .NONE), // EQUAL
    makeRule(null,      binary, .EQUALITY), // EQUAL_EQUAL
    makeRule(null,      binary, .COMPARISON), // GREATER
    makeRule(null,      binary, .COMPARISON), // GREATER_EQUAL
    makeRule(null,      binary, .COMPARISON), // LESS
    makeRule(null,      binary, .COMPARISON), // LESS_EQUAL
    makeRule(variable,  null,   .NONE), // IDENTIFIER
    makeRule(string,    null,   .NONE), // STRING
    makeRule(number,    null,   .NONE), // NUMBER
    makeRule(null,      and_,   .AND), // AND
    makeRule(null,      null,   .NONE), // CLASS
    makeRule(null,      null,   .NONE), // ELSE
    makeRule(literal,   null,   .NONE), // FALSE
    makeRule(null,      null,   .NONE), // FOR
    makeRule(null,      null,   .NONE), // FUN
    makeRule(null,      null,   .NONE), // IF
    makeRule(literal,   null,   .NONE), // NIL
    makeRule(null,      or_,    .OR), // OR
    makeRule(null,      null,   .NONE), // PRINT
    makeRule(null,      null,   .NONE), // RETURN
    makeRule(null,      null,   .NONE), // SUPER
    makeRule(null,      null,   .NONE), // THIS
    makeRule(literal,   null,   .NONE), // TRUE
    makeRule(null,      null,   .NONE), // VAR
    makeRule(null,      null,   .NONE), // WHILE
    makeRule(null,      null,   .NONE), // ERROR
    makeRule(null,      null,   .NONE), // EOF
};
// zig fmt: on

fn errorAt(token: *Token, msg: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    std.debug.print("[line {d}] Error", .{token.line});

    switch (token.type) {
        .ERROR => {}, // Do nothing
        .EOF => std.debug.print("at end", .{}),
        else => std.debug.print(" at '{s}'", .{token.str}),
    }

    std.debug.print(": {s}\n", .{msg});
    parser.had_error = true;
}

inline fn errorAtCurrent(msg: []const u8) void {
    errorAt(&parser.current, msg);
}

inline fn errorPrev(msg: []const u8) void {
    errorAt(&parser.previous, msg);
}

fn consume(ty: TT, msg: []const u8) void {
    if (parser.current.type == ty) {
        advance();
        return;
    }

    errorAtCurrent(msg);
}

// TODO: refactor into Compiler struct
fn endCompiler() !*ObjFunction {
    try emitReturn();
    const fun = current.?.function.?;
    if (!parser.had_error) @import("debug.zig").disassembleChunk(currentChunk(), if (fun.name) |name| name.ptr[0..name.len] else "<script>");

    current = current.?.enclosing;
    return fun;
}

fn variable(can_assign: bool) !void {
    try namedVariable(parser.previous, can_assign);
}

fn namedVariable(name: Token, can_assign: bool) !void {
    var get_op: Op = undefined;
    var set_op: Op = undefined;

    var arg: ?u8 = current.?.resolveLocal(name.str);
    if (arg) |_| {
        get_op = Op.GET_LOCAL;
        set_op = Op.SET_LOCAL;
    } else if (current.?.resolveUpvalue(name.str)) |uv| {
        arg = uv;
        get_op = Op.GET_UPVALUE;
        set_op = Op.SET_UPVALUE;
    } else {
        arg = try identifierConstant(&name);
        get_op = Op.GET_GLOBAL;
        set_op = Op.SET_GLOBAL;
    }
    if (can_assign and match(.EQUAL)) {
        try expression();
        try emitBytes(set_op, arg.?);
    } else {
        try emitBytes(get_op, arg.?);
    }
}

fn dot(can_assign: bool) !void {
    consume(.IDENTIFIER, "Expect property name after '.'.");
    const name = try identifierConstant(&parser.previous);
    if (can_assign and match(.EQUAL)) {
        try expression();
        try emitBytes(Op.SET_PROPERTY, name);
    } else {
        try emitBytes(Op.GET_PROPERTY, name);
    }
}

fn string(_: bool) !void {
    try emitConstant(Value.obj(try copyString(parser.previous.str[1 .. parser.previous.str.len - 1])));
}

fn literal(_: bool) !void {
    switch (parser.previous.type) {
        .FALSE => try emitOp(.FALSE),
        .NIL => try emitOp(.NIL),
        .TRUE => try emitOp(.TRUE),
        else => unreachable,
    }
}

fn makeConstant(val: Value) !u8 {
    return currentChunk().addConstant(val) catch |err| {
        errorPrev("Too many constants in one chunk.");
        return err;
    };
}

fn number(_: bool) !void {
    const value = std.fmt.parseFloat(f64, parser.previous.str) catch {
        errorPrev("Could not parse number.");
        return;
    };

    try emitConstant(Value{ .number = value });
}

inline fn emitConstant(val: Value) !void {
    try emitBytes(.CONSTANT, try makeConstant(val));
}

fn patchJump(offset: usize) !void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump = currentChunk().count() - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        errorPrev("Too much code to jump over.");
    }

    currentChunk().code.items[offset] = @truncate(jump >> 8);
    currentChunk().code.items[offset + 1] = @truncate(jump);
}

fn emitByte(byte: u8) !void {
    ////std.debug.print("{s} {d}\n", .{parser.previous.str, parser.previous.line});
    try currentChunk().write(byte, parser.previous.line);
}

inline fn emitOp(op: Op) !void {
    try emitByte(@intFromEnum(op));
}

inline fn emitBytes(byte1: Op, byte2: u8) !void {
    try emitByte(@intFromEnum(byte1));
    try emitByte(byte2);
}

inline fn emitReturn() !void {
    try emitOp(Op.NIL);
    try emitOp(.RETURN);
}

fn emitJump(op: Op) !usize {
    try emitOp(op);
    try emitByte(0xff);
    try emitByte(0xff);
    return currentChunk().count() - 2;
}

fn emitLoop(loopStart: usize) !void {
    try emitOp(.LOOP);

    const offset = currentChunk().count() - loopStart + 2;
    if (offset > std.math.maxInt(u16)) errorPrev("Loop body too large.");

    // std.debug.print("{d}\n", .{offset >> 8});
    // std.debug.print("{d}\n", .{offset & 0xff});
    try emitByte(@truncate(offset >> 8));
    try emitByte(@truncate(offset));
}

fn advance() void {
    parser.previous = parser.current;
    while (true) {
        parser.current = try scanToken();
        if (parser.current.type != .ERROR) break;
        errorAtCurrent(parser.current.str);
    }
    //std.debug.print("{s} {d}\n", .{parser.previous.str, parser.previous.line});
}

fn parsePrecedence(precedence: Precendence) !void {
    advance();
    if (getRule(parser.previous.type).prefix) |prefix_rule| {
        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precendence.ASSIGNMENT);
        try prefix_rule(can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.type).precedence)) {
            advance();
            const infix_rule = getRule(parser.previous.type).infix.?;
            try infix_rule(can_assign);
        }

        if (can_assign and match(.EQUAL)) errorPrev("Invalid assignment target.");
    } else {
        errorPrev("Expect expression.");
        return;
    }
}

inline fn getRule(token: TT) ParseRule {
    return rules[@intFromEnum(token)];
}

inline fn beginScope() void {
    current.?.scope_depth += 1;
}

fn endScope() !void {
    if (current) |curr| {
        curr.scope_depth -= 1;
        while (curr.local_count > 0 and curr.locals[curr.local_count - 1].depth > curr.scope_depth) : (curr.local_count -= 1) {
            if (curr.locals[curr.local_count - 1].is_captured) {
                try emitOp(Op.CLOSE_UPVALUE);
            } else {
                try emitOp(Op.POP);
            }
        }
    } else unreachable;
}

fn block() void {
    while (!check(.RIGHT_BRACE) and !check(.EOF)) {
        declaration() catch synchronize();
    }

    consume(.RIGHT_BRACE, "Expect '}' after block.");
}

fn grouping(_: bool) !void {
    try expression();
    consume(.RIGHT_PAREN, "Expect ')' after expression.");
}

fn expression() !void {
    try parsePrecedence(.ASSIGNMENT);
}

fn unary(_: bool) !void {
    const op_type = parser.previous.type;
    // Compile the operand
    try parsePrecedence(.UNARY);

    switch (op_type) {
        .MINUS => try emitOp(.NEGATE),
        .BANG => try emitOp(.NOT),
        else => unreachable,
    }
}

inline fn nextPrecedence(precedence: Precendence) Precendence {
    return @enumFromInt(@intFromEnum(precedence) + 1);
}

fn binary(_: bool) !void {
    const operator_type = parser.previous.type;
    const rule = getRule(operator_type);
    try parsePrecedence(nextPrecedence(rule.precedence));

    switch (operator_type) {
        .BANG_EQUAL => try emitBytes(.EQUAL, @intFromEnum(Op.NOT)),
        .EQUAL_EQUAL => try emitOp(.EQUAL),
        .GREATER => try emitOp(.GREATER),
        .GREATER_EQUAL => try emitBytes(.LESS, @intFromEnum(Op.NOT)),
        .LESS => try emitOp(.LESS),
        .LESS_EQUAL => try emitBytes(.GREATER, @intFromEnum(Op.NOT)),
        .PLUS => try emitOp(.ADD),
        .MINUS => try emitOp(.SUBTRACT),
        .STAR => try emitOp(.MULTIPLY),
        .SLASH => try emitOp(.DIVIDE),
        else => unreachable,
    }
}

inline fn check(tt: TT) bool {
    return parser.current.type == tt;
}

fn statement() anyerror!void {
    if (match(.PRINT)) {
        try printStatement();
    } else if (match(.IF)) {
        try ifStatement();
    } else if (match(.RETURN)) {
        try returnStatement();
    } else if (match(.WHILE)) {
        try whileStatement();
    } else if (match(.FOR)) {
        try forStatement();
    } else if (match(.LEFT_BRACE)) {
        beginScope();
        block();
        try endScope();
    } else {
        try expressionStatement();
    }
}

fn returnStatement() !void {
    if (current.?.type == .script) {
        errorPrev("Can't return from top-level code.");
    }
    if (match(.SEMICOLON)) {
        try emitReturn();
    } else {
        try expression();
        consume(.SEMICOLON, "Expect ';' after return value.");
        try emitOp(.RETURN);
    }
}

fn expressionStatement() !void {
    try expression();
    consume(.SEMICOLON, "Expect ';' after expression.");
    try emitOp(.POP);
}

fn printStatement() !void {
    try expression();
    consume(.SEMICOLON, "Expect ';' after value.");
    try emitOp(.PRINT);
}

fn ifStatement() !void {
    consume(.LEFT_PAREN, "Expect '(' after 'if'.");
    try expression();
    consume(.RIGHT_PAREN, "Expect ')' after condition.");

    const then_jump = try emitJump(.JUMP_IF_FALSE);
    try emitOp(Op.POP);
    try statement();
    const else_jump = try emitJump(.JUMP);

    try patchJump(then_jump);
    try emitOp(Op.POP);

    if (match(.ELSE)) try statement();
    try patchJump(else_jump);
}

fn whileStatement() !void {
    const loop_start = currentChunk().count();
    consume(.LEFT_PAREN, "Expect '(' after 'while'.");
    try expression();
    consume(.RIGHT_PAREN, "Expect ')' after condition.");

    const exit_jump = try emitJump(.JUMP_IF_FALSE);
    try emitOp(.POP);
    try statement();
    try emitLoop(loop_start);

    try patchJump(exit_jump);
    try emitOp(.POP);
}

fn forStatement() !void {
    beginScope();
    consume(.LEFT_PAREN, "Expect '(' after 'for'.");
    if (!match(.SEMICOLON)) {
        // no initializer
        if (match(.VAR)) {
            try varDeclaration();
        } else {
            try expressionStatement();
        }
    }

    var loop_start = currentChunk().count();
    var exit_jump: ?usize = null;

    if (!match(.SEMICOLON)) {
        try expression();
        consume(.SEMICOLON, "Expect ';' after loop condition.");

        exit_jump = try emitJump(.JUMP_IF_FALSE);
        try emitOp(.POP);
    }

    if (!match(.RIGHT_PAREN)) {
        const body_jump = try emitJump(.JUMP);
        const inc_start = currentChunk().count();
        try expression();
        try emitOp(.POP);
        consume(.RIGHT_PAREN, "Expect ')' after for clauses.");

        try emitLoop(loop_start);
        loop_start = inc_start;
        try patchJump(body_jump);
    }

    try statement();
    try emitLoop(loop_start);

    if (exit_jump) |jump| {
        try patchJump(jump);
        try emitOp(.POP);
    }

    try endScope();
}

fn identifierConstant(name: *const Token) !u8 {
    return makeConstant(Value.obj(try copyString(name.str)));
}

fn parseVariable(msg: []const u8) !u8 {
    consume(.IDENTIFIER, msg);

    declareVariable();
    if (current.?.scope_depth > 0) return 0;

    return identifierConstant(&parser.previous);
}

fn declareVariable() void {
    if (current.?.scope_depth == 0) return;

    const name = parser.previous.str;
    addLocal(name);
}

fn identifiersEqual(a: []const u8, b: []const u8) bool {
    return a.len == b.len and std.mem.order(u8, a, b) == .eq;
}

// TODO: factor out error functions into an actual error union so we can propogate
fn addLocal(name: []const u8) void {
    if (current) |curr| {
        if (curr.local_count == 256) {
            errorPrev("Too many local variables in function.");
            return;
        }
        var local = &curr.locals[curr.local_count];
        curr.local_count += 1;
        local.name = name;
        local.depth = -1;
        local.is_captured = false;
    } else unreachable;
}

inline fn markInitialized() void {
    if (current) |curr| {
        if (curr.scope_depth == 0) return;
        curr.locals[curr.local_count - 1].depth = curr.scope_depth;
    } else unreachable;
}

fn defineVariable(global: u8) !void {
    if (current.?.scope_depth > 0) {
        markInitialized();
        return;
    }
    try emitBytes(.DEFINE_GLOBAL, global);
}

fn and_(_: bool) !void {
    const end_jump = try emitJump(.JUMP_IF_FALSE);
    try emitOp(.POP);
    try parsePrecedence(.AND);
    try patchJump(end_jump);
}

fn or_(_: bool) !void {
    const else_jump = try emitJump(.JUMP_IF_FALSE);
    const end_jump = try emitJump(.JUMP);

    try patchJump(else_jump);
    try emitOp(.POP);

    try parsePrecedence(.OR);
    try patchJump(end_jump);
}

fn call(_: bool) !void {
    const arg_count = try argumentList();
    try emitBytes(Op.CALL, arg_count);
}

fn argumentList() !u8 {
    var arg_count: u8 = 0;
    if (!check(.RIGHT_PAREN)) {
        while (true) {
            try expression();
            if (arg_count == 255) {
                errorPrev("Can't have more than 255 arguments.");
                break;
            }
            arg_count += 1;
            if (!match(.COMMA)) break;
        }
    }
    consume(.RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

fn varDeclaration() !void {
    const global: u8 = try parseVariable("Expect variable name.");
    if (match(.EQUAL)) {
        try expression();
    } else {
        try emitOp(.NIL);
    }

    consume(.SEMICOLON, "Expect ';' after variable declaration.");
    try defineVariable(global);
}

fn function(ty: FunctionType) !void {
    var compiler = Compiler{};
    try compiler.init(ty);
    beginScope();

    consume(.LEFT_PAREN, "Expect '(' after function name.");
    if (!check(.RIGHT_PAREN)) {
        while (true) {
            const arity_inc = @addWithOverflow(current.?.function.?.arity, 1);
            if (arity_inc[1] == 1) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            current.?.function.?.arity = arity_inc[0];
            const constant = try parseVariable("Expect parameter name.");
            try defineVariable(constant);
            if (!match(.COMMA)) break;
        }
    }
    consume(.RIGHT_PAREN, "Expect ')' after parameters.");
    consume(.LEFT_BRACE, "Expect '{' before function body.");
    block();
    const fun = try endCompiler();

    try emitBytes(.CLOSURE, try makeConstant(Value.obj(fun)));

    // enumerate upvalues
    for (compiler.upvalues[0..fun.upvalue_count]) |uv| {
        try emitByte(if (uv.is_local) 1 else 0);
        try emitByte(uv.index);
    }
}

fn classDeclaration() !void {
    consume(.IDENTIFIER, "Expect class name.");
    const name_constant: u8 = try identifierConstant(&parser.previous);
    declareVariable();
    try emitBytes(Op.CLASS, name_constant);
    try defineVariable(name_constant);

    consume(.LEFT_BRACE, "Expect '{' before class body.");

    // methods

    consume(.RIGHT_BRACE, "Expect '}' after class body.");
}

fn funDeclaration() !void {
    const global = try parseVariable("Expect function name.");
    markInitialized();
    try function(.function);
    try defineVariable(global);
}

fn declaration() !void {
    if (match(.CLASS)) {
        try classDeclaration();
    } else if (match(.FUN)) {
        try funDeclaration();
    } else if (match(.VAR)) {
        try varDeclaration(); // catch synchronize();
    } else {
        try statement(); // catch synchronize();
    }
}

fn synchronize() void {
    parser.panic_mode = false;

    while (parser.current.type != .EOF) : (advance()) {
        if (parser.previous.type == .SEMICOLON) return;
        switch (parser.current.type) {
            .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
            else => continue,
        }
    }
}

pub fn compile(source: []const u8) !*ObjFunction {
    Scanner.init(source);
    var compiler = Compiler{};
    try compiler.init(.script);
    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(.EOF)) {
        declaration() catch {
            synchronize();
            //return InterpretError.CompileError;
        };
    }

    consume(.EOF, "Expect end of expression.");

    return endCompiler() catch {
        return InterpretError.CompileError;
    };
}

fn match(tt: TT) bool {
    if (!check(tt)) return false;
    advance();
    return true;
}
