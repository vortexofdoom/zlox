const std = @import("std");
const vm = @import("vm.zig");
const InterpretError = vm.InterpretError;
const Scanner = @import("scanner.zig");
const chunk_ = @import("chunk.zig");
const Value = @import("value.zig").Value;
const object = @import("object.zig");
const copyString = object.copyString;
const allocateString = object.takeString;
const allocateObject = object.allocateObject;
const Obj = object.Obj;
const ObjString = object.ObjString;
const ObjType = object.ObjType;
const Op = chunk_.Op;
const Chunk = chunk_.Chunk;
const scanToken = Scanner.scanToken;
const Token = Scanner.Token;
const TT = Scanner.TokenType;

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
};

const Compiler = struct {
    locals: [256]Local = .{Local{}} ** 256,
    local_count: u32 = 0,
    scope_depth: u32 = 0,

    fn init(self: *Compiler) void {
        current = self;
    }

    fn resolveLocal(self: *Compiler, name: []const u8) isize {
        var i = self.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals[i];
            if (identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    errorPrev("Can't read local variable in its own initializer.");
                }
                return i;
            }
        }

        return -1;
    }
};

var parser = Parser{
    .current = undefined,
    .previous = undefined,
    .had_error = false,
    .panic_mode = false,
};

var current: *Compiler = undefined;

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

const rules = [_]ParseRule{
    makeRule(grouping, null, .NONE), // LEFT_PAREN
    makeRule(null, null, .NONE), // RIGHT_PAREN
    makeRule(null, null, .NONE), // LEFT_BRACE
    makeRule(null, null, .NONE), // RIGHT_BRACE
    makeRule(null, null, .NONE), // COMMA
    makeRule(null, null, .NONE), // DOT
    makeRule(unary, binary, .TERM), // MINUS
    makeRule(null, binary, .TERM), // PLUS
    makeRule(null, null, .NONE), // SEMICOLON
    makeRule(null, binary, .FACTOR), // SLASH
    makeRule(null, binary, .FACTOR), // STAR
    makeRule(unary, null, .NONE), // BANG
    makeRule(null, binary, .COMPARISON), // BANG_EQUAL
    makeRule(null, null, .NONE), // EQUAL
    makeRule(null, binary, .COMPARISON), // EQUAL_EQUAL
    makeRule(null, binary, .COMPARISON), // GREATER
    makeRule(null, binary, .COMPARISON), // GREATER_EQUAL
    makeRule(null, binary, .COMPARISON), // LESS
    makeRule(null, binary, .COMPARISON), // LESS_EQUAL
    makeRule(variable, null, .NONE), // IDENTIFIER
    makeRule(string, null, .NONE), // STRING
    makeRule(number, null, .NONE), // NUMBER
    makeRule(null, null, .NONE), // AND
    makeRule(null, null, .NONE), // CLASS
    makeRule(null, null, .NONE), // ELSE
    makeRule(literal, null, .NONE), // FALSE
    makeRule(null, null, .NONE), // FOR
    makeRule(null, null, .NONE), // FUN
    makeRule(null, null, .NONE), // IF
    makeRule(literal, null, .NONE), // NIL
    makeRule(null, null, .NONE), // OR
    makeRule(null, null, .NONE), // PRINT
    makeRule(null, null, .NONE), // RETURN
    makeRule(null, null, .NONE), // SUPER
    makeRule(null, null, .NONE), // THIS
    makeRule(literal, null, .NONE), // TRUE
    makeRule(null, null, .NONE), // VAR
    makeRule(null, null, .NONE), // WHILE
    makeRule(null, null, .NONE), // ERROR
    makeRule(null, null, .NONE), // EOF
};

var curr_chunk: *Chunk = undefined;

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

fn endCompiler() !void {
    try emitReturn();
}

fn variable(can_assign: bool) !void {
    try namedVariable(parser.previous, can_assign);
}

fn namedVariable(name: Token, can_assign: bool) !void {
    var get_op: Op = undefined;
    var set_op: Op = undefined;

    var arg: isize = current.resolveLocal(name.str);
    if (arg != -1) {
        get_op = Op.GET_LOCAL;
        set_op = Op.SET_LOCAL;
    } else {
        arg = try identifierConstant(&name);
        get_op = Op.GET_GLOBAL;
        set_op = Op.SET_GLOBAL;
    }
    if (can_assign and match(.EQUAL)) {
        try expression();
        try emitBytes(set_op, @truncate(@as(usize, @bitCast(arg))));
    } else {
        try emitBytes(get_op, @truncate(@as(usize, @bitCast(arg))));
    }
}

fn string(_: bool) !void {
    try emitConstant(Value.obj(try copyString(parser.previous.str[1 .. parser.previous.str.len - 1], curr_chunk.allocator)));
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
    return curr_chunk.addConstant(val) catch |err| {
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

fn emitByte(byte: u8) !void {
    try curr_chunk.write(byte, parser.previous.line);
}

inline fn emitOp(op: Op) !void {
    try emitByte(@intFromEnum(op));
}

inline fn emitBytes(byte1: Op, byte2: u8) !void {
    try emitByte(@intFromEnum(byte1));
    try emitByte(byte2);
}

inline fn emitReturn() !void {
    try emitOp(.RETURN);
}

fn advance() void {
    parser.previous = parser.current;
    while (true) {
        parser.current = scanToken() catch {
            errorAtCurrent(parser.current.str);
            continue;
        };
        break;
    }
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
    current.scope_depth += 1;
}

fn endScope() !void {
    current.scope_depth -= 1;
    while (current.local_count > 0 and current.locals[current.local_count - 1].depth > current.scope_depth) {
        try emitOp(Op.POP);
        current.local_count -= 1;
    }
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

fn statement() !void {
    if (match(.PRINT)) {
        try printStatement();
    } else if (match(.LEFT_BRACE)) {
        beginScope();
        block();
        try endScope();
    } else {
        try expressionStatement();
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

fn identifierConstant(name: *const Token) !u8 {
    return makeConstant(Value.obj(try copyString(name.str, vm.Vm.allocator)));
}

fn parseVariable(msg: []const u8) !u8 {
    consume(.IDENTIFIER, msg);

    declareVariable();
    if (current.scope_depth > 0) return 0;

    return identifierConstant(&parser.previous);
}

fn declareVariable() void {
    if (current.scope_depth == 0) return;

    const name = parser.previous.str;
    addLocal(name);
}

fn identifiersEqual(a: []const u8, b: []const u8) bool {
    return a.len == b.len and std.mem.order(u8, a, b) == .eq;
}

// TODO: factor out error functions into an actual error union so we can propogate
fn addLocal(name: []const u8) void {
    if (current.local_count == 256) {
        errorPrev("Too many local variables in function.");
        return;
    }
    var local = &current.locals[current.local_count];
    current.local_count += 1;
    local.name = name;
    local.depth = -1;
    local.depth = @as(isize, current.scope_depth);
}

inline fn markInitialized() void {
    current.locals[current.local_count - 1].depth = current.scope_depth;
}

fn defineVariable(global: u8) !void {
    if (current.scope_depth > 0) {
        markInitialized();
        return;
    }
    try emitBytes(.DEFINE_GLOBAL, global);
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

fn declaration() !void {
    if (match(.VAR)) {
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

pub fn compile(source: []const u8, chunk: *Chunk) !void {
    Scanner.init(source);
    var compiler = Compiler{};
    compiler.init();
    curr_chunk = chunk;
    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(.EOF)) {
        //std.debug.print("hi", .{});
        declaration() catch {
            synchronize();
            //return InterpretError.CompileError;
        };
    }

    consume(.EOF, "Expect end of expression.");

    endCompiler() catch {
        return InterpretError.CompileError;
    };
}

fn match(tt: TT) bool {
    if (!check(tt)) return false;
    //std.debug.print("hi", .{});
    advance();
    return true;
}
