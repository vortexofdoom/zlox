const std = @import("std");
const Scanner = @import("scanner.zig");
const chunk_ = @import("chunk.zig");
const Value = @import("value.zig").Value;
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

var parser = Parser{
    .current = undefined,
    .previous = undefined,
    .had_error = false,
    .panic_mode = false,
};

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

const ParseFn = *const fn () anyerror!void;
const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precendence,
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
    makeRule(null, null, .NONE), // BANG
    makeRule(null, null, .NONE), // BANG_EQUAL
    makeRule(null, null, .NONE), // EQUAL
    makeRule(null, null, .NONE), // EQUAL_EQUAL
    makeRule(null, null, .NONE), // GREATER
    makeRule(null, null, .NONE), // GREATER_EQUAL
    makeRule(null, null, .NONE), // LESS
    makeRule(null, null, .NONE), // LESS_EQUAL
    makeRule(null, null, .NONE), // IDENTIFIER
    makeRule(null, null, .NONE), // STRING
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
        advance() catch {};
        return;
    }

    errorAtCurrent(msg);
}

fn endCompiler() !void {
    try emitReturn();
}

fn literal() !void {
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

fn number() !void {
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

fn advance() !void {
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
    try advance();
    if (getRule(parser.previous.type).prefix) |prefix_rule| {
        try prefix_rule();

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.type).precedence)) {
            try advance();
            const infix_rule = getRule(parser.previous.type).infix.?;
            try infix_rule();
        }
    } else {
        errorPrev("Expect expression.");
        return;
    }
}

inline fn getRule(token: TT) ParseRule {
    return rules[@intFromEnum(token)];
}

fn grouping() !void {
    try expression();
    consume(.RIGHT_PAREN, "Expect ')' after expression.");
}

fn expression() !void {
    try parsePrecedence(.ASSIGNMENT);
}

fn unary() !void {
    const op_type = parser.previous.type;
    // Compile the operand
    try parsePrecedence(.UNARY);

    switch (op_type) {
        .MINUS => try emitOp(.NEGATE),
        else => unreachable,
    }
}

inline fn nextPrecedence(precedence: Precendence) Precendence {
    return @enumFromInt(@intFromEnum(precedence) + 1);
}

fn binary() !void {
    const operator_type = parser.previous.type;
    const rule = getRule(operator_type);
    try parsePrecedence(nextPrecedence(rule.precedence));

    switch (operator_type) {
        .PLUS => try emitOp(.ADD),
        .MINUS => try emitOp(.SUBTRACT),
        .STAR => try emitOp(.MULTIPLY),
        .SLASH => try emitOp(.DIVIDE),
        else => unreachable,
    }
}

pub fn compile(source: []const u8, chunk: *Chunk) !void {
    Scanner.init(source);
    curr_chunk = chunk;

    parser.had_error = false;
    parser.panic_mode = false;

    advance() catch |err| {
        std.io.getStdErr().writer().print("{any}", .{err});
    };

    expression() catch {};
    consume(.EOF, "Expect end of expression.");

    endCompiler() catch {
        return @import("vm.zig").InterpretError.CompileError;
    };
}
