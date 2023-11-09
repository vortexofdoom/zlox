const std = @import("std");
const ascii = std.ascii;

start: [*]const u8,
current: [*]const u8,
line: usize,

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,
    // keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF,
};

pub const Token = struct {
    type: TokenType,
    str: []const u8,
    line: usize,
};

pub var scanner: @This() = .{
    .start = undefined,
    .current = undefined,
    .line = 1,
};

pub fn init(source: []const u8) void {
    scanner.start = source.ptr;
    scanner.current = source.ptr;
    scanner.line = 1;
}

inline fn isAtEnd() bool {
    return scanner.current[0] == 0;
}

fn makeToken(ty: TokenType) Token {
    return .{
        .type = ty,
        .str = scanner.start[0..@intFromPtr(scanner.current) - @intFromPtr(scanner.start)],
        .line = scanner.line,
    };
}

fn advance() u8 {
    const res = scanner.current[0];
    scanner.current += 1;
    return res;
}

fn match(expected: u8) bool {
    if (isAtEnd() or scanner.current[0] != expected) return false;
    scanner.current += 1;
    return true;
}

fn skipWhitespace() void {
    while (true) : (scanner.current += 1) {
        switch (peek()) {
            '/' => {
                if (scanner.current[1] == '/') {
                    while (peek() != '\n' and !isAtEnd()) {
                        scanner.current += 1;
                    }
                } else return;
            },
            ' ', '\t' => continue,
            '\n' => scanner.line += 1,
            else => return,
        }
    }
}

inline fn peek() u8 {
    return scanner.current[0];
}

fn string() !Token {
    while (peek() != '"' and !isAtEnd()) : (scanner.current += 1) {
        if (peek() == '\n') scanner.line += 1;
    }

    if (isAtEnd()) return .{
        .type = .ERROR,
        .str = "Unterminated string.",
        .line = scanner.line,
    };

    scanner.current += 1;
    return makeToken(TokenType.STRING);
}

fn isAlpha(c: u8) bool {
    return switch (c) {
        'A'...'Z', 'a'...'z', '_' => true,
        else => false,
    };
}

fn identifier() Token {
    while (isAlpha(peek()) or ascii.isDigit(peek())) {
        scanner.current += 1;
    }

    return switch (scanner.start[0]) {
        'a' => checkKeyword(1, "nd", .AND),
        'c' => checkKeyword(1, "lass", .CLASS),
        'e' => checkKeyword(1, "lse", .ELSE),
        'f' => {
            return if (current().len > 1) {
                return switch (scanner.start[1]) {
                    'a' => checkKeyword(2, "lse", .FALSE),
                    'o' => checkKeyword(2, "r", .FOR),
                    'u' => checkKeyword(2, "n", .FUN),
                    else => makeToken(.IDENTIFIER),
                };
            } else makeToken(.IDENTIFIER);
        },
        'i' => checkKeyword(1, "f", .IF),
        'n' => checkKeyword(1, "il", .NIL),
        'o' => checkKeyword(1, "r", .OR),
        'p' => checkKeyword(1, "rint", .PRINT),
        'r' => checkKeyword(1, "eturn", .RETURN),
        's' => checkKeyword(1, "uper", .SUPER),
        't' => {
            return if (current().len > 1) {
                return switch (scanner.start[1]) {
                    'h' => checkKeyword(2, "is", .THIS),
                    'u' => checkKeyword(2, "ue", .TRUE),
                    else => makeToken(.IDENTIFIER),
                };
            } else makeToken(.IDENTIFIER);
        },
        'v' => checkKeyword(1, "ar", .VAR),
        'w' => checkKeyword(1, "hile", .WHILE),
        else => makeToken(.IDENTIFIER),
    };
}

inline fn current() []const u8 {
    return scanner.start[0..currLen()];
}

inline fn currLen() usize {
    return @intFromPtr(scanner.current) - @intFromPtr(scanner.start);
}

fn checkKeyword(start: usize, rest: []const u8, ty: TokenType) Token {
    return if (current().len == rest.len and std.mem.order(u8, current()[start..], rest) == .eq)
        makeToken(ty) 
    else 
        makeToken(.IDENTIFIER);
}

fn number() Token {
    while (ascii.isDigit(peek())) {
        scanner.current += 1;
    }

    if (peek() == '.' and ascii.isDigit(scanner.current[1])) scanner.current += 1;

    while (ascii.isDigit(peek())) {
        scanner.current += 1;
    }

    return makeToken(TokenType.NUMBER);
}

pub fn scanToken() !Token {
    skipWhitespace();

    //std.debug.print("start: {x}, current: {x}", .{@intFromPtr(scanner.start), @intFromPtr(scanner.current)});
    scanner.start = scanner.current;
    if (isAtEnd()) return Token {
        .type = .EOF,
        .str = "",
        .line = scanner.line,
    };

    const c: u8 = advance();

    if (isAlpha(c)) return identifier();
    if (ascii.isDigit(c)) return number();

    switch (c) {
        '(' => return makeToken(TokenType.LEFT_PAREN),
        ')' => return makeToken(TokenType.RIGHT_PAREN),
        '{' => return makeToken(TokenType.LEFT_BRACE),
        '}' => return makeToken(TokenType.RIGHT_BRACE),
        ';' => return makeToken(TokenType.SEMICOLON),
        ',' => return makeToken(TokenType.COMMA),
        '.' => return makeToken(TokenType.DOT),
        '-' => return makeToken(TokenType.MINUS),
        '+' => return makeToken(TokenType.PLUS),
        '/' => return makeToken(TokenType.SLASH),
        '*' => return makeToken(TokenType.STAR),
        '!' => return makeToken(if (match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
        '=' => return makeToken(if (match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
        '<' => return makeToken(if (match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
        '>' => return makeToken(if (match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
        '"' => return try string(),
        else => return .{
            .type = .ERROR,
            .str = "Unexpected character.",
            .line = scanner.line,
        },
    }
}