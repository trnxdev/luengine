// 3.1 - Lexical Conventions (https://www.lua.org/manual/5.4/manual.html#3.1)
const std = @import("std");

pub const Token = struct {
    kind: Kind,
    lexeme: []const u8,
    loc: Location,

    pub inline fn init(kind: Kind, lexeme: []const u8, loc: Location) @This() {
        return .{
            .kind = kind,
            .lexeme = lexeme,
            .loc = loc,
        };
    }

    pub const Kind = enum(u8) {
        EOF = 0,

        Plus,
        Minus,
        Star,
        Percent,
        Caret,
        Hash,
        Ampersand,
        Pipe,
        LeftParenthesis,
        RightParenthesis,
        LeftCurlyBrace,
        RightCurlyBrace,
        LeftBracket,
        RightBracket,
        Semicolon,
        Comma,
        Equal,
        EqualEqual,
        Slash,
        Less,
        LessLess,
        LessEqual,
        Greater,
        GreaterGreater,
        GreaterEqual,
        Colon,
        ColonColon,
        Dot,
        DotDot,
        DotDotDot,
        Tilde,
        TildeEqual,

        // Names (also called identifiers) in Lua can be any string of Latin letters,
        // Arabic-Indic digits, and underscores, not beginning with a digit
        // and not being a reserved word.
        // Identifiers are used to name variables, table fields, and labels.
        Name,
        Number,
        String,

        // NOTE: Every Token.Kind after 120 is a Keyword
        // The following keywords are reserved and cannot be used as names:
        @"and" = 120,
        @"break" = 121,
        do = 122,
        @"else" = 123,
        end = 125,
        false = 126,
        @"for" = 127,
        function = 128,
        goto = 129,
        @"if" = 130,
        in = 131,
        local = 132,
        nil = 133,
        not = 134,
        @"or" = 135,
        repeat = 136,
        @"return" = 137,
        then = 138,
        true = 139,
        until = 140,
        @"while" = 141,

        inline fn is_keyword(self: @This()) bool {
            return @intFromEnum(self) >= 120;
        }
    };

    pub const Location = struct {
        start: usize = 0,
        offset: usize = 0,
        line: usize = 1,
        line_offset: usize = 0,
    };
};

allocator: std.mem.Allocator,
source: []const u8,
loc: Token.Location,

pub inline fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
    return .{
        .source = source,
        .loc = Token.Location{},
        .allocator = allocator,
    };
}

inline fn skipWhitespace(self: *@This()) void {
    o: while (true) {
        // Lua is a free-form language.
        // It ignores spaces and comments between lexical elements (tokens),
        // except as delimiters between two tokens.
        // In source code, Lua recognizes as spaces
        switch (self.peek()) {
            ' ', // the standard ASCII whitespace characters space,
            std.ascii.control_code.ff, // form feed,
            '\n', // newline,
            std.ascii.control_code.cr, // carriage return,
            std.ascii.control_code.ht, // horizontal tab,
            std.ascii.control_code.vt, // vertical tab.
            => {
                if (self.peek() == '\n') {
                    self.loc.line += 1;
                    self.loc.line_offset = 0;
                }

                _ = self.advance();
            },
            '-' => {
                if (self.match("--[[")) {
                    while (!self.match("]]") and !self.isEOF()) {
                        if (self.peek() == '\n')
                            self.loc.line += 1;

                        _ = self.advance();
                    }
                } else if (self.match("--") and !self.isEOF()) {
                    while (self.peek() != '\n') {
                        _ = self.advance();
                    }
                    self.loc.line += 1;
                } else {
                    break :o;
                }
            },
            else => break :o,
        }
    }

    return;
}

// FIXME: Leaky, maybe use arena
const ScanArrayError = std.mem.Allocator.Error || ScanError;
pub fn scanArray(self: *@This()) ScanArrayError![]Token {
    var tokens = std.ArrayListUnmanaged(Token){};
    errdefer tokens.deinit(self.allocator);

    o: while (true) {
        const token = try self.scan();
        try tokens.append(self.allocator, token);

        if (tokens.getLast().kind == .EOF)
            break :o;
    }

    return try tokens.toOwnedSlice(self.allocator);
}

const ScanError = error{
    UnrecognizedCharacter,
} || StringLiteralTokenError || NumberLiteralTokenError;
pub fn scan(self: *@This()) ScanError!Token {
    self.skipWhitespace();

    if (self.isEOF())
        return Token.init(.EOF, "<EOF>", self.loc);

    self.loc.start = self.loc.offset;
    const c = self.advance();

    return switch (c) {
        '+' => Token.init(.Plus, "+", self.loc),
        '-' => Token.init(.Minus, "-", self.loc),
        '*' => Token.init(.Star, "*", self.loc),
        '%' => Token.init(.Percent, "%", self.loc),
        '^' => Token.init(.Caret, "^", self.loc),
        '#' => Token.init(.Hash, "#", self.loc),
        '&' => Token.init(.Ampersand, "&", self.loc),
        '|' => Token.init(.Pipe, "|", self.loc),
        '(' => Token.init(.LeftParenthesis, "(", self.loc),
        ')' => Token.init(.RightParenthesis, ")", self.loc),
        '{' => Token.init(.LeftCurlyBrace, "{", self.loc),
        '}' => Token.init(.RightCurlyBrace, "}", self.loc),
        '[' => if (self.match("["))
            try self.stringLiteralToken(.Brackets)
        else
            Token.init(.LeftBracket, "[", self.loc),
        ']' => Token.init(.RightBracket, "]", self.loc),
        ';' => Token.init(.Semicolon, ";", self.loc),
        ',' => Token.init(.Comma, ",", self.loc),
        '/' => Token.init(.Slash, "/", self.loc),
        // -- One or Two Character Tokens
        // =, ==
        '=' => if (self.match("="))
            Token.init(.EqualEqual, "==", self.loc)
        else
            Token.init(.Equal, "=", self.loc),
        // ~, ~=
        '~' => if (self.match("="))
            Token.init(.TildeEqual, "~=", self.loc)
        else
            Token.init(.Tilde, "~", self.loc),
        // :, ::
        ':' => if (self.match(":"))
            Token.init(.ColonColon, "::", self.loc)
        else
            Token.init(.Colon, ":", self.loc),
        // -- One, Two or Three Character Tokens
        // ., .., ...
        '.' => if (self.match(".."))
            Token.init(.DotDotDot, "...", self.loc)
        else if (self.match("."))
            Token.init(.DotDot, "..", self.loc)
        else
            Token.init(.Dot, ".", self.loc),
        // <, <<, <=
        '<' => if (self.match("="))
            Token.init(.LessEqual, "<=", self.loc)
        else if (self.match("<"))
            Token.init(.LessLess, "<<", self.loc)
        else
            Token.init(.Less, "<", self.loc),
        // >, >>, >=
        '>' => if (self.match("="))
            Token.init(.GreaterEqual, ">=", self.loc)
        else if (self.match(">"))
            Token.init(.GreaterGreater, ">>", self.loc)
        else
            Token.init(.Greater, ">", self.loc),
        // I dont know what im doing :D
        '0'...'9' => try self.numberLiteralToken(),
        // See comment at Token.Kind Identifier
        'a'...'z',
        'A'...'Z',
        '_',
        => {
            o: while (true) {
                if (self.isEOF())
                    break :o;

                switch (self.peek()) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => _ = self.advance(),
                    else => break :o,
                }
            }

            const name = self.source[self.loc.start..self.loc.offset];

            if (std.meta.stringToEnum(Token.Kind, name)) |maybe_keyword_kind| {
                // NOTE: Make sure it's an actual keyword so
                // the user cannot pass "Identifier"
                if (maybe_keyword_kind.is_keyword())
                    return Token.init(maybe_keyword_kind, name, self.loc);
            }

            return Token.init(.Name, name, self.loc);
        },
        '\'' => try self.stringLiteralToken(.SingleQuote),
        '"' => try self.stringLiteralToken(.DoubleQuote),
        else => {
            std.debug.print("Unrecognized Character {c}", .{c});
            return ScanError.UnrecognizedCharacter;
        },
    };
}

inline fn advance(self: *@This()) u8 {
    if (self.isEOF())
        unreachable;

    defer self.loc.offset += 1;
    defer self.loc.line_offset += 1;
    return self.peek();
}

inline fn peek(self: *@This()) u8 {
    return if (self.isEOF()) 0 else self.source[self.loc.offset];
}

inline fn peekNext(self: *@This()) u8 {
    return if (self.loc.offset + 1 >= self.source.len) 0 else self.source[self.loc.offset + 1];
}

inline fn match(self: *@This(), to_match: []const u8) bool {
    const adjust = to_match.len;
    const end = self.loc.offset + adjust;

    if (end > self.source.len)
        return false;

    const matches = std.mem.eql(
        u8,
        self.source[self.loc.offset..end],
        to_match,
    );

    if (matches) {
        self.loc.offset += adjust;
        self.loc.line_offset += adjust;
    }

    return matches;
}

inline fn isEOF(self: @This()) bool {
    return self.loc.offset >= self.source.len;
}

const NumberLiteralTokenError = std.mem.Allocator.Error;
fn numberLiteralToken(self: *@This()) NumberLiteralTokenError!Token {
    var num_literal = std.ArrayList(u8).init(self.allocator);
    defer num_literal.deinit();

    var is_hexadecimal: bool = false;
    var is_float: bool = false;
    var has_exponent: bool = false;

    self.loc.offset -= 1;
    self.loc.line_offset -= 1;

    if (self.match("0x") or self.match("0X")) {
        is_hexadecimal = true;

        // == Hexadecimal
        try num_literal.appendSlice("0x");
    }

    while (true) : (_ = self.advance()) {
        if (is_hexadecimal and isHexadecic(self.peek()))
            try num_literal.append(self.peek())
        else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
            try num_literal.append(self.peek())
        else
            break;
    }

    if (self.match(".")) {
        is_float = true;
        try num_literal.append('.');

        while (true) : (_ = self.advance()) {
            if (is_hexadecimal and isHexadecic(self.peek()))
                try num_literal.append(self.peek())
            else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
                try num_literal.append(self.peek())
            else
                break;
        }
    }

    // zig fmt: off
    if ((is_hexadecimal and self.match("P") or self.match("p"))
    or (!is_hexadecimal and self.match("E") or self.match("e"))) {
    // zig fmt: on
        has_exponent = true;
        try num_literal.append(std.ascii.toUpper(self.source[self.loc.offset - 1]));

        if (self.match("-"))
            try num_literal.append('-')
        else if (self.match("+"))
            try num_literal.append('+');

        while (true) : (_ = self.advance()) {
            if (is_hexadecimal and isHexadecic(self.peek()))
                try num_literal.append(self.peek())
            else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
                try num_literal.append(self.peek())
            else
                break;
        }
    }

    return Token.init(.Number, try num_literal.toOwnedSlice(), self.loc);
}

const LuaStringType = enum {
    SingleQuote,
    DoubleQuote,
    Brackets,
};

const StringLiteralTokenError = error{
    UnterminatedString,
    InvalidEscapeChar,
    NotHexadec,
    Utf8CannotEncodeSurrogateHalf,
    CodepointTooLarge,
} || std.mem.Allocator.Error || std.fmt.ParseIntError;
fn stringLiteralToken(self: *@This(), string_type: LuaStringType) StringLiteralTokenError!Token {
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(self.allocator);

    o: while (true) {
        if (self.isEOF())
            return error.UnterminatedString;

        const char = self.advance();

        if (string_type == .SingleQuote and char == '\'')
            break :o;

        if (string_type == .DoubleQuote and char == '"')
            break :o;

        // FIXME: Support = in Bracket strings
        if (string_type == .Brackets and char == ']' and self.match("]"))
            break :o;

        if (string_type == .SingleQuote or string_type == .DoubleQuote) {
            if (char == '\n')
                return error.UnterminatedString;

            if (char == '\\') {
                const escaping_char = self.advance();

                // 3.1 - Lexical Conventions (https://www.lua.org/manual/5.4/manual.html#3)
                switch (escaping_char) {
                    'a' => try output.append(self.allocator, std.ascii.control_code.bel), // Bell
                    'b' => try output.append(self.allocator, std.ascii.control_code.bs), // Backspace
                    'f' => try output.append(self.allocator, std.ascii.control_code.ff), // Form Feed
                    'n' => try output.append(self.allocator, '\n'), // Newline
                    'r' => try output.append(self.allocator, '\r'), // Carriage Return
                    't' => try output.append(self.allocator, '\t'), // Horizontal Tab
                    'v' => try output.append(self.allocator, std.ascii.control_code.vt), // Vertical Tab
                    '"' => try output.append(self.allocator, '"'),
                    '[' => try output.append(self.allocator, '['),
                    ']' => try output.append(self.allocator, ']'),
                    '\'' => try output.append(self.allocator, '\''),
                    '\\' => try output.append(self.allocator, '\\'),
                    'z' => {
                        _ = self.advance();
                    },
                    '\n' => {
                        try output.append(self.allocator, '\n');
                    },
                    'u' => {
                        if (!self.match("{"))
                            unreachable;

                        const start = self.loc.offset;

                        while (self.peek() != '}') {
                            _ = self.advance();
                        }

                        const unicode_codepoint = self.source[start..self.loc.offset];
                        _ = self.advance(); // Closing }

                        var utf8_output = try self.allocator.alloc(u8, 8);
                        errdefer self.allocator.free(utf8_output);

                        const bytes_written = try std.unicode.utf8Encode(
                            try std.fmt.parseInt(u21, unicode_codepoint, 16),
                            utf8_output,
                        );

                        // TODO: Do we need to resize?
                        if (bytes_written != 8) {
                            std.debug.assert(self.allocator.resize(utf8_output, bytes_written));
                            utf8_output = utf8_output[0..bytes_written];
                        }

                        try output.appendSlice(self.allocator, utf8_output);
                    },
                    'x' => {
                        const hexadecimal_0 = self.advance();
                        const hexadecimal_1 = self.advance();

                        if (!isHexadecic(hexadecimal_0)) {
                            return error.NotHexadec;
                        }

                        if (!isHexadecic(hexadecimal_1)) {
                            return error.NotHexadec;
                        }

                        const hexadecimal_char = try std.fmt.parseInt(
                            u8,
                            &[_]u8{ hexadecimal_0, hexadecimal_1 },
                            16,
                        );

                        try output.append(self.allocator, hexadecimal_char);
                    },
                    '0'...'9' => {
                        const start = self.loc.offset - 1;

                        if (std.ascii.isDigit(self.peek()))
                            _ = self.advance();

                        if (std.ascii.isDigit(self.peek()))
                            _ = self.advance();

                        const ascii_char_codepoint = self.source[start..self.loc.offset];

                        const ascii_char = try std.fmt.parseInt(
                            u8,
                            ascii_char_codepoint,
                            10,
                        );

                        try output.append(
                            self.allocator,
                            ascii_char,
                        );
                    },
                    else => {
                        std.debug.print("Invalid Escape Char {c}\n", .{escaping_char});
                        return error.InvalidEscapeChar;
                    },
                }

                continue :o;
            }
        }

        try output.append(self.allocator, char);
    }

    return Token{
        .lexeme = try output.toOwnedSlice(self.allocator),
        .loc = self.loc,
        .kind = .String,
    };
}

inline fn isHexadecic(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        'a'...'f', 'A'...'F' => true,
        else => false,
    };
}
