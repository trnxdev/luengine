const std = @import("std");
const AST = @import("ast.zig");
const Token = @import("scanner.zig").Token;

allocator: std.mem.Allocator,
tokens: []const Token,
token_idx: usize,

pub fn init(allocator: std.mem.Allocator, tokens: []const Token) @This() {
    return .{
        .allocator = allocator,
        .tokens = tokens,
        .token_idx = 0,
    };
}

pub fn deinit(_: *@This()) void {}

pub inline fn parseRoot(self: *@This()) anyerror![]*AST.Statement {
    return try self.parseUntil(&.{.EOF});
}

pub fn parseUntil(self: *@This(), comptime tags: []const Token.Kind) anyerror![]*AST.Statement {
    var block = std.ArrayList(*AST.Statement).init(self.allocator);
    defer block.deinit();

    o: while (true) {
        inline for (tags) |tag| {
            if (self.peekToken().kind == tag)
                break :o;
        }

        const stmt = try self.parseStatement();
        try block.append(stmt);
    }

    return try block.toOwnedSlice();
}

// Statements
pub fn parseStatement(self: *@This()) anyerror!*AST.Statement {
    while (self.peekToken().kind == .Semicolon) {
        _ = try self.advanceIfCurrentKindEql(.Semicolon);
    }

    const cur = self.token_idx;

    return self.parseStatementInner() catch {
        self.token_idx = cur;
        const prefix_expr = try self.parsePrefixExpression();

        if (!(prefix_expr.* == .Prefix and prefix_expr.Prefix == .FunctionCall))
            return error.FrickYou;

        const function_call_stmt = try self.allocator.create(AST.Statement);
        function_call_stmt.* = .{
            .FuncCall = prefix_expr.Prefix.FunctionCall,
        };

        return function_call_stmt;
    };
}

fn parseStatementInner(self: *@This()) anyerror!*AST.Statement {
    const val = switch (self.peekToken().kind) {
        .ColonColon => try self.parseLabelStatement(),
        .goto => try self.parseGotoStatement(),
        .function => try self.parseFunctionDefStatement(false),
        .local => try self.parseLocalAssignmentStatement(),
        .Name => try self.parseAssignmentStatement(),
        .do => try self.parseDoStatement(),
        .@"while" => {
            _ = try self.advanceIfCurrentKindEql(.@"while");
            const while_exp = try self.parseExpression();
            _ = try self.advanceIfCurrentKindEql(.do);
            const block = try self.parseUntil(&.{.end});
            _ = try self.advanceIfCurrentKindEql(.end);
            const while_stmt = try self.allocator.create(AST.Statement);
            while_stmt.* = .{ .While = .{
                .Block = block,
                .WhileCondition = while_exp,
            } };
            return while_stmt;
        },
        .repeat => {
            _ = try self.advanceIfCurrentKindEql(.repeat);
            const block = try self.parseUntil(&.{.until});
            _ = try self.advanceIfCurrentKindEql(.until);
            const until = try self.parseExpression();
            const repeat = try self.allocator.create(AST.Statement);
            repeat.* = .{ .Repeat = .{
                .Block = block,
                .UntilCondition = until,
            } };
            return repeat;
        },
        .@"if" => try self.parseIfStatement(),
        .@"return" => {
            _ = try self.advanceIfCurrentKindEql(.@"return");
            const return_stmt = try self.allocator.create(AST.Statement);
            const backed = self.token_idx;

            return_stmt.* = .{
                .Return = self.parseExpression() catch v: {
                    self.token_idx = backed;
                    break :v null;
                },
            };

            return return_stmt;
        },
        .@"break" => {
            _ = try self.advanceIfCurrentKindEql(.@"break");
            const break_stmt = try self.allocator.create(AST.Statement);
            break_stmt.* = .Break;
            return break_stmt;
        },
        .@"for" => try self.parseForStatement(),
        else => {
            std.debug.print("\n\n{}", .{self.peekToken()});
            return error.WrongStatement;
        },
    };

    return val;
}

pub fn parseForStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.@"for");

    switch (self.tokens[self.token_idx + 1].kind) {
        // Generic
        // stat ::= for namelist in explist do block end
        .Comma, .in => {
            var names = std.ArrayList([]const u8).init(self.allocator);
            defer names.deinit();

            const name_0 = try self.advanceIfCurrentKindEql(.Name);
            try names.append(name_0.lexeme);

            while (self.peekToken().kind == .Comma) {
                _ = try self.advanceIfCurrentKindEql(.Comma);
                const name = try self.advanceIfCurrentKindEql(.Name);
                try names.append(name.lexeme);
            }

            if (self.peekToken().kind == .Comma)
                _ = try self.advanceIfCurrentKindEql(.Comma);

            _ = try self.advanceIfCurrentKindEql(.in);

            const expression = try self.parseExplist();

            _ = try self.advanceIfCurrentKindEql(.do);
            const block = try self.parseUntil(&.{.end});
            _ = try self.advanceIfCurrentKindEql(.end);

            const generic_for_stmt = try self.allocator.create(AST.Statement);

            generic_for_stmt.* = .{ .For = .{ .Generic = .{
                .Body = block,
                .Expressions = expression,
                .Names = try names.toOwnedSlice(),
            } } };

            return generic_for_stmt;
        },
        // Numeric
        // stat ::= for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
        // for i = 0, 1
        .Equal => {
            const name = try self.advanceIfCurrentKindEql(.Name);
            _ = try self.advanceIfCurrentKindEql(.Equal);

            const start = try self.parseExpression();
            _ = try self.advanceIfCurrentKindEql(.Comma);

            const end = try self.parseExpression();
            var increment: ?*AST.Expression = null;

            if (self.peekToken().kind == .Comma) {
                _ = try self.advanceIfCurrentKindEql(.Comma);
                increment = try self.parseExpression();
            }

            _ = try self.advanceIfCurrentKindEql(.do);
            const block = try self.parseUntil(&.{.end});
            _ = try self.advanceIfCurrentKindEql(.end);

            const numeric_for_stmt = try self.allocator.create(AST.Statement);
            numeric_for_stmt.* = .{ .For = .{
                .Numerical = .{
                    .Body = block,
                    .Start = start,
                    .End = end,
                    .Increment = increment,
                    .Name = name.lexeme,
                },
            } };

            return numeric_for_stmt;
        },
        else => unreachable,
    }

    unreachable;
}

// stat ::= if exp then block {elseif exp then block} [else block] end
pub fn parseIfStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.peekTokenIfKindEql(.@"if");

    const if_stmt = try self.allocator.create(AST.Statement);

    _ = try self.advanceIfCurrentKindEql(.@"if");

    const if_expr = try self.parseExpression();
    _ = try self.advanceIfCurrentKindEql(.then);

    const if_block = try self.parseUntil(&.{ .@"else", .end });

    if_stmt.* = .{
        .If = .{
            .@"if" = .{
                .Block = if_block,
                .Condition = if_expr,
            },
            .@"else" = null,
        },
    };

    if (self.peekToken().kind == .@"else") {
        _ = try self.advanceIfCurrentKindEql(.@"else");

        const else_block = try self.parseUntil(&.{.end});

        if_stmt.If.@"else" = .{
            .Block = else_block,
        };
    }

    _ = try self.advanceIfCurrentKindEql(.end);

    return if_stmt;
}

pub fn parseDoStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.do);
    const block = try self.parseUntil(&.{.end});
    _ = try self.advanceIfCurrentKindEql(.end);

    const do_stmt = try self.allocator.create(AST.Statement);

    do_stmt.* = .{
        .Do = block,
    };

    return do_stmt;
}

pub fn parseGotoStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.goto);
    const label = try self.advanceIfCurrentKindEql(.Name);

    const goto_stmt = try self.allocator.create(AST.Statement);

    goto_stmt.* = .{
        .Goto = .{ .label = label.lexeme },
    };

    return goto_stmt;
}

pub fn parseLabelStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.ColonColon);
    const name = try self.advanceIfCurrentKindEql(.Name);
    _ = try self.advanceIfCurrentKindEql(.ColonColon);

    const label_stmt = try self.allocator.create(AST.Statement);

    label_stmt.* = .{
        .Label = .{
            .name = name.lexeme,
        },
    };

    return label_stmt;
}

pub fn parseFunctionDefStatement(self: *@This(), local: bool) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.function);

    const name_str = (try self.advanceIfCurrentKindEql(.Name)).lexeme;

    const body: AST.Funcbody = try self.parseFunctionBody();
    const fa = try self.allocator.create(AST.Statement);

    fa.* = .{
        .FunctionDef = .{
            .body = body,
            .name = name_str,
            .is_local = local,
        },
    };

    return fa;
}

pub fn namelist(self: *@This()) anyerror![]const []const u8 {
    var names = std.ArrayList([]const u8).init(self.allocator);
    defer names.deinit();

    var must_be_next: bool = false;
    o: while (true) {
        const exp = self.advanceIfCurrentKindEql(.Name) catch {
            if (must_be_next)
                unreachable;

            return try names.toOwnedSlice();
        };
        try names.append(exp.lexeme);

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
            continue :o;
        } else {
            must_be_next = false;
        }

        break :o;
    }

    return try names.toOwnedSlice();
}

pub fn parseLocalAssignmentStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.local);

    if (self.peekToken().kind == .function)
        return try self.parseFunctionDefStatement(true);

    const names = try self.namelist();

    if (names.len == 0) {
        @panic("TODO");
    }

    var expressions: []*AST.Expression = &.{};

    if (self.peekToken().kind == .Equal) {
        _ = try self.advanceIfCurrentKindEql(.Equal);
        expressions = try self.parseExplist();
    }

    const local_assign = try self.allocator.create(AST.Statement);

    local_assign.* = .{
        .LocalAssignment = .{
            .Values = expressions,
            .Variables = names,
        },
    };

    return local_assign;
}

pub fn parseAssignmentStatement(self: *@This()) anyerror!*AST.Statement {
    var varlist = std.ArrayList(AST.Expression.Prefix.Var).init(self.allocator);
    defer varlist.deinit();

    o: while (true) {
        switch (self.peekToken().kind) {
            .Name => {
                const @"var" = try self.parsePrefixExpression();
                if (!(@"var".* == .Prefix and @"var".Prefix == .Var)) {
                    return error.NotPrefixVar;
                }

                try varlist.append(@"var".Prefix.Var);

                if (self.peekToken().kind == .Comma) {
                    _ = try self.advanceIfCurrentKindEql(.Comma);
                    continue :o;
                }

                break :o;
            },
            else => unreachable,
        }
    }

    _ = try self.advanceIfCurrentKindEql(.Equal);

    const expressions: []*AST.Expression = try self.parseExplist();
    const assign = try self.allocator.create(AST.Statement);

    assign.* = .{
        .Assignment = .{
            .Values = expressions,
            .Variables = try varlist.toOwnedSlice(),
        },
    };

    return assign;
}

pub fn parseExplist(self: *@This()) anyerror![]*AST.Expression {
    var expressions = std.ArrayList(*AST.Expression).init(self.allocator);
    defer expressions.deinit();

    var must_be_next: bool = false;
    o: while (true) {
        const exp = self.parseExpression() catch {
            if (must_be_next)
                unreachable;

            // FIXME: check that the error is for wrong expression
            return try expressions.toOwnedSlice();
        };
        try expressions.append(exp);

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
            continue :o;
        } else {
            must_be_next = false;
        }

        break :o;
    }

    return try expressions.toOwnedSlice();
}

pub fn parseMultiArg(self: *@This()) ![]AST.FunctionCall.Arg {
    var arglist = std.ArrayList(AST.FunctionCall.Arg).init(self.allocator);
    defer arglist.deinit();

    var must_be_next: bool = false;
    o: while (self.peekToken().kind != .RightParenthesis) {
        try arglist.append(
            self.parseExpression() catch {
                if (must_be_next)
                    unreachable;

                break :o;
            },
        );

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
        }
    }

    return try arglist.toOwnedSlice();
}

pub fn parseFunctionBody(self: *@This()) !AST.Funcbody {
    _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);

    const parlist = try self.namelist();

    _ = try self.advanceIfCurrentKindEql(.RightParenthesis);

    const block = try self.parseUntil(&.{.end});

    const body: AST.Funcbody = .{
        .Block = block,
        .Parameters = parlist,
    };

    _ = try self.advanceIfCurrentKindEql(.end);

    return body;
}

// Precedence
//     or
//     and
//     <     >     <=    >=    ~=    ==
//     ..
//     +     -
//     *     /    %
//     unary operators (not   #    -)
//      ^
pub fn parseOr(self: *@This()) !*AST.Expression {
    const next = parseAnd;
    const lhs = try next(self);

    while (self.peekToken().kind == .@"or") {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .Or,
        } };
    }

    return lhs;
}

pub fn parseAnd(self: *@This()) !*AST.Expression {
    const next = parseComparison;
    const lhs = try next(self);

    while (self.peekToken().kind == .@"and") {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .And,
        } };
    }

    return lhs;
}

pub fn parseComparison(self: *@This()) !*AST.Expression {
    const next = parseConcat;
    const lhs = try next(self);

    while (switch (self.peekToken().kind) {
        .Less, .Greater, .LessEqual, .GreaterEqual, .TildeEqual, .EqualEqual => true,
        else => false,
    }) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = switch (op) {
                .Less => .Lt,
                .Greater => .Gt,
                .LessEqual => .Lte,
                .GreaterEqual => .Gte,
                .TildeEqual => .Neql,
                .EqualEqual => .Eql,
                else => unreachable,
            },
        } };
    }

    return lhs;
}

pub fn parseConcat(self: *@This()) !*AST.Expression {
    const next = parseTerm;
    const lhs = try next(self);

    while (self.peekToken().kind == .DotDot) {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .Concat,
        } };
    }

    return lhs;
}

pub fn parseTerm(self: *@This()) !*AST.Expression {
    const next = parseMul;
    const lhs = try next(self);

    while (self.peekToken().kind == .Plus or self.peekToken().kind == .Minus) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = if (op == .Plus) .Add else .Sub,
        } };
    }

    return lhs;
}

pub fn parseMul(self: *@This()) !*AST.Expression {
    const next = parseUnary;
    const lhs = try next(self);

    while (switch (self.peekToken().kind) {
        .Star, .Slash, .Percent => true,
        else => false,
    }) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = switch (op) {
                .Star => .Mul,
                .Slash => .FloatDiv,
                .Percent => .Modulo,
                else => unreachable,
            },
        } };
    }

    return lhs;
}

pub fn parseUnary(self: *@This()) !*AST.Expression {
    const next = parseExponentiation;
    var changed: bool = false;
    const changed_expr: *AST.Expression = try self.allocator.create(AST.Expression);

    while (switch (self.peekToken().kind) {
        .Hash, .Minus, .not => true,
        else => false,
    }) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        changed = true;
        changed_expr.* = .{ .Unary = .{
            .op = switch (op) {
                .Hash => .Len,
                .Minus => .UnaryMinus,
                .not => .Not,
                else => unreachable,
            },
            .rhs = try rhs.clone(self.allocator),
        } };
    }

    return if (changed)
        changed_expr
    else
        next(self);
}

pub fn parseExponentiation(self: *@This()) !*AST.Expression {
    const next = parsePrimary;
    const lhs = try next(self);

    while (self.peekToken().kind == .Caret) {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .Exponentiation,
        } };
    }

    return lhs;
}

// Expressions
pub fn parseExpression(self: *@This()) anyerror!*AST.Expression {
    return try self.parseOr();
}

pub fn parsePrimary(self: *@This()) anyerror!*AST.Expression {
    const exp = try self.allocator.create(AST.Expression);

    switch (self.peekToken().kind) {
        .String => {
            const str = try self.advanceIfCurrentKindEql(.String);

            exp.* = .{ .Literal = .{
                .String = str.lexeme,
            } };
        },
        .Number => {
            const num_tok = try self.advanceIfCurrentKindEql(.Number);
            const num_val = try std.fmt.parseFloat(f64, num_tok.lexeme);

            exp.* = .{ .Literal = .{
                .Numeral = num_val,
            } };
        },
        .true => {
            _ = try self.advanceIfCurrentKindEql(.true);
            exp.* = .{ .Literal = .{ .Bool = true } };
        },
        .false => {
            _ = try self.advanceIfCurrentKindEql(.false);
            exp.* = .{ .Literal = .{ .Bool = false } };
        },
        .nil => {
            _ = try self.advanceIfCurrentKindEql(.nil);
            exp.* = .{ .Literal = .Nil };
        },
        .LeftCurlyBrace => {
            exp.* = (try self.parseTableConstructorExpr()).*;
        },
        .function => exp.* = (try self.parseFunctionDefExpression()).*,
        .LeftBracket => {
            _ = try self.advanceIfCurrentKindEql(.LeftBracket);
            const expressions = try self.parseExplist();
            _ = try self.advanceIfCurrentKindEql(.RightBracket);
            exp.* = .{
                .Literal = .{
                    .List = expressions,
                },
            };
        },
        else => exp.* = (try self.parsePrefixExpression()).*,
    }

    return exp;
}

// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
// functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
// https://github.com/GavinHigham/lpil53/blob/90815131726cc4a917fc777ba1268b507456a774/parser.lua#L350
pub fn parsePrefixExpression(self: *@This()) anyerror!*AST.Expression {
    const prefixexp: *AST.Expression = try self.allocator.create(AST.Expression);

    if (self.peekToken().kind == .LeftParenthesis) {
        _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);
        prefixexp.* = .{ .Prefix = .{
            .Group = try self.parseExpression(),
        } };
        _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    } else if (self.peekToken().kind == .Name) {
        const name = try self.advanceIfCurrentKindEql(.Name);
        prefixexp.* = .{ .Prefix = .{ .Var = .{ .Identifier = name.lexeme } } };
    } else {
        return error.Idk;
    }

    o: while (true) {
        const token = self.peekToken();

        switch (token.kind) {
            .LeftBracket => {
                _ = try self.advanceIfCurrentKindEql(.LeftBracket);
                const exp = try self.parseExpression();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);

                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try prefixexp.clone(self.allocator)).Prefix;
                prefixexp.* = .{ .Prefix = .{ .Var = .{ .Access = .{
                    .Prefix = pref,
                    .Field = exp,
                } } } };
            },
            .Dot => {
                _ = try self.advanceIfCurrentKindEql(.Dot);
                const str_field = (try self.advanceIfCurrentKindEql(.Name)).lexeme;

                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try prefixexp.clone(self.allocator)).Prefix;

                prefixexp.* = .{ .Prefix = .{ .Var = .{ .DotAccess = .{
                    .Prefix = pref,
                    .Field = str_field,
                } } } };
            },
            .LeftParenthesis => {
                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try prefixexp.clone(self.allocator)).Prefix;

                prefixexp.* = .{ .Prefix = .{
                    .FunctionCall = .{
                        .args = try self.parseFuncArgs(),
                        .callee = pref,
                    },
                } };
            },
            else => break :o,
        }
    }

    return prefixexp;
}

pub fn parseFuncArgs(self: *@This()) anyerror![]AST.FunctionCall.Arg {
    _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);
    const args = try self.parseMultiArg();
    _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    return args;
}

pub fn parseTableConstructorExpr(self: *@This()) anyerror!*AST.Expression {
    _ = try self.advanceIfCurrentKindEql(.LeftCurlyBrace);
    var fields = std.ArrayList(AST.Expression.TableConstructor.Field).init(self.allocator);
    defer fields.deinit();

    while (self.peekToken().kind != .RightCurlyBrace) {
        switch (self.peekToken().kind) {
            .LeftBracket => {
                _ = try self.advanceIfCurrentKindEql(.LeftBracket);
                const expr_idx = try self.parseExpression();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);
                _ = try self.advanceIfCurrentKindEql(.Equal);
                const val_expr = try self.parseExpression();

                try fields.append(.{
                    .Key = .{ .Expression = expr_idx },
                    .Value = val_expr,
                });
            },
            .Name => o: {
                if (self.tokens[self.token_idx + 1].kind == .Equal) {
                    const name = try self.advanceIfCurrentKindEql(.Name);
                    _ = try self.advanceIfCurrentKindEql(.Equal);
                    const value = try self.parseExpression();

                    try fields.append(.{
                        .Key = .{ .Name = name.lexeme },
                        .Value = value,
                    });
                } else {
                    const value = try self.parseExpression();

                    try fields.append(.{
                        .Key = null,
                        .Value = value,
                    });
                    break :o;
                }
            },
            else => {
                const value = try self.parseExpression();

                try fields.append(.{
                    .Key = null,
                    .Value = value,
                });
            },
        }

        if (self.peekToken().kind == .Semicolon or self.peekToken().kind == .Comma) {
            _ = self.advance();
        }
    }

    _ = try self.advanceIfCurrentKindEql(.RightCurlyBrace);
    const table_constr_expr = try self.allocator.create(AST.Expression);
    table_constr_expr.* = .{ .Literal = .{ .Table = .{
        .field_list = try fields.toOwnedSlice(),
    } } };
    return table_constr_expr;
}

// only var, var.var, var[var]
pub fn parseVarPrefixExprIndependant(self: *@This()) anyerror!*AST.Expression {
    const name = try self.advanceIfCurrentKindEql(.Name);
    const var_prefix = try self.allocator.create(AST.Expression);

    var_prefix.* = .{
        .Prefix = .{
            .Var = .{
                .Identifier = name.lexeme,
            },
        },
    };

    o: while (true) {
        switch (self.peekToken().kind) {
            .Dot => {
                _ = try self.advanceIfCurrentKindEql(.Dot);
                const field = try self.advanceIfCurrentKindEql(.Name);
                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try var_prefix.clone(self.allocator)).Prefix;

                var_prefix.* = .{ .Prefix = .{
                    .Var = .{ .DotAccess = .{
                        .Field = field.lexeme,
                        .Prefix = pref,
                    } },
                } };
            },
            .LeftBracket => {
                _ = try self.advanceIfCurrentKindEql(.LeftBracket);
                const exp = try self.parseExpression();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);

                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try var_prefix.clone(self.allocator)).Prefix;

                var_prefix.* = .{ .Prefix = .{
                    .Var = .{ .Access = .{
                        .Field = exp,
                        .Prefix = pref,
                    } },
                } };
            },
            else => break :o,
        }
    }

    return var_prefix;
}

pub fn parseFunctionDefExpression(self: *@This()) anyerror!*AST.Expression {
    _ = try self.advanceIfCurrentKindEql(.function);
    const body = try self.parseFunctionBody();

    const fa = try self.allocator.create(AST.Expression);
    fa.* = .{
        .FunctionDef = .{
            .body = body,
        },
    };

    return fa;
}

// Helpers
inline fn peekTokenIfKindEql(self: @This(), kind: Token.Kind) !Token {
    const token = self.peekToken();

    if (token.kind != kind)
        return error.ExpectedAnotherKind;

    return token;
}

inline fn advanceIfCurrentKindEql(self: *@This(), kind: Token.Kind) !Token {
    _ = try self.peekTokenIfKindEql(kind);
    return self.advance();
}

inline fn advance(self: *@This()) Token {
    defer self.token_idx += 1;
    return self.peekToken();
}

inline fn peekToken(self: @This()) Token {
    if (self.isEOF())
        unreachable;

    return self.tokens[self.token_idx];
}

inline fn isEOF(self: @This()) bool {
    return self.token_idx >= self.tokens.len;
}
