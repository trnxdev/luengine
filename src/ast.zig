const std = @import("std");

// 3.3 - Statements (https://www.lua.org/manual/5.4/manual.html#3.3)
pub const Statement = union(enum) {
    Assignment: Assignment, // Impl
    LocalAssignment: LocalAssignment, // Impl
    Break: void, // Impl
    //  Chunk: Block,
    Do: Block, // Impl
    For: For, // Impl
    FunctionDef: FunctionDef, // Impl
    Goto: Goto, // Impl
    If: If, // Impl
    FuncCall: FunctionCall,
    Label: Label, // Impl
    Repeat: Repeat, // Impl
    Return: ?*Expression, // Impl
    While: While, // Impl

    pub const Assignment = struct {
        Variables: []Expression.Prefix.Var,
        Values: []*Expression,
    };

    pub const LocalAssignment = struct {
        Variables: Namelist,
        Values: []*Expression,
    };

    pub const Block = []*Statement;
    pub const Name = []const u8;
    pub const Namelist = []const Name;

    pub const For = union(enum) {
        Generic: Generic,
        Numerical: Numerical,

        pub const Generic = struct {
            Names: Namelist,
            Expressions: []*Expression,
            Body: Block,
        };

        pub const Numerical = struct {
            Name: Name,
            Start: *Expression,
            End: *Expression,
            Increment: ?*Expression,
            Body: Block,
        };
    };

    pub const FunctionDef = struct {
        name: Funcname,
        body: Funcbody,
        is_local: bool,

        pub const Funcname = []const u8;
    };

    pub const Goto = struct {
        label: []const u8,
    };

    pub const If = struct {
        @"if": struct {
            Block: Block,
            Condition: *Expression,
        },
        @"else": ?struct {
            Block: Block,
        },
    };

    pub const Label = struct {
        name: Name,
    };

    pub const Repeat = struct {
        Block: Block,
        UntilCondition: *Expression,
    };

    pub const While = struct {
        Block: Block,
        WhileCondition: *Expression,
    };
};

pub const Expression = union(enum) {
    Prefix: Prefix,
    Literal: Literal,
    FunctionDef: FunctionDef,
    VarArg: void,
    Binary: Binary,
    Unary: Unary,

    pub fn clone(self: *@This(), allocator: std.mem.Allocator) !*Expression {
        const new = try allocator.create(Expression);
        new.* = self.*;
        return new;
    }

    pub const Prefix = union(enum) {
        Var: Var,
        FunctionCall: FunctionCall,
        Group: *Expression,

        pub const Var = union(enum) {
            Identifier: []const u8,
            Access: Access,
            DotAccess: DotAccess,

            pub const Access = struct {
                Prefix: *Prefix,
                Field: *Expression,
            };

            pub const DotAccess = struct {
                Prefix: *Prefix,
                Field: []const u8,
            };
        };
    };

    pub const Literal = union(enum) {
        Nil,
        Table: TableConstructor,
        List: []*Expression,
        Bool: bool,
        Numeral: f64,
        String: []const u8,
    };

    pub const Binary = struct {
        lhs: *Expression,
        rhs: *Expression,
        op: BinOp,

        const BinOp = enum {
            Add,
            Sub,
            Mul,
            FloatDiv,
            Modulo,
            Exponentiation,

            BitAnd,
            BitOR,
            BitXOR,
            BitRightShift,
            BitLeftShift,

            Eql,
            Neql,
            Lt,
            Gt,
            Lte,
            Gte,

            Concat,

            Or,
            And,
        };
    };

    pub const Unary = struct {
        rhs: *Expression,
        op: UnOp,

        const UnOp = enum {
            UnaryMinus,
            Not,
            Len,
        };
    };

    pub const TableConstructor = struct {
        field_list: []Field,

        pub const Field = struct {
            Key: ?Key,
            Value: *Expression,

            pub const Key = union(enum) {
                Name: []const u8,
                Expression: *Expression,
            };
        };
    };

    pub const FunctionDef = struct {
        body: Funcbody,
    };
};

pub const Funcbody = struct {
    /// VA is always last and exists only once
    Parameters: Parlist,
    Block: Statement.Block,

    pub const Parlist = []const []const u8;
};

pub const FunctionCall = struct {
    callee: Callee,
    args: []Arg,

    pub const Callee = *Expression.Prefix;

    pub const Arg = *Expression;
};
