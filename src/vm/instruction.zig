const Value = @import("value.zig").Value;

pub const Instruction = struct {
    kind: Kind,
    A: usize,

    const Kind = enum {
        Add,
        Sub,
        Mul,
        Div,
        Lte,
        Concat,
        LoadConst, // A: Index of the Constant (function.constants)
        Pop,
        Dup,
        Exit,
        Negate,
        SetLocal, // A: Index where to set the Local
        GetLocal, // A: Index where to get the Local
        Call, // Calls popped value
        Jump,
        Jump_True,
        Jump_False,

        Return, // A: 1 if val 0 if nil
    };
};
