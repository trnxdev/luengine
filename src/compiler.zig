const std = @import("std");
const AST = @import("ast.zig");
const VM = @import("vm/vm.zig");
const Instruction = @import("vm/instruction.zig").Instruction;
const Object = @import("vm/object.zig");
const Value = @import("vm/value.zig").Value;

allocator: std.mem.Allocator,
function: *Object.Function,
chunk: WChunk,
constants: Constants,
natives: Natives,
ident_to_usize: IdentUsize,
labels: Labels,

pub const Labels = std.StringHashMap(usize);
pub const Natives = std.StringHashMap(usize);

const Constants = std.ArrayList(Value);
const WChunk = std.ArrayList(Instruction);
const IdentUsize = std.StringHashMap(usize);

pub fn init(allocator: std.mem.Allocator) !@This() {
    return .{
        .allocator = allocator,
        .function = undefined,
        .constants = undefined,
        .chunk = undefined,
        .ident_to_usize = undefined,
        .labels = undefined,
        .natives = undefined,
    };
}

pub fn deinit(self: *@This()) void {
    _ = self;
}

pub fn compileRoot(self: *@This(), root_block: []*AST.Statement) !*Object.Function {
    var root_function = try Object.Function.create(self.allocator);
    errdefer root_function.free(self.allocator);

    self.function = root_function;

    self.chunk = WChunk.init(self.allocator);
    errdefer self.chunk.deinit();

    self.constants = Constants.init(self.allocator);
    errdefer self.constants.deinit();

    self.natives = Natives.init(self.allocator);
    errdefer self.natives.deinit();

    self.ident_to_usize = IdentUsize.init(self.allocator);
    errdefer self.ident_to_usize.deinit();

    self.labels = Labels.init(self.allocator);
    errdefer self.labels.deinit();

    try self.defineNatives();

    try self.compileBlock(root_block);
    try self.chunk.append(.{
        .kind = .Exit,
        .A = 0,
    });

    root_function.arity = 0;
    root_function.chunk = try self.chunk.toOwnedSlice();
    root_function.constants = try self.constants.toOwnedSlice();

    return root_function;
}

fn compileBlock(self: *@This(), block: []*AST.Statement) !void {
    for (block) |stmt| {
        try self.compileStmt(stmt.*);
    }
}

pub fn hash(self: *@This(), str: []const u8, clobber: bool) !usize {
    if (self.ident_to_usize.get(str)) |num|
        return num;

    const hashed = self.ident_to_usize.count();

    _ = if (clobber)
        try self.ident_to_usize.put(str, hashed)
    else
        try self.ident_to_usize.putNoClobber(str, hashed);

    return @intCast(hashed);
}

pub fn compileStmt(self: *@This(), stmt: AST.Statement) anyerror!void {
    switch (stmt) {
        .FuncCall => |fc| {
            try self.compileFunccall(fc);
        },
        .LocalAssignment => |la| {
            if (la.Values.len != la.Variables.len) {
                @panic("Lengths must match!");
            }

            for (la.Variables, la.Values) |l, e| {
                try self.compileExpr(e.*);

                try self.chunk.append(Instruction{
                    .kind = .SetLocal,
                    .A = try self.hash(l, false),
                });
            }
        },
        .Break => return error.Br,
        .Goto => |goto| {
            const jump_to = self.labels.get(goto.label) orelse return error.UnknownJumpTarget;
            try self.chunk.append(.{ .kind = .Jump, .A = jump_to });
        },
        .Label => |label| {
            try self.labels.putNoClobber(label.name, self.chunk.items.len);
        },
        .If => |if_stmt| {
            try self.compileExpr(if_stmt.@"if".Condition.*);

            const patch_end: usize = self.chunk.items.len;
            try self.chunk.append(.{
                .kind = .Jump_False,
                .A = 0,
            });

            try self.compileBlock(if_stmt.@"if".Block);

            self.chunk.items[patch_end].A = self.chunk.items.len;
        },
        .Return => |expr| {
            if (expr) |ex| {
                try self.compileExpr(ex.*);
            }

            try self.chunk.append(Instruction{
                .kind = .Return,
                .A = @intFromBool(expr != null),
            });
        },
        .FunctionDef => |fd| {
            var created_function: *Object.Function = undefined;

            {
                const old_chunk = self.chunk;
                const old_constants = self.constants;
                const old_hash = self.ident_to_usize;
                const old_func = self.function;
                const old_natives = self.natives;

                defer self.chunk = old_chunk;
                defer self.constants = old_constants;
                defer self.ident_to_usize = old_hash;
                defer self.function = old_func;
                defer self.natives = old_natives;

                self.chunk = WChunk.init(self.allocator);
                self.constants = Constants.init(self.allocator);
                self.ident_to_usize = IdentUsize.init(self.allocator);
                self.natives = Natives.init(self.allocator);

                self.function = try Object.Function.create(self.allocator);
                self.function.name = try Object.String.create(self.allocator, fd.name);
                self.function.arity = fd.body.Parameters.len;
                created_function = self.function;

                // reserve the 0 const always for itself in case of recursion
                try self.constants.append(Value.initNil());
                try self.defineNatives();

                for (fd.body.Parameters) |par| {
                    _ = try self.hash(par, false);
                }

                try self.compileBlock(fd.body.Block);

                self.function.constants = try self.constants.toOwnedSlice();
                self.function.chunk = try self.chunk.toOwnedSlice();
            }

            const new_func_idx = self.constants.items.len;
            try self.constants.append(created_function.obj.asValue());

            created_function.constants[0] = created_function.obj.asValue();

            try self.chunk.append(.{
                .kind = .LoadConst,
                .A = new_func_idx,
            });
            try self.chunk.append(.{
                .kind = .SetLocal,
                .A = try self.hash(fd.name, false),
            });
        },
        else => {
            unreachable;
        },
    }
}

pub fn compileExpr(self: *@This(), expr: AST.Expression) anyerror!void {
    switch (expr) {
        .Binary => |b| {
            try self.compileExpr(b.lhs.*);
            try self.compileExpr(b.rhs.*);

            try self.chunk.append(.{
                .kind = switch (b.op) {
                    .Add => .Add,
                    .Sub => .Sub,
                    .Mul => .Mul,
                    .FloatDiv => .Div,

                    .Lt => .Lt,
                    .Gt => .Gt,
                    .Lte => .Lte,
                    .Gte => .Gte,

                    .Concat => .Concat,

                    else => unreachable,
                },
                .A = 0,
            });
        },
        .Literal => |l| {
            const constant = try self.literalToValue(l);

            try self.chunk.append(Instruction{
                .kind = .LoadConst,
                .A = self.constants.items.len,
            });

            try self.constants.append(constant);
        },
        .Prefix => |p| {
            switch (p) {
                .FunctionCall => |fc| {
                    try self.compileFunccall(fc);
                },
                .Var => |v| {
                    switch (v) {
                        .Identifier => |i| o: {
                            if (self.function.name) |name| {
                                if (std.mem.eql(u8, name.value, i)) {
                                    try self.chunk.append(.{
                                        .kind = .LoadConst,
                                        .A = 0,
                                    });
                                    break :o;
                                }
                            }

                            var it = self.natives.iterator();

                            while (it.next()) |e| {
                                if (std.mem.eql(u8, e.key_ptr.*, i)) {
                                    try self.chunk.append(.{
                                        .kind = .LoadConst,
                                        .A = e.value_ptr.*,
                                    });
                                    break :o;
                                }
                            }

                            const val = try self.hash(i, false);

                            try self.chunk.append(.{
                                .kind = .GetLocal,
                                .A = val,
                            });
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

pub fn literalToValue(self: *@This(), l: AST.Expression.Literal) anyerror!Value {
    const val = try self.allocator.create(Value);
    errdefer self.allocator.destroy(val);

    val.* = switch (l) {
        .Bool => |b| Value.initBool(b),
        .Numeral => |n| Value.initNumber(n),
        .String => |s| (try Object.String.create(self.allocator, s)).obj.asValue(),
        .Nil => Value.initNil(),
        .Table => @panic("TODO"),
    };

    return val.*;
}

pub fn compileFunccall(self: *@This(), func_call: AST.FunctionCall) anyerror!void {
    for (func_call.args) |arg| {
        try self.compileExpr(arg.*);
    }

    try self.compileExpr(.{ .Prefix = func_call.callee.* });

    try self.chunk.append(.{
        .kind = .Call,
        .A = 0,
    });
}

pub fn defineNatives(self: *@This()) !void {
    const print_ln = try Object.NativeFunction.create(self.allocator, 1, println);

    try self.natives.put("println", self.constants.items.len);
    try self.constants.append(print_ln.obj.asValue());
}

pub fn println(vm: *VM.VM, args: []Value) Value {
    _ = vm;

    const to_print = args[0];

    if (to_print.isBool())
        std.debug.print("{s}", .{if (to_print.asBool()) "true" else "false"})
    else if (to_print.isNil())
        std.debug.print("nil", .{})
    else if (to_print.isNumber())
        std.debug.print("{d}", .{to_print.asNumber()})
    else if (to_print.isObjectOfType(.String))
        std.debug.print("{s}", .{to_print.asObjectOfType(.String).value})
    else
        std.debug.print("UNKNOWN", .{});

    std.debug.print("\n", .{});

    return Value.initNil();
}
