const std = @import("std");
const Instruction = @import("instruction.zig").Instruction;
const Value = @import("value.zig").Value;
const Object = @import("object.zig");

pub const Chunk = []Instruction;
pub const VM = struct {
    frames: [64]CallFrame,
    frame_idx: usize,
    stack: [10000]Value, // std.ArrayList(Value),
    stack_i: usize,

    pub inline fn currentFrame(self: *@This()) *CallFrame {
        return &self.frames[self.frame_idx - 1];
    }

    pub fn push(self: *@This(), val: Value) !void {
        self.stack[self.stack_i] = val;
        self.stack_i += 1;
        // try self.stack.append(val);
    }

    pub fn pop(self: *@This()) Value {
        self.stack_i -= 1;
        return self.stack[self.stack_i];
        // return self.stack.pop();
    }

    pub inline fn allocFrame(self: *@This()) *CallFrame {
        defer self.frame_idx += 1;
        return &self.frames[self.frame_idx];
    }

    pub fn deallocLastFrame(self: *@This()) void {
        if (self.frame_idx == 0)
            unreachable;

        self.frame_idx -= 1;
    }

    inline fn lastIndex(self: @This()) usize {
        return self.stack_i - 1;
    }

    pub inline fn getLast(self: @This()) Value {
        return self.at(self.lastIndex());
    }

    pub inline fn at(self: @This(), index: usize) Value {
        return self.stack[index];
    }
};

pub const CallFrame = struct {
    function: *Object.Function,
    locals: [512]Value,
    ip: usize,
};

pub fn execute(allocator: std.mem.Allocator, root_func: *Object.Function) !void {
    std.debug.assert(root_func.isRoot());

    var vm: VM = .{
        .frames = undefined,
        .frame_idx = 0,
        .stack = undefined,
        .stack_i = 0,
    };

    vm.allocFrame().* = .{
        .function = root_func,
        .ip = 0,
        .locals = undefined,
    };

    //  defer for (vm.currentFrame().locals) |local| {
    //      if (local.isObject()) {
    //          local.asObject().destroy(allocator);
    //      }
    //  };

    o: while (true) {
        if (vm.currentFrame().ip >= vm.currentFrame().function.chunk.len)
            unreachable; // Program should exit with .Exit instruction

        const instr = vm.currentFrame().function.chunk[vm.currentFrame().ip];

        switch (instr.kind) {
            .LoadConst => try vm.push(vm.currentFrame().function.constants[instr.A]),
            .Pop => _ = vm.pop(),
            .Dup => try vm.push(vm.getLast()),
            .Concat => {
                const b = vm.pop().asObjectOfType(.String).value;
                const a = vm.pop().asObjectOfType(.String).value;

                const str = try Object.String.create(allocator, "");
                allocator.free(str.value);

                str.value = try std.mem.concat(allocator, u8, &.{ a, b });

                try vm.push(str.obj.asValue());
            },
            .Add => try vm.push(Value.initNumber(vm.pop().asNumber() + vm.pop().asNumber())),
            .Sub => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initNumber(a.asNumber() - b.asNumber()));
            },
            .Mul => try vm.push(Value.initNumber(vm.pop().asNumber() * vm.pop().asNumber())),
            .Div => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initNumber(a.asNumber() / b.asNumber()));
            },
            .Lte => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initBool(a.asNumber() <= b.asNumber()));
            },
            .SetLocal => vm.currentFrame().locals[instr.A] = vm.pop(),
            .GetLocal => try vm.push(vm.currentFrame().locals[instr.A]),
            .Return => {
                if (vm.frame_idx == 0)
                    @panic("Use Exit!");

                if (instr.A == 0)
                    try vm.push(Value.initNil());

                vm.deallocLastFrame();
            },
            .Jump => {
                vm.currentFrame().ip = instr.A;
                continue :o;
            },
            .Jump_True => {
                if (vm.pop().asBool()) {
                    vm.currentFrame().ip = instr.A;
                    continue :o;
                }
            },
            .Jump_False => {
                if (!vm.pop().asBool()) {
                    vm.currentFrame().ip = instr.A;
                    continue :o;
                }
            },
            .Negate => {
                vm.stack[vm.lastIndex()] = Value.initBool(!vm.getLast().asBool());
            },
            .Call => {
                const func = vm.pop();
                if (func.isObjectOfType(.Function)) {
                    const function = func.asObjectOfType(.Function);

                    vm.allocFrame().* = .{
                        .function = function,
                        .ip = 0,
                        .locals = undefined,
                    };

                    for (0..function.arity) |idx| {
                        vm.currentFrame().locals[idx] = vm.pop();
                    }

                    continue :o;
                } else if (func.isObjectOfType(.NativeFunction)) {
                    const native_function = func.asObjectOfType(.NativeFunction);

                    var args = std.ArrayList(Value).init(allocator);
                    defer args.deinit();

                    for (0..native_function.arity) |_| {
                        try args.append(vm.pop());
                    }

                    const native_fn: *const fn (*VM, []Value) Value = @ptrFromInt(native_function.ptr);
                    const val = native_fn(&vm, try args.toOwnedSlice());

                    try vm.push(val);
                } else {
                    unreachable;
                }
            },
            .Exit => break :o,
        }

        vm.currentFrame().ip += 1;
        continue :o;
    }
}
