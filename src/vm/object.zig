const std = @import("std");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const Chunk = @import("vm.zig").Chunk;

type: Type,

const Object = @This();

pub fn allocate(allocator: std.mem.Allocator, comptime T: type, objType: Type) !*Object {
    const ptr = try allocator.create(T);

    ptr.obj = Object{
        .type = objType,
    };

    return &ptr.obj;
}

pub fn comptimeAs(self: *@This(), comptime object_type: Type) ObjectTypeToType(object_type) {
    return self.as(ObjectTypeToType(object_type));
}

pub fn destroy(self: *@This(), allocator: std.mem.Allocator) void {
    return switch (self.type) {
        .String => self.as(*Object.String).free(allocator),
        .Function => self.as(*Object.Function).free(allocator),
        .NativeFunction => self.as(*Object.NativeFunction).free(allocator),
        .List => self.as(*Object.List).free(allocator),
    };
}

pub const Type = enum {
    String,
    Function,
    NativeFunction,
    List,
};

pub const String = struct {
    obj: Object,
    value: []u8,

    pub fn create(allocator: std.mem.Allocator, bytes: []const u8) !*String {
        const obj = try Object.allocate(allocator, String, .String);
        const out = obj.as(*Object.String);

        out.* = String{
            .obj = obj.*,
            .value = try allocator.dupe(u8, bytes),
        };

        return out;
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        allocator.free(self.value);
        allocator.destroy(self);
    }
};

pub const Function = struct {
    obj: Object,
    chunk: Chunk,
    arity: usize,
    name: ?*Object.String,
    constants: []Value,

    pub inline fn isRoot(self: @This()) bool {
        return self.name == null;
    }

    pub fn create(allocator: std.mem.Allocator) !*Function {
        const obj = try Object.allocate(allocator, Function, .Function);
        const out = obj.as(*Object.Function);

        out.* = Function{
            .obj = obj.*,
            .name = null,
            .arity = 0,
            .chunk = &.{},
            .constants = &.{},
        };

        return out;
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        allocator.free(self.constants);
        allocator.free(self.chunk);

        if (self.name) |n| {
            n.free(allocator);
        }

        allocator.destroy(self);
    }
};

pub const NativeFunction = struct {
    obj: Object,
    arity: usize,
    ptr: usize,
    name: *Object.String,

    pub fn create(allocator: std.mem.Allocator, name: []const u8, arity: usize, ptr: *const fn (*VM, []Value) Value) !*NativeFunction {
        const obj = try Object.allocate(allocator, NativeFunction, .NativeFunction);
        const out = obj.as(*Object.NativeFunction);

        out.* = NativeFunction{
            .obj = obj.*,
            .arity = arity,
            .name = try Object.String.create(allocator, name),
            .ptr = @intFromPtr(ptr),
        };

        return out;
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        self.name.free(allocator);
        allocator.destroy(self);
    }
};

pub const List = struct {
    obj: Object,
    items: []Value,

    pub fn create(allocator: std.mem.Allocator, items: []Value) !*List {
        const obj = try Object.allocate(allocator, List, .List);
        const out = obj.as(*Object.List);

        out.* = List{
            .obj = obj.*,
            .items = items,
        };

        return out;
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        allocator.free(self.items);
        allocator.destroy(self);
    }
};

pub fn asValue(self: *Object) Value {
    return Value.initObject(self);
}

pub fn as(self: *Object, comptime res_type: type) res_type {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn ObjectTypeToType(comptime object_type: Object.Type) type {
    return (switch (object_type) {
        .String => *Object.String,
        .Function => *Object.Function,
        .NativeFunction => *Object.NativeFunction,
        .List => *Object.List,
    });
}
