const std = @import("std");
const builtin = @import("builtin");
const Scanner = @import("scanner.zig");
const Parser = @import("parser.zig");
const Compiler = @import("compiler.zig");
const VM = @import("vm/vm.zig");

const LUA_VERSION = "5.1";
const MAX_USIZE = std.math.maxInt(usize);

const Command = enum {
    help,
    debug,
};

pub fn deeperFmt(value: anytype, max_depth: usize) DeeperFmt(@TypeOf(value)) {
    return .{ .value = value, .max_depth = max_depth };
}
fn DeeperFmt(comptime T: type) type {
    return struct {
        value: T,
        max_depth: usize,

        pub fn format(
            self: @This(),
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try std.fmt.formatType(self.value, fmt_str, options, writer, self.max_depth);
        }
    };
}

const Debug = builtin.mode == .Debug;

pub fn main() !u8 {
    var gpa = if (Debug) std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = if (Debug) gpa.allocator() else std.heap.raw_c_allocator;
    defer _ = if (Debug) gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_allocator = arena.allocator();
    defer arena.deinit();

    const args = try std.process.argsAlloc(arena_allocator);
    defer std.process.argsFree(arena_allocator, args);

    if (args.len < 2) {
        const exit_code = help_menu();
        return exit_code;
    }

    if (std.meta.stringToEnum(Command, args[1])) |cmd| {
        const exit_code = switch (cmd) {
            .debug => try debug(arena_allocator, args),
            .help => help_menu(),
        };

        return exit_code;
    }

    _ = help_menu();
    return 1;
}

fn help_menu() u8 {
    // TODO
    std.debug.print("TODO", .{});
    return 0;
}

fn debug(allocator: std.mem.Allocator, args: [][]u8) !u8 {
    if (args.len < 3) {
        std.log.err("Usage: {s} {s} <file>", .{ args[0], args[1] });
        return 1;
    }

    const path = args[2];

    const file = std.fs.cwd().readFileAlloc(
        allocator,
        path,
        MAX_USIZE,
    ) catch |e| {
        std.log.err("Encountered \"{s}\" while trying to read \"{s}\"", .{ @errorName(e), path });
        return 1;
    };
    defer allocator.free(file);

    var scanner = Scanner.init(allocator, file);
    //defer scanner.deinit();

    const tokens = scanner.scanArray() catch |e| {
        std.log.err("Encountered \"{s}\" while scanning tokens (at {d}:{d})", .{ @errorName(e), scanner.loc.line, scanner.loc.line_offset });
        return 1;
    };
    defer allocator.free(tokens);

    //const stdout_writer = std.io.getStdOut().writer();
    //var buffered_stdout = std.io.bufferedWriter(stdout_writer);
    //const buffered_writer = buffered_stdout.writer();
    //
    //for (tokens) |token| {
    //    buffered_writer.print("{}: {s}\n", .{ token.kind, token.lexeme }) catch |e| {
    //        std.log.err("Encountered \"{s}\" while outputting the result", .{@errorName(e)});
    //        return 1;
    //    };
    //
    //    //if (token.kind == .String or token.kind == .Number) {
    //    //    allocator.free(token.lexeme);
    //    //}
    //}

    //  buffered_stdout.flush() catch unreachable;

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    const root = try parser.parseRoot();

    // for (root, 0..) |stmt, idx| {
    //     std.debug.print("{d}: {}\n", .{ idx, deeperFmt(stmt, 300) });
    // }

    var compiler = try Compiler.init(allocator);
    defer compiler.deinit();

    const root_func = try compiler.compileRoot(root);
    try VM.execute(allocator, root_func);

    return 0;
}
