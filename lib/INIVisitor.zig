const std = @import("std");
const INI5 = @import("INI5.zig");

const INISection = INI5.INISection;
const INIValue = INI5.INIValue;
const INIValueType = INI5.INIValueType;

const parseFile = INI5.parseFile;
pub const std_options: std.Options = .{
    .logFn = log,
};

pub fn log(
    comptime message_level: std.log.Level,
    comptime _: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    switch (message_level) {
        .info => {
            const stdout = std.io.getStdOut().writer();
            _ = stdout.write("\x1b[;1mInfo: ") catch unreachable;
            _ = stdout.write("\x1b[0m") catch unreachable;
            stdout.print(format, args) catch unreachable;
            _ = stdout.write("\n") catch unreachable;
        },

        .err => {
            const stdout = std.io.getStdErr().writer();
            _ = stdout.write("\x1b[;1mError: ") catch unreachable;
            _ = stdout.write("\x1b[0m") catch unreachable;
            stdout.print(format, args) catch unreachable;
            _ = stdout.write("\n") catch unreachable;
        },

        .warn => {
            const stdout = std.io.getStdErr().writer();
            _ = stdout.write("\x1b[;1mWarning: ") catch unreachable;
            _ = stdout.write("\x1b[0m") catch unreachable;
            stdout.print(format, args) catch unreachable;
            _ = stdout.write("\n") catch unreachable;
        },
        .debug => {
            const stdout = std.io.getStdErr().writer();
            _ = stdout.write("\x1b[;1mDebug: ") catch unreachable;
            _ = stdout.write("\x1b[0m") catch unreachable;
            stdout.print(format, args) catch unreachable;
            _ = stdout.write("\n") catch unreachable;
        },
    }
}

pub fn visitSection(name: []const u8, section: INISection, indent: usize) void {
    for (0..indent) |_| {
        std.log.info(" ", .{});
    }

    std.log.info("section {s}", .{name});

    var iterator = section.variables.iterator();
    while (iterator.next()) |assignment| {
        std.log.info("   | assignment {s}", .{assignment.key_ptr.*});
        std.log.info("   | value type {any}", .{assignment.value_ptr.*});
    }
}

pub fn main() !void {
    var GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{
        .enable_memory_limit = true,
        .thread_safe = true,
        .safety = true,
    }){};

    const gpa = GeneralPurposeAllocator.allocator();

    var c_alloc = std.heap.ArenaAllocator.init(gpa);
    defer c_alloc.deinit();

    const arena = c_alloc.allocator();

    const args = try std.process.argsAlloc(arena);

    if (args.len < 2) {
        std.log.err("requires a FILE argument.", .{});
        return;
    }

    const sections = parseFile(arena, args[1]) catch |err| {
        std.log.err("Failed to read file: '{s}'", .{args[1]});
        std.log.err("({any})", .{err});
        std.process.exit(1);
    };
    var section_list = sections.iterator();

    while (section_list.next()) |item| {
        visitSection(item.key_ptr.*, item.value_ptr.*, 0);
    }
}
