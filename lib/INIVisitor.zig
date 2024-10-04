const std = @import("std");
const INI5 = @import("INI5.zig");

const INISection = INI5.INISection;
const INIValue = INI5.INIValue;
const INIValueType = INI5.INIValueType;

const parseFile = INI5.parseFile;

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
    var c_alloc = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer c_alloc.deinit();

    const arena = c_alloc.allocator();

    const args = try std.process.argsAlloc(arena);

    if (args.len < 2) {
        std.log.err("requires a FILE argument.", .{});
        return;
    }

    const sections = parseFile(arena, args[1]) catch |err| {
        std.log.err("Failed to read file: '{s}'", .{args[1]});
        std.log.err("code::({any})", .{err});
        std.process.exit(1);
    };
    var section_list = sections.iterator();

    while (section_list.next()) |item| {
        visitSection(item.key_ptr.*, item.value_ptr.*, 0);
    }
}