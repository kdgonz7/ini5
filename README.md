# INI5

An INI parser library written in Zig, for Zig. Optimizations and benchmarks
shall be provided soon. Converts INI into AST format, supports sections, section
breaks, and simple assignments and values.

```zig
var c_alloc = std.heap.ArenaAllocator.init(std.heap.c_allocator);
defer c_alloc.deinit();

const arena = c_alloc.allocator();

const sections = parseFile(arena, "hello.ini") catch |err| {
    std.log.err("Failed to read file: '{s}'", .{args[1]});
    std.log.err("({any})", .{err});
    std.process.exit(1);
};

var section_list = sections.iterator();

while (section_list.next()) |item| {
    visit(item.key_ptr.*, item.value_ptr.*, 0);
}

fn visit(name: []const u8, section: INISection, indent: usize) {
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
```
