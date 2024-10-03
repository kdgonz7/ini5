# INI5

An INI parser library written in Zig, for Zig. Optimizations and benchmarks
shall be provided soon. Converts INI into AST format, supports sections, section
breaks, and simple assignments and values.

```zig
// from 'creating multiple AST Sections, they should all have some data in them'
var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
defer testing_arena_allocator.deinit();

const testing_allocator = testing_arena_allocator.allocator();

var tokenizer = Tokenizer.init(testing_allocator);

tokenizer.input_text = "[abc]\na = 5\nb = 5\n\n[def]\nc=6\nd=7\n";

try tokenizer.tokenizeFromCurrentPosition();

var ast_generator = ASTGenerator.init(testing_allocator, &tokenizer.token_result);
const root = try ast_generator.generateRootNode();

try std.testing.expectEqual(2, root.root_node.children.items.len);

const sector_abc: ASTNodeSection = root.root_node.children.items[0].section;

try std.testing.expectEqual(2, sector_abc.children.items.len);
try std.testing.expectEqual(true, std.mem.eql(u8, "abc", sector_abc.section_name));
try std.testing.expectEqual(true, std.mem.eql(u8, sector_abc.children.items[0].assignment.lhs, "a"));
try std.testing.expectEqual(5, sector_abc.children.items[1].assignment.rhs.number);
try std.testing.expectEqual(true, std.mem.eql(u8, sector_abc.children.items[1].assignment.lhs, "b"));
try std.testing.expectEqual(5, sector_abc.children.items[1].assignment.rhs.number);
```
