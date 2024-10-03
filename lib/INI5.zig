//! ## Ini5
//!
//! The 5th attempt at making a feasible INI configuration parser, in 20 minutes.
//! Ini5 is meant to be robust, with a large suite of tests and real world,
//! applicable examples.
//!

// Copyright (C) Kai D. Gonzalez, 2024
// Licensed under BSD-2C. See LICENSE for details.

const std = @import("std");
const ascii = std.ascii;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const ValueType = enum {
    number,
    string,
    generic_value,
    nil,
};

pub const Value = union(ValueType) {
    number: i64,
    string: []const u8,
    generic_value: []const u8,
    nil: bool,
};

pub const Site = struct {
    begin: usize,
    end: usize,
};

pub const TokenType = enum {
    equal_sign,
    left_bracket,
    right_bracket,
    star,
    plus,
    minus,
    slash,
    identifier,
    number,
    section_break,
};

pub const Token = struct {
    token_body: []const u8,
    token_type: TokenType,
};

pub const TokenizerError = error{
    UnexpectedToken,
    OutOfMemory,
};

pub const ParserError = error{
    Overflow,
    OutOfMemory,

    InvalidCharacter,
    UnexpectedValue,
    UnexpectedToken,

    UnclosedSection,
    SectionExpectsName,
    AssignmentMissingTokens,
};

pub const Tokenizer = struct {
    input_text: []const u8,
    current_position: usize,
    token_result: ArrayList(Token),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Tokenizer {
        return Tokenizer{
            .input_text = "",
            .current_position = 0,
            .token_result = ArrayList(Token).init(allocator),
            .allocator = allocator,
        };
    }

    /// `A-Z`, `a-z`, `0-9`
    pub fn matchesIdentifierRules(_: *Tokenizer, char: u8) bool {
        return ascii.isAlphanumeric(char);
    }

    pub fn tokenizeFromCurrentPosition(self: *Tokenizer) TokenizerError!void {
        if (self.input_text.len == 0) return;

        while (self.current_position < self.input_text.len) {
            switch (self.input_text[self.current_position]) {
                'A'...'Z', 'a'...'z' => try self.tokenizeCurrentIdentifier(),
                '0'...'9' => try self.tokenizeCurrentNumber(),
                ';' => try self.ignoreSingleLineComment(),
                '\n' => try self.tokenizeBreak(),
                '=' => try self.tokenizeCurrentCharacter(TokenType.equal_sign),
                '[' => try self.tokenizeCurrentCharacter(TokenType.left_bracket),
                ']' => try self.tokenizeCurrentCharacter(TokenType.right_bracket),
                '+' => try self.tokenizeCurrentCharacter(TokenType.plus),
                '-' => try self.tokenizeCurrentCharacter(TokenType.minus),
                '*' => try self.tokenizeCurrentCharacter(TokenType.star),
                '/' => try self.tokenizeCurrentCharacter(TokenType.slash),
                else => {
                    if (!ascii.isWhitespace(self.input_text[self.current_position])) {
                        return error.UnexpectedToken;
                    }
                },
            }

            self.current_position += 1;
        }
    }

    pub fn tokenizeCurrentCharacter(self: *Tokenizer, as_type: TokenType) !void {
        try self.token_result.append(Token{
            .token_body = self.input_text[self.current_position .. self.current_position + 1],
            .token_type = as_type,
        });
    }

    pub fn tokenizeCurrentIdentifier(self: *Tokenizer) !void {
        const start = self.current_position;

        while (self.current_position < self.input_text.len and self.matchesIdentifierRules(self.input_text[self.current_position])) {
            self.current_position += 1;
        }

        try self.token_result.append(Token{
            .token_body = self.input_text[start..self.current_position],
            .token_type = TokenType.identifier,
        });

        // to combat starting after the current token, move back one
        // e.g. after the parsed identifier, we are HERE:
        //      ab+cd
        //         ^
        // We should be here:
        //      ab+cd
        //        ^
        self.current_position -= 1;
    }

    pub fn tokenizeCurrentNumber(self: *Tokenizer) !void {
        const start = self.current_position;

        while (self.current_position < self.input_text.len and ascii.isAlphanumeric(self.input_text[self.current_position])) {
            self.current_position += 1;
        }

        try self.token_result.append(Token{
            .token_body = self.input_text[start..self.current_position],
            .token_type = TokenType.number,
        });

        self.current_position -= 1;
    }

    pub fn tokenizeBreak(self: *Tokenizer) !void {
        if (self.input_text[self.current_position - 1] == '\n') {
            try self.token_result.append(Token{
                .token_body = "\n",
                .token_type = TokenType.section_break,
            });
        }
    }

    pub fn ignoreSingleLineComment(self: *Tokenizer) !void {
        while (self.input_text[self.current_position] != '\n') {
            self.current_position += 1;
        }
    }

    pub fn deinit(self: *Tokenizer) void {
        self.token_result.deinit();
    }
};

pub const ASTNodeAssignment = struct {
    lhs: []const u8,
    rhs: Value,
};

pub const ASTNodeSection = struct {
    section_name: []const u8,
    children: ArrayList(ASTNode),
};

pub const ASTNodeRoot = struct {
    children: ArrayList(ASTNode),
};

pub const ASTNodeType = enum {
    assignment,
    section,
    root_node,
    value,
};

pub const ASTNode = union(ASTNodeType) {
    assignment: ASTNodeAssignment,
    section: ASTNodeSection,
    root_node: ASTNodeRoot,
    value: Value,
};

pub const ASTGenerator = struct {
    current_token_position: usize,
    tokens: *ArrayList(Token),
    allocator: Allocator,

    pub fn init(allocator: Allocator, with_tokens: *ArrayList(Token)) ASTGenerator {
        return ASTGenerator{
            .current_token_position = 0,
            .tokens = with_tokens,
            .allocator = allocator,
        };
    }

    pub fn generateRootNode(self: *ASTGenerator) !ASTNode {
        var root = ASTNode{
            .root_node = ASTNodeRoot{
                .children = ArrayList(ASTNode).init(self.allocator),
            },
        };

        while (self.current_token_position < self.tokens.items.len) {
            var current_token = self.tokens.items[self.current_token_position];

            switch (current_token.token_type) {
                TokenType.left_bracket => {
                    try root.root_node.children.append(try self.generateSection());
                },
                TokenType.equal_sign => {
                    try root.root_node.children.append(try self.generateAssignment());
                },
                TokenType.number => {
                    try root.root_node.children.append(ASTNode{
                        .value = try self.turnTokenIntoValue(&current_token),
                    });
                },
                else => {},
            }

            self.current_token_position += 1;
        }

        return root;
    }

    pub fn generateAssignment(self: *ASTGenerator) !ASTNode {
        if (self.current_token_position == 0 or self.current_token_position >= self.tokens.items.len or self.current_token_position + 1 >= self.tokens.items.len) {
            return error.AssignmentMissingTokens;
        }
        const last_token = self.tokens.items[self.current_token_position - 1];

        const node = ASTNode{
            .assignment = ASTNodeAssignment{
                .lhs = last_token.token_body,
                .rhs = try self.turnTokenIntoValue(&self.tokens.items[self.current_token_position + 1]),
            },
        };

        self.current_token_position += 1;

        return node;
    }

    pub fn generateSection(self: *ASTGenerator) ParserError!ASTNode {
        // TODO: add checks here
        self.current_token_position += 1;

        if (self.current_token_position >= self.tokens.items.len) {
            return error.SectionExpectsName;
        }

        var section_node = ASTNode{
            .section = ASTNodeSection{
                .children = ArrayList(ASTNode).init(self.allocator),
                .section_name = self.tokens.items[self.current_token_position].token_body,
            },
        };

        self.current_token_position += 1;

        if (self.current_token_position >= self.tokens.items.len or self.tokens.items[self.current_token_position].token_type != TokenType.right_bracket) {
            return error.UnclosedSection;
        }

        self.current_token_position += 1;

        while (self.current_token_position < self.tokens.items.len) {
            switch (self.tokens.items[self.current_token_position].token_type) {
                TokenType.equal_sign => {
                    try section_node.section.children.append(try self.generateAssignment());
                },

                TokenType.section_break => {
                    break;
                },

                TokenType.identifier => {},

                else => {
                    return error.UnexpectedToken;
                },
            }
            self.current_token_position += 1;
        }

        return section_node;
    }

    pub fn turnTokenIntoValue(_: *ASTGenerator, token: *Token) ParserError!Value {
        return switch (token.token_type) {
            TokenType.identifier => Value{
                .generic_value = token.token_body,
            },
            TokenType.number => Value{
                .number = try std.fmt.parseInt(i64, token.token_body, 0),
            },
            else => {
                return error.UnexpectedValue;
            },
        };
    }
};

fn tokenizeString(str_in: []const u8, allocator: Allocator) !Tokenizer {
    var tokenizer_to_return = Tokenizer.init(allocator);
    tokenizer_to_return.input_text = str_in;
    try tokenizer_to_return.tokenizeFromCurrentPosition();

    return tokenizer_to_return;
}

test "using the tokenizer to tokenize equal signs and stars" {
    var tokens = try tokenizeString("*=", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(true, tokens.token_result.items[0].token_type == TokenType.star);
    try std.testing.expectEqual(true, tokens.token_result.getLast().token_type == TokenType.equal_sign);
    try std.testing.expectEqual('=', tokens.token_result.getLast().token_body[0]);
}

test "testing tokenizer outputs with all available symbols" {
    var tokens = try tokenizeString("+/*=-", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(tokens.token_result.items.len, 5);
    try std.testing.expectEqual(tokens.token_result.items[0].token_type, TokenType.plus);
    try std.testing.expectEqual(tokens.token_result.items[1].token_type, TokenType.slash);
    try std.testing.expectEqual(tokens.token_result.items[2].token_type, TokenType.star);
    try std.testing.expectEqual(tokens.token_result.items[3].token_type, TokenType.equal_sign);
    try std.testing.expectEqual(tokens.token_result.items[4].token_type, TokenType.minus);
}

test "tokenizing identifiers simple" {
    var tokens = try tokenizeString("ab+cd", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(tokens.token_result.items[0].token_type, TokenType.identifier);
    try std.testing.expectEqual(tokens.token_result.items[1].token_type, TokenType.plus);
    try std.testing.expectEqual(tokens.token_result.items[2].token_type, TokenType.identifier);

    try std.testing.expectEqual(std.mem.eql(u8, tokens.token_result.items[0].token_body, "ab"), true);
    try std.testing.expectEqual(std.mem.eql(u8, tokens.token_result.items[1].token_body, "+"), true);
    try std.testing.expectEqual(std.mem.eql(u8, tokens.token_result.items[2].token_body, "cd"), true);
}

test "tokenizing more complex numbers" {
    var tokens = try tokenizeString("0xAF", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(1, tokens.token_result.items.len);
    try std.testing.expectEqual(true, std.mem.eql(u8, tokens.token_result.items[0].token_body, "0xAF"));
}

test "tokenizing a simple program and checking its results" {
    var tokens = try tokenizeString("[section 331]\na = 1\nb = 3\nc = funip1\n\n", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(14, tokens.token_result.items.len);
    try std.testing.expectEqual(TokenType.left_bracket, tokens.token_result.items[0].token_type);
    try std.testing.expectEqual(TokenType.identifier, tokens.token_result.items[1].token_type);
    try std.testing.expectEqual(TokenType.number, tokens.token_result.items[2].token_type);
    try std.testing.expectEqual(TokenType.section_break, tokens.token_result.items[tokens.token_result.items.len - 1].token_type);
}

test "unpredictable inputs should yield similar results" {
    var tokens = try tokenizeString("                        []", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(TokenType.left_bracket, tokens.token_result.items[0].token_type);
    try std.testing.expectEqual(TokenType.right_bracket, tokens.token_result.items[1].token_type);

    tokens.current_position = 0;
    tokens.token_result.clearRetainingCapacity();

    tokens.input_text = "                        [                                    ]";

    try tokens.tokenizeFromCurrentPosition();
    try std.testing.expectEqual(TokenType.left_bracket, tokens.token_result.items[0].token_type);
    try std.testing.expectEqual(TokenType.right_bracket, tokens.token_result.items[1].token_type);
}

test "weird inputs should cause soft errors" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testing_arena_allocator.deinit();
    const testing_arena = testing_arena_allocator.allocator();

    try std.testing.expectError(error.UnexpectedToken, tokenizeString(" ?{}", testing_arena));
}

test "comments should be ignored entirely" {
    var tokens = try tokenizeString("; ?{}?\na", std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(1, tokens.token_result.items.len);
}

test "for using the ast generator to generate values from tokens" {
    var tokenizer = Tokenizer.init(std.testing.allocator);
    defer tokenizer.deinit();

    tokenizer.input_text = "1";
    try tokenizer.tokenizeFromCurrentPosition();

    var ast_generator = ASTGenerator.init(std.testing.allocator, &tokenizer.token_result);

    try std.testing.expectEqual(@as(i64, 1), (try ast_generator.turnTokenIntoValue(&tokenizer.token_result.items[0])).number);
    try std.testing.expectEqual(@as(i64, 3), (try ast_generator.turnTokenIntoValue(&tokenizer.token_result.items[0])).number + 2);
}

test "using the actual AST generator for a simple number as the root expression" {
    var tokenizer = Tokenizer.init(std.testing.allocator);
    defer tokenizer.deinit();

    tokenizer.input_text = "1";
    try tokenizer.tokenizeFromCurrentPosition();

    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testing_arena_allocator.deinit();

    const testing_arena = testing_arena_allocator.allocator();
    var ast_generator = ASTGenerator.init(testing_arena, &tokenizer.token_result);

    const root = try ast_generator.generateRootNode();
    const value = root.root_node.children.items[0];

    switch (value) {
        ASTNodeType.value => |val| {
            try std.testing.expectEqual(@as(i64, 1), val.number);
        },

        else => {
            try std.testing.expect(false);
        },
    }
}

test "using the AST generator for a simple assignment" {
    var tokenizer = Tokenizer.init(std.testing.allocator);
    defer tokenizer.deinit();

    tokenizer.input_text = "a = 5";
    try tokenizer.tokenizeFromCurrentPosition();

    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testing_arena_allocator.deinit();
    const testing_arena = testing_arena_allocator.allocator();

    var ast_generator = ASTGenerator.init(testing_arena, &tokenizer.token_result);

    const root = try ast_generator.generateRootNode();
    const first_node = root.root_node.children.items[0];

    switch (first_node) {
        ASTNodeType.assignment => |assignment| {
            try std.testing.expectEqual('a', assignment.lhs[0]);
        },

        else => {
            try std.testing.expect(false); // logically ambitious for the example given
        },
    }
}

test "multiple statements should also work" {
    var tokenizer = Tokenizer.init(std.testing.allocator);
    defer tokenizer.deinit();

    tokenizer.input_text = "a = 5\nb = 5";
    try tokenizer.tokenizeFromCurrentPosition();

    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testing_arena_allocator.deinit();
    const testing_arena = testing_arena_allocator.allocator();

    var ast_generator = ASTGenerator.init(testing_arena, &tokenizer.token_result);
    const root = try ast_generator.generateRootNode();

    switch (root) {
        ASTNodeType.root_node => |node| {
            // TODO: find a better way to check these people's types.

            for (node.children.items) |child| { // unwrap the children
                switch (child) {
                    ASTNodeType.assignment => {}, // assignments are fine
                    else => {
                        try std.testing.expect(false); // otherwise that's not right
                    },
                }
            }
        },
        else => {
            try std.testing.expect(false);
        },
    }
}

test "creating AST sections" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testing_arena_allocator.deinit();

    const testing_allocator = testing_arena_allocator.allocator();

    var tokenizer = Tokenizer.init(testing_allocator);

    tokenizer.input_text = "[abc]\na = 5\nb = 5\n";
    try tokenizer.tokenizeFromCurrentPosition();

    var ast_generator = ASTGenerator.init(testing_allocator, &tokenizer.token_result);
    const root = try ast_generator.generateRootNode();

    const sector_abc: ASTNodeSection = root.root_node.children.items[0].section;

    try std.testing.expectEqual(true, std.mem.eql(u8, "abc", sector_abc.section_name));
    try std.testing.expectEqual(2, sector_abc.children.items.len);
}

test "creating multiple AST Sections, they should all have some data in them" {
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

    const sector_def: ASTNodeSection = root.root_node.children.items[1].section;

    try std.testing.expectEqual(2, sector_def.children.items.len);
    try std.testing.expectEqual(true, std.mem.eql(u8, "def", sector_def.section_name));
    try std.testing.expectEqual(true, std.mem.eql(u8, sector_def.children.items[0].assignment.lhs, "c")); // section def -> assignment
    try std.testing.expectEqual(6, sector_def.children.items[0].assignment.rhs.number);
    try std.testing.expectEqual(true, std.mem.eql(u8, sector_def.children.items[1].assignment.lhs, "d"));
    try std.testing.expectEqual(7, sector_def.children.items[1].assignment.rhs.number);
}

test "in patching the rough spots" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testing_arena_allocator.deinit();

    const testing_allocator = testing_arena_allocator.allocator();

    var tokenizer = Tokenizer.init(testing_allocator);

    tokenizer.input_text = "\x12\x13\x14";

    try std.testing.expectError(error.UnexpectedToken, tokenizer.tokenizeFromCurrentPosition());
    tokenizer.current_position = 0;
    tokenizer.input_text = "[a";

    try tokenizer.tokenizeFromCurrentPosition();

    var ast_generator = ASTGenerator.init(testing_allocator, &tokenizer.token_result);

    try std.testing.expectError(error.UnclosedSection, ast_generator.generateRootNode());
}
