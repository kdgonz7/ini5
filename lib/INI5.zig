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
    boolean,
    nil,
};

pub const Value = union(ValueType) {
    number: i64,
    string: []const u8,
    generic_value: []const u8,
    boolean: bool,
    nil,
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
    string,

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

pub const ASTError = error{
    ASTConversionError,
};

pub const SectionError = error{
    VariableNotFound,
};

pub const SectionConversionError = error{
    OutOfMemory,
    ValueCanNotBeSectionized,
    ExpectedAssignment,
};

pub const TokenizerSettings = struct {
    warn_about_different_comment_tokens: bool = true,

    pub fn defaultSettings() TokenizerSettings {
        return TokenizerSettings{};
    }
};

pub const Tokenizer = struct {
    input_text: []const u8,
    current_position: usize,
    token_result: ArrayList(Token),
    allocator: Allocator,
    settings: TokenizerSettings = TokenizerSettings.defaultSettings(),
    comment_char: ?u8 = null,

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
        return ascii.isAlphanumeric(char) or char == '.' or char == '_' or char == '-';
    }

    pub fn tokenizeFromCurrentPosition(self: *Tokenizer) TokenizerError!void {
        if (self.input_text.len == 0) return;

        while (self.current_position < self.input_text.len) {
            switch (self.input_text[self.current_position]) {
                'A'...'Z', 'a'...'z' => try self.tokenizeCurrentIdentifier(),
                '"' => try self.tokenizeLiteral(),
                '0'...'9' => try self.tokenizeCurrentNumber(),
                ';', '#' => try self.ignoreSingleLineComment(),
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

    pub fn tokenizeLiteral(self: *Tokenizer) !void {
        const start = self.current_position;

        self.current_position += 1;

        while (self.current_position < self.input_text.len and self.input_text[self.current_position] != '"') {
            self.current_position += 1;

            if (self.current_position < self.input_text.len) {
                if (self.input_text[self.current_position] == '\\') {
                    self.current_position += 1;
                }
            }
        }

        self.current_position += 1;

        try self.token_result.append(Token{
            .token_body = self.input_text[start..self.current_position],
            .token_type = TokenType.string,
        });
    }

    pub fn tokenizeBreak(self: *Tokenizer) !void {
        if (self.current_position < self.input_text.len and self.input_text[self.current_position - 1] == '\n') {
            try self.token_result.append(Token{
                .token_body = "\n",
                .token_type = TokenType.section_break,
            });
        }
    }

    pub fn ignoreSingleLineComment(self: *Tokenizer) !void {
        if (self.settings.warn_about_different_comment_tokens) {
            if (self.comment_char == null) {
                self.comment_char = self.input_text[self.current_position];
            } else {
                if (self.input_text[self.current_position] != self.comment_char) {
                    std.log.warn("({d}) different comment character '{?c}' when '{?c}' was expected to begin with.", .{
                        self.current_position,
                        self.input_text[self.current_position],
                        self.comment_char,
                    });

                    if (self.input_text.len > 3) {
                        std.log.warn("near: {s}...", .{self.input_text[self.current_position - 1 ..]});
                    }
                }
            }
        }

        while (self.current_position < self.input_text.len and self.input_text[self.current_position] != '\n') {
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

    pub fn rootNode(self: *ASTNode) ASTError!ASTNodeRoot {
        switch (self.*) {
            .root_node => |_| {
                return self.root_node;
            },
            else => {
                return error.ASTConversionError;
            },
        }
    }
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

    pub fn generateAbstractSyntaxTree(self: *ASTGenerator) !ASTNode {
        var initial_node = ASTNode{
            .root_node = ASTNodeRoot{
                .children = ArrayList(ASTNode).init(self.allocator),
            },
        };

        while (self.current_token_position < self.tokens.items.len) {
            var current_token = self.tokens.items[self.current_token_position];

            switch (current_token.token_type) {
                TokenType.left_bracket => {
                    try initial_node.root_node.children.append(try self.generateSection());
                },
                TokenType.equal_sign => {
                    try initial_node.root_node.children.append(try self.generateAssignment());
                },
                TokenType.number => {
                    try initial_node.root_node.children.append(ASTNode{
                        .value = try self.turnTokenIntoValue(&current_token),
                    });
                },
                else => {},
            }

            self.current_token_position += 1;
        }

        return initial_node;
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

                TokenType.identifier,
                TokenType.number,
                TokenType.string,
                => {},

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
            TokenType.identifier => {
                if (std.mem.eql(u8, token.token_body, "yes") or std.mem.eql(u8, token.token_body, "no")) {
                    return Value{
                        .boolean = std.mem.eql(u8, token.token_body, "yes"),
                    };
                } else {
                    return Value{
                        .generic_value = token.token_body,
                    };
                }
            },

            TokenType.number => Value{
                .number = try std.fmt.parseInt(i64, token.token_body, 0),
            },

            TokenType.string => Value{
                .string = token.token_body,
            },

            else => {
                return error.UnexpectedValue;
            },
        };
    }
};

pub const INIValue = Value;
pub const INIValueType = ValueType;
pub const INISections = std.StringHashMap(INISection);
pub const INISection = struct {
    variables: std.StringHashMap(INIValue),

    pub fn init(allocator: Allocator) INISection {
        return INISection{
            .variables = std.StringHashMap(INIValue).init(allocator),
        };
    }

    pub fn declareVariableAndSetNil(self: *INISection, name: []const u8) !void {
        try self.variables.put(name, INIValue{.nil});
    }

    pub fn setVariable(self: *INISection, name: []const u8, value: INIValue) !void {
        try self.variables.put(name, value);
    }

    pub fn hasVariable(self: *const INISection, name: []const u8) bool {
        return self.variables.get(name) != null;
    }

    pub fn extractValue(self: *const INISection, name: []const u8) SectionError!INIValue {
        if (self.variables.get(name)) |variable| {
            return variable;
        } else {
            return error.VariableNotFound;
        }
    }

    pub fn repr(_: *INISection) []const u8 {}

    pub fn deinit(self: *INISection) void {
        self.variables.deinit();
    }
};

pub fn convertASTIntoSections(allocator: Allocator, node: ASTNode) SectionConversionError!INISections {
    var return_list = INISections.init(allocator);

    switch (node) {
        ASTNodeType.root_node => |*root| {
            for (root.children.items) |child| {
                try return_list.put(try getSectionName(child), try convertSectionFromChild(allocator, child));
            }
        },
        else => {
            return error.ValueCanNotBeSectionized;
        },
    }

    return return_list;
}

pub fn convertSectionFromChild(allocator: Allocator, child: ASTNode) !INISection {
    switch (child) {
        ASTNodeType.section => |sec| {
            var sect = INISection.init(allocator);

            for (sec.children.items) |assignment| {
                const unwrapped = try convertAssignment(assignment);
                try sect.setVariable(unwrapped.lhs, unwrapped.rhs);
            }

            return sect;
        },

        else => {
            return error.ValueCanNotBeSectionized;
        },
    }
}

pub fn getSectionName(child: ASTNode) ![]const u8 {
    switch (child) {
        ASTNodeType.section => |sec| {
            return sec.section_name;
        },

        else => {
            return error.ValueCanNotBeSectionized;
        },
    }
}

pub fn convertAssignment(node: ASTNode) !ASTNodeAssignment {
    switch (node) {
        ASTNodeType.assignment => |assignment| {
            return assignment;
        },

        else => {
            return error.ExpectedAssignment;
        },
    }
}

pub fn parseConfigurationString(allocator: Allocator, str: []const u8) !INISections {
    var tokenizer = Tokenizer.init(allocator);
    var ast_generator = ASTGenerator.init(allocator, &tokenizer.token_result);
    defer tokenizer.deinit();

    tokenizer.input_text = str;
    tokenizer.current_position = 0;

    try tokenizer.tokenizeFromCurrentPosition();

    const ast = try ast_generator.generateAbstractSyntaxTree();

    return convertASTIntoSections(allocator, ast);
}

pub fn parseConfigurationFile(allocator: Allocator, file_name: []const u8) !INISections {
    const file = try std.fs.cwd().readFileAlloc(allocator, file_name, std.math.maxInt(i64));
    const sections = parseConfigurationString(allocator, file);

    return sections;
}

pub const parse = parseConfigurationString;
pub const parseFile = parseConfigurationFile;

// Tests

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

    const root = try ast_generator.generateAbstractSyntaxTree();
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

    var ast = try ast_generator.generateAbstractSyntaxTree();
    const first_node = (try ast.rootNode()).children.items[0];

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
    const ast = try ast_generator.generateAbstractSyntaxTree();

    switch (ast) {
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
    var ast = try ast_generator.generateAbstractSyntaxTree();
    const root = try ast.rootNode();

    const sector_abc: ASTNodeSection = root.children.items[0].section;

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

    var ast = try ast_generator.generateAbstractSyntaxTree();
    const root = try ast.rootNode();

    try std.testing.expectEqual(2, root.children.items.len);

    const sector_abc: ASTNodeSection = root.children.items[0].section;

    try std.testing.expectEqual(2, sector_abc.children.items.len);
    try std.testing.expectEqual(true, std.mem.eql(u8, "abc", sector_abc.section_name));
    try std.testing.expectEqual(true, std.mem.eql(u8, sector_abc.children.items[0].assignment.lhs, "a"));
    try std.testing.expectEqual(5, sector_abc.children.items[1].assignment.rhs.number);
    try std.testing.expectEqual(true, std.mem.eql(u8, sector_abc.children.items[1].assignment.lhs, "b"));
    try std.testing.expectEqual(5, sector_abc.children.items[1].assignment.rhs.number);

    const sector_def: ASTNodeSection = root.children.items[1].section;

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

    try std.testing.expectError(error.UnclosedSection, ast_generator.generateAbstractSyntaxTree());
}

test "other kinds of values" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    const testing_arena = testing_arena_allocator.allocator();
    defer testing_arena_allocator.deinit();

    var tokenizer = Tokenizer.init(testing_arena);
    tokenizer.input_text = "[Main]\nhello = world\ncool = \"cool people all over the world\"";

    try tokenizer.tokenizeFromCurrentPosition();

    var ast_generator = ASTGenerator.init(testing_arena, &tokenizer.token_result);
    var ast = try ast_generator.generateAbstractSyntaxTree();
    const root_node = try ast.rootNode();

    const Main: ASTNodeSection = root_node.children.items[0].section;

    try std.testing.expect(std.mem.eql(u8, Main.section_name, "Main"));
    try std.testing.expectEqual(2, Main.children.items.len);

    const hello = Main.children.items[0].assignment;
    try std.testing.expectEqualStrings(hello.lhs, "hello");
    try std.testing.expectEqualStrings(hello.rhs.generic_value, "world");

    const cool = Main.children.items[1].assignment;
    try std.testing.expectEqualStrings(cool.lhs, "cool");
    try std.testing.expectEqualStrings(cool.rhs.string, "\"cool people all over the world\"");
}

test "strings not getting in the way of other data types" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    const testing_arena = testing_arena_allocator.allocator();
    defer testing_arena_allocator.deinit();

    var tokenizer = Tokenizer.init(testing_arena);
    tokenizer.input_text = "[Main]\ncool = \"cool people all over the world\"\na = 25";

    try tokenizer.tokenizeFromCurrentPosition();

    var ast_generator = ASTGenerator.init(testing_arena, &tokenizer.token_result);
    var ast = try ast_generator.generateAbstractSyntaxTree();
    const root_node = try ast.rootNode();

    try std.testing.expectEqual(1, root_node.children.items.len);
    try std.testing.expectEqual(2, root_node.children.items[0].section.children.items.len);
    try std.testing.expectEqualStrings(root_node.children.items[0].section.children.items[0].assignment.rhs.string, "\"cool people all over the world\"");
}

test "ini section objects" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    const testing_arena = testing_arena_allocator.allocator();
    defer testing_arena_allocator.deinit();

    var section1 = INISection.init(testing_arena);
    try section1.setVariable("hello", INIValue{ .number = 50 });

    try std.testing.expect(section1.hasVariable("hello"));
    try std.testing.expectEqual(@as(i64, 50), (try section1.extractValue("hello")).number);
}

test "converting an existing syntax tree into sections" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    const testing_arena = testing_arena_allocator.allocator();
    defer testing_arena_allocator.deinit();

    var tokenizer = Tokenizer.init(testing_arena);
    tokenizer.input_text = "[Main]\ncool = \"cool people all over the world\"\na = 25";

    try tokenizer.tokenizeFromCurrentPosition();

    var ast_generator = ASTGenerator.init(testing_arena, &tokenizer.token_result);
    const ast = try ast_generator.generateAbstractSyntaxTree();

    // this is one of the only differences, now we have a K-V mapping of
    // identifiers into values.
    const sections = try convertASTIntoSections(testing_arena, ast);

    try std.testing.expectEqual(true, sections.get("Main").?.hasVariable("cool"));
    try std.testing.expectEqual(true, sections.get("Main").?.hasVariable("a"));
}

test "using the shorthand for less boilerplate" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    const testing_arena = testing_arena_allocator.allocator();
    defer testing_arena_allocator.deinit();

    // parseConfigurationString takes care of the heavy loads,
    // lexing, parsing, generating the AST, and generating the sections. Those
    // are all of the steps to parse the config format.
    var sections = try parseConfigurationString(testing_arena, "[Main]\ncool = \"cool people all over the world\"\na = 25");

    try std.testing.expectEqual(true, sections.get("Main").?.hasVariable("cool"));
    try std.testing.expectEqual(true, sections.get("Main").?.hasVariable("a"));
}

test "boolean values" {
    var testing_arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    const testing_arena = testing_arena_allocator.allocator();
    defer testing_arena_allocator.deinit();

    var sections = try parseConfigurationString(testing_arena, "[Main]\ncool = yes");

    try std.testing.expectEqual(true, sections.get("Main").?.hasVariable("cool"));
    try std.testing.expectEqual(true, (try sections.get("Main").?.extractValue("cool")).boolean);
}
