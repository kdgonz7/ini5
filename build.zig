const std = @import("std");

pub fn build(b: *std.Build) void {
    const ini5_tests = b.addTest(.{
        .root_source_file = b.path("lib/INI5.zig"),
    });

    const test_step = b.step("test", "Run unit tests for INI5. They should all be passing.");
    test_step.dependOn(&b.addRunArtifact(ini5_tests).step);
}
