const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target_options = b.standardTargetOptions(.{});
    const optimize_options = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseSafe,
    });
    const ini5_tests = b.addTest(.{
        .root_source_file = b.path("lib/INI5.zig"),
    });

    const test_step = b.step("tests", "Run unit tests for INI5. They should all be passing.");
    test_step.dependOn(&b.addRunArtifact(ini5_tests).step);

    const build_step = b.step("visitor", "Build the Ini visitor program (to print INI section results)");
    const ini5_visitor = b.addExecutable(.{
        .name = "ini-visitor",
        .target = target_options,
        .optimize = optimize_options,
        .root_source_file = b.path("lib/INIVisitor.zig"),
        .link_libc = true,
    });

    build_step.dependOn(&b.addInstallArtifact(ini5_visitor, .{
        .dest_sub_path = "inivisitor",
    }).step);
}
