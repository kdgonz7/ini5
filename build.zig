const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target_options = b.standardTargetOptions(.{});
    const optimize_options = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseSafe,
    });

    const ini5_tests = b.addTest(.{
        .root_source_file = b.path("lib/INI5.zig"),
    });

    const ini5_visitor = b.addExecutable(.{
        .name = "ini-visitor",
        .target = target_options,
        .optimize = optimize_options,
        .root_source_file = b.path("lib/INIVisitor.zig"),
        .link_libc = false,
    });

    var inivisitor_install_artifact = b.addInstallArtifact(ini5_visitor, .{
        .dest_sub_path = "inivisitor",
    });

    var ini5_tests_artifact = b.addRunArtifact(ini5_tests);

    const test_step = b.step("tests", "Run unit tests for INI5. They should all be passing.");
    const build_step = b.step("visitor", "Build the Ini visitor program (to print INI section results)");
    const all_step = b.step("all", "Builds everything, visitor and tests first.");
    test_step.dependOn(&ini5_tests_artifact.step);
    build_step.dependOn(&inivisitor_install_artifact.step);

    all_step.dependOn(&ini5_tests.step);
    all_step.dependOn(&ini5_visitor.step);
}
