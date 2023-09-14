const std = @import("std");
const benchmark = @import("benchmark/build.zig");

pub const pkg = std.build.Pkg{
    .name = "matchfinder",
    .source = .{ .path = thisDir() ++ "/src/main.zig" },
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const options = b.addOptions();
    options.addOption(usize, "max_pattern_len", 32);

    const lib = b.addStaticLibrary(.{
        .name = "matchfinder",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const m = options.createModule();
    lib.addModule("mf_options", m);

    // benchmark
    var bench_exe = benchmark.package(b, optimize, target);
    bench_exe.linkLibrary(lib);
    b.installArtifact(bench_exe);

    const run_benchmark = b.addRunArtifact(bench_exe);
    run_benchmark.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_benchmark.addArgs(args);
    }
    const run_bench_step = b.step("benchmark", "Run the example app");
    run_bench_step.dependOn(&run_benchmark.step);

    b.installArtifact(lib);
    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    main_tests.addModule("mf_options", m);

    const run_main_tests = b.addRunArtifact(main_tests);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
}

pub fn buildTests(
    b: *std.build.Builder,
    build_mode: std.builtin.Mode,
    target: std.zig.CrossTarget,
) *std.build.LibExeObjStep {
    const tests = b.addTest(pkg.source.path);
    tests.setBuildMode(build_mode);
    tests.setTarget(target);
    return tests;
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}
