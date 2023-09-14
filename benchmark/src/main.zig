const std = @import("std");
const time = std.time;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const mf = @import("matchfinder");

fn benchmark_lines(allocator: Allocator, path: []const u8) !ArrayList([]const u8) {
    var list = try ArrayList([]const u8).initCapacity(allocator, 80000);
    const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    const reader = file.reader();

    const content = try reader.readAllAlloc(allocator, 1024 * 1024 * 4);

    var i: usize = 0;
    var start: usize = 0;
    // TODO: use streamUntilDelimiter
    while (i < content.len) : (i += 1) {
        if (content[i] == '\n') {
            try list.append(content[start..i]);
            start = i + 1;
        }
    }

    return list;
}

const Result = struct {
    text: []const u8,
    score: isize,
};

fn sortByScore(context: void, a: Result, b: Result) bool {
    _ = context;
    return a.score > b.score;
}

fn sortByScoreValue(context: void, a: isize, b: isize) bool {
    _ = context;
    return a > b;
}

fn run(a: Allocator, texts: []const []const u8) !void {
    // make sure there is no results so we get worse-case scenario
    const pattern = "netled";

    var results = ArrayList(Result).init(a);

    var timer = try time.Timer.start();
    for (texts) |text| {
        if (mf.match(text, pattern, false, mf.MatchType.fuzzy)) |score| {
            try results.append(.{ .text = text, .score = score });
        }
    }
    const lapsed = timer.lap();

    var sorted = try results.toOwnedSlice();
    std.sort.heap(Result, sorted, {}, sortByScore);

    std.log.warn("\npattern: {s} *** matched: {d}/{d} ** time: {d}ms\n-----------------", .{ pattern, sorted.len, texts.len, lapsed / 1_000_000 });

    if (sorted.len > 0) {
        for (0..@min(10, sorted.len - 1)) |i| {
            std.log.warn("[{d:4}] {s}", .{ sorted[i].score, sorted[i].text });
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const a = arena.allocator();
    const f = try std.fs.cwd().realpathAlloc(a, "benchmark/src/bench_data.txt");
    const lines = try benchmark_lines(a, f);
    std.log.warn("line: {s}", .{lines.items[5]});
    try run(a, lines.items);
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}

test "simple test" {}
