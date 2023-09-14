const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const algo = @import("algo.zig");
const term = @import("term.zig");
const pattern = @import("pattern.zig");
const Pattern = pattern.Pattern;
const testing = std.testing;
const expect = testing.expect;
const sliceEq = testing.expectEqualSlices;

pub const match = algo.match;
pub const MatchType = pattern.MatchType;

pub const MatchFinder = struct {
    const Self = @This();

    allocator: Allocator,
    term: term.Term,
    smart_case: bool = false,
    texts: []const []const u8 = undefined,

    pub fn init(allocator: Allocator, texts: []const []const u8) Self {
        return .{
            .allocator = allocator,
            .term = term.Term.init(allocator),
            .texts = texts,
        };
    }
    pub fn deinit(self: Self) void {
        self.term.deinit();
    }

    const Result = struct {
        text: []const u8,
        score: isize,

        fn sortByScore(context: void, a: Result, b: Result) bool {
            _ = context;
            return a.score > b.score;
        }
    };

    /// client owns the result slice.
    pub fn search(self: *Self, term_str: []const u8) ![]const []const u8 {
        try self.term.parse(term_str);

        var results = ArrayList(Result).init(self.allocator);
        defer results.deinit();
        for (self.texts) |text| {
            if (self.reduceScore(text, null, self.term.expr)) |score| {
                const res = Result{ .text = text, .score = score };
                try results.append(res);
            }
        }

        // sort based on score and copy only texts for final results
        std.sort.heap(Result, results.items, {}, Result.sortByScore);
        var sorted = try self.allocator.alloc([]const u8, results.items.len);
        std.log.warn("------------", .{});
        for (results.items, 0..results.items.len) |item, i| {
            sorted[i] = item.text;
            std.log.warn("[{d}]: {s}", .{ item.score, item.text });
        }

        return sorted;
    }
    fn reduceScore(self: Self, text: []const u8, acc: ?isize, expr: term.Expr) ?isize {
        // short circuit both "and" and "or" by evaluating left branch first
        return switch (expr) {
            term.Expr.pattern => |_pattern| {
                const case_sen = self.smart_case and pattern.caseVaries(_pattern.raw);
                const score = match(text, _pattern.raw, case_sen, _pattern.match_type);

                return if (score) |pattern_s| {
                    return if (acc) |acc_s| {
                        // FIXME: Should it be addition or max? same for other branches
                        // return ss +| s;
                        return @max(pattern_s, acc_s);
                    } else pattern_s;
                } else null;
            },
            term.Expr.and_op => |op| {
                const _ls = self.reduceScore(text, acc, op.l.*);
                return if (_ls) |ls| {
                    const _rs = self.reduceScore(text, ls, op.r.*);
                    return if (_rs) |rs| {
                        // return s +| ss;
                        return @max(ls, rs);
                    } else null;
                } else null;
            },
            term.Expr.or_op => |op| {
                const _ls = self.reduceScore(text, acc, op.l.*);
                return if (_ls) |ls| {
                    return ls;
                } else {
                    const _rs = self.reduceScore(text, acc, op.r.*);
                    return if (_rs) |rs| rs else null;
                };
            },
        };
    }
};

test "Matcher" {
    const a = testing.allocator;
    // ? is used to make a distinction when we make assertions. They don't matter
    // ^ to reduce the score for order test
    // $ to eliminate boundary
    const texts = &[_][]const u8{ "unique", "foo", "foobar", "barfoo", "$order^^^", "xyz_", "abxy", "$order^", "$order^^" };
    var matcher = MatchFinder.init(a, texts);
    defer matcher.deinit();
    {
        const results = try matcher.search("unique");
        defer a.free(results);
        try expect(results.len == 1);
        try sliceEq(u8, "unique", results[0]);
    }
    { // order
        const results = try matcher.search("order");
        defer a.free(results);
        try expect(results.len == 3);
        try sliceEq(u8, "$order^", results[0]);
        try sliceEq(u8, "$order^^", results[1]);
        try sliceEq(u8, "$order^^^", results[2]);
    }
    { // exact, suffix and prefix
        const exact = try matcher.search("'foo");
        defer a.free(exact);
        try expect(exact.len == 3);

        const prefix = try matcher.search("^foo");
        defer a.free(prefix);
        try expect(prefix.len == 2);
        try sliceEq(u8, "foobar", prefix[1]);

        const suffix = try matcher.search("foo$");
        defer a.free(suffix);
        try expect(suffix.len == 2);
        try sliceEq(u8, "barfoo", suffix[1]);
    }
    { // inverse: exact, suffix and prefix
        const inverse = try matcher.search("!unique");
        defer a.free(inverse);
        try expect(inverse.len == texts.len - 1);

        const in_pre = try matcher.search("!^foo");
        defer a.free(in_pre);
        try expect(in_pre.len == texts.len - 2);

        const in_suf = try matcher.search("!foo$");
        defer a.free(in_suf);
        try expect(in_suf.len == texts.len - 2);
    }
    {
        const or_bin = try matcher.search("unique | foo");
        defer a.free(or_bin);
        try expect(or_bin.len == 4);

        const and_bin = try matcher.search("foo bar");
        defer a.free(and_bin);
        try expect(and_bin.len == 2);

        const or_and = try matcher.search("foo foo | bar");
        defer a.free(or_and);
        try expect(or_and.len == 3);
    }
}
