const std = @import("std");
const Allocator = std.mem.Allocator;
const toLower = std.ascii.toLower;
const toUpper = std.ascii.toUpper;
const MatchType = @import("pattern.zig").MatchType;
const testing = std.testing;
const expect = testing.expect;

// FIXME: get this from straight type bit size
const max_straight_exponent = 32;

const path_separator = [_]u8{ '.', '/', '\\' };
const boundary_set = [_]u8{ '_', ' ', '-' } ++ path_separator;

const Score = struct {
    _copy: isize = 0,
    _delete: isize = 0,
    // negative score for when match is the beg/end of a boundary
    _boundary_delete: isize = 0,
    // positive score for when match is the beg/end of a boundary
    _boundary_match: isize = 0,
    _kill: isize = 0,
    _straight_acc: isize = 0,
    _full: bool = false,

    const qc: isize = 1;
    const qd: isize = -1;
    const qb_del: isize = -1;
    const qb_match: isize = 1;
    const qk: isize = -1;
    const qf: isize = std.math.maxInt(isize);
    inline fn qs(x: u5) isize {
        // TODO: evaluate the below with a comparison test
        // the score of 1 will be +2 which, combined by copy will contribute +3 in total. How this will impact short strings scoring?
        // It feels like the score of 0 and 1 should be all 0?

        // return if (x == 0 or x == 1) 0 else (@as(isize, 1) << (x + 1)) - 1;
        return (@as(isize, 1) << (x + 1)) - 2;
    }

    inline fn copy(self: *Score, x: isize) void {
        self._copy += x;
    }
    inline fn delete(self: *Score, x: isize) void {
        self._delete += x;
    }
    inline fn boundary_delete(self: *Score) void {
        self._boundary_delete += 1;
    }
    inline fn boundary_match(self: *Score) void {
        self._boundary_match += 1;
    }
    inline fn kill(self: *Score, x: isize) void {
        self._kill += x;
    }
    inline fn straight(self: *Score, x: u5) void {
        self._straight_acc += qs(x);
    }
    inline fn full(self: *Score) void {
        self._full = true;
    }
    pub inline fn score(self: Score) isize {
        return if (self._full) qf else self._copy * qc +
            self._delete * qd +
            self._boundary_delete * qb_del +
            self._boundary_match * qb_match +
            self._kill * qk +
            self._straight_acc;
    }
    fn string(self: Score, allocator: Allocator) ![]u8 {
        return if (self._full) std.fmt.allocPrint(allocator, "FULL MATCH", .{}) else std.fmt.allocPrint(allocator, "\ncopy: {d}\ndelete: {d}\nboundary_del: {d}\nboundary_match: {d}\nkill: {d}\nstraight_acc: {d}\nSCORE: {d}\n--------", .{
            self._copy,
            self._delete,
            self._boundary_delete,
            self._boundary_match,
            self._kill,
            self._straight_acc,
            self.score(),
        });
    }
};

pub fn match(text: []const u8, pattern: []const u8, is_case_sensitive: bool, match_type: MatchType) ?isize {
    if (pattern.len == 0) return (Score{}).score(); // empty pattern matches everything, even in inverse

    const MT = MatchType;
    const is_fuzzy = switch (match_type) {
        MT.exact,
        MT.prefix_exact,
        MT.suffix_exact,
        MT.inverse_exact,
        MT.inverse_prefix_exact,
        MT.inverse_suffix_exact,
        => false,
        MT.fuzzy => true,
    };

    const score = if (is_fuzzy) fuzzyMatch(text, pattern, is_case_sensitive) else exactMatch(text, pattern, is_case_sensitive, match_type);
    return if (score) |value| value.score() else null;
}
test "empty pattern and full match" {
    const cs = true; // case-sensitive
    const ci = false; // case-insensitive
    const MT = MatchType;
    const full_score = (Score{ ._full = true }).score();
    inline for (@typeInfo(MT).Enum.fields) |field| {
        const match_type: MT = @enumFromInt(field.value);
        const empty_score = (Score{}).score();

        var s = match("foo", "", ci, match_type);
        try expect(s.? == empty_score);
        s = match("foo", "", cs, match_type);
        try expect(s.? == empty_score);

        if (match_type == MT.inverse_exact or match_type == MT.inverse_prefix_exact or match_type == MT.inverse_suffix_exact) continue;
        s = match("foo", "foo", ci, match_type);
        try expect(s.? == full_score);
        s = match("Foo", "Foo", cs, match_type);
        try expect(s.? == full_score);
        s = match("Foo", "foo", cs, match_type);
        try expect(s == null);
    }
}
fn exactMatch(text: []const u8, pattern: []const u8, is_case_sensitive: bool, match_type: MatchType) ?Score {
    const MT = MatchType;
    // negate determines the inverse match.
    const negate = switch (match_type) {
        MT.exact, MT.prefix_exact, MT.suffix_exact => false,
        MT.inverse_exact, MT.inverse_prefix_exact, MT.inverse_suffix_exact => true,
        MT.fuzzy => unreachable,
    };
    // reverse determines the direction of match. From end-to-beg (reverse) or vice versa
    const reverse = switch (match_type) {
        MT.inverse_exact, MT.exact, MT.suffix_exact, MT.inverse_suffix_exact => true,
        MT.prefix_exact, MT.inverse_prefix_exact => false,
        MT.fuzzy => unreachable,
    };

    var i = if (reverse) text.len else 0;
    var j = if (reverse) pattern.len else 0;
    while ((i > 0 and j > 0 and reverse) or (i < text.len and j < pattern.len and !reverse)) : ({
        // i = i + if(reverse) -1 else 1 // FIXME: why I can't use this to simplify loop? error: value with comptime-only type 'comptime_int' depends on runtime control flow
        if (reverse) {
            i -= 1;
        } else {
            i += 1;
        }
    }) {
        const t_idx = if (reverse) i - 1 else i;
        const p_idx = if (reverse) j - 1 else j;
        const t_ch = text[t_idx];
        const p_ch = pattern[p_idx];
        const ti = if (is_case_sensitive) t_ch else toLower(t_ch);
        const pj = if (is_case_sensitive) p_ch else toLower(p_ch);

        if ((ti == pj and !negate) or (ti == pj and negate)) {
            if (reverse) {
                j -= 1;
            } else {
                j += 1;
            }
        } else if (match_type == MT.exact) {
            j = if (reverse) pattern.len else 0;
        } else {
            break;
        }
    }

    const match_found = (j == if (reverse) 0 else pattern.len) != negate;
    return if (match_found) {
        var score = Score{};
        if (negate) return score; // inverse doesn't have a baseline score

        if (pattern.len == text.len) {
            score.full();
        } else {
            score.copy(@intCast(pattern.len));
            score.straight(@intCast(@min(pattern.len, max_straight_exponent)));
            if (i == 0 or i == text.len or
                (reverse and isStartBoundary(text, i)) or
                (!reverse and isEndBoundary(text, i - 1)))
            {
                score.boundary_match();
            }
        }
        return score;
    } else null;
}
test "exact match" {
    const cs = true; // case-sensitive
    const ci = false; // case-insensitive
    const MT = MatchType;
    const qs = Score.qs;
    const empty_score = (Score{}).score();

    { // exact
        const mt = MT.exact;
        var s = exactMatch("foobar", "bar", ci, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);

        s = exactMatch("fOobar", "fOo", cs, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);
        try expect(1 == s.?._boundary_match);

        s = exactMatch("foo_barxar", "bar", cs, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);
        try expect(1 == s.?._boundary_match);

        s = exactMatch("fooBar", "Bar", cs, mt);
        try expect(1 == s.?._boundary_match);

        s = exactMatch("foobar", "Bar", cs, mt);
        try expect(s == null);

        s = exactMatch("foo", "foobar", cs, mt);
        try expect(s == null);
    }
    { // suffix_exact
        const mt = MT.suffix_exact;
        var s = exactMatch("foobar", "bar", ci, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);

        s = exactMatch("foobar", "foo", ci, mt);
        try expect(null == s);

        s = exactMatch("fooBar", "Bar", cs, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);
        try expect(1 == s.?._boundary_match);

        s = exactMatch("foobAr", "bAr", cs, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);
        try expect(0 == s.?._boundary_match);

        s = exactMatch("foobAr", "bar", cs, mt);
        try expect(s == null);
        s = exactMatch("foobar", "bAr", cs, mt);
        try expect(s == null);

        s = exactMatch("foobar", "xar", ci, mt);
        try expect(s == null);
        s = exactMatch("foobar", "bax", ci, mt);
        try expect(s == null);

        s = exactMatch("foo", "foobar", cs, mt);
        try expect(s == null);
    }
    { // prefix_exact
        const mt = MT.prefix_exact;
        var s = exactMatch("foobar", "foo", ci, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);

        s = exactMatch("fooBar", "foo", ci, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);
        try expect(1 == s.?._boundary_match);

        s = exactMatch("fOobar", "fOo", cs, mt);
        try expect(3 == s.?._copy);
        try expect(qs(3) == s.?._straight_acc);
        try expect(0 == s.?._boundary_match);

        s = exactMatch("fOobar", "foo", cs, mt);
        try expect(s == null);
        s = exactMatch("foobar", "fOo", cs, mt);
        try expect(s == null);

        s = exactMatch("foobar", "fox", ci, mt);
        try expect(s == null);
        s = exactMatch("foobar", "xoo", ci, mt);
        try expect(s == null);

        s = exactMatch("foo", "foobar", cs, mt);
        try expect(s == null);
    }
    { // inverse_prefix_exact
        const mt = MT.inverse_prefix_exact;
        var s = exactMatch("foobar", "foo", ci, mt);
        try expect(s == null);

        s = exactMatch("xoobar", "foo", ci, mt);
        try expect(s.?.score() == empty_score);
        s = exactMatch("foxbar", "foo", ci, mt);
        try expect(s.?.score() == empty_score);

        s = exactMatch("Foobar", "Foo", cs, mt);
        try expect(s == null);
        s = exactMatch("Foobar", "foo", cs, mt);
        try expect(s.?.score() == empty_score);

        s = exactMatch("foo", "foobar", cs, mt);
        try expect(s.?.score() == empty_score);
    }
    { // inverse_suffix_exact
        const mt = MT.inverse_suffix_exact;
        var s = exactMatch("foobar", "bar", ci, mt);
        try expect(s == null);

        s = exactMatch("foobax", "bar", ci, mt);
        try expect(s.?.score() == empty_score);
        s = exactMatch("fooxar", "bar", ci, mt);
        try expect(s.?.score() == empty_score);

        s = exactMatch("foobaR", "baR", cs, mt);
        try expect(s == null);
        s = exactMatch("fooBar", "bar", cs, mt);
        try expect(s.?.score() == empty_score);

        s = exactMatch("foo", "foobar", cs, mt);
        try expect(s.?.score() == empty_score);
    }
}

fn fuzzyMatch(text: []const u8, pattern: []const u8, is_case_sensitive: bool) ?Score {
    var score = Score{};

    var i = text.len;
    var j = pattern.len;
    var last_pj: u8 = undefined;

    var delete_acc: isize = 0;
    var straight_acc: u5 = 0;

    var boundary_slice: ?[]const u8 = null;
    var boundary_end: usize = undefined;

    while (i > 0 and j > 0) : (i -= 1) {
        const ti = if (is_case_sensitive) text[i - 1] else toLower(text[i - 1]);
        const pj = if (is_case_sensitive) pattern[j - 1] else toLower(pattern[j - 1]);

        if (isEndBoundary(text, i - 1)) {
            boundary_end = i;
        }
        if (isStartBoundary(text, i - 1)) {
            boundary_slice = text[i - 1 .. boundary_end];
        } else {
            boundary_slice = null;
        }

        if (ti == pj) {
            score.copy(1);
            last_pj = pj;
            straight_acc += 1;

            if (boundary_slice) |b| {
                delete_acc = 0;
                // we don't want to give negative score if straight len is the same as boundary. i.e. full match for this pattern
                // a boundary full match as extra positive score (boundary_match)
                if (straight_acc != b.len) {
                    score.boundary_delete();
                } else {
                    score.boundary_match();
                }
            }

            j -= 1;
        } else if (last_pj == ti) {
            score.copy(1);
        } else {
            delete_acc += 1;
            // commit straight
            score.straight(straight_acc);

            straight_acc = 0;
        }

        if (boundary_slice) |_| {
            score.delete(delete_acc);
            delete_acc = 0;
        }
    }

    return if (j == 0) {
        if (text.len == pattern.len) {
            score.full();
        } else {
            // commit what is left + kill
            score.straight(straight_acc);
            score.delete(delete_acc);

            // calculate kill
            while (i > 0) : (i -= 1) {
                var ti = text[i - 1];
                if (inPathSepSet(ti)) break else score.kill(1);
            }
        }

        return score;
    } else null;
}

test "fuzzy match" {
    const cs = true; // case-sensitive
    const ci = false; // case-insensitive
    { // Copy
        var r = fuzzyMatch("axy", "a", ci);
        try expect(r.?._copy == 1);
        r = fuzzyMatch("xya", "a", ci);
        try expect(r.?._copy == 1);

        r = fuzzyMatch("cbbaa", "cba", ci);
        try expect(r.?._copy == 5);
        // Last pj respect case-sensitivity
        r = fuzzyMatch("CBBAA", "cba", ci);
        try expect(r.?._copy == 5);
        r = fuzzyMatch("cbBaA", "cba", cs);
        try expect(r.?._copy == 3);
        // Last pj is only counted once because, search is terminated when j = 0
        r = fuzzyMatch("bbaa", "ba", ci);
        try expect(r.?._copy == 3);

        r = fuzzyMatch("baxyax", "ba", ci);
        try expect(r.?._copy == 3);
    }
    { // Straight
        const qs = Score.qs;

        var r = fuzzyMatch("_a", "a", ci);
        try expect(r.?._straight_acc == qs(1));

        r = fuzzyMatch("?abc?", "abc", ci);
        try expect(r.?._straight_acc == qs(3));

        r = fuzzyMatch("?abbbccc?", "abc", ci);
        try expect(r.?._straight_acc == qs(3));

        r = fuzzyMatch("?ab?cde?", "abcde", ci);
        try expect(r.?._straight_acc == qs(2) + qs(3));

        r = fuzzyMatch("?ab_cde?", "abcde", ci);
        try expect(r.?._straight_acc == qs(2) + qs(3));
    }
    { // Delete
        var r = fuzzyMatch("?axx", "a", ci);
        try expect(r.?._delete == 2);

        r = fuzzyMatch("?bxxaxxx", "ba", ci);
        try expect(r.?._delete == 5);
    }
    { // Boundary
        var r = fuzzyMatch("_axx", "a", cs);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 1);

        r = fuzzyMatch("_AXX", "a", ci);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 1);

        r = fuzzyMatch("?_a_B_c", "abc", ci);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 0);
        try expect(r.?._boundary_match == 3);

        r = fuzzyMatch("_axx_foo", "afoo", ci);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 1);
        try expect(r.?._boundary_match == 1);

        r = fuzzyMatch("AxxFoo", "afoo", ci);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 1);
        try expect(r.?._boundary_match == 1);

        r = fuzzyMatch("AxxFoo", "AFoo", cs);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 1);
        try expect(r.?._boundary_match == 1);

        r = fuzzyMatch("_axx_fff", "af", ci);
        try expect(r.?._delete == 0);
        try expect(r.?._boundary_delete == 1);
        // last pj is not counted as full-boundary match
        try expect(r.?._boundary_match == 0);

        r = fuzzyMatch("_?ax_xbx", "ab", ci);
        try expect(r.?._delete == 4);
        try expect(r.?._boundary_delete == 0);
    }
    { // Kill
        var r = fuzzyMatch("a", "a", ci);
        try expect(r.?._kill == 0);

        r = fuzzyMatch("xxa???", "a", ci);
        try expect(r.?._kill == 2);

        r = fuzzyMatch("??/xxa???", "a", ci);
        try expect(r.?._kill == 2);
    }
    {
        const full_score = (Score{ ._full = true }).score();
        var r = fuzzyMatch("foobar", "foo", cs);
        try expect(r.?.score() != full_score); // regression: making sure we don't just consider i==0 for full match
    }
}

fn inBoundarySet(ch: u8) bool {
    inline for (boundary_set) |b| {
        if (ch == b) return true;
    }
    return false;
}
fn inPathSepSet(ch: u8) bool {
    inline for (path_separator) |s| {
        if (ch == s) return true;
    }
    return false;
}

const isLower = std.ascii.isLower;
const isUpper = std.ascii.isUpper;
fn isStartBoundary(text: []const u8, i: usize) bool {
    const current = text[i];
    if (i == 0) {
        return !inBoundarySet(current);
    }

    const prev = text[i - 1];
    return (isUpper(current) and isLower(prev)) or inBoundarySet(prev);
}
fn isEndBoundary(text: []const u8, i: usize) bool {
    const current = text[i];
    if (i == text.len - 1) {
        return !inBoundarySet(current);
    }

    const next = text[i + 1];
    return (isLower(current) and isUpper(next)) or inBoundarySet(next);
}

test "start/end boundary" {
    var s = isStartBoundary("a", 0);
    var e = isEndBoundary("a", 0);
    try expect(s and e);

    s = isStartBoundary("_", 0);
    e = isEndBoundary("_", 0);
    try expect((s or e) == false);

    s = isStartBoundary("Foo", 0);
    e = isEndBoundary("Foo", 2);
    try expect(s and e);

    s = isStartBoundary("Foo", 2);
    e = isEndBoundary("Foo", 0);
    try expect((s or e) == false);

    s = isStartBoundary("FooBar", 3);
    e = isEndBoundary("FooBar", 2);
    try expect(s and e);

    s = isStartBoundary("FooBar", 2);
    e = isEndBoundary("FooBar", 3);
    try expect((s or e) == false);

    s = isStartBoundary("foo_bar", 4);
    e = isEndBoundary("foo_bar", 2);
    try expect(s and e);

    s = isStartBoundary("foo_bar", 3);
    e = isEndBoundary("foo_bar", 3);
    try expect((s or e) == false);

    s = isStartBoundary("BaR", 2);
    e = isEndBoundary("BaR", 2);
    try expect(s and e);
}
