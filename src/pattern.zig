const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

pub const MatchType = enum {
    fuzzy,
    exact,
    prefix_exact,
    suffix_exact,
    inverse_exact,
    inverse_prefix_exact,
    inverse_suffix_exact,
};

pub const Pattern = struct {
    raw: []const u8 = undefined,
    match_type: MatchType = MatchType.fuzzy,

    fn allocPrint(pattern: Pattern, alloc: Allocator) ![]const u8 {
        const MT = MatchType;
        return switch (pattern.match_type) {
            MT.fuzzy => std.fmt.allocPrint(alloc, "{s}", .{pattern.raw}),
            MT.exact => std.fmt.allocPrint(alloc, "'{s}", .{pattern.raw}),
            MT.prefix_exact => std.fmt.allocPrint(alloc, "^{s}", .{pattern.raw}),
            MT.suffix_exact => std.fmt.allocPrint(alloc, "{s}$", .{pattern.raw}),
            MT.inverse_exact => std.fmt.allocPrint(alloc, "!{s}", .{pattern.raw}),
            MT.inverse_prefix_exact => std.fmt.allocPrint(alloc, "!^{s}", .{pattern.raw}),
            MT.inverse_suffix_exact => std.fmt.allocPrint(alloc, "!{s}$", .{pattern.raw}),
        };
    }
};

pub fn caseVaries(text: []const u8) bool {
    if (text.len < 2) return false;

    var case = std.ascii.isUpper(text[0]);
    var i: usize = 1;
    while (i < text.len) : (i += 1) {
        case = case != std.ascii.isUpper(text[i]);
        if (case) {
            return true;
        }
    }

    return false;
}

test "case varies" {
    try expect(!caseVaries(""));
    try expect(!caseVaries("f"));
    try expect(!caseVaries("F"));
    try expect(!caseVaries("FF"));
    try expect(!caseVaries("ff"));

    try expect(caseVaries("Fo"));
    try expect(caseVaries("fF"));
    try expect(caseVaries("fOo"));
    try expect(caseVaries("FoO"));
}
