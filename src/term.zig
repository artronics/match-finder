const std = @import("std");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const pattern = @import("pattern.zig");
const MatchType = pattern.MatchType;
const Pattern = pattern.Pattern;
const testing = std.testing;
const expect = testing.expect;
const sliceEq = testing.expectEqualSlices;
// TODO: import mf_options doesn't work when running test for this file
// const options = @import("mf_options");
const options = .{ .max_pattern_len = 64, .max_sub_pattern_size = 8 };
const max_pattern_len = options.max_pattern_len;

const BinExpr = struct {
    l: *Expr,
    r: *Expr,

    fn string(e: BinExpr, alloc: Allocator, str: *ArrayList(u8)) !void {
        try e.l.string(alloc, str);
        try str.append(' ');
        try e.r.string(alloc, str);
        try str.append(')');
    }
};
pub const Expr = union(enum) {
    and_op: BinExpr,
    or_op: BinExpr,
    pattern: Pattern,

    fn string(e: Expr, alloc: Allocator, str: *ArrayList(u8)) Allocator.Error!void {
        switch (e) {
            Expr.and_op => |op| {
                try str.appendSlice("(and ");
                try op.string(alloc, str);
            },
            Expr.or_op => |op| {
                try str.appendSlice("(or ");
                try op.string(alloc, str);
            },
            Expr.pattern => |ptrn| {
                const s = try ptrn.allocPrint(alloc);
                defer alloc.free(s);
                try str.appendSlice(s);
            },
        }
    }
};

pub const Term = struct {
    arena: ArenaAllocator,
    expr: Expr = undefined,
    buf: []u8 = undefined,

    pub fn init(allocator: Allocator) Term {
        return .{ .arena = ArenaAllocator.init(allocator) };
    }
    pub fn deinit(self: Term) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Term, term: []const u8) !void {
        _ = self.arena.reset(ArenaAllocator.ResetMode.retain_capacity);
        const alloc = self.arena.allocator();

        self.buf = try alloc.alloc(u8, term.len);

        var scanner = Scanner.init(alloc, term, self.buf);
        defer scanner.deinit();
        const tokens = try scanner.scan();

        var parser = Parser.init(alloc, tokens);

        self.expr = try parser.parse();
    }

    fn allocPrint(term: Term, alloc: Allocator) ![]const u8 {
        var string = ArrayList(u8).init(alloc);
        defer string.deinit();
        try term.expr.string(alloc, &string);

        return string.toOwnedSlice();
    }
};

test "term" {
    const a = testing.allocator;
    var term = Term.init(a);
    defer term.deinit();

    {
        try term.parse("foo bar");

        try sliceEq(u8, "foo", term.expr.and_op.l.pattern.raw);
        try sliceEq(u8, "bar", term.expr.and_op.r.pattern.raw);
    }
    {
        try term.parse("baz | bax");

        try sliceEq(u8, "baz", term.expr.or_op.l.pattern.raw);
        try sliceEq(u8, "bax", term.expr.or_op.r.pattern.raw);
    }
    {
        try term.parse("foobar");
        try sliceEq(u8, "foobar", term.expr.pattern.raw);
    }
}
test "term handling errors" {
    const a = testing.allocator;
    var term = Term.init(a);
    defer term.deinit();

    // Insufficient tokens should produce an empty pattern with default fields
    {
        try term.parse("foo | ");

        try expect(Expr.or_op == term.expr);
        try sliceEq(u8, "foo", term.expr.or_op.l.pattern.raw);

        try sliceEq(u8, "", term.expr.or_op.r.pattern.raw);
        try expect(MatchType.fuzzy == term.expr.or_op.r.pattern.match_type);
    }
    {
        try term.parse("bar ");

        try expect(Expr.and_op == term.expr);
        try sliceEq(u8, "bar", term.expr.and_op.l.pattern.raw);

        try sliceEq(u8, "", term.expr.and_op.r.pattern.raw);
        try expect(MatchType.fuzzy == term.expr.and_op.r.pattern.match_type);
    }
    // Pending for pattern shouldn't cause errors, instead should assume empty string
    {
        try term.parse("!");

        try expect(Expr.pattern == term.expr);
        try sliceEq(u8, "", term.expr.pattern.raw);
        try expect(MatchType.inverse_exact == term.expr.pattern.match_type);
    }
    {
        try term.parse("!^");

        try expect(Expr.pattern == term.expr);
        try sliceEq(u8, "", term.expr.pattern.raw);
        try expect(MatchType.inverse_prefix_exact == term.expr.pattern.match_type);
    }
    {
        try term.parse("!$");

        try expect(Expr.pattern == term.expr);
        try sliceEq(u8, "", term.expr.pattern.raw);
        try expect(MatchType.inverse_suffix_exact == term.expr.pattern.match_type);
    }
}

const ParseError = error{InsufficientToken};

const Parser = struct {
    const Self = @This();

    allocator: Allocator,
    tokens: []const Token,
    current: usize = 0,

    fn init(allocator: Allocator, tokens: []const Token) Self {
        return .{
            .allocator = allocator,
            .tokens = tokens,
        };
    }

    fn parse(self: *Self) !Expr {
        return self.expr();
    }

    fn expr(self: *Self) !Expr {
        return self.andExpr();
    }

    // andExpr -> orExpr (<space> orExpr)*
    fn andExpr(self: *Self) !Expr {
        var lhs = try self.orExpr();

        while (self.matchAny(&.{Tag.and_op})) {
            var l = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(l);
            l.* = lhs;

            const rhs = try self.orExpr();
            var r = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(r);
            r.* = rhs;

            lhs = Expr{ .and_op = BinExpr{ .l = l, .r = r } };
        }

        return lhs;
    }
    // orExpr -> pattern ( | pattern)*
    fn orExpr(self: *Self) !Expr {
        var lhs = self.pattern();

        while (self.matchAny(&.{Tag.or_op})) {
            var l = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(l);
            l.* = lhs;

            const rhs = self.pattern();
            var r = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(r);
            r.* = rhs;

            lhs = Expr{ .or_op = BinExpr{ .l = l, .r = r } };
        }

        return lhs;
    }

    // pattern -> !?^?TEXT$?
    fn pattern(self: *Self) Expr {
        if (self.matchAny(&.{ Tag.exact, Tag.inverse, Tag.prefix, Tag.suffix, Tag.text })) {
            var _pattern = Pattern{};
            const prev = self.previous();
            switch (prev.tag) {
                Tag.text => {
                    _pattern.raw = prev.lexeme.?;
                    if (self.match(Tag.suffix)) {
                        _pattern.match_type = MatchType.suffix_exact;
                        _ = self.advance();
                    }
                },
                Tag.exact => {
                    _pattern.match_type = MatchType.exact;
                    const text = self.consume(Tag.text, "expected text; fallback to empty string") catch Token{ .tag = Tag.text, .lexeme = "" };
                    _pattern.raw = text.lexeme.?;
                },
                Tag.inverse => {
                    if (self.match(Tag.text)) {
                        _pattern.match_type = MatchType.inverse_exact;
                    } else if (self.match(Tag.prefix)) {
                        _pattern.match_type = MatchType.inverse_prefix_exact;
                        _ = self.advance();
                    }
                    const text = self.consume(Tag.text, "expected text; fallback to empty string") catch Token{ .tag = Tag.text, .lexeme = "" };
                    _pattern.raw = text.lexeme.?;
                    if (self.match(Tag.suffix)) {
                        // FIXME: !^foo$ makes no sense. Yet here, if this happened we'll ignore the prefix and instead use the suffix
                        // What is the right behaviour?
                        _pattern.match_type = MatchType.inverse_suffix_exact;
                        _ = self.advance();
                    }
                },
                Tag.prefix => {
                    _pattern.match_type = MatchType.prefix_exact;
                    const text = self.consume(Tag.text, "expected text; fallback to empty string") catch Token{ .tag = Tag.text, .lexeme = "" };
                    _pattern.raw = text.lexeme.?;
                },
                else => unreachable,
            }
            return Expr{ .pattern = _pattern };
        }
        return .{ .pattern = .{} };
    }
    fn advance(self: *Self) Token {
        if (!self.isEof()) self.current += 1;
        return self.previous();
    }

    fn matchAny(self: *Self, tags: []const Tag) bool {
        for (tags) |tag| {
            if (self.match(tag)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn match(self: *Self, tag: Tag) bool {
        return self.tokens[self.current].tag == tag;
    }

    fn isEof(self: *Self) bool {
        return self.tokens[self.current].tag == Tag.eof;
    }

    fn previous(self: *Self) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Self, tag: Tag, msg: []const u8) !Token {
        if (self.match(tag)) return self.advance();
        std.log.warn("Unexpected token: {s}", .{msg});
        return ParseError.InsufficientToken;
    }

    test "parser" {
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        {
            // empty
            const tokens = [_]Token{.{ .tag = Tag.eof }};

            var parser = Parser.init(arena.allocator(), &tokens);
            const exp = try parser.parse();
            try sliceEq(u8, "", exp.pattern.raw);
            try expect(exp.pattern.match_type == MatchType.fuzzy);
        }
        {
            // foo ^bar baz$ ->  (and (and foo ^bar) baz$)
            const tokens = [_]Token{
                .{ .tag = Tag.text, .lexeme = "foo" },

                .{ .tag = Tag.and_op },

                .{ .tag = Tag.prefix },
                .{ .tag = Tag.text, .lexeme = "bar" },

                .{ .tag = Tag.and_op },

                .{ .tag = Tag.text, .lexeme = "baz" },
                .{ .tag = Tag.suffix },

                .{ .tag = Tag.eof },
            };

            var parser = Parser.init(arena.allocator(), &tokens);

            const exp = try parser.parse();

            const root = exp.and_op;
            // left
            const l_and = root.l.and_op;
            try sliceEq(u8, "foo", l_and.l.pattern.raw);
            try expect(l_and.l.pattern.match_type == MatchType.fuzzy);

            try sliceEq(u8, "bar", l_and.r.pattern.raw);
            try expect(l_and.r.pattern.match_type == MatchType.prefix_exact);
            // right
            try sliceEq(u8, "baz", root.r.pattern.raw);
            try expect(root.r.pattern.match_type == MatchType.suffix_exact);
        }
        {
            // 'foo | !bar !baz$ | !^bax ->  (and (or 'foo !bar) (or !baz$ !^bax))
            const tokens = [_]Token{
                .{ .tag = Tag.exact },
                .{ .tag = Tag.text, .lexeme = "foo" },

                .{ .tag = Tag.or_op },

                .{ .tag = Tag.inverse },
                .{ .tag = Tag.text, .lexeme = "bar" },

                .{ .tag = Tag.and_op },

                .{ .tag = Tag.inverse },
                .{ .tag = Tag.text, .lexeme = "baz" },
                .{ .tag = Tag.suffix },

                .{ .tag = Tag.or_op },

                .{ .tag = Tag.inverse },
                .{ .tag = Tag.prefix },
                .{ .tag = Tag.text, .lexeme = "bax" },

                .{ .tag = Tag.eof },
            };
            var parser = Parser.init(arena.allocator(), &tokens);

            const exp = try parser.parse();

            const and_expr = exp.and_op;
            // left
            const l_or = and_expr.l.or_op;
            try sliceEq(u8, "foo", l_or.l.pattern.raw);
            try expect(l_or.l.pattern.match_type == MatchType.exact);

            try sliceEq(u8, "bar", l_or.r.pattern.raw);
            try expect(l_or.r.pattern.match_type == MatchType.inverse_exact);
            // right
            const r_or = and_expr.r.or_op;
            try sliceEq(u8, "baz", r_or.l.pattern.raw);
            try expect(r_or.l.pattern.match_type == MatchType.inverse_suffix_exact);

            try sliceEq(u8, "bax", r_or.r.pattern.raw);
            try expect(r_or.r.pattern.match_type == MatchType.inverse_prefix_exact);
        }
    }
};

const Tag = enum {
    and_op,
    or_op,
    exact,
    prefix,
    suffix,
    inverse,
    text,
    eof,
};
const Token = struct {
    tag: Tag,
    lexeme: ?[]const u8 = null,
};

const Scanner = struct {
    const delimiter = ' ';

    allocator: Allocator,
    source: []const u8,
    tokens: ArrayList(Token),

    buf: []u8,
    buf_i: usize = 0,

    start: usize = 0,
    current: usize = 0,

    fn init(allocator: Allocator, source: []const u8, buf: []u8) Scanner {
        return .{
            .allocator = allocator,
            .source = source,
            .buf = buf,
            .tokens = ArrayList(Token).init(allocator),
        };
    }
    fn deinit(s: Scanner) void {
        s.tokens.deinit();
    }
    fn scan(s: *Scanner) ![]const Token {
        while (!s.isEof()) {
            try s.pattern();
            try s.operator();
        }
        try s.tokens.append(.{ .tag = Tag.eof });
        return s.tokens.items;
    }
    fn pattern(s: *Scanner) !void {
        if (s.match('!')) {
            try s.tokens.append(.{ .tag = Tag.inverse });

            if (s.match('\'')) {
                try s.tokens.append(.{ .tag = Tag.exact });
            } else if (s.match('^')) {
                try s.tokens.append(.{ .tag = Tag.prefix });
            }
        } else if (s.match('\'')) {
            try s.tokens.append(.{ .tag = Tag.exact });
        } else if (s.match('^')) {
            try s.tokens.append(.{ .tag = Tag.prefix });
        } else if (s.peek() == '\\' and (s.peekNext() == '\'' or s.peekNext() == '^' or s.peekNext() == '!')) {
            _ = s.advance();
        }
        try s.matchText();
    }
    fn operator(s: *Scanner) !void {
        if (s.match(' ')) {
            if (s.peek() != '|') {
                try s.tokens.append(.{ .tag = Tag.and_op });
            } else if (s.peek() == '|' and s.peekNext() == delimiter) {
                try s.tokens.append(.{ .tag = Tag.or_op });
                _ = s.advance();
                _ = s.advance();
            }
        }
    }
    fn matchText(s: *Scanner) !void {
        const start = s.buf_i;
        var has_suffix = false;

        // break until under:  "foo", "foo ", "foo$", "foo$ " and account for escape: "foo\$", "foo\ bar" etc
        while (!s.isEof()) {
            if (s.peek() == '\\' and (s.peekNext() == ' ' or s.peekNext() == '$')) {
                _ = s.advance();
                s.buf[s.buf_i] = s.advance();
                s.buf_i += 1;
            } else if (s.peek() == '$' and (s.peekNext() == delimiter or s.peekNext() == 0)) {
                _ = s.advance();
                has_suffix = true;
                break;
            } else if (s.peek() == delimiter) {
                break;
            } else {
                s.buf[s.buf_i] = s.advance();
                s.buf_i += 1;
            }
        }
        try s.tokens.append(.{ .tag = Tag.text, .lexeme = s.buf[start..s.buf_i] });
        if (has_suffix) {
            try s.tokens.append(.{ .tag = Tag.suffix });
        }
    }

    fn advance(s: *Scanner) u8 {
        const ch = s.source[s.current];
        s.current += 1;

        return ch;
    }
    fn match(s: *Scanner, expected: u8) bool {
        if (s.isEof()) return false;
        if (s.source[s.current] != expected) return false;

        s.current += 1;

        return true;
    }
    fn peek(s: *Scanner) u8 {
        if (s.isEof()) return 0;
        return s.source[s.current];
    }
    fn peekNext(s: *Scanner) u8 {
        if (s.current + 1 >= s.source.len) return 0;
        return s.source[s.current + 1];
    }
    fn isEof(s: *Scanner) bool {
        return s.current >= s.source.len;
    }

    test "Scanner" {
        const a = testing.allocator;
        var buf = try a.alloc(u8, 1024);
        defer a.free(buf);
        {
            const term = "";
            var s = Scanner.init(a, term, buf);
            defer s.deinit();

            const tokens = try s.scan();

            try expect(tokens[0].tag == Tag.eof);
        }
        {
            const term = "foobar";
            var s = Scanner.init(a, term, buf);
            defer s.deinit();

            const tokens = try s.scan();

            try expect(tokens[0].tag == Tag.text);
            try sliceEq(u8, "foobar", tokens[0].lexeme.?);
        }
        {
            const term = "foo bar | baz";
            var s = Scanner.init(a, term, buf);
            defer s.deinit();

            const tokens = try s.scan();

            try expect(tokens[0].tag == Tag.text);
            try sliceEq(u8, "foo", tokens[0].lexeme.?);

            try expect(tokens[1].tag == Tag.and_op);

            try expect(tokens[2].tag == Tag.text);
            try sliceEq(u8, "bar", tokens[2].lexeme.?);

            try expect(tokens[3].tag == Tag.or_op);

            try expect(tokens[4].tag == Tag.text);
            try sliceEq(u8, "baz", tokens[4].lexeme.?);
        }
        {
            const term = "!'foo !^bar$ !bax | ^!baz$";
            var s = Scanner.init(a, term, buf);
            defer s.deinit();

            const tokens = try s.scan();

            try expect(tokens[0].tag == Tag.inverse);
            try expect(tokens[1].tag == Tag.exact);
            try expect(tokens[2].tag == Tag.text);
            try sliceEq(u8, "foo", tokens[2].lexeme.?);
            try expect(tokens[3].tag == Tag.and_op);
            try expect(tokens[4].tag == Tag.inverse);
            try expect(tokens[5].tag == Tag.prefix);
            try sliceEq(u8, "bar", tokens[6].lexeme.?);
            try expect(tokens[7].tag == Tag.suffix);
            try expect(tokens[8].tag == Tag.and_op);
            try expect(tokens[9].tag == Tag.inverse);
            try expect(tokens[10].tag == Tag.text); // bax
            try expect(tokens[11].tag == Tag.or_op);
            try expect(tokens[12].tag == Tag.prefix);
            try expect(tokens[13].tag == Tag.text);
            try sliceEq(u8, "!baz", tokens[13].lexeme.?); // ! after match_type is just character
            try expect(tokens[14].tag == Tag.suffix);
        }
    }
    test "Scanner escape" {
        const a = testing.allocator;
        var buf = try a.alloc(u8, 1024);
        defer a.free(buf);

        const term = "\\!foo\\ bar\\$\\ ^bax\\ |\\ baz";
        var s = Scanner.init(a, term, buf);
        defer s.deinit();

        const tokens = try s.scan();

        try expect(tokens[0].tag == Tag.text);
        try sliceEq(u8, "!foo bar$ ^bax | baz", tokens[0].lexeme.?);
    }
};
