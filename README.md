### MatchFinder
A fuzzy finder similar to [fzf-native-vim](https://github.com/nvim-telescope/telescope-fzf-native.nvim/tree/main/src) but with different scoring algorithm.

**NOTE:** This is not a CLI tool but a library.

### Search term
The search term is backward compatible with fzf. 
```
foo -> fuzzy
'foo -> exact
^foo -> prefix-exact
foo$ -> suffix-exact
!foo -> inverse-exact
!^foo -> inverse-prefix-exact
!foo$ -> inverse-suffix-exact

// Note that OR has precedence over AND
foo ^bar baz$ ->  (and (and foo ^bar) baz$)
'foo | !bar !baz$ | !^bax ->  (and (or 'foo !bar) (or !baz$ !^bax))
```

### Usage
See `main.zig` tests for more examples.
```zig
test "example usage" {
    const a = testing.allocator;

    const texts = &[_][]const u8{ "unique", "foo", "foobar", "barfoo", "$order^^^", "xyz_", "abxy", "$order^", "$order^^" };
    var matcher = MatchFinder.init(a, texts);
    defer matcher.deinit();

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
```