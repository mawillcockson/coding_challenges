const std = @import("std");
const pow = std.math.pow;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var a: u2 = 0;
    try stdout.print("a starts with a value of -> {d}\n", .{a});

    const max_int = comptime c: {
        break :c std.math.maxInt(@TypeOf(a));
    };
    try stdout.print("a can hold a value up to {d}\n", .{max_int});

    inline for (0..max_int) |_| {
        try stdout.print("add 1 to get -> {d}\n", .{a + 1});
        a += 1;
    }
    try stdout.print("add 1 more to overflow\n", .{});
    a += 1;

    try stdout.print("a is -> {d}\n", .{a});
}
