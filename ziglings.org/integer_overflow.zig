const std = @import("std");
const pow = std.math.pow;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var a: u2 = 0;
    try stdout.print("a starts with a value of -> {d}\n", .{a});

    const Int = @typeInfo(@TypeOf(a)).int;
    comptime if (Int.signedness != .unsigned) {
        @compileError("Expected a to be an unsigned in");
    };
    const max_int = comptime c: {
        break :c pow(u16, 2, Int.bits) -| 1;
    };
    try stdout.print("a can hold a value up to {d}\n", .{max_int});

    inline for (0..max_int) |_| {
        a += 1;
    }
    a += 1;

    try stdout.print("a is -> {d}\n", .{a});
}
