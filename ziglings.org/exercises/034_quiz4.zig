//
// Quiz time. See if you can make this program work!
//
// Solve this any way you like, just be sure the output is:
//
//     my_num=42
//
const std = @import("std");

const NumError = error{IllegalNumber};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const my_num: u32 = getNumber() catch |err| switch (err) {
        NumError.IllegalNumber => 42,
        // Is it useful at all to do this? I'm hoping this is very explicit
        // about stating this will only ever expect to handle one error type,
        // and other error types should be a compile-time error, indicating
        // this statement needs to be updated.
        else => unreachable,
    };

    try stdout.print("my_num={}\n", .{my_num});
}

// This function is obviously weird and non-functional. But you will not be changing it for this quiz.
fn getNumber() NumError!u32 {
    if (false) return NumError.IllegalNumber;
    return 42;
}
