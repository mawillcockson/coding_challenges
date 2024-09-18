const std = @import("std");
const testing = std.testing;
const print = std.debug.print;

test "if statement" {
    const a = true;
    const x: u2 =
        if (a)
        1
    else
        0;
    try testing.expectEqual(1, x);
}

test "while loop" {
    var i: u8 = 254;
    while (i < 255) {
        i +|= 1;
        print("{}", .{i});
    }
    i +|= 255;
    print("{}", .{i});
    print("done", .{});
}

test "while with continue expression" {
    var sum: u8 = 0;
    var i: u8 = 1;
    while (i <= 10) : (i += 1) {
        sum += 1;
    }
    try testing.expectEqual(10, sum);
}

test "while with continue" {
    var sum: u8 = 0;
    var i: u8 = 1;
    while (i <= 3) : (i += 1) {
        if (i == 2) {
            continue;
        }
        sum += 1;
    }
    try testing.expectEqual(2, sum);
}

test "for" {
    const string = "hello";
    for (string) |char| {
        print("char -> {}\n", .{char});
    }
}
