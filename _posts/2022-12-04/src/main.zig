const std = @import("std");
const stdout = std.io.getStdOut().writer();

const Range = struct {
    from: u64,
    to: u64,
    fn contains(self: *Range, range: Range) bool {
        return self.from <= range.from and range.to <= self.to;
    }
    fn overlaps(self: *Range, range: Range) bool {
        // There's probably a simpler way...
        return (self.from <= range.from and range.from <= self.to)
            or (self.from <= range.to and range.to <= self.to)
            or (range.from <= self.from and self.from <= range.to)
            or (range.from <= self.to and self.to <= range.to)
            or self.contains(range)
            or (range.from <= self.from and self.to <= range.to);
    }
};

pub fn main() anyerror!void {
    var input = @embedFile("input");
    var trimmedInput = std.mem.trim(u8, input, "\n");
    var lines = std.mem.split(u8, trimmedInput, "\n");

    var contained: u64 = 0;
    var overlapped: u64 = 0;

    while (lines.next()) |line| {
        var ranges = std.mem.split(u8, line, ",");

        var range1 = try parseRange(ranges.next().?);
        var range2 = try parseRange(ranges.next().?);

        if (range1.contains(range2) or range2.contains(range1)) {
            contained += 1;
        }

        if (range1.overlaps(range2)) {
            overlapped += 1;
        }
    }

    try stdout.print("{}\n", .{contained});
    try stdout.print("{}\n", .{overlapped});
}

fn parseRange(str: []const u8) !Range {
    var parts = std.mem.split(u8, str, "-");
    var from = try std.fmt.parseUnsigned(u64, parts.next().?, 10);
    var to = try std.fmt.parseUnsigned(u64, parts.next().?, 10);

    return Range{.from = from, .to = to};
}

