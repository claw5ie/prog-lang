pub fn DoublyLinkedList(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const Node = struct {
            payload: T,
            next: ?*Node = null,
            prev: ?*Node = null,
        };

        pub const Iterator = struct {
            node: ?*Node,

            pub fn next(it: *Iterator) ?*T {
                if (it.node) |node| {
                    it.node = node.next;
                    return &node.payload;
                } else {
                    return null;
                }
            }

            pub fn has_next(it: Iterator) bool {
                return it.node != null;
            }

            pub fn prev(it: *Iterator) ?*T {
                if (it.node) |node| {
                    it.node = node.prev;
                    return &node.payload;
                } else {
                    return null;
                }
            }

            pub fn has_prev(it: Iterator) bool {
                return it.node != null;
            }
        };

        first: ?*Node = null,
        last: ?*Node = null,
        count: usize = 0,

        pub fn iterator(list: Self) Iterator {
            return .{ .node = list.first };
        }

        pub fn reverse_iterator(list: Self) Iterator {
            return .{ .node = list.last };
        }

        pub fn grab_last(list: Self) *T {
            return &list.last.?.payload;
        }

        pub fn insert_last(list: *Self, node: *Node) void {
            if (list.count != 0) {
                list.last.?.next = node;
                node.prev = list.last;
                list.last = node;
                list.count += 1;
            } else {
                list.first = node;
                list.last = node;
                list.count = 1;
            }
        }
    };
}
