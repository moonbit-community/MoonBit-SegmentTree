enum Node {
  Nil
  Node(Int, Node, Node)
} derive(Show)

let empty_node : Node = Node(0, Nil, Nil)

fn op_add(self : Node, v : Node) -> Node {
  match (self, v) {
    (Node(left, _, _), Node(right, _, _)) => Node(left + right, self, v)
    (Node(_), Nil) => self
    (Nil, Node(_)) => v
    (Nil, Nil) => Nil
  }
}

fn build(data : ArrayView[Int]) -> Node {
  if data.length() == 1 {
    Node(data[0], Nil, Nil)
  } else {
    let mid = (data.length() + 1) >> 1
    build(data[0:mid]) + build(data[mid:])
  }
}

fn query(self : Node, l : Int, r : Int, query_l : Int, query_r : Int) -> Node {
  if query_l > r || l > query_r {
    empty_node
  } else if query_l <= l && query_r >= r {
    self
  } else {
    let Node(_, left, right) = self
    let mid = (l + r) >> 1
    left.query(l, mid, query_l, query_r) +
    right.query(mid + 1, r, query_l, query_r)
  }
}

fn main {
  let tree = build([1, 2, 3, 4, 5][:])
  let sum = match tree.query(1, 5, 1, 3) {
    Node(sum, _, _) => sum
    _ => panic()
  }
  println(sum)
}
