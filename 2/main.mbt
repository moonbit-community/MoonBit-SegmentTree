enum Data {
  Data(~sum : Int, ~len : Int)
} derive(Show)

enum LazyTag {
  Nil
  Tag(Int)
} derive(Show)

enum Node {
  Nil
  Node(~data : Data, ~tag : LazyTag, ~left : Node, ~right : Node)
} derive(Show)

let empty_node : Node = Node(
  data=Data(sum=0, len=0),
  tag=Nil,
  left=Nil,
  right=Nil,
)

fn op_add(self : Data, v : Data) -> Data {
  match (self, v) {
    (Data(sum=a, len=len_a), Data(sum=b, len=len_b)) =>
      Data(sum=a + b, len=len_a + len_b)
  }
}

fn op_add(self : Node, v : Node) -> Node {
  match (self, v) {
    (Node(data=l, ..), Node(data=r, ..)) =>
      Node(data=l + r, tag=Nil, left=self, right=v)
    (Node(_), Nil) => self
    (Nil, Node(_)) => v
    (Nil, Nil) => Nil
  }
}

fn op_add(self : LazyTag, v : LazyTag) -> LazyTag {
  match (self, v) {
    (Tag(a), Tag(b)) => Tag(a + b)
    (Tag(_), Nil) => self
    (Nil, Tag(_)) => v
    (Nil, Nil) => Nil
  }
}

fn apply(self : Node, v : LazyTag) -> Node {
  match (self, v) {
    (Node(data=Data(sum=a, len=length), ~tag, ~left, ~right), Tag(v) as new_tag) =>
      Node(
        data=Data(sum=a + v * length, len=length),
        tag=tag + new_tag,
        ~left,
        ~right,
      )
    (_, Nil) => self
    (Nil, _) => Nil
  }
}

fn build(data : ArrayView[Int]) -> Node {
  if data.length() == 1 {
    Node(data=Data(sum=data[0], len=1), tag=Nil, left=Nil, right=Nil)
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
    guard let Node(~tag, ~left, ~right, ..) = self
    let mid = (l + r) >> 1
    left.apply(tag).query(l, mid, query_l, query_r) +
    right.apply(tag).query(mid + 1, r, query_l, query_r)
  }
}

fn modify(
  self : Node,
  l : Int,
  r : Int,
  modify_l : Int,
  modify_r : Int,
  tag : LazyTag
) -> Node {
  if modify_l > r || l > modify_r {
    self
  } else if modify_l <= l && modify_r >= r {
    self.apply(tag)
  } else {
    guard let Node(~left, ~right, ..) = self
    left.apply(tag) + right.apply(tag)
  }
}

fn main {
  let tree = build([1, 2, 3, 4, 5][:])
  println(tree.modify(1, 5, 1, 3, Tag(1)).query(1, 5, 1, 3))
}
