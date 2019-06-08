package com

package object jcc {
  class BTreeRich[T](val node: BTree[T], val level: Int, val parent: Option[BTree[T]])
    extends BTree[T](node.value, node.left, node.right) {

    def isCousin(node: BTreeRich[T]): Boolean = node.level == this.level && node.parent != this.parent

    override def hasLeft: Boolean = node.hasLeft
    override def hasRight: Boolean = node.hasRight
    override val left: Option[BTreeRich[T]] = if (node.left.isDefined) Option(BTreeRich(node.left.get, level + 1, Some(this))) else None
    override val right: Option[BTreeRich[T]] = if (node.right.isDefined) Option(BTreeRich(node.right.get, level + 1, Some(this))) else None
  }

  object BTreeRich {
    def apply[T](node: BTree[T], level: Int, parent: Option[BTree[T]]): BTreeRich[T] = new BTreeRich(node, level, parent)
  }

  val alphabet: Map[Int, Char] = Map(
    1 -> 'a',
    2 -> 'b',
    3 -> 'c',
    4 -> 'd',
    5 -> 'e',
    6 -> 'f',
    7 -> 'g',
    8 -> 'h',
    9 -> 'i',
    10 -> 'j',
    11 -> 'k',
    12 -> 'l',
    13 -> 'm',
    14 -> 'n',
    15 -> 'o',
    16 -> 'p',
    17 -> 'q',
    18 -> 'r',
    19 -> 's',
    20 -> 't',
    21 -> 'u',
    22 -> 'v',
    23 -> 'w',
    24 -> 'x',
    25 -> 'y',
    26 -> 'z',
  )
}
