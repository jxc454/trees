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
}
