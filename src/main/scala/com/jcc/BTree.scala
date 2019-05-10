package com.jcc

import scala.annotation.tailrec

trait Tree[T]

case class BTree[T](value: T, left: Option[BTree[T]], right: Option[BTree[T]]) extends Tree[T] {
  def hasLeft: Boolean = left.isDefined
  def hasRight: Boolean = right.isDefined
}

object BTree {
  def apply[T](value: T, left: BTree[T], right: BTree[T]): BTree[T] = BTree(value, Option(left), Option(right))
  def apply[T](value: T): BTree[T] = BTree(value, None, None)

  @tailrec
  def addNode[T](btree: BTree[T], node: BTree[T]): BTree[T] = {
    if (!btree.hasLeft) return btree.copy(left = Option(node))
    if (!btree.hasRight) return btree.copy(right = Option(node))

    addNode(btree.left.get, node)
  }

  def buildTree[T](values: T*): BTree[T] = {
    require(values.nonEmpty)
    val inputs: scala.collection.mutable.Queue[T] = scala.collection.mutable.Queue()
    values.foreach(k => inputs.enqueue{k})

    val treeStart: BTree[T] = this(treeStart)

    values.foldLeft(treeStart)((acc, k) => {

    })
  }
}

object Foo {
  val tree: BTree[Int] = BTree(5)
  val tree1: BTree[Int] = BTree(1)
  val tree2: BTree[Int] = BTree(4, tree, tree1)
  val tree3: BTree[Int] = BTree(4, tree2, tree)
}