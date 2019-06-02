package com.jcc

import scala.collection.mutable

trait Tree[T]

case class BTree[T](value: T, left: Option[BTree[T]], right: Option[BTree[T]]) extends Tree[T] {
  def hasLeft: Boolean = left.isDefined
  def hasRight: Boolean = right.isDefined

  def inOrderWithLevel(currentLevel: Int = 0): List[(T, Int)] = {
    val list: mutable.ListBuffer[(T, Int)] = mutable.ListBuffer()

    if (this.hasLeft) list ++= this.left.get.inOrderWithLevel(currentLevel + 1)
    list += ((this.value, currentLevel))
    if (this.hasRight) list ++= this.right.get.inOrderWithLevel(currentLevel + 1)

    list.toList
  }

  def identical(that: BTree[T]): Boolean = this.inOrderWithLevel() == that.inOrderWithLevel()

  def height: Int = {
    val leftHeight: Int = 1 + (if (this.hasLeft) this.left.get.height else 0)
    val rightHeight: Int = 1 + (if (this.hasRight) this.right.get.height else 0)

    math.max(leftHeight, rightHeight)
  }

  def mapPostOrder[B](f: T => B): BTree[B] = {
    val left: Option[BTree[B]] = if (this.hasLeft) Option(this.left.get.mapPostOrder(f)) else None
    val right: Option[BTree[B]] = if (this.hasRight) Option(this.right.get.mapPostOrder(f)) else None
    BTree(f(this.value), left, right)
  }

  def mapPreOrder[B](f: T => B): BTree[B] = {
    val newValue = f(this.value)
    val left: Option[BTree[B]] = if (this.hasLeft) Option(this.left.get.mapPreOrder(f)) else None
    val right: Option[BTree[B]] = if (this.hasRight) Option(this.right.get.mapPreOrder(f)) else None
    BTree(newValue, left, right)
  }

  def mapInOrder[B](f: T => B): BTree[B] = {
    val left: Option[BTree[B]] = if (this.hasLeft) Option(this.left.get.mapInOrder(f)) else None
    val newValue = f(this.value)
    val right: Option[BTree[B]] = if (this.hasRight) Option(this.right.get.mapInOrder(f)) else None
    BTree(newValue, left, right)
  }

  def traverseLevel(f: BTree[T] => Unit): Unit = {
    val q: mutable.Queue[BTree[T]] = new mutable.Queue[BTree[T]]()

    q.enqueue(this)
    var tree: BTree[T] = null

    while (q.nonEmpty) {
      tree = q.dequeue()

      if (tree.hasLeft) q.enqueue(tree.left.get)
      if (tree.hasRight) q.enqueue(tree.right.get)

      f(tree)
    }
  }

  def traverseSpiral(f: BTree[T] => Unit): Unit = {
    val stack1: mutable.ArrayStack[BTree[T]] = new mutable.ArrayStack[BTree[T]]()
    val stack2: mutable.ArrayStack[BTree[T]] = new mutable.ArrayStack[BTree[T]]()
    val q1: mutable.Queue[BTree[T]] = new mutable.Queue[BTree[T]]()
    val q2: mutable.Queue[BTree[T]] = new mutable.Queue[BTree[T]]()

    stack1.push(this)
    var stack1Tree: BTree[T] = null
    var stack2Tree: BTree[T] = null

    while (stack1.nonEmpty || stack2.nonEmpty) {
      if (stack1.nonEmpty) {
        while (stack1.nonEmpty) q1.enqueue(stack1.pop)

        while (q1.nonEmpty) {
          stack1Tree = q1.dequeue()
          if (stack1Tree.hasRight) stack2.push(stack1Tree.right.get)
          if (stack1Tree.hasLeft) stack2.push(stack1Tree.left.get)
          f(stack1Tree)
        }
      }
      if (stack2.nonEmpty) {
        while (stack2.nonEmpty) q2.enqueue(stack2.pop)

        while (q2.nonEmpty) {
          stack2Tree = q2.dequeue()
          if (stack2Tree.hasLeft) stack1.push(stack2Tree.left.get)
          if (stack2Tree.hasRight) stack1.push(stack2Tree.right.get)
          f(stack2Tree)
        }
      }
    }
  }
}

case class BSTreeOfInt(value: Int, left: Option[BSTreeOfInt], right: Option[BSTreeOfInt]) extends Tree[Int] {
  def hasLeft: Boolean = left.isDefined
  def hasRight: Boolean = right.isDefined
}

object BTree {
  def apply[T](value: T, left: BTree[T], right: BTree[T]): BTree[T] = BTree(value, Option(left), Option(right))
  def apply[T](value: T): BTree[T] = BTree(value, None, None)

  private def addNode[T](seq: Seq[T], i: Int): BTree[T] = {
    val left: Option[BTree[T]] = if (seq.lengthCompare(2 * i + 1) > 0) Option(addNode(seq, 2 * i + 1)) else None
    val right: Option[BTree[T]] = if (seq.lengthCompare(2 * i + 2) > 0) Option(addNode(seq, 2 * i + 2)) else None
    BTree(seq(i), left, right)
  }

  def buildTree[T](values: T*): BTree[T] = {
    require(values.nonEmpty)
    addNode(values, 0)
  }

  def buildBSTreeOfInt(values: Int*): Option[BSTreeOfInt] = {
    if (values.isEmpty) {
      None
    } else {
      val sortedList: Seq[Int] = MergeSortAsc(values: _*)
      val (left: Seq[Int], right: Seq[Int]) = sortedList.splitAt(sortedList.length / 2)
      Option(BSTreeOfInt(right.head, buildBSTreeOfInt(left: _*), buildBSTreeOfInt(right.tail: _*)))
    }
  }

  def deleteTree[T](tree: BTree[T]): Unit = {
    tree match {
      case BTree(_, Some(leftTree), Some(rightTree)) =>
        deleteTree(leftTree)
        deleteTree(rightTree)
      case BTree(_, Some(leftTree), None) => deleteTree(leftTree)
      case BTree(_, None, Some(rightTree)) => deleteTree(rightTree)
      case BTree(_, None, None) =>
    }
    // tree.delete
  }
}

object MergeSortAsc {
  def apply(values: Int*): Seq[Int] = {
    if (values.length < 2) {
      values
    } else {
      val (left: Seq[Int], right: Seq[Int]) = values.splitAt(values.length / 2)

      val leftSorted: Seq[Int] = MergeSortAsc(left: _*)
      val rightSorted: Seq[Int] = MergeSortAsc(right: _*)

      merge(leftSorted, rightSorted)
    }
  }

  def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = left.toList match {
    case leftH :: leftT => right.toList match {
      case rightH :: rightT => if (leftH < rightH) {
        leftH +: merge(leftT, right)
      } else {
        rightH +: merge(left, rightT)
      }
      case Nil => left
    }
    case Nil => right
  }
}
