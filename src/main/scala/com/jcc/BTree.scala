package com.jcc

import scala.annotation.tailrec
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
