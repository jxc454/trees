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

  def addNode[T](seq: Seq[T], i: Int): BTree[T] = {
    val left: Option[BTree[T]] = if (seq.lengthCompare(2 * i + 1) > 0) Option(addNode(seq, 2 * i + 1)) else None
    val right: Option[BTree[T]] = if (seq.lengthCompare(2 * i + 2) > 0) Option(addNode(seq, 2 * i + 2)) else None
    BTree(seq(i), left, right)
  }

  def buildTree[T](values: T*): BTree[T] = {
    require(values.nonEmpty)
    addNode(values, 0)
  }

//  def buildTreedBSTree[T](values: T*): Option[BTree[T]] = {
//    require(values.nonEmpty)
//
//  }
}

object Foo {
  val tree: BTree[Int] = BTree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val sorted: Seq[Int] = MergeSortAsc(Seq(2, 6, 2, 5, 4, 5, 7): _*)
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

  def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = left match {
    case leftH :: leftT => right match {
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
