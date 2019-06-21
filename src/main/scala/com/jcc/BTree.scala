package com.jcc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  def traverseInOrder(f: T => Unit): Unit = {
    if (this.hasLeft) this.left.get.traverseInOrder(f)
    f(this.value)
    if (this.hasRight) this.right.get.traverseInOrder(f)
  }

  def traverseLevel(leftToRight: Boolean = true)(f: BTree[T] => Unit): Unit = {
    val q: mutable.Queue[BTree[T]] = new mutable.Queue[BTree[T]]()

    q.enqueue(this)
    var tree: BTree[T] = null

    while (q.nonEmpty) {
      tree = q.dequeue()

      if (leftToRight) {
        if (tree.hasLeft) q.enqueue(tree.left.get)
        if (tree.hasRight) q.enqueue(tree.right.get)
      } else {
        if (tree.hasRight) q.enqueue(tree.right.get)
        if (tree.hasLeft) q.enqueue(tree.left.get)
      }

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

  def reverseLevelTraversal(f: BTree[T] => Unit): Unit = {
    val stack: mutable.ArrayStack[BTree[T]] = mutable.ArrayStack[BTree[T]]()
    this.traverseLevel(leftToRight = false)(stack.push)
    while (stack.nonEmpty) f(stack.pop)
  }

  def sideToSideTraversal(leftToRight: Boolean = true)(f: BTree[T] => Unit): Unit = {
    val q1: mutable.Queue[BTree[T]] = mutable.Queue[BTree[T]]()
    val q2: mutable.Queue[BTree[T]] = mutable.Queue[BTree[T]]()
    var stack1: mutable.ArrayStack[BTree[T]] = new mutable.ArrayStack[BTree[T]]()
    var stack2: mutable.ArrayStack[BTree[T]] = new mutable.ArrayStack[BTree[T]]()

    q1.enqueue(this)

    while (q1.nonEmpty || q2.nonEmpty) {
      if (q1.nonEmpty) {
        while (q1.nonEmpty) {
          val q1Tree = q1.dequeue()
          if (q1Tree.hasRight) q2.enqueue(q1Tree.right.get)
          if (q1Tree.hasLeft) q2.enqueue(q1Tree.left.get)

          stack1.push(q1Tree)
        }

        while (stack1.nonEmpty) {
          f(stack1.pop)
          stack1 = stack1.reverse
        }
      } else if (q2.nonEmpty) {
        while (q2.nonEmpty) {
          val q2Tree = q2.dequeue()
          if (q2Tree.hasRight) q1.enqueue(q2Tree.right.get)
          if (q2Tree.hasLeft) q1.enqueue(q2Tree.left.get)

          stack2.push(q2Tree)
        }

        while (stack2.nonEmpty) {
          f(stack2.pop)
          stack2 = stack2.reverse
        }
      }
    }
  }

  def sideViewTraversal(left: Boolean = true)(f: BTree[T] => Unit): Unit = {
    val q1 = mutable.Queue[BTree[T]]()
    val q2 = mutable.Queue[BTree[T]]()
    var touched: Boolean = false

    q1.enqueue(this)

    while (q1.nonEmpty || q2.nonEmpty) {
      touched = false
      if (q1.nonEmpty) {
        while (q1.nonEmpty) {
          val tree = q1.dequeue()
          if (left) {
            if (tree.hasLeft) q2.enqueue(tree.left.get)
            if (tree.hasRight) q2.enqueue(tree.right.get)
          } else {
            if (tree.hasRight) q2.enqueue(tree.right.get)
            if (tree.hasLeft) q2.enqueue(tree.left.get)
          }

          if (!touched) {
            touched = true
            f(tree)
          }
        }
      } else if (q2.nonEmpty) {
        while (q2.nonEmpty) {
          val tree = q2.dequeue()
          if (left) {
            if (tree.hasLeft) q1.enqueue(tree.left.get)
            if (tree.hasRight) q1.enqueue(tree.right.get)
          } else {
            if (tree.hasRight) q1.enqueue(tree.right.get)
            if (tree.hasLeft) q1.enqueue(tree.left.get)
          }

          if (!touched) {
            touched = true
            f(tree)
          }
        }
      }
    }
  }

  def bottomViewTraversal(f: BTree[T] => Unit): Unit = {
    val lanes = mutable.SortedMap[Int, mutable.ArrayStack[BTree[T]]]()
    val q = mutable.Queue[(BTree[T], Int)]()

    q.enqueue((this, 0))

    while (q.nonEmpty) {
      val treeTuple = q.dequeue
      val tree = treeTuple._1
      val lane = treeTuple._2

      if (tree.hasLeft) q.enqueue((tree.left.get, lane - 1))
      if (tree.hasRight) q.enqueue((tree.right.get, lane + 1))

      val stack = if (lanes.contains(lane)) lanes(lane) else new mutable.ArrayStack[BTree[T]]()
      stack.push(tree)
      lanes(lane) = stack
    }

    lanes.toList.foreach(intAndStack => {
      val (_, stack) = intAndStack
      f(stack.pop)
    })
  }

  def topViewTraversal(f: BTree[T] => Unit): Unit = {
    val lanes = mutable.SortedMap[Int, BTree[T]]()
    val q = mutable.Queue[(BTree[T], Int)]()

    q.enqueue((this, 0))

    while (q.nonEmpty) {
      val treeTuple = q.dequeue()
      val tree = treeTuple._1
      val lane = treeTuple._2

      if (tree.hasLeft) q.enqueue((tree.left.get, lane -1))
      if (tree.hasRight) q.enqueue((tree.right.get, lane + 1))

      if (!lanes.contains(lane)) lanes(lane) = tree
    }

    lanes.toList.foreach(laneAndTree => {
      val (_, tree) = laneAndTree
      f(tree)
    })
  }

  def nextNode(node: BTree[T]): Option[BTree[T]] = {
    val q1 = mutable.Queue[BTree[T]]()
    val q2 = mutable.Queue[BTree[T]]()

    q1.enqueue(this)

    while (q1.nonEmpty || q2.nonEmpty) {
      if (q1.nonEmpty) {
        while (q1.nonEmpty) {
          val tree = q1.dequeue
          if (tree == node) return if (q1.nonEmpty) Option(q1.dequeue) else None
          if (tree.hasLeft) q2.enqueue(tree.left.get)
          if (tree.hasRight) q2.enqueue(tree.right.get)
        }
      }
      if (q2.nonEmpty) {
        while (q2.nonEmpty) {
          val tree = q2.dequeue
          if (tree == node) return if (q2.nonEmpty) Option(q2.dequeue) else None
          if (tree.hasLeft) q1.enqueue(tree.left.get)
          if (tree.hasRight) q1.enqueue(tree.right.get)
        }
      }
    }

    None
  }

  def isComplete: Boolean = {
    val q1 = mutable.Queue[BTree[T]]()
    val buf: ListBuffer[T] = ListBuffer()

    q1.enqueue(this)

    while (q1.nonEmpty) {
      val tree = q1.dequeue
      buf += tree.value
      if (tree.hasLeft) q1.enqueue(tree.left.get)
      if (tree.hasRight) q1.enqueue(tree.right.get)
    }

    val levelOrder: List[T] = buf.toList

    val inOrderTree: BTree[T] = BTree.buildTree(levelOrder: _*)

    inOrderTree == this
  }

  def firstCousins(node1: BTree[T], node2: BTree[T]): Boolean = {
    var foundNode1: Boolean = false
    var foundNode2: Boolean = false
    val q = mutable.Queue[BTree[T]]()

    q.enqueue(this)

    while (q.nonEmpty && !foundNode1 && !foundNode2) {
      val tree = q.dequeue

      if (tree.hasLeft) q.enqueue(tree.left.get)
      if (tree.hasRight) q.enqueue(tree.right.get)

      val (grandkids1: Seq[BTree[T]], grandkids2: Seq[BTree[T]]) = getKids(tree) match {
        case Some(lChild :: rChild :: Nil) => (getKids(lChild).getOrElse(Seq()), getKids(rChild).getOrElse(Seq()))
        case Some(child :: Nil) => (getKids(child).getOrElse(Seq()), Seq())
        case None => (Seq(), Seq())
        case Some(Nil) => (Seq(), Seq())
      }

      foundNode1 = (grandkids1 ++ grandkids2).contains(node1)
      foundNode2 = (grandkids1 ++ grandkids2).contains(node2)

      if (grandkids1.nonEmpty && grandkids2.nonEmpty) {
        // compare
        if ((grandkids1.contains(node1) && grandkids2.contains(node2)) ||
          (grandkids1.contains(node2) && grandkids2.contains(node1))) return true
      }
    }

    false
  }

  def findCousins(node: BTree[T]): Option[Seq[BTree[T]]] = {
    val buf = mutable.ListBuffer[BTreeRich[T]]()

    def checkNode(richNode: BTreeRich[T]): Unit = {
      if (richNode.hasLeft) checkNode(richNode.left.get)
      buf += richNode
      if (richNode.hasRight) checkNode(richNode.right.get)
    }

    checkNode(BTreeRich(this, 1, None))

    buf.find(_.node == node) match {
      case Some(bTree) => Option(buf.filter(_.isCousin(bTree)).map(_.node))
      case None => None
    }
  }

  def matchValues(node: BTree[T]): Boolean = {
    def inOrderTraversal(tree: BTree[T])(f: T => Unit): Unit = {
      if (tree.hasLeft) inOrderTraversal(tree.left.get)(f)
      f(tree.value)
      if (tree.hasRight) inOrderTraversal(tree.right.get)(f)
    }

    // get in-order traversal of node
    val nodeTraversal: mutable.ListBuffer[T] = mutable.ListBuffer()
    inOrderTraversal(node)(nodeTraversal += _)

    def innerMatchValues(node: BTree[T], parentNode: BTree[T]): Boolean = {
      // does node value match
      if (parentNode.value == node.value) {
        // get in-order traversal of the matching node
        val matchingNodeTraversal: mutable.ListBuffer[T] = mutable.ListBuffer()
        inOrderTraversal(parentNode)(matchingNodeTraversal += _)

        if (nodeTraversal == matchingNodeTraversal) true else false
      } else if (parentNode.hasLeft) {
        innerMatchValues(node, parentNode.left.get)
      } else if (parentNode.hasRight) {
        innerMatchValues(node, parentNode.right.get)
      } else false
    }

    innerMatchValues(node, this)
  }

  def matchValuesBetter(node: BTree[T]): Boolean = {
    def inOrderTraversal(tree: BTree[T])(f: T => Unit): Unit = {
      if (tree.hasLeft) inOrderTraversal(tree.left.get)(f)
      f(tree.value)
      if (tree.hasRight) inOrderTraversal(tree.right.get)(f)
    }

    def preOrderTraversal(tree: BTree[T])(f: T => Unit): Unit = {
      f(tree.value)
      if (tree.hasLeft) preOrderTraversal(tree.left.get)(f)
      if (tree.hasRight) preOrderTraversal(tree.right.get)(f)
    }

    // an in-order and pre/post-order uniquely ID a binary tree
    val nodeInOrder, nodePreOrder, treeInOrder, treePreOrder = ListBuffer[T]()

    inOrderTraversal(node)(nodeInOrder += _)
    inOrderTraversal(this)(treeInOrder += _)
    preOrderTraversal(node)(nodePreOrder += _)
    preOrderTraversal(this)(treePreOrder += _)

    treeInOrder.intersect(nodeInOrder).nonEmpty && treePreOrder.intersect(nodePreOrder).nonEmpty
  }

  def diameter: Int = {
    var max: Int = 0
    def getMaxLength(node: BTree[T]): Int = {
      val leftLength: Int = node.left.map(getMaxLength).getOrElse(0)
      val rightLength: Int = node.right.map(getMaxLength).getOrElse(0)

      val lengthOfPathThrough: Int = 1 + leftLength + rightLength
      val maxPathLengthUpTo: Int = 1 + math.max(leftLength, rightLength)

      if (lengthOfPathThrough > max) max = lengthOfPathThrough
      maxPathLengthUpTo
    }

    getMaxLength(this)
    max
  }

  def symmetric: Boolean = {
    def getPath(node: BTree[T]): List[Int] = {
      val leftPath = node.left.map(0 :: getPath(_)).getOrElse(Nil)
      val rightPath = node.right.map(1 :: getPath(_)).getOrElse(Nil)
      leftPath ::: rightPath
    }

    if (!this.hasLeft || !this.hasRight) return false

    val leftSide: List[Int] = 0 :: getPath(this.left.get)
    val rightSide: List[Int] = 1 :: getPath(this.right.get)

    if (leftSide.length == rightSide.length) {
      !leftSide.zip(rightSide).map(leftRight => leftRight._1 + leftRight._2).exists(_ != 1)
    } else false
  }

  def symmetric2: Boolean = {
    def getSymmetric(node1: Option[BTree[T]], node2: Option[BTree[T]]): Boolean = {
      if (node1.isEmpty && node2.isEmpty) {
        true
      } else if (node1.isDefined && node2.isDefined) {
        getSymmetric(node1.flatMap(_.left), node2.flatMap(_.right)) &&
          getSymmetric(node1.flatMap(_.right), node2.flatMap(_.left))
      } else false
    }

    getSymmetric(this.left, this.right)
  }

  def mirror: BTree[T] = BTree(this.value, this.right.map(_.mirror), this.left.map(_.mirror))

  def flatten: BTree[T] = this match {
    case BTree(_, None, None) => this
    case BTree(v, None, Some(r)) => new BTree(v, None, Option(r.flatten))
    case BTree(v, Some(l), None) => new BTree(v, None, Option(l.flatten))
    case BTree(v, Some(l), Some(r)) => new BTree(v, None, Option(appendToFarRight(l.flatten, r.flatten)))
  }

  def canMatchWithChildSwap(node: BTree[T]): Boolean = {
    // node values are different
    if (node.value != this.value) return false

    (this.left.forall(_.canMatchWithChildSwap(node.left.getOrElse(BTree.buildTree()))) ||
      this.left.forall(_.canMatchWithChildSwap(node.right.getOrElse(BTree.buildTree())))) &&
    (this.right.forall(_.canMatchWithChildSwap(node.left.getOrElse(BTree.buildTree()))) ||
      this.right.forall(_.canMatchWithChildSwap(node.right.getOrElse(BTree.buildTree()))))
  }

  // given that node1 and node2 are in this tree
  def LCA(node1: BTree[T], node2: BTree[T]): BTree[T] = {
    def innerLCA(tree: BTree[T], node1: BTree[T], node2: BTree[T]): Option[BTree[T]] = {
      if (tree.value == node1.value) return Option(tree)
      if (tree.value == node2.value) return Option(tree)

      val leftSide = tree.left.flatMap(t => innerLCA(t, node1, node2))
      val rightSide = tree.right.flatMap(t => innerLCA(t, node1, node2))

      (leftSide, rightSide) match {
        case (Some(_), Some(_)) => Option(tree)
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case _ => None
      }
    }

    innerLCA(this, node1, node2).get
  }

  def pathsToLeafs: List[List[T]] = {
    def innerPaths(tree: BTree[T]): List[List[T]] = {
      (tree.left.map(node => innerPaths(node)), tree.right.map(node => innerPaths(node))) match {
        case (Some(l), Some(r)) => l.map(tree.value :: _) ++ r.map(tree.value :: _)
        case (None, None) => List(List(tree.value))
        case (Some(l), None) => l.map(tree.value :: _)
        case (None, Some(r)) => r.map(tree.value :: _)
      }
    }
    innerPaths(this)
  }

  private def getChildrenAtLevel(left: Boolean)(level: Int): Option[Seq[BTree[T]]] = {
    None
  }

  private def getLeftChildrenAtLevel(level: Int): Option[Seq[BTree[T]]] = getChildrenAtLevel(left = true)(level)
  private def getRightChildrenAtLevel(level: Int): Option[Seq[BTree[T]]] = getChildrenAtLevel(left = false)(level)

  private def getKids(node: BTree[T]): Option[Seq[BTree[T]]] = {
    val kids = Seq(node.left, node.right).flatten
    if (kids.isEmpty) None else Option(kids)
  }

  private def appendToFarRight(node1: BTree[T], appendMe: BTree[T]): BTree[T] = node1 match {
    case BTree(v, l, None) => new BTree(v, l, Option(appendMe))
    case BTree(v, l, r) => new BTree(v, l, Option(appendToFarRight(r.get, appendMe)))
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

  def sumTree(node: BTree[Int]): BTree[Int] = {
    val newLeft = if (node.hasLeft) Option(sumTree(node.left.get)) else None
    val newRight = if (node.hasRight) Option(sumTree(node.right.get)) else None
    val newValue =
      (if (node.hasLeft) node.left.get.value else 0) +
      (if (node.hasRight) node.right.get.value else 0) +
      (if (newLeft.isDefined) newLeft.get.value else 0) +
      (if (newRight.isDefined) newRight.get.value else 0)

    BTree(newValue, newLeft, newRight)
  }

  def allWords(rightPredicate: Int => Boolean)(globalNums: List[Int]): List[List[Int]] = {
    val memo: mutable.Map[Int, List[List[Int]]] = mutable.Map[Int, List[List[Int]]]()

    val getSecondHead: (Int, Int) => Option[Int] = (num1, num2) => Option(num1 * 10 + num2).filter(rightPredicate)

    def allWordsHelper(nums: List[Int], currentIndex: Int): List[List[Int]] = {

      val (head: Option[Int], tail: Option[List[Int]]) = nums match {
        case Nil => (None, None)
        case h :: Nil => (Option(h), None)
        case h :: t => (Option(h), Option(t))
      }

      val (secondHead: Option[Int], secondTail: Option[List[Int]]) = tail match {
        case None => (None, None)
        case Some(Nil) => (None, None)
        case Some(h :: Nil) => (getSecondHead(head.get, h), None)
        case Some(h :: t) => {
          val opSecondHead = getSecondHead(head.get, h)
          (opSecondHead, opSecondHead.map(_ => t))
        }
      }

      if (!memo.contains(currentIndex)) {
        val headResult: Option[List[List[Int]]] = tail
          .map(t => allWordsHelper(t, currentIndex + 1))
          .map(_.map(list => head.get :: list)) match {
            case None => if (head.isDefined) Option(List(List(head.get))) else None
            case Some(list) => Option(list)
        }

        val secondHeadResult: Option[List[List[Int]]] = secondTail
          .map(t => allWordsHelper(t, currentIndex + 2))
          .map(_.map(list => secondHead.get :: list)) match {
            case None => if (secondHead.isDefined) Option(List(List(secondHead.get))) else None
            case Some(list) => Option(list)
        }

        headResult match {
          case Some(res1) => memo += (currentIndex -> (res1 ::: secondHeadResult.getOrElse(List())))
          case None =>
        }
      }
      memo.getOrElse(currentIndex, List())
    }
    allWordsHelper(globalNums, 0)
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
