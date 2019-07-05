package com.jcc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Tree[+T] {
  def isEmpty: Boolean = this match {
    case NilTree => true
    case _ => false
  }

  def hasLeft: Boolean = this match {
    case NilTree => false
    case x => !x.left.isEmpty
  }

  def hasRight: Boolean = this match {
    case NilTree => false
    case x => !x.right.isEmpty
  }

  def leftOption: Option[Tree[T]] = this match {
    case NilTree => None
    case Cons(_, left, _) => Tree.childOption(left)
  }

  def rightOption: Option[Tree[T]] = this match {
    case NilTree => None
    case Cons(_, _, right) => Tree.childOption(right)
  }

  def value: T = this match {
    case NilTree => throw new NoSuchElementException
    case Cons(v, _, _) => v
  }

  def left: Tree[T] = this match {
    case NilTree => throw new NoSuchElementException
    case Cons(_, left, _) => left 
  }
  
  def right: Tree[T] = this match {
    case NilTree => throw new NoSuchElementException
    case Cons(_, _, right) => right
  }

  def inOrderWithLevel(currentLevel: Int = 0): List[(T, Int)] = {
    val list: mutable.ListBuffer[(T, Int)] = mutable.ListBuffer()

    if (this.hasLeft) list ++= this.left.inOrderWithLevel(currentLevel + 1)
    list += ((this.value, currentLevel))
    if (this.hasRight) list ++= this.right.inOrderWithLevel(currentLevel + 1)

    list.toList
  }

  def height: Int = {
    val leftHeight: Int = 1 + (if (this.hasLeft) this.left.height else 0)
    val rightHeight: Int = 1 + (if (this.hasRight) this.right.height else 0)

    math.max(leftHeight, rightHeight)
  }

  def mapPostOrder[B](f: T => B): Tree[B] = {
    val left: Tree[B] = if (this.hasLeft) this.left.mapPostOrder(f) else NilTree
    val right: Tree[B] = if (this.hasRight) this.right.mapPostOrder(f) else NilTree
    Tree(f(this.value), left, right)
  }

  def mapPreOrder[B](f: T => B): Tree[B] = {
    val newValue = f(this.value)
    val left: Tree[B] = if (this.hasLeft) this.left.mapPreOrder(f) else NilTree
    val right: Tree[B] = if (this.hasRight) this.right.mapPreOrder(f) else NilTree
    Tree(newValue, left, right)
  }

  def mapInOrder[B](f: T => B): Tree[B] = {
    val left: Tree[B] = if (this.hasLeft) this.left.mapInOrder(f) else NilTree
    val newValue = f(this.value)
    val right: Tree[B] = if (this.hasRight) this.right.mapInOrder(f) else NilTree
    Tree(newValue, left, right)
  }

  def traverseInOrder(f: T => Unit): Unit = {
    if (this.hasLeft) this.left.traverseInOrder(f)
    f(this.value)
    if (this.hasRight) this.right.traverseInOrder(f)
  }

  def traverseLevel(leftToRight: Boolean = true)(f: Tree[T] => Unit): Unit = {
    val q: mutable.Queue[Tree[T]] = new mutable.Queue[Tree[T]]()

    q.enqueue(this)
    var tree: Tree[T] = null

    while (q.nonEmpty) {
      tree = q.dequeue()

      if (leftToRight) {
        if (tree.hasLeft) q.enqueue(tree.left)
        if (tree.hasRight) q.enqueue(tree.right)
      } else {
        if (tree.hasRight) q.enqueue(tree.right)
        if (tree.hasLeft) q.enqueue(tree.left)
      }

      f(tree)
    }
  }

  def traverseSpiral(f: Tree[T] => Unit): Unit = {
    val stack1: mutable.ArrayStack[Tree[T]] = new mutable.ArrayStack[Tree[T]]()
    val stack2: mutable.ArrayStack[Tree[T]] = new mutable.ArrayStack[Tree[T]]()
    val q1: mutable.Queue[Tree[T]] = new mutable.Queue[Tree[T]]()
    val q2: mutable.Queue[Tree[T]] = new mutable.Queue[Tree[T]]()

    stack1.push(this)
    var stack1Tree: Tree[T] = null
    var stack2Tree: Tree[T] = null

    while (stack1.nonEmpty || stack2.nonEmpty) {
      if (stack1.nonEmpty) {
        while (stack1.nonEmpty) q1.enqueue(stack1.pop)

        while (q1.nonEmpty) {
          stack1Tree = q1.dequeue()
          if (stack1Tree.hasRight) stack2.push(stack1Tree.right)
          if (stack1Tree.hasLeft) stack2.push(stack1Tree.left)
          f(stack1Tree)
        }
      }
      if (stack2.nonEmpty) {
        while (stack2.nonEmpty) q2.enqueue(stack2.pop)

        while (q2.nonEmpty) {
          stack2Tree = q2.dequeue()
          if (stack2Tree.hasLeft) stack1.push(stack2Tree.left)
          if (stack2Tree.hasRight) stack1.push(stack2Tree.right)
          f(stack2Tree)
        }
      }
    }
  }

  def reverseLevelTraversal(f: Tree[T] => Unit): Unit = {
    val stack: mutable.ArrayStack[Tree[T]] = mutable.ArrayStack[Tree[T]]()
    this.traverseLevel(leftToRight = false)(stack.push)
    while (stack.nonEmpty) f(stack.pop)
  }

  def sideToSideTraversal(leftToRight: Boolean = true)(f: Tree[T] => Unit): Unit = {
    val q1: mutable.Queue[Tree[T]] = mutable.Queue[Tree[T]]()
    val q2: mutable.Queue[Tree[T]] = mutable.Queue[Tree[T]]()
    var stack1: mutable.ArrayStack[Tree[T]] = new mutable.ArrayStack[Tree[T]]()
    var stack2: mutable.ArrayStack[Tree[T]] = new mutable.ArrayStack[Tree[T]]()

    q1.enqueue(this)

    while (q1.nonEmpty || q2.nonEmpty) {
      if (q1.nonEmpty) {
        while (q1.nonEmpty) {
          val q1Tree = q1.dequeue()
          if (q1Tree.hasRight) q2.enqueue(q1Tree.right)
          if (q1Tree.hasLeft) q2.enqueue(q1Tree.left)

          stack1.push(q1Tree)
        }

        while (stack1.nonEmpty) {
          f(stack1.pop)
          stack1 = stack1.reverse
        }
      } else if (q2.nonEmpty) {
        while (q2.nonEmpty) {
          val q2Tree = q2.dequeue()
          if (q2Tree.hasRight) q1.enqueue(q2Tree.right)
          if (q2Tree.hasLeft) q1.enqueue(q2Tree.left)

          stack2.push(q2Tree)
        }

        while (stack2.nonEmpty) {
          f(stack2.pop)
          stack2 = stack2.reverse
        }
      }
    }
  }

  def sideViewTraversal(left: Boolean = true)(f: Tree[T] => Unit): Unit = {
    val q1 = mutable.Queue[Tree[T]]()
    val q2 = mutable.Queue[Tree[T]]()
    var touched: Boolean = false

    q1.enqueue(this)

    while (q1.nonEmpty || q2.nonEmpty) {
      touched = false
      if (q1.nonEmpty) {
        while (q1.nonEmpty) {
          val tree = q1.dequeue()
          if (left) {
            if (tree.hasLeft) q2.enqueue(tree.left)
            if (tree.hasRight) q2.enqueue(tree.right)
          } else {
            if (tree.hasRight) q2.enqueue(tree.right)
            if (tree.hasLeft) q2.enqueue(tree.left)
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
            if (tree.hasLeft) q1.enqueue(tree.left)
            if (tree.hasRight) q1.enqueue(tree.right)
          } else {
            if (tree.hasRight) q1.enqueue(tree.right)
            if (tree.hasLeft) q1.enqueue(tree.left)
          }

          if (!touched) {
            touched = true
            f(tree)
          }
        }
      }
    }
  }

  def bottomViewTraversal(f: Tree[T] => Unit): Unit = {
    val lanes = mutable.SortedMap[Int, mutable.ArrayStack[Tree[T]]]()
    val q = mutable.Queue[(Tree[T], Int)]()

    q.enqueue((this, 0))

    while (q.nonEmpty) {
      val treeTuple = q.dequeue
      val tree = treeTuple._1
      val lane = treeTuple._2

      if (tree.hasLeft) q.enqueue((tree.left, lane - 1))
      if (tree.hasRight) q.enqueue((tree.right, lane + 1))

      val stack = if (lanes.contains(lane)) lanes(lane) else new mutable.ArrayStack[Tree[T]]()
      stack.push(tree)
      lanes(lane) = stack
    }

    lanes.toList.foreach(intAndStack => {
      val (_, stack) = intAndStack
      f(stack.pop)
    })
  }

  def topViewTraversal(f: Tree[T] => Unit): Unit = {
    val lanes = mutable.SortedMap[Int, Tree[T]]()
    val q = mutable.Queue[(Tree[T], Int)]()

    q.enqueue((this, 0))

    while (q.nonEmpty) {
      val treeTuple = q.dequeue()
      val tree = treeTuple._1
      val lane = treeTuple._2

      if (tree.hasLeft) q.enqueue((tree.left, lane -1))
      if (tree.hasRight) q.enqueue((tree.right, lane + 1))

      if (!lanes.contains(lane)) lanes(lane) = tree
    }

    lanes.toList.foreach(laneAndTree => {
      val (_, tree) = laneAndTree
      f(tree)
    })
  }



  def isComplete: Boolean = {
    val q1 = mutable.Queue[Tree[T]]()
    val buf: ListBuffer[T] = ListBuffer()

    q1.enqueue(this)

    while (q1.nonEmpty) {
      val tree = q1.dequeue
      buf += tree.value
      if (tree.hasLeft) q1.enqueue(tree.left)
      if (tree.hasRight) q1.enqueue(tree.right)
    }

    val levelOrder: List[T] = buf.toList

    val inOrderTree: Tree[T] = Tree.buildTree(levelOrder: _*)

    inOrderTree == this
  }





  def diameter: Int = {
    var max: Int = 0
    def getMaxLength(node: Tree[T]): Int = {
      val leftLength: Int = node.leftOption.map(getMaxLength).getOrElse(0)
      val rightLength: Int = node.rightOption.map(getMaxLength).getOrElse(0)

      val lengthOfPathThrough: Int = 1 + leftLength + rightLength
      val maxPathLengthUpTo: Int = 1 + math.max(leftLength, rightLength)

      if (lengthOfPathThrough > max) max = lengthOfPathThrough
      maxPathLengthUpTo
    }

    getMaxLength(this)
    max
  }

  def symmetric: Boolean = {
    def getPath(node: Tree[T]): List[Int] = {
      val leftPath = node.leftOption.map(0 :: getPath(_)).getOrElse(Nil)
      val rightPath = node.rightOption.map(1 :: getPath(_)).getOrElse(Nil)
      leftPath ::: rightPath
    }

    if (!this.hasLeft || !this.hasRight) return false

    val leftSide: List[Int] = 0 :: getPath(this.leftOption.get)
    val rightSide: List[Int] = 1 :: getPath(this.rightOption.get)

    if (leftSide.length == rightSide.length) {
      !leftSide.zip(rightSide).map(leftRight => leftRight._1 + leftRight._2).exists(_ != 1)
    } else false
  }

  def symmetric2: Boolean = {
    def getSymmetric(node1: Tree[T], node2: Tree[T]): Boolean = {
      if (node1.isEmpty && node2.isEmpty) {
        true
      } else if (!node1.isEmpty && !node2.isEmpty) {
        getSymmetric(node1.left, node2.right) &&
          getSymmetric(node1.right, node2.left)
      } else false
    }

    getSymmetric(this.left, this.right)
  }

  def mirror: Tree[T] = Tree(this.value, this.rightOption.map(_.mirror).getOrElse(NilTree), this.leftOption.map(_.mirror).getOrElse(NilTree))

  def flatten: Tree[T] = this match {
    case NilTree => NilTree
    case Cons(_, NilTree, NilTree) => this
    case Cons(v, NilTree, r) => Cons(v, NilTree, r.flatten)
    case Cons(v, l, NilTree) => Cons(v, NilTree, l.flatten)
    case Cons(v, l, r) => Cons(v, NilTree, Tree.appendToFarRight(l.flatten, r.flatten))
  }




  def pathsToLeafs: List[List[T]] = {
    def innerPaths(tree: Tree[T]): List[List[T]] = {
      (tree.leftOption.map(node => innerPaths(node)), tree.rightOption.map(node => innerPaths(node))) match {
        case (Some(l), Some(r)) => l.map(tree.value :: _) ++ r.map(tree.value :: _)
        case (None, None) => List(List(tree.value))
        case (Some(l), None) => l.map(tree.value :: _)
        case (None, Some(r)) => r.map(tree.value :: _)
      }
    }
    innerPaths(this)
  }

  def corners: List[Tree[T]] = {
    val q1: mutable.Queue[Tree[T]] = mutable.Queue()
    val q2: mutable.Queue[Tree[T]] = mutable.Queue()
    val corners: mutable.ListBuffer[Option[Tree[T]]] = mutable.ListBuffer()
    q1.enqueue(this)

    while (q1.nonEmpty || q2.nonEmpty) {
      if (q1.nonEmpty) {
        val nodes: List[Tree[T]] = q1.toList
        corners += nodes.headOption
        if (nodes.lengthCompare(1) > 0) corners += nodes.lastOption

        while (q1.nonEmpty) {
          val node: Tree[T] = q1.dequeue
          node.leftOption.foreach(l => q2.enqueue(l))
          node.rightOption.foreach(r => q2.enqueue(r))
        }
      } else {
        val nodes: List[Tree[T]] = q2.toList
        corners += nodes.headOption
        if (nodes.lengthCompare(1) > 0) corners += nodes.lastOption

        while (q2.nonEmpty) {
          val node: Tree[T] = q2.dequeue
          node.leftOption.foreach(l => q1.enqueue(l))
          node.rightOption.foreach(r => q1.enqueue(r))
        }
      }
    }
    corners.toList.flatten
  }
}

object Tree {
  def apply[T](v: T, left: Tree[T], right: Tree[T]): Tree[T] = Cons(v, left, right)
  def apply[T](v: T): Tree[T] = Cons(v, NilTree, NilTree)

  private def childOption[T](tree: Tree[T]): Option[Tree[T]] = tree match {
    case NilTree => None
    case z => Some(z)
  }

  def identical[T](tree: Tree[T])(that: Tree[T]): Boolean = tree.inOrderWithLevel() == that.inOrderWithLevel()

  def nextNode[T](tree: Tree[T], node: Tree[T]): Option[Tree[T]] = {
    val q1 = mutable.Queue[Tree[T]]()
    val q2 = mutable.Queue[Tree[T]]()

    q1.enqueue(tree)

    while (q1.nonEmpty || q2.nonEmpty) {
      if (q1.nonEmpty) {
        while (q1.nonEmpty) {
          val tree = q1.dequeue
          if (tree == node) return if (q1.nonEmpty) Option(q1.dequeue) else None
          if (tree.hasLeft) q2.enqueue(tree.left)
          if (tree.hasRight) q2.enqueue(tree.right)
        }
      }
      if (q2.nonEmpty) {
        while (q2.nonEmpty) {
          val tree = q2.dequeue
          if (tree == node) return if (q2.nonEmpty) Option(q2.dequeue) else None
          if (tree.hasLeft) q1.enqueue(tree.left)
          if (tree.hasRight) q1.enqueue(tree.right)
        }
      }
    }

    None
  }

  private def appendToFarRight[T](node1: Tree[T], appendMe: Tree[T]): Tree[T] = node1 match {
    case Cons(v, l, NilTree) => Cons(v, l, appendMe)
    case Cons(v, l, r) => Cons(v, l, appendToFarRight(r, appendMe))
  }


  private def addNode[T](seq: Seq[T], i: Int): Tree[T] = {
    val left: Tree[T] = if (seq.lengthCompare(2 * i + 1) > 0) addNode(seq, 2 * i + 1) else NilTree
    val right: Tree[T] = if (seq.lengthCompare(2 * i + 2) > 0) addNode(seq, 2 * i + 2) else NilTree
    Tree(seq(i), left, right)
  }

  def buildTree[T](values: T*): Tree[T] = {
    if (values.isEmpty) return NilTree
    addNode(values, 0)
  }

  def firstCousins[T](tree: Tree[T], node1: Tree[T], node2: Tree[T]): Boolean = {
    var foundNode1: Boolean = false
    var foundNode2: Boolean = false
    val q = mutable.Queue[Tree[T]]()

    def getKids(node: Tree[T]): Option[Seq[Tree[T]]] = {
      val kids = Seq(node.left, node.right)
      if (kids.isEmpty) None else Option(kids)
    }

    q.enqueue(tree)

    while (q.nonEmpty && !foundNode1 && !foundNode2) {
      val tree = q.dequeue

      if (tree.hasLeft) q.enqueue(tree.left)
      if (tree.hasRight) q.enqueue(tree.right)

      val (grandkids1: Seq[Tree[T]], grandkids2: Seq[Tree[T]]) = getKids(tree) match {
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

  def findCousins[T](tree: Tree[T], node: Tree[T]): Option[Seq[Tree[T]]] = {
    val buf = mutable.ListBuffer[TreeRich[T]]()

    def checkNode(richNode: TreeRich[T]): Unit = {
      if (richNode.hasLeft) checkNode(richNode.left)
      buf += richNode
      if (richNode.hasRight) checkNode(richNode.right)
    }

    val start: TreeRich[T] = TreeRich(tree, 1, None)

    checkNode(start)

    buf.find(_.tree == node) match {
      case Some(bTree) => Option(buf.filter(TreeRich.isCousin(_, bTree)).map(_.tree))
      case None => None
    }
  }

  def matchValues[T](tree: Tree[T], node: Tree[T]): Boolean = {
    def inOrderTraversal(tree: Tree[T])(f: T => Unit): Unit = {
      if (tree.hasLeft) inOrderTraversal(tree.left)(f)
      f(tree.value)
      if (tree.hasRight) inOrderTraversal(tree.right)(f)
    }

    // get in-order traversal of node
    val nodeTraversal: mutable.ListBuffer[T] = mutable.ListBuffer()
    inOrderTraversal(node)(nodeTraversal += _)

    def innerMatchValues(node: Tree[T], parentNode: Tree[T]): Boolean = {
      // does node value match
      if (parentNode.value == node.value) {
        // get in-order traversal of the matching node
        val matchingNodeTraversal: mutable.ListBuffer[T] = mutable.ListBuffer()
        inOrderTraversal(parentNode)(matchingNodeTraversal += _)

        if (nodeTraversal == matchingNodeTraversal) true else false
      } else if (parentNode.hasLeft) {
        innerMatchValues(node, parentNode.left)
      } else if (parentNode.hasRight) {
        innerMatchValues(node, parentNode.right)
      } else false
    }

    innerMatchValues(node, tree)
  }

  def matchValuesBetter[T](tree: Tree[T], node: Tree[T]): Boolean = {
    def inOrderTraversal(tree: Tree[T])(f: T => Unit): Unit = {
      if (tree.hasLeft) inOrderTraversal(tree.left)(f)
      f(tree.value)
      if (tree.hasRight) inOrderTraversal(tree.right)(f)
    }

    def preOrderTraversal(tree: Tree[T])(f: T => Unit): Unit = {
      f(tree.value)
      if (tree.hasLeft) preOrderTraversal(tree.left)(f)
      if (tree.hasRight) preOrderTraversal(tree.right)(f)
    }

    // an in-order and pre/post-order uniquely ID a binary tree
    val nodeInOrder, nodePreOrder, treeInOrder, treePreOrder = ListBuffer[T]()

    inOrderTraversal(node)(nodeInOrder += _)
    inOrderTraversal(tree)(treeInOrder += _)
    preOrderTraversal(node)(nodePreOrder += _)
    preOrderTraversal(tree)(treePreOrder += _)

    treeInOrder.intersect(nodeInOrder).nonEmpty && treePreOrder.intersect(nodePreOrder).nonEmpty
  }

  // given that node1 and node2 are in this tree
  def LCA[T](tree: Tree[T], node1: Tree[T], node2: Tree[T]): Tree[T] = {
    def innerLCA(tree: Tree[T], node1: Tree[T], node2: Tree[T]): Option[Tree[T]] = {
      if (tree.value == node1.value) return Option(tree)
      if (tree.value == node2.value) return Option(tree)

      val leftSide = tree.leftOption.flatMap(t => innerLCA(t, node1, node2))
      val rightSide = tree.rightOption.flatMap(t => innerLCA(t, node1, node2))

      (leftSide, rightSide) match {
        case (Some(_), Some(_)) => Option(tree)
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case _ => None
      }
    }

    innerLCA(tree, node1, node2).get
  }

  def distance[T](tree: Tree[T], node1: Tree[T], node2: Tree[T]): Int = {
    def innerDistance(tree: Tree[T], node1: Tree[T], node2: Tree[T]): Int = {
      val left = tree.leftOption.map(l => innerDistance(l, node2, node1)).getOrElse(0)
      val right = tree.rightOption.map(r => innerDistance(r, node2, node1)).getOrElse(0)

      if (tree == node1 || tree == node2) {
        if (left == 0 && right == 0) 1 else left + right
      } else {
        if (left == 0 && right == 0) 0 else 1 + left + right
      }
    }

    innerDistance(tree, node1, node2) - 1
  }

  def canMatchWithChildSwap[T](tree: Tree[T], node: Tree[T]): Boolean = {
    // node values are different
    if (node.value != tree.value) return false

    (tree.leftOption.forall(Tree.canMatchWithChildSwap(_, node.leftOption.getOrElse(NilTree))) ||
      tree.leftOption.forall(Tree.canMatchWithChildSwap(_, node.rightOption.getOrElse(NilTree)))) &&
      (tree.rightOption.forall(Tree.canMatchWithChildSwap(_, node.leftOption.getOrElse(NilTree))) ||
        tree.rightOption.forall(Tree.canMatchWithChildSwap(_, node.rightOption.getOrElse(NilTree))))
  }

  def ancestors[T](tree: Tree[T], node: Tree[T]): List[Tree[T]] = {
    def findNode(tree: Tree[T], node: Tree[T], ancestors: List[Tree[T]]): List[Tree[T]] = {
      if (tree == node) {
        ancestors
      } else {
        tree.leftOption.map(l => findNode(l, node, tree :: ancestors)).getOrElse(List()) ++
          tree.rightOption.map(r => findNode(r, node, tree :: ancestors)).getOrElse(List())
      }
    }

    findNode(tree, node, Nil).reverse
  }

  def convertToLinkedList[T](tree: Tree[T]): Option[JccLinkedList[T]] = {
    val third = tree.rightOption.flatMap(convertToLinkedList[T])
    val middle = JccLinkedList(tree.value, third)
    val first = tree.leftOption.flatMap(convertToLinkedList[T])

    def append(ll: JccLinkedList[T], last: JccLinkedList[T]): JccLinkedList[T] = ll.next match {
      case Some(n) => JccLinkedList(ll.value, Option(append(n, last)))
      case None => JccLinkedList(ll.value, Option(last))
    }

    first match {
      case None => Option(middle)
      case Some(n) => Option(append(n, middle))
    }
  }

  def sumTree(node: Tree[Int]): Tree[Int] = {
    val newLeft = if (node.hasLeft) sumTree(node.left) else NilTree
    val newRight = if (node.hasRight) sumTree(node.right) else NilTree
    val newValue: Int =
      (if (node.hasLeft) node.left.value else 0) +
        (if (node.hasRight) node.right.value else 0) +
        (if (!newLeft.isEmpty) newLeft.value else 0) +
        (if (!newRight.isEmpty) newRight.value else 0)

    Tree[Int](newValue, newLeft, newRight)
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

  def verticalSum(tree: Tree[Int]): Seq[Int] = {
    val sums: mutable.Map[Int, Int] = mutable.Map[Int, Int]()

    def innerVerticalSum(innerTree: Tree[Int], longitude: Int): Unit = {
      if (sums.contains(longitude)) {
        sums.update(longitude, sums(longitude) + innerTree.value)
      } else {
        sums += (longitude -> innerTree.value)
      }
      innerTree.leftOption.foreach(l => innerVerticalSum(l, longitude - 1))
      innerTree.rightOption.foreach(r => innerVerticalSum(r, longitude + 1))
    }

    innerVerticalSum(tree, 0)
    sums.toSeq.sortBy(_._1).map(_._2)
  }

  def diagonalSum(tree: Tree[Int]): Seq[Int] = {
    val diagonals: mutable.Map[Int, Int] = mutable.Map()

    def innerDiagonalSum(innerTree: Tree[Int], latitude: Int, longitude: Int): Unit = {
      val key: Int = latitude - longitude
      if (diagonals.contains(key)) {
        diagonals.update(key, diagonals(key) + innerTree.value)
      } else {
        diagonals += (key -> innerTree.value)
      }
      innerTree.leftOption.foreach(l => innerDiagonalSum(l, latitude + 1, longitude - 1))
      innerTree.rightOption.foreach(r => innerDiagonalSum(r, latitude + 1, longitude + 1))
    }

    innerDiagonalSum(tree, 0, 0)
    diagonals.toSeq.sortBy(_._1).map(_._2)
  }

  def buildTreeOfInt(values: Int*): Tree[Int] = {
    if (values.isEmpty) {
      NilTree
    } else {
      val sortedList: Seq[Int] = MergeSortAsc(values: _*)
      val (left: Seq[Int], right: Seq[Int]) = sortedList.splitAt(sortedList.length / 2)
      Tree[Int](right.head, buildTreeOfInt(left: _*), buildTreeOfInt(right.tail: _*))
    }
  }
}

case object NilTree extends Tree[Nothing]

case class Cons[+T](v: T, l: Tree[T], r: Tree[T]) extends Tree[T]

object Cons {


  def deleteTree[T](tree: Tree[T]): Unit = {
    tree match {
      case Cons(_, leftTree, rightTree) =>
        deleteTree(leftTree)
        deleteTree(rightTree)
      case Cons(_, Cons(v, l, r), NilTree) => deleteTree(Cons(v, l, r))
      case Cons(_, NilTree, Cons(v, l, r)) => deleteTree(Cons(v, l, r))
      case Cons(_, NilTree, NilTree) =>
    }
  }

//  def sinkZeroes(tree: Option[Tree[Int]]): Option[Tree[Int]] = tree match {
//    case None => None
//    case Some(t) if t.left.isEmpty && t.right.isEmpty => Option(t)
//    case Some(t) => {
//      t.leftOption.flatMap(n => sinkZeroes(n))
//      t.rightOption.flatMap(n => sinkZeroes(Option(n)))
//    }
//  }
}

case class TreeRich[+T](tree: Tree[T], level: Int, parent: Option[Tree[T]]) extends Tree[T] {

  override def hasLeft: Boolean = tree.hasLeft
  override def hasRight: Boolean = tree.hasRight

  override lazy val left: TreeRich[T] = tree match {
    case NilTree => TreeRich(NilTree, -999, None)
    case t => if (!t.left.isEmpty) TreeRich(t.left, level + 1, Some(t)) else TreeRich(NilTree, -999, None)
  }

  override lazy val right: TreeRich[T] = tree match {
    case NilTree => TreeRich(NilTree, -999, None)
    case t => if (!t.right.isEmpty) TreeRich(t.right, level + 1, Some(t)) else TreeRich(NilTree, -999, None)
  }
}

object TreeRich {
  def apply[T](node: Tree[T], level: Int, parent: Option[Tree[T]]): TreeRich[T] = new TreeRich(node, level, parent)
  def isCousin[T](tree: TreeRich[T], node: TreeRich[T]): Boolean = node.level == tree.level && node.parent != tree.parent
}

case class DoublyLinkedList[T](value: T, next: Option[DoublyLinkedList[T]], parent: Option[DoublyLinkedList[T]])
case class JccLinkedList[T](value: T, next: Option[JccLinkedList[T]]) {
  def hasNext: Boolean = next.nonEmpty
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
