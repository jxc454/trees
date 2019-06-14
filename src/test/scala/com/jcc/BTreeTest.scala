package com.jcc

import org.scalatest._
import scala.collection.mutable

class BTreeTest extends FlatSpec with MustMatchers {
  "buildTree" should "build a Tree" in {
    BTree.buildTree(3, 6, 2, 7, 1, 8)
  }

  "buildBSTreeOfInt" should "build a Tree" in {
    BTree.buildBSTreeOfInt(1, 2, 3)
  }

  "inOrderWithLevel" should "build a list of Tuple2" in {
    val tree: BTree[Char] = BTree.buildTree('a', 'b', 'c')
    val id: List[(Char, Int)] = tree.inOrderWithLevel()

    id must be(List(('b', 1), ('a', 0), ('c', 1)))
  }

  "inOrderWithLevel" should "build a more complex list of Tuple2" in {
    val treeLeft: BTree[Char] = BTree.buildTree('a', 'b', 'c')
    val treeRight: BTree[Char] = BTree.buildTree('d', 'e', 'f', 'g')

    val tree: BTree[Char] = BTree('h', treeLeft, treeRight)
    val id: List[(Char, Int)] = tree.inOrderWithLevel()

    id must be(List(('b', 2), ('a', 1), ('c', 2), ('h', 0), ('g', 3), ('e', 2), ('d', 1), ('f', 2)))
  }

  "identical" should "return true for identical Trees" in {
    val treeLeft: BTree[Char] = BTree.buildTree('a', 'b', 'c')
    val treeRight: BTree[Char] = BTree.buildTree('d', 'e', 'f', 'g')

    val tree: BTree[Char] = BTree('h', treeLeft, treeRight)
    val tree2: BTree[Char] = BTree('h', treeLeft, treeRight)
    tree.identical(tree2) must be (true)
    tree2.identical(tree) must be (true)
  }

  "identical" should "return false for different Trees" in {
    val treeLeft: BTree[Char] = BTree.buildTree('a', 'b', 'c')
    val treeRight: BTree[Char] = BTree.buildTree('d', 'e', 'f', 'g')

    val tree: BTree[Char] = BTree('h', treeRight, treeLeft)
    val tree2: BTree[Char] = BTree('h', treeLeft, treeRight)
    tree.identical(tree2) must be (false)
    tree2.identical(tree) must be (false)
  }

  "height" should "return 1 for single leaf node" in {
    val tree = BTree(1)
    tree.height must be (1)
  }

  "height" should "return 2 for simple 3-node tree" in {
    val tree = BTree.buildTree(1, 2, 3)
    tree.height must be(2)
  }

  "height" should "count correctly for a unbalanced tree" in {
    val right = BTree(1)
    val left = BTree.buildTree(2, 3, 4, 5)

    val tree = BTree(6, left, right)

    tree.height must be (4)
  }

  "mapPostOrder" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    val _ = tree.mapPostOrder(k => {buf += k; k})

    buf.toList must be (List(4, 5, 2, 6, 7, 3, 1))
  }

  "mapPreOrder" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    val _ = tree.mapPreOrder(k => {buf += k; k})

    buf.toList must be (List(1, 2, 4, 5, 3, 6, 7))
  }
  "mapInOrder" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    val _ = tree.mapInOrder(k => {buf += k; k})

    buf.toList must be (List(4, 2, 5, 1, 6, 3, 7))
  }

  "traverseByLevel" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.traverseLevel()(tree => buf += tree.value)

    buf.toList must be(List(1, 2, 3, 4, 5, 6, 7))
  }

  "traverseSpiral" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.traverseSpiral(tree => buf += tree.value)

    buf.toList must be(List(1, 2, 3, 7, 6, 5, 4))
  }

  "reverseLevelTraversal" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.reverseLevelTraversal(tree => buf += tree.value)

    buf.toList must be(List(4, 5, 6, 7, 2, 3, 1))
  }

  "sideToSideTraversal" should "traverse in the correct order" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.sideToSideTraversal()(tree => buf += tree.value)

    buf.toList must be(List(1, 2, 3, 4, 7, 5, 6))
  }

  "sideViewTraversal" should "traverse in the correct order" in {
    val fTree: BTree[Char] = BTree('f', Option(BTree.buildTree('h')), None)
    val eTree: BTree[Char] = BTree('e', fTree, BTree('g'))
    val cTree: BTree[Char] = BTree('c', Option(eTree), None)
    val bTree: BTree[Char] = BTree('b', Option(BTree('d')), None)
    val root: BTree[Char] = BTree('a', Option(bTree), Option(cTree))

    val bufLeft: mutable.ListBuffer[Char] = mutable.ListBuffer()
    val bufRight: mutable.ListBuffer[Char] = mutable.ListBuffer()

    root.sideViewTraversal()(tree => bufLeft += tree.value)
    root.sideViewTraversal(left = false)(tree => bufRight += tree.value)

    bufLeft.toList must be(List('a', 'b', 'd', 'f', 'h'))
    bufRight.toList must be(List('a', 'c', 'e', 'g', 'h'))
  }

  "bottomViewTraversal" should "traverse in the correct order" in {
    val fTree: BTree[Char] = BTree('f', Option(BTree.buildTree('h')), None)
    val eTree: BTree[Char] = BTree('e', fTree, BTree('g'))
    val cTree: BTree[Char] = BTree('c', Option(eTree), None)
    val bTree: BTree[Char] = BTree('b', Option(BTree('d')), Option(BTree('x')))
    val root: BTree[Char] = BTree('a', Option(bTree), Option(cTree))

    val bufBottom: mutable.ListBuffer[Char] = mutable.ListBuffer()

    root.bottomViewTraversal(tree => bufBottom += tree.value)

    bufBottom.toList must be(List('h', 'f', 'e', 'g'))
  }

  "topViewTraversal" should "traverse in the correct order" in {
    val fTree: BTree[Char] = BTree('f', Option(BTree.buildTree('h')), None)
    val eTree: BTree[Char] = BTree('e', fTree, BTree('g'))
    val cTree: BTree[Char] = BTree('c', Option(eTree), None)
    val bTree: BTree[Char] = BTree('b', Option(BTree('d')), Option(BTree('x')))
    val root: BTree[Char] = BTree('a', Option(bTree), Option(cTree))

    val bufTop: mutable.ListBuffer[Char] = mutable.ListBuffer()

    root.topViewTraversal(tree => bufTop += tree.value)

    bufTop.toList must be(List('d', 'b', 'a', 'c'))
  }

  "nextNode" should "return the next node if there is one" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
    tree.nextNode(tree.left.get).get must be(tree.right.get)
    tree.nextNode(tree.right.get) must be(None)

    tree.nextNode(BTree.buildTree(4)).get must be(tree.left.get.right.get)
  }

  "isComplete" should "return true if the tree is complete" in {
    BTree.buildTree(1, 2, 3, 4, 5, 6, 7).isComplete must be(true)
    BTree(1, None, Option(BTree(2))).isComplete must be(false)
  }

  "cousins" should "identify cousin nodes" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

    tree.firstCousins(tree.left.get.left.get, tree.right.get.right.get) must be(true)
    tree.left.get.firstCousins(tree.left.get.left.get.right.get, tree.left.get.right.get.left.get) must be(true)
    tree.firstCousins(tree.left.get.left.get, tree.right.get.right.get.left.get) must be(false)
  }

  "findCousins" should "find cousins" in {
    val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    tree.findCousins(tree.left.get.right.get).map(_.map(_.value)).get must be(Seq(6, 7))
    tree.findCousins(tree.left.get.right.get.left.get).map(_.map(_.value)).get must be(Seq(8, 9, 12, 13, 14, 15))
  }

  "sumTree" should "create a new sum tree" in {
    val tree: BTree[Int] = BTree.buildTree(Seq.fill(math.pow(2, 4).toInt - 1)(1): _*)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()
    BTree.sumTree(tree).traverseInOrder(buf += _)

    buf must be(Seq(0, 2, 0, 6, 0, 2, 0, 14, 0, 2, 0, 6, 0, 2, 0))
  }

  "allWords" should "build all words from a list of Ints" in {
    BTree.allWords(_ <= 26)(List(1, 2)).map(_.map(alphabet(_)).mkString("")) must be(List("ab", "l"))
    BTree.allWords(_ <= 26)(List(1, 6, 6, 2)).map(_.map(alphabet(_)).mkString("")) must be(List("affb", "pfb"))
    BTree.allWords(_ <= 26)(List(1, 1, 1)).map(_.map(alphabet(_)).mkString("")) must be(List("aaa", "ak", "ka"))
    BTree.allWords(_ <= 26)(List()) must be(List())
  }

  "matchValues" should "find a subtree within another tree" in {
    val matchTree: BTree[Int] = BTree.buildTree(2, 4, 5)

    BTree.buildTree(1, 2, 3, 4, 5, 6, 7).matchValues(matchTree) must be(true)
    BTree.buildTree(2, 4, 5, 4, 5, 6, 7).matchValues(matchTree) must be(false)
    BTree.buildTree(7, 6, 5, 4, 3, 2, 1, 0).matchValues(BTree.buildTree(4, 0)) must be(true)
  }

  "matchValuesBetter" should "find a subtree within another tree" in {
    val matchTree: BTree[Int] = BTree.buildTree(2, 4, 5)

    BTree.buildTree(1, 2, 3, 4, 5, 6, 7).matchValues(matchTree) must be(true)
    BTree.buildTree(2, 4, 5, 4, 5, 6, 7).matchValues(matchTree) must be(false)
    BTree.buildTree(7, 6, 5, 4, 3, 2, 1, 0).matchValues(BTree.buildTree(4, 0)) must be(true)
  }

  "diameter" should "find the diameter of the tree" in {
    val f: BTree[Char] = BTree('f')
    val e: BTree[Char] = BTree('e', None, Option(f))
    val d: BTree[Char] = BTree('d')
    val b: BTree[Char] = BTree('b', d, e)
    val c: BTree[Char] = BTree('c')
    val a: BTree[Char] = BTree('a', b, c)

    a.diameter must be(5)
    b.diameter must be(4)
    d.diameter must be(1)
  }

  "symmetric" should "identify a symmetric tree" in {
    BTree.buildTree(1, 2, 3).symmetric must be(true)
    BTree.buildTree(1, 2, 3, 4).symmetric must be(false)

    val d: BTree[Char] = BTree('d')
    val e: BTree[Char] = BTree('e')
    val b: BTree[Char] = BTree('b', None, Option(d))
    val c: BTree[Char] = BTree('c', Option(e), None)
    val a: BTree[Char] = BTree('a', b, c)

    a.symmetric must be(true)
  }

  "symmetric2" should "identify a symmetric tree" in {
    BTree.buildTree(1, 2, 3).symmetric2 must be(true)
    BTree.buildTree(1, 2, 3, 4).symmetric2 must be(false)

    val d: BTree[Char] = BTree('d')
    val e: BTree[Char] = BTree('e')
    val b: BTree[Char] = BTree('b', None, Option(d))
    val c: BTree[Char] = BTree('c', Option(e), None)
    val a: BTree[Char] = BTree('a', b, c)

    a.symmetric2 must be(true)
  }

  "mirror" should "produce a mirror of a tree" in {
    BTree.buildTree(1, 2, 3).mirror must be(BTree.buildTree(1, 3, 2))
    BTree.buildTree(1, 2, 3).mirror must be(BTree.buildTree(1, 3, 2))

    val d: BTree[Char] = BTree('d')
    val e: BTree[Char] = BTree('e')
    val b: BTree[Char] = BTree('b', None, Option(d))
    val c: BTree[Char] = BTree('c', Option(e), None)
    val a: BTree[Char] = BTree('a', b, c)

    val b2: BTree[Char] = BTree('b', Option(d), None)
    val c2: BTree[Char] = BTree('c', None, Option(e))
    val a2: BTree[Char] = BTree('a', c2, b2)

    a.mirror must be(a2)
  }

  "flatten" should "flatten a tree to the right" in {
    BTree.buildTree(1, 2, 3, 4, 5).flatten must be(
      BTree(1, None, Some(BTree(2, None, Some(BTree(4, None, Some(BTree(5, None, Some(BTree(3, None, None)))))))))
    )
  }
}
