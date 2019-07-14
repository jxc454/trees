package com.jcc

import org.scalatest._
import scala.collection.mutable

class TreeTest extends FlatSpec with MustMatchers {
  "buildTree" should "build a Tree" in {
    Tree.buildTree(3, 6, 2, 7, 1, 8)
  }

  "buildTree[Int]" should "build a Tree" in {
    Tree.buildTree[Int](1, 2, 3)
  }

  "inOrderWithLevel" should "build a list of Tuple2" in {
    val tree: Tree[Char] = Tree.buildTree('a', 'b', 'c')
    val id: List[(Char, Int)] = tree.inOrderWithLevel()

    id must be(List(('b', 1), ('a', 0), ('c', 1)))
  }

  "inOrderWithLevel" should "build a more complex list of Tuple2" in {
    val treeLeft: Tree[Char] = Tree.buildTree('a', 'b', 'c')
    val treeRight: Tree[Char] = Tree.buildTree('d', 'e', 'f', 'g')

    val tree: Tree[Char] = Tree('h', treeLeft, treeRight)
    val id: List[(Char, Int)] = tree.inOrderWithLevel()

    id must be(List(('b', 2), ('a', 1), ('c', 2), ('h', 0), ('g', 3), ('e', 2), ('d', 1), ('f', 2)))
  }

  "identical" should "return true for identical Trees" in {
    val treeLeft: Tree[Char] = Tree.buildTree('a', 'b', 'c')
    val treeRight: Tree[Char] = Tree.buildTree('d', 'e', 'f', 'g')

    val tree: Tree[Char] = Tree('h', treeLeft, treeRight)
    val tree2: Tree[Char] = Tree('h', treeLeft, treeRight)
    Tree.identical(tree)(tree2) must be (true)
    Tree.identical(tree)(tree2) must be (true)
  }

  "identical" should "return false for different Trees" in {
    val treeLeft: Tree[Char] = Tree.buildTree('a', 'b', 'c')
    val treeRight: Tree[Char] = Tree.buildTree('d', 'e', 'f', 'g')

    val tree: Tree[Char] = Tree('h', treeRight, treeLeft)
    val tree2: Tree[Char] = Tree('h', treeLeft, treeRight)
    Tree.identical(tree)(tree2) must be (false)
    Tree.identical(tree)(tree2) must be (false)
  }

  "height" should "return 1 for single leaf node" in {
    val tree: Tree[Int] = Tree(1)
    tree.height must be (1)
  }

  "height" should "return 2 for simple 3-node tree" in {
    val tree = Tree.buildTree(1, 2, 3)
    tree.height must be(2)
  }

  "height" should "count correctly for a unbalanced tree" in {
    val right = Tree(1)
    val left = Tree.buildTree(2, 3, 4, 5)

    val tree = Tree(6, left, right)

    tree.height must be (4)
  }

  "mapPostOrder" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    val _ = tree.mapPostOrder(k => {buf += k; k})

    buf.toList must be (List(4, 5, 2, 6, 7, 3, 1))
  }

  "mapPreOrder" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    val _ = tree.mapPreOrder(k => {buf += k; k})

    buf.toList must be (List(1, 2, 4, 5, 3, 6, 7))
  }
  "mapInOrder" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    val _ = tree.mapInOrder(k => {buf += k; k})

    buf.toList must be (List(4, 2, 5, 1, 6, 3, 7))
  }

  "traverseByLevel" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.traverseLevel()(tree => buf += tree.value)

    buf.toList must be(List(1, 2, 3, 4, 5, 6, 7))
  }

  "traverseSpiral" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.traverseSpiral(tree => buf += tree.value)

    buf.toList must be(List(1, 2, 3, 7, 6, 5, 4))
  }

  "reverseLevelTraversal" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.reverseLevelTraversal(tree => buf += tree.value)

    buf.toList must be(List(4, 5, 6, 7, 2, 3, 1))
  }

  "sideToSideTraversal" should "traverse in the correct order" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

    tree.sideToSideTraversal()(tree => buf += tree.value)

    buf.toList must be(List(1, 2, 3, 4, 7, 5, 6))
  }

  "sideViewTraversal" should "traverse in the correct order" in {
    val fTree: Tree[Char] = Tree('f', Tree.buildTree('h'), NilTree)
    val eTree: Tree[Char] = Tree('e', fTree, Tree('g'))
    val cTree: Tree[Char] = Tree('c', eTree, NilTree)
    val bTree: Tree[Char] = Tree('b', Tree('d'), NilTree)
    val root: Tree[Char] = Tree('a', bTree, cTree)

    val bufLeft: mutable.ListBuffer[Char] = mutable.ListBuffer()
    val bufRight: mutable.ListBuffer[Char] = mutable.ListBuffer()

    root.sideViewTraversal()(tree => bufLeft += tree.value)
    root.sideViewTraversal(left = false)(tree => bufRight += tree.value)

    bufLeft.toList must be(List('a', 'b', 'd', 'f', 'h'))
    bufRight.toList must be(List('a', 'c', 'e', 'g', 'h'))
  }

  "bottomViewTraversal" should "traverse in the correct order" in {
    val fTree: Tree[Char] = Tree('f', Tree('h'), NilTree)
    val eTree: Tree[Char] = Tree('e', fTree, Tree('g'))
    val cTree: Tree[Char] = Tree('c', eTree, NilTree)
    val bTree: Tree[Char] = Tree('b', Tree('d'), Tree('x'))
    val root: Tree[Char] = Tree('a', bTree, cTree)

    val bufBottom: mutable.ListBuffer[Char] = mutable.ListBuffer()

    root.bottomViewTraversal(tree => bufBottom += tree.value)

    bufBottom.toList must be(List('h', 'f', 'e', 'g'))
  }

  "topViewTraversal" should "traverse in the correct order" in {
    val fTree: Tree[Char] = Tree('f', Tree('h'), NilTree)
    val eTree: Tree[Char] = Tree('e', fTree, Tree('g'))
    val cTree: Tree[Char] = Tree('c', eTree, NilTree)
    val bTree: Tree[Char] = Tree('b', Tree('d'), Tree('x'))
    val root: Tree[Char] = Tree('a', bTree, cTree)

    val bufTop: mutable.ListBuffer[Char] = mutable.ListBuffer()

    root.topViewTraversal(tree => bufTop += tree.value)

    bufTop.toList must be(List('d', 'b', 'a', 'c'))
  }

  "nextNode" should "return the next node if there is one" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    Tree.nextNode(tree, tree.left).get must be(tree.right)
    Tree.nextNode(tree, tree.right) must be(None)

    Tree.nextNode(tree, Tree.buildTree(4)).get must be(tree.left.right)
  }

  "isComplete" should "return true if the tree is complete" in {
    Tree.buildTree(1, 2, 3, 4, 5, 6, 7).isComplete must be(true)
    Tree(1, NilTree, Tree(2)).isComplete must be(false)
  }

  "cousins" should "identify cousin nodes" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

    Tree.firstCousins(tree, tree.left.left, tree.right.right) must be(true)
    Tree.firstCousins(tree.left, tree.left.left.right, tree.left.right.left) must be(true)
    Tree.firstCousins(tree, tree.left.left, tree.right.right.left) must be(false)
  }

  "findCousins" should "find cousins" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    Tree.findCousins(tree, tree.left.right).map(_.map(_.value)).get must be(Seq(6, 7))
    Tree.findCousins(tree, tree.left.right.left).map(_.map(_.value)).get must be(Seq(8, 9, 12, 13, 14, 15))
  }

  "sumTree" should "create a new sum tree" in {
    val tree: Tree[Int] = Tree.buildTree(Seq.fill(math.pow(2, 4).toInt - 1)(1): _*)
    val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()
    Tree.sumTree(tree).traverseInOrder(buf += _)

    buf must be(Seq(0, 2, 0, 6, 0, 2, 0, 14, 0, 2, 0, 6, 0, 2, 0))
  }

  "allWords" should "build all words from a list of Ints" in {
    Tree.allWords(_ <= 26)(List(1, 2)).map(_.map(alphabet(_)).mkString("")) must be(List("ab", "l"))
    Tree.allWords(_ <= 26)(List(1, 6, 6, 2)).map(_.map(alphabet(_)).mkString("")) must be(List("affb", "pfb"))
    Tree.allWords(_ <= 26)(List(1, 1, 1)).map(_.map(alphabet(_)).mkString("")) must be(List("aaa", "ak", "ka"))
    Tree.allWords(_ <= 26)(List()) must be(List())
  }

  "matchValues" should "find a subtree within another tree" in {
    val matchTree: Tree[Int] = Tree.buildTree(2, 4, 5)

    Tree.matchValues(Tree.buildTree(1, 2, 3, 4, 5, 6, 7), matchTree) must be(true)
    Tree.matchValues(Tree.buildTree(2, 4, 5, 4, 5, 6, 7), matchTree) must be(false)
    Tree.matchValues(Tree.buildTree(7, 6, 5, 4, 3, 2, 1, 0), Tree.buildTree(4, 0)) must be(true)
  }

  "matchValuesBetter" should "find a subtree within another tree" in {
    val matchTree: Tree[Int] = Tree.buildTree(2, 4, 5)

    Tree.matchValues(Tree.buildTree(1, 2, 3, 4, 5, 6, 7), matchTree) must be(true)
    Tree.matchValues(Tree.buildTree(2, 4, 5, 4, 5, 6, 7), matchTree) must be(false)
    Tree.matchValues(Tree.buildTree(7, 6, 5, 4, 3, 2, 1, 0), Tree.buildTree(4, 0)) must be(true)
  }

  "diameter" should "find the diameter of the tree" in {
    val f: Tree[Char] = Tree('f')
    val e: Tree[Char] = Tree('e', NilTree, f)
    val d: Tree[Char] = Tree('d')
    val b: Tree[Char] = Tree('b', d, e)
    val c: Tree[Char] = Tree('c')
    val a: Tree[Char] = Tree('a', b, c)

    a.diameter must be(5)
    b.diameter must be(4)
    d.diameter must be(1)
  }

  "symmetric" should "identify a symmetric tree" in {
    Tree.buildTree(1, 2, 3).symmetric must be(true)
    Tree.buildTree(1, 2, 3, 4).symmetric must be(false)

    val d: Tree[Char] = Tree('d')
    val e: Tree[Char] = Tree('e')
    val b: Tree[Char] = Tree('b', NilTree, d)
    val c: Tree[Char] = Tree('c', e, NilTree)
    val a: Tree[Char] = Tree('a', b, c)

    a.symmetric must be(true)
  }

  "symmetric2" should "identify a symmetric tree" in {
    Tree.buildTree(1, 2, 3).symmetric2 must be(true)
    Tree.buildTree(1, 2, 3, 4).symmetric2 must be(false)

    val d: Tree[Char] = Tree('d')
    val e: Tree[Char] = Tree('e')
    val b: Tree[Char] = Tree('b', NilTree, d)
    val c: Tree[Char] = Tree('c', e, NilTree)
    val a: Tree[Char] = Tree('a', b, c)

    a.symmetric2 must be(true)
  }

  "mirror" should "produce a mirror of a tree" in {
    Tree.buildTree(1, 2, 3).mirror must be(Tree.buildTree(1, 3, 2))
    Tree.buildTree(1, 2, 3).mirror must be(Tree.buildTree(1, 3, 2))

    val d: Tree[Char] = Tree('d')
    val e: Tree[Char] = Tree('e')
    val b: Tree[Char] = Tree('b', NilTree, d)
    val c: Tree[Char] = Tree('c', e, NilTree)
    val a: Tree[Char] = Tree('a', b, c)

    val b2: Tree[Char] = Tree('b', d, NilTree)
    val c2: Tree[Char] = Tree('c', NilTree, e)
    val a2: Tree[Char] = Tree('a', c2, b2)

    a.mirror must be(a2)
  }

  "flatten" should "flatten a tree to the right" in {
    Tree.buildTree(1, 2, 3, 4, 5).flatten must be(
      Tree(1, NilTree, Tree(2, NilTree, Tree(4, NilTree, Tree(5, NilTree, Tree(3, NilTree, NilTree)))))
    )
  }

  "canMatchWithChildSwap" should "return true if trees can match by swapping children" in {
    Tree.canMatchWithChildSwap(Tree.buildTree(1, 2, 3), Tree.buildTree(1, 3, 2)) must be(true)
    Tree.canMatchWithChildSwap(Tree.buildTree(1, 2, 3), Tree.buildTree(1, 2, 3)) must be(true)
    Tree.canMatchWithChildSwap(Tree.buildTree(1, 2, 3), Tree.buildTree(1, 4, 3)) must be(false)
    Tree.canMatchWithChildSwap(Tree.buildTree(1, 2, 3, 4, 5, 6, 7),
      Tree.buildTree(1, 2, 3, 6, 7, 4, 5)) must be(false)
    Tree.canMatchWithChildSwap(Tree.buildTree(1, 2, 3, 4, 5, 6, 7),
      Tree.buildTree(1, 3, 2, 7, 6, 4, 5)) must be(true)
  }

  "LCA" should "find lowest common ancestor of two nodes" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9)
    Tree.LCA(tree, Tree.buildTree(2), Tree.buildTree(9)).value must be(2)
    Tree.LCA(tree, Tree.buildTree(3), Tree.buildTree(9)).value must be(1)
    Tree.LCA(tree, Tree.buildTree(1), Tree.buildTree(5)).value must be(1)
    Tree.LCA(tree, Tree.buildTree(8), Tree.buildTree(9)).value must be(4)
  }

  "pathsToLeafs" should "return a list of paths from root node to leaf nodes" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9)
    tree.pathsToLeafs must be(List(
      List(1, 2, 4, 8), List(1, 2, 4, 9), List(1, 2, 5), List(1, 3, 6), List(1, 3, 7),
    ))
  }

  "ancestors" should "find ancestors of a node" in {
    val tree = Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9)
    Tree.ancestors(tree, Tree(9)).map(_.value) must be(Seq(1, 2, 4))
  }

  "distance" should "calculate distance between nodes" in {
    val tree = Tree.buildTree(1, 2, 3)
    Tree.distance(tree, Tree(2), Tree(3)) must be(2)
    val tree2 = Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8, 9)
    Tree.distance(tree2, Tree.buildTree(8), Tree.buildTree(7)) must be(5)
  }

  "verticalSum" should "calculate vertical sum of a tree" in {
    val left, right = Tree.buildTreeOfInt(1, 2, 3, 4, 5)
    val tree = Tree[Int](0, left, right)
    Tree.verticalSum(tree) must be (Seq(1, 2, 8, 7, 7, 5))
  }

  "diagonalSum" should "sum negative slope diagonals in a tree of Int" in {
    val tree: Tree[Int] = Tree.buildTreeOfInt(1, 2, 3, 4, 5, 6, 7)
    Tree.diagonalSum(tree) must be(Seq(17, 10, 1))
  }

  "corners" should "find all corners" in {
    Tree.buildTree(1, 2, 3, 4, 5, 6).corners.map(_.value) must be(List(1, 2, 3, 4, 6))
    Tree.buildTree(1, 2, 3, 4, 5, 6, 7).corners.map(_.value) must be(List(1, 2, 3, 4, 7))
    Tree.buildTree(1, 2, 3, 4, 5, 6, 7, 8).corners.map(_.value) must be(List(1, 2, 3, 4, 7, 8))
  }

  "convertToLinkedList" should "build a linked list from a Tree" in {
    val values: mutable.ListBuffer[Int] = mutable.ListBuffer()
    val ll = Tree.convertToLinkedList(Tree.buildTree(1, 2, 3, 4, 5, 6)).get

    def extract(l: JccLinkedList[Int]): Unit = {
      values += l.value
      l.next.foreach(extract)
    }

    extract(ll)
    values must be(mutable.ListBuffer(4, 2, 5, 1, 6, 3))
  }

  "sinkZeroes" should "push all zeroes to the bottom of the tree" in {
    val tree = Tree.buildTree[Int](0, 0, 0, 1, 1, 1, 1)
    Tree.sinkZeroes(tree) must be(Tree.buildTree[Int](1, 1, 1, 0, 0, 0, 1))
    val tree2 = Tree.buildTree[Int](0, 0, 0, 0, 0, 0)
    Tree.sinkZeroes(tree2) must be(tree2)
    val tree3 = Tree.buildTree[Int](0, 0, 0, 0, 0, 0)
    Tree.sinkZeroes(tree3) must be(tree3)
    val tree4 = Tree.buildTree[Int](1, 1, 1, 0, 0, 0)
    Tree.sinkZeroes(tree4) must be(tree4)
  }

  "removeHalfNodes" should "result in a complete tree" in {
    Tree.buildTree(1, 2, 3, 4).removeHalfNodes must be(Tree.buildTree(1, 4, 3))
    val e = Tree('e', NilTree, NilTree)
    val d = Tree('d', NilTree, NilTree)
    val c = Tree('c', d, e)
    val b = Tree('b', c, NilTree)
    val a = Tree('a', NilTree, b)
    a.removeHalfNodes must be(c)
  }

  "truncatePathsLessThan" should "clear paths less than N" in {
    val tree1 = Tree.buildTree(1, 2, 3, 4, 5, 6, 7)
    Tree.truncatePathsLessThan(tree1, 8) must be(Tree.buildTree(3, 6, 7))
    val tree2 = Tree.buildTree(1, 2, 3, 4, 5, 1, 7)
    Tree.truncatePathsLessThan(tree2, 8) must be(NilTree)
    Tree.truncatePathsLessThan(tree2, 0) must be(tree2)
  }

  "maxRootToLeafPath" should "find a path with the max sum" in {
    def buildList(ints: Seq[Int]): Option[JccLinkedList[Int]] =
      (ints :\ (None: Option[JccLinkedList[Int]]))((k, acc) => Option(JccLinkedList(k, acc)))

    val res = Tree.maxRootToLeafPath(Tree.buildTree[Int](1, 2, 3, 4, 5, 6, 7))
    val expected = buildList(Seq(1, 3, 7))
    res must be(expected)

    val res2 = Tree.maxRootToLeafPath(Tree.buildTree[Int](1, 2, 3, 7, 5, 6, 2))
    val expected2 = buildList(Seq(1, 2, 7))
    res2 must be(expected2)

    val c4 = Tree(4)
    val b4 = Tree(4, NilTree, c4)
    val a4 = Tree(4, NilTree, b4)
    val a5 = Tree(5)
    val a6 = Tree(6, NilTree, a5)
    val root = Tree(0, a6, a4)

    val res3 = Tree.maxRootToLeafPath(root)
    val expected3 = buildList(Seq(0, 4, 4, 4))
    res3 must be(expected3)
  }

  "heightBalanced" should "return true if the tree is height balanced" in {
    NilTree.heightBalanced must be(true)
    Tree.buildTree(1, 2, 3).heightBalanced must be(true)
    Tree(0, NilTree, Tree.buildTree(1, 2, 3)).heightBalanced must be (false)
  }

  "leftChildRightCousin" should "create a left-child/right-cousin tree" in {
    NilTree.leftChildRightCousin must be(NilTree)
    Tree.buildTree(1, 2, 3).leftChildRightCousin must be(Tree(1, Tree(2, NilTree, Tree(3)), NilTree))
    val allLefts = Tree(1, Tree(2, Tree(3), NilTree), NilTree)
    allLefts.leftChildRightCousin must be (allLefts)
  }
}
