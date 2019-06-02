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

      tree.traverseLevel(tree => buf += tree.value)

      buf.toList must be(List(1, 2, 3, 4, 5, 6, 7))
    }

    "traverseSpiral" should "traverse in the correct order" in {
      val tree = BTree.buildTree(1, 2, 3, 4, 5, 6, 7)
      val buf: mutable.ListBuffer[Int] = mutable.ListBuffer()

      tree.traverseSpiral(tree => buf += tree.value)

      buf.toList must be(List(1, 2, 3, 7, 6, 5, 4))
    }
}
