package com.jcc

import org.scalatest._

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
}
