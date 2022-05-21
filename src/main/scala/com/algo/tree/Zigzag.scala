package com.algo.tree


object Zigzag extends App {

//  val input = Array(1,null,1,1,1,null,null,1,1,null,1,null,null,null,1,null,1)
//  val treeNode = new TreeNode(1)
//  treeNode.right = new TreeNode(1)
//  treeNode.right.left = new TreeNode(1)
//  treeNode.right.right = new TreeNode(1)
//
//  treeNode.right.right.left = new TreeNode(1)
//  treeNode.right.right.right = new TreeNode(1)
//
//  treeNode.right.right.left.right = new TreeNode(1)
//  treeNode.right.right.left.right.right = new TreeNode(1)

//  val input = Array(1,null,1,1,1,null,null,1,1)
//  val treeNode = new TreeNode(1)
//  treeNode.right = new TreeNode(1)
//  treeNode.right.left = new TreeNode(1)
//  treeNode.right.right = new TreeNode(1)
//
//  treeNode.right.right.left = new TreeNode(1)
//  treeNode.right.right.right = new TreeNode(1)

//  val input = Array(1, 1)
//  val treeNode = new TreeNode(1)
//  treeNode.right = new TreeNode(1)
//  treeNode.right.left = new TreeNode(1)
//  treeNode.right.right = new TreeNode(1)
//  treeNode.right.right.left = new TreeNode(1)
//  treeNode.right.right.right = new TreeNode(1)
//  treeNode.right.right.left.right = new TreeNode(1)
//  treeNode.right.right.left.right.right = new TreeNode(1)

  val input = Array(1,1,1,null,1,null,null,1,1,null,1)
  val treeNode = new TreeNode(1)
  treeNode.left = new TreeNode(1)
  treeNode.right = new TreeNode(1)
  treeNode.left.right = new TreeNode(1)
  treeNode.left.right.left = new TreeNode(1)
  treeNode.left.right.right = new TreeNode(1)
  treeNode.left.right.left.right = new TreeNode(1)


   class TreeNode(var _value: Int) {
      var value: Int = _value
      var left: TreeNode = null
      var right: TreeNode = null
   }

  Solution.longestZigZag(treeNode)

  object Solution {

    object SubTree extends Enumeration {
      type SubTree = Value
      val Left, Right = Value
    }

    import SubTree._

    def longestZigZag(root: TreeNode): Int = {
      val a = Math.max(findLongestZigZag(root.left, SubTree.Left), findLongestZigZag(root.right, SubTree.Right))
      println(a)
      a
    }

    private def findLongestZigZag(node: TreeNode, currSubTree: SubTree, maxLen: Int = 0): Int = {
      if(node == null) {
        maxLen
      } else {
        val (nextZigZagNode, nextSubTree) = findNextZigZagNode(node, currSubTree)
        if (nextZigZagNode == null) {
          val (nextInlineNode, inlineSubTree) = findNextInlineNode(node, currSubTree)
          Math.max(maxLen + 1, findLongestZigZag(nextInlineNode, inlineSubTree))
        } else {
          val (nextInlineNode, inlineSubTree) = findNextInlineNode(node, currSubTree)
          Math.max(findLongestZigZag(nextZigZagNode, nextSubTree, maxLen + 1), findLongestZigZag(nextInlineNode, inlineSubTree))

        }
      }
    }

    private def findNextZigZagNode(node: TreeNode, currSubTree: SubTree): (TreeNode, SubTree) = {
      currSubTree match {
        case SubTree.Left => (node.right, SubTree.Right)
        case SubTree.Right => (node.left, SubTree.Left)
      }
    }

    private def findNextInlineNode(node: TreeNode, currSubTree: SubTree): (TreeNode, SubTree) = {
      currSubTree match {
        case SubTree.Left => (node.left, SubTree.Left)
        case SubTree.Right => (node.right, SubTree.Right)
      }
    }
  }
}
