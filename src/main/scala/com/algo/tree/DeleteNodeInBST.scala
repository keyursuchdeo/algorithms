package com.algo.tree

object DeleteNodeInBST extends App {

//  val r = new TreeNode(5)
//  r.left = new TreeNode(3)
//  r.right = new TreeNode(6)
//  r.left.left = new TreeNode(2)
//  r.left.right = new TreeNode(4)
//  r.right.right = new TreeNode(7)

  val r = new TreeNode(3)
  r.left = new TreeNode(2)
  r.right = new TreeNode(4)
  r.left.left = new TreeNode(1)

  val res = Solution.deleteNode(r, 3)
  println(res)

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def deleteNode(root: TreeNode, key: Int): TreeNode = {
      @scala.annotation.tailrec
      def find(currNode: TreeNode, parentNode: Option[TreeNode]): (Option[TreeNode], Option[TreeNode]) = {
        if (currNode == null) {
          (None, parentNode)
        } else if (currNode.value == key) {
          (Option(currNode), parentNode)
        } else if (key > currNode.value) {
          find(currNode.right, Option(currNode))
        } else {
          find(currNode.left, Option(currNode))
        }
      }

      def findInOrderSuccessor(node: TreeNode) = {
        @scala.annotation.tailrec
        def find(currNode: TreeNode): TreeNode = {
          if (currNode.left == null) {
            currNode
          } else {
            find(currNode.left)
          }
        }

        find(node)
      }

      def findInOrderPredecessor(node: TreeNode) = {
        @scala.annotation.tailrec
        def find(currNode: TreeNode): TreeNode = {
          if (currNode.right == null) {
            currNode
          } else {
            find(currNode.right)
          }
        }

        find(node)
      }

      find(root, None) match {
        case (None, _) => root
        case (Some(node), None) if node.left == null && node.right == null =>
          null
        case (Some(node), Some(pNode)) if node.left == null && node.right == null =>
          if (pNode.left == node) {
            pNode.left = null
          } else {
            pNode.right = null
          }
          root
        case (Some(node), _) if node.left == null =>
          val replacementNode = findInOrderSuccessor(node.right)
          val replacementValue = replacementNode.value
          deleteNode(node, replacementValue)
          node.value = replacementValue
          root
        case (Some(node), _) =>
          val replacementNode = findInOrderPredecessor(node.left)
          val replacementValue = replacementNode.value
          deleteNode(node, replacementValue)
          node.value = replacementValue
          root

      }
    }
  }

}
