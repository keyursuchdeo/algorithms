package com.algo.dfs

import scala.collection.mutable

object BinarySearchTree extends App {

//  val input = Array(2, 1, 3)
//  val treeNode = new TreeNode(2)
//  treeNode.left = new TreeNode(1)
//  //treeNode.left.right = new TreeNode(3)
//  treeNode.right = new TreeNode(3)

//  val input = Array(5, 1, 4, null, null, 3, 6)
//  val treeNode = new TreeNode(5)
//  treeNode.left = new TreeNode(1)
//  treeNode.right = new TreeNode(4)
//  val l = treeNode.left
//  val r = treeNode.right
//  l.left = null
//  l.right = null
//  r.left = new TreeNode(3)
//  r.right = new TreeNode(6)

  val input = Array(5, 4, 7, 2, null, 6, 8, 1, 3, null, null)
  val treeNode = new TreeNode(5)
  treeNode.left = new TreeNode(4)
  treeNode.right = new TreeNode(7)

  treeNode.left.left = new TreeNode(2)
  treeNode.right.left = new TreeNode(6)
  treeNode.right.right = new TreeNode(8)

  treeNode.left.left.left = new TreeNode(1)
  treeNode.left.left.right = new TreeNode(3)
  treeNode.right.right.right = new TreeNode(9)

//  val input = Array(1, 1)
//  val treeNode = new TreeNode(1)
//  treeNode.left = new TreeNode(1)

  val output = Solution.isValidBST(treeNode)
  println(output)

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  object Solution {

    object SubTree extends Enumeration {
      type SubTree = Value
      val Left, Right = Value
    }

    import SubTree._


    def isValidBST(root: TreeNode): Boolean = {
      if (root == null) true else {
        val realTreeNode = RealTreeNode(root, None, None)
        val stack: mutable.Stack[RealTreeNode] = initializeStack(realTreeNode)
        validate(stack, currentState =  true)
      }
    }

    @scala.annotation.tailrec
    private def validate(stack: mutable.Stack[RealTreeNode], currentState: Boolean): Boolean = {
      if (stack.isEmpty) {
        currentState
      } else {
        val node = stack.pop()
        if(isValidBSTNode(node)) {
          pushChildrenToStack(stack, node)
          validate(stack, currentState = true)
        } else {
          false
        }
      }
    }

    private def pushChildrenToStack(stack: mutable.Stack[RealTreeNode], root: RealTreeNode) = {
      pushToStackIfNonEmpty(stack, root.right)
      pushToStackIfNonEmpty(stack, root.left)
    }

    private def pushToStackIfNonEmpty(stack: mutable.Stack[RealTreeNode], optNode: Option[RealTreeNode]): mutable.Seq[RealTreeNode] = {
      optNode match {
        case Some(node) => stack.push(node)
        case _ => stack
      }
    }

    private def initializeStack(root: RealTreeNode): mutable.Stack[RealTreeNode] = {
      mutable.Stack[RealTreeNode](root)
    }

    private def isValidBSTNode(node: RealTreeNode) = {
      (node.parentValue, node.subTree) match {
        case (Some(parentValue), Some(Left)) => isValidLeftBSTNode(node, parentValue)
        case (Some(parentValue), Some(Right)) => isValidRightBSTNode(node, parentValue)
        case (_, _) => isValidRootBSTNode(node)
      }
    }

    private def isValidRootBSTNode(node: RealTreeNode) = {
      (node.left, node.right) match {
        case (None, None) => true
        case (Some(left), None) if node.value > left.value => true
        case (None, Some(right)) if node.value < right.value => true
        case (Some(left), Some(right)) if node.value > left.value && node.value < right.value => true
        case (_, _) => false
      }
    }

    private def isValidLeftBSTNode(node: RealTreeNode, parentValue: Int) = {
      (node.left, node.right) match {
        case (None, None) => true
        case (Some(left), None) if node.value > left.value => true
        case (None, Some(right)) if node.value < right.value && right.value < parentValue => true
        case (Some(left), Some(right)) if node.value > left.value && node.value < right.value && right.value < parentValue => true
        case (_, _) => false
      }
    }

    private def isValidRightBSTNode(node: RealTreeNode, parentValue: Int) = {
      (node.left, node.right) match {
        case (None, None) => true
        case (Some(left), None) if node.value > left.value && left.value > parentValue => true
        case (None, Some(right)) if node.value < right.value => true
        case (Some(left), Some(right)) if node.value > left.value && left.value > parentValue && node.value < right.value => true
        case (_, _) => false
      }
    }

    case class RealTreeNode(value: Int,
                            left: Option[RealTreeNode],
                            right: Option[RealTreeNode],
                            parentValue: Option[Int],
                            subTree: Option[SubTree])

    object RealTreeNode {
      def apply(treeNode: TreeNode, parentValue: Option[Int], subTree: Option[SubTree]): RealTreeNode = {
        val left: Option[TreeNode] = Option(treeNode.left)
        val right: Option[TreeNode] = Option(treeNode.right)
        RealTreeNode(
          treeNode.value,
          left.map(RealTreeNode.apply(_, Option(treeNode.value), Option(Left))),
          right.map(RealTreeNode.apply(_, Option(treeNode.value), Option(Right))),
          parentValue,
          subTree
        )
      }
    }
  }

}
