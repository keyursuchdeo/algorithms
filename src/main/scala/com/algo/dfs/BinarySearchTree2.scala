package com.algo.dfs

import scala.collection.mutable

object BinarySearchTree2 extends App {

//  val input = Array(2, 1, 3)
//  val treeNode = new TreeNode(2)
//  treeNode.left = new TreeNode(1)
////  treeNode.left.right = new TreeNode(3)
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

//  val input = Array(5, 4, 7, 2, null, 6, 8, 1, 3, null, null)
//  val treeNode = new TreeNode(5)
//  treeNode.left = new TreeNode(4)
//  treeNode.right = new TreeNode(7)
//
//  treeNode.left.left = new TreeNode(2)
//  treeNode.right.left = new TreeNode(6)
//  treeNode.right.right = new TreeNode(8)
//
//  treeNode.left.left.left = new TreeNode(1)
//  treeNode.left.left.right = new TreeNode(3)
//  treeNode.right.right.right = new TreeNode(9)

    val input = Array(-2147483648,null,2147483647)
    val treeNode = new TreeNode(-2147483648)
    treeNode.right = new TreeNode(2147483647)


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
        val realTreeNode = RealTreeNode(root, Long.MinValue, Long.MaxValue)
        println(s"root: $realTreeNode")
        val stack: mutable.Stack[RealTreeNode] = initializeStack(realTreeNode)
        println(s"stack: $stack")
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
          println(s"stack: $stack")
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
      (node.left, node.right) match {
        case (None, None) => true
        case (Some(left), None) if left.value > left.lowerLimit &&  left.value < left.upperLimit => true
        case (None, Some(right)) if right.value > right.lowerLimit &&  right.value < right.upperLimit  => true
        case (Some(left), Some(right))
          if left.value > left.lowerLimit &&  left.value < left.upperLimit
            && right.value > right.lowerLimit &&  right.value < right.upperLimit => true
        case (_, _) => false
      }
    }

    case class RealTreeNode(private val treeNode: TreeNode,
                            lowerLimit: Long,
                            upperLimit: Long) {
      lazy val value: Int = treeNode.value
      lazy val left: Option[RealTreeNode] =
        Option(treeNode.left).map(RealTreeNode(_, calculateLowerLimit(Left), calculateUpperLimit(Left)))
      lazy val right: Option[RealTreeNode] =
        Option(treeNode.right).map(RealTreeNode(_, calculateLowerLimit(Right), calculateUpperLimit(Right)))

      private def calculateLowerLimit(subTree: SubTree): Long =
        subTree match {
          case Left => this.lowerLimit
          case Right => this.value
        }

      private def calculateUpperLimit(subTree: SubTree): Long =
        subTree match {
          case Left => this.value
          case Right => this.upperLimit
        }

      override def toString: String =
        s"value -> $value, lowerLimit -> $lowerLimit, upperLimit -> $upperLimit, left -> $left, right -> $right"
    }
  }

}
