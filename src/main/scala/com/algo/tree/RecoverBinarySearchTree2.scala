package com.algo.tree

object RecoverBinarySearchTree2 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def recoverTree(root: TreeNode): Unit = {

      def findPossibleSwaps(currNode: TreeNode, lowerLimitNode: TreeNode,
                            upperLimitNode: TreeNode, firstSwap: Option[(TreeNode, TreeNode)],
                            secondSwap: Option[(TreeNode, TreeNode)]): (Option[(TreeNode, TreeNode)], Option[(TreeNode, TreeNode)]) = {
        if(currNode == null) {
          (firstSwap, secondSwap)
        } else if((currNode.value == Int.MinValue && currNode.value == lowerLimitNode.value) ||
          (currNode.value == Int.MaxValue && currNode.value == upperLimitNode.value) ||
          (currNode.value > lowerLimitNode.value && currNode.value < upperLimitNode.value)) {
          val (f, s) = findPossibleSwaps(currNode.left, lowerLimitNode, currNode, firstSwap, secondSwap)
          findPossibleSwaps(currNode.right, currNode, upperLimitNode, f, s)
        } else if (currNode.value > lowerLimitNode.value) {
          (firstSwap, secondSwap) match {
            case (None, _) =>
              val (f, s) = findPossibleSwaps(currNode.left, lowerLimitNode, currNode, Option(currNode, upperLimitNode), secondSwap)
              findPossibleSwaps(currNode.right, currNode, upperLimitNode, f, s)
            case (Some(_), _) =>
              (firstSwap, Option(currNode, upperLimitNode))
          }
        } else if (currNode.value < upperLimitNode.value) {
            (firstSwap, secondSwap) match {
              case (None, _) =>
                val (f, s) = findPossibleSwaps(currNode.left, lowerLimitNode, currNode, Option(lowerLimitNode, currNode), secondSwap)
                findPossibleSwaps(currNode.right, currNode, upperLimitNode, f, s)
              case (Some(_), _) =>
                (firstSwap, Option(lowerLimitNode, currNode))
            }
        } else {
          (firstSwap, secondSwap)
        }
      }

      val (f, s) = findPossibleSwaps(root.left, new TreeNode(Int.MinValue), root, None, None)
      val (uf, us) = findPossibleSwaps(root.right, root, new TreeNode(Int.MaxValue), f, s)
      (uf, us) match {
        case (Some((f1, _)), Some((_, s2))) =>
          val temp = s2.value
          s2.value = f1.value
          f1.value = temp
        case (Some((f1, f2)), _) =>
          val temp = f1.value
          f1.value = f2.value
          f2.value = temp
        case _ => ()
      }

    }
  }
}
