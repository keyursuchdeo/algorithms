package com.algo.tree

object BinarySearchTreeIterator extends App {


  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  class BSTIterator(_root: TreeNode) {

    private var elements: Seq[Int] = traverse(_root)

    def traverse(currNode: TreeNode): Seq[Int] = {
      if(currNode == null) {
        Nil
      } else {
        traverse(currNode.left) ++ Seq(currNode.value) ++ traverse(currNode.right)
      }
    }

    def next(): Int = {
      val head = elements.head
      elements = elements.tail
      head
    }

    def hasNext(): Boolean = {
      elements.nonEmpty
    }

  }

  /**
   * Your BSTIterator object will be instantiated and called as such:
   * var obj = new BSTIterator(root)
   * var param_1 = obj.next()
   * var param_2 = obj.hasNext()
   */

}
