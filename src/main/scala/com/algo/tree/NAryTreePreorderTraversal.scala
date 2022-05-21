package com.algo.tree

object NAryTreePreorderTraversal extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var children: List[Node] = List()
  }


  object Solution {
    def preorder(root: Node): List[Int] = {
      def traverse(currNode: Node, output: Seq[Int]): Seq[Int] = {
        println(output)
        if (currNode == null) {
          Nil
        } else {
          Seq(currNode.value) ++ currNode.children.flatMap(child => {
            traverse(child, Nil)
          })
        }
      }

      traverse(root, Nil).toList
    }
  }

}
