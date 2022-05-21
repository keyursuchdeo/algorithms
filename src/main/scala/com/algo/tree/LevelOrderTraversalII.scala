package com.algo.tree

import scala.collection.immutable.ListMap
import scala.collection.mutable

object LevelOrderTraversalII extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def levelOrderBottom(root: TreeNode): List[List[Int]] = {
      @scala.annotation.tailrec
      def traverse(queue: mutable.Queue[(TreeNode, Int)], map: ListMap[Int, Vector[Int]]): List[List[Int]] = {
        if(queue.isEmpty) {
          map.values.map(_.toList).toList
        } else {
          val (poppedNode, level) = queue.dequeue()
          if(poppedNode == null) {
            traverse(queue, map)
          } else {
            queue.enqueue((poppedNode.left, level + 1))
            queue.enqueue((poppedNode.right, level + 1))
            map.get(level) match {
              case Some(nodes) =>
                traverse(queue, map + (level -> (nodes :+ poppedNode.value)))
              case None =>
                traverse(queue, map + (level -> Vector(poppedNode.value)))
            }
          }
        }
      }

      traverse(mutable.Queue((root, 0)), ListMap())
    }
  }

}
