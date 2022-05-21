package com.algo.tree

import scala.collection.mutable

object SerDeserBST2 extends App {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  class Codec {
    // Encodes a list of strings to a single string.
    def serialize(root: TreeNode): String = {
      def serializeTree(node: TreeNode): Seq[Int] = {
        if(node == null) {
          Nil
        } else {
          node.value +: (serializeTree(node.left) ++ serializeTree(node.right))
        }
      }

      serializeTree(root).mkString(",")
    }

    // Decodes a single string to a list of strings.
    def deserialize(s: String): TreeNode = {

      @scala.annotation.tailrec
      def findNode(value: Int, prevNode: TreeNode, nodes: Seq[TreeNode]): (TreeNode, String, Seq[TreeNode]) = {
        if(nodes.isEmpty) {
          if(value < prevNode.value) {
            (prevNode, "left", nodes)
          } else {
            (prevNode, "right", nodes)
          }
        } else {
          val head = nodes.head
          if(value < head.value) {
            if(head.left == null) {
              (head, "left", nodes)
            } else {
              (prevNode, "right", nodes)
            }
          } else {
            findNode(value, head, nodes.tail)
          }
        }
      }

      @scala.annotation.tailrec
      def deserializeValues(values: Seq[Int], root: TreeNode, currLeft: Seq[TreeNode], currRight: Seq[TreeNode]): TreeNode = {
        if(values.isEmpty) {
          root
        } else {
          val value = values.head
          if(value < root.value) {
            val (node, direction, remainingNodes) = findNode(value, root, currLeft)
            val currNode = new TreeNode(value)
            if(direction == "left") {
              node.left = currNode
              deserializeValues(values.tail, root, currNode +: remainingNodes, currRight)
            } else {
              node.right = currNode
              deserializeValues(values.tail, root, currNode +: remainingNodes, currRight)
            }
          } else {
            val (node, direction, remainingNodes) = findNode(value, root, currRight)
            val currNode = new TreeNode(value)
            if(direction == "left") {
              node.left = currNode
              deserializeValues(values.tail, root, currLeft, currNode +: remainingNodes)
            } else {
              node.right = currNode
              deserializeValues(values.tail, root, currLeft, currNode +: remainingNodes)
            }

          }
        }
      }

      if(s == null || s.isEmpty) {
        null
      } else {
        val nums = s.split(",").map(_.toInt)
        deserializeValues(nums.tail, new TreeNode(nums.head), Nil, Nil)
      }
    }
  }

}
