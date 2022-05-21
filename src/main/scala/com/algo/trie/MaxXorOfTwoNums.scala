package com.algo.trie

object MaxXorOfTwoNums extends App {

  /*object Solution {
    def findMaximumXOR(nums: Array[Int]): Int = {

      case class Node(endOfNum: Boolean, nodes: Array[Node]) {
        require(nodes.length == 2)
      }

      def initializeRoot(num: Int): Node = {
        val binNum: Array[Char] = num.toBinaryString.reverse.toCharArray
        val root = Node(endOfNum = false, new Array[Node](2))

        @scala.annotation.tailrec
        def initialize(index: Int, currNode: Node): Unit = {
          if (index == binNum.length - 1) {
            val node = Node(endOfNum = true, new Array[Node](2))
            if (binNum(index) == '0') {
              currNode.nodes(0) = node
            } else {
              currNode.nodes(1) = node
            }
          } else {
            val node = Node(endOfNum = false, new Array[Node](2))
            if (binNum(index) == '0') {
              currNode.nodes(0) = node
            } else {
              currNode.nodes(1) = node
            }
            initialize(index + 1, node)
          }
        }

        initialize(0, root)
        root
      }

      def insertNum(node: Node, num: Int) = {
        val binNum: Array[Char] = num.toBinaryString.reverse.toCharArray
        def insert(index: Int, currNode: Node) = {
          if (index == binNum.length - 1) {
            val node = Node(endOfNum = true, new Array[Node](2))
            if (binNum(index) == '0') {
              if(currNode.nodes(0) == null) {
                currNode.nodes(0) = node
              } else {
                ()
              }
            } else {
              if(currNode.nodes(1) == null) {
                currNode.nodes(1) = node
              } else {
                ()
              }
            }
          } else {
            val node = Node(endOfNum = false, new Array[Node](2))
            if (binNum(index) == '0') {
              currNode.nodes(0) = node
            } else {
              currNode.nodes(1) = node
            }
            initialize(index + 1, node)
          }
        }
      }

      def find(root: Node, index: Int, max: Int): Int = {
        if (index == nums.length) {
          max
        } else {

        }
      }

      val root = initializeRoot(nums.head)
      find(root, 1, 0)

    }
  }*/

}
