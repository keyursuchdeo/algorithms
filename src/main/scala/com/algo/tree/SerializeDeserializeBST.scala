package com.algo.tree

object SerializeDeserializeBST extends App {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }


  class Codec {
    // Encodes a list of strings to a single string.
    def serialize(root: TreeNode): String = {

      @scala.annotation.tailrec
      def prepSerializedString(nodes: Seq[Option[TreeNode]], seq: Seq[Option[Int]]): String = {
        if (nodes.flatten.isEmpty) {
          seq.toArray.mkString(",")
        } else {
          val allChildNodes: Seq[Option[TreeNode]] =
            nodes.flatMap(optNode => {
              optNode match {
                case Some(node) =>
                  Seq(Option(node.left), Option(node.right))
                case None =>
                  Seq(None, None)
              }
            })
          prepSerializedString(allChildNodes, seq ++ nodes.map(_.map(_.value)))
        }
      }

      prepSerializedString(Seq(Option(root)), Nil)
    }

    // Decodes a single string to a list of strings.
    def deserialize(s: String): TreeNode = {

      @scala.annotation.tailrec
      def setChildNodes(parentNodes: Seq[TreeNode], childNodes: Seq[TreeNode]): Unit = {
        if(parentNodes.isEmpty) {
          ()
        } else {
          val (currParentChildNodes, laterParentChildNodes) = childNodes.splitAt(2)
          if(parentNodes.head == null) {
            setChildNodes(parentNodes.tail, laterParentChildNodes)
          } else {
            parentNodes.head.left = currParentChildNodes.head
            parentNodes.head.right = currParentChildNodes.tail.head
            setChildNodes(parentNodes.tail, laterParentChildNodes)
          }
        }
      }

      @scala.annotation.tailrec
      def prepTreeNode(chars: Seq[Option[Int]], level: Int, previousLevelNodes: Seq[TreeNode]): Unit = {
        if(chars.isEmpty) {
          ()
        } else {
          val (currLevelValues, laterLevelValues) = chars.splitAt(2 << level)
          val currLevelNodes: Seq[TreeNode] = currLevelValues.map {
            case Some(value) => new TreeNode(value)
            case _ => null
          }
          setChildNodes(previousLevelNodes, currLevelNodes)
          prepTreeNode(laterLevelValues, level + 1, currLevelNodes)
        }
      }

      def stringToArray(str: String): Array[Option[Int]] = {
        s.split(",").map(value => {
          if(value == "None") {
            None
          } else {
            Some(value.substring(5, value.indexOf(")")).toInt)
          }
        })
      }

      if(s.isEmpty) {
        null
      } else {
        val chars: Array[Option[Int]] = stringToArray(s)
        chars.head match {
          case Some(value) =>
            val head = new TreeNode(value)
            prepTreeNode(chars.tail, 0, Seq(head))
            head
          case None => null
        }
      }
    }
  }

}
