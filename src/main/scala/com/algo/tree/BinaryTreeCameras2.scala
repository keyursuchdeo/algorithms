package com.algo.tree

object BinaryTreeCameras2 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def minCameraCover(root: TreeNode): Int = {

      var map: Map[(TreeNode, Boolean, Boolean), Long] = Map[(TreeNode, Boolean, Boolean), Long]()

      def isLeafNode(currNode: TreeNode): Boolean = {
        currNode != null && currNode.left == null && currNode.right == null
      }

      def find(currNode: TreeNode, camera: Boolean, parentCamera: Boolean): Long = {
        if(currNode == null) {
          if(camera) Int.MaxValue else 0
        } else if (isLeafNode(currNode)) {
          if(camera) 1 else if(parentCamera) 0 else Int.MaxValue
        } else {
          map.get((currNode, camera, parentCamera)) match {
            case Some(value) => value
            case _ =>
              val value =
                if (camera) {
                  1 +
                    Math.min(
                      find(currNode.left, camera = true, parentCamera = true),
                      find(currNode.left, camera = false, parentCamera = true)
                    ) +
                    Math.min(
                      find(currNode.right, camera = true, parentCamera = true),
                      find(currNode.right, camera = false, parentCamera = true)
                    )
                } else {
                  if(parentCamera) {
                    Math.min(
                      find(currNode.left, camera = true, parentCamera = false),
                      find(currNode.left, camera = false, parentCamera = false)
                    ) +
                      Math.min(
                        find(currNode.right, camera = true, parentCamera = false),
                        find(currNode.right, camera = false, parentCamera = false)
                      )
                  } else {
                    val op1 =
                      find(currNode.left, camera = true, parentCamera = false) +
                        Math.min(
                          find(currNode.right, camera = true, parentCamera = false),
                          find(currNode.right, camera = false, parentCamera = false)
                        )
                    val op2 =
                      find(currNode.right, camera = true, parentCamera = false) +
                        Math.min(
                          find(currNode.left, camera = true, parentCamera = false),
                          find(currNode.left, camera = false, parentCamera = false)
                        )
                    Math.min(op1, op2)
                  }
                }
              map = map + ((currNode, camera, parentCamera) -> value)
              value
          }
        }
      }

      Math.min(
        find(root, camera =  true, parentCamera = false),
        find(root, camera =  false, parentCamera = false)
      ).toInt
    }
  }

}
