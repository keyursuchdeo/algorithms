package com.algo.tree

object CloneGraph extends App {

  val one = new Node(1)
  val two = new Node(2)
  val three = new Node(3)
  val four = new Node(4)

  one.neighbors = List(two, four)
  two.neighbors = List(one, three)
  three.neighbors = List(two, four)
  four.neighbors = List(one, three)

  val res = Solution.cloneGraph(one)
  println(res)

  class Node(var _value: Int) {
    var value: Int = _value
    var neighbors: List[Node] = List()
  }


  object Solution {

    def cloneGraph(graph: Node): Node = {
      var visitedNodes: Map[Int, Node] = Map[Int, Node]()
      def clone(node: Node): Node = {
        visitedNodes.get(node.value) match {
          case Some(visitedNode) => visitedNode
          case _ =>
            val clonedNode = new Node(node.value)
            visitedNodes = visitedNodes + (node.value -> clonedNode)
            clonedNode.neighbors = node.neighbors.map(clone)
            clonedNode
        }
      }
      if(graph == null) graph else clone(graph)
    }
  }

}
