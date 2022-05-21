package com.algo.dfs

object EvaluateDiv extends App {
  object Solution {
    def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {

      @scala.annotation.tailrec
      def buildGraph(currEquations: List[List[String]], index: Int, map: Map[String, Map[String, Double]]): Map[String, Map[String, Double]] = {
        if(currEquations.isEmpty) {
          map
        } else {
          val List(n1, n2) = currEquations.head
          val n1Map: Map[String, Double] = map.getOrElse(n1, Map())
          val n2Map: Map[String, Double] = map.getOrElse(n2, Map())

          buildGraph(
            currEquations.tail,
            index + 1,
            (map + (n1 -> (n1Map + (n2 -> values(index))))) + (n2 -> (n2Map + (n1 -> 1 / values(index))))
          )
        }
      }

      val graph: Map[String, Map[String, Double]] = buildGraph(equations, 0, Map())

      def evaluate(from: String, to: String): Double = {
        var visited: Set[String] = Set()
        var output: Option[Double] = None
        def dfs(currFrom: String, multiplier: Double): Unit = {
            val neighboursWithWeight = graph(currFrom)
            neighboursWithWeight.foreach(nw => {
              val (n, w) = nw
              if(!visited.contains(n)) {
                if(n == to) {
                  output = Option(multiplier * w)
                } else {
                  visited = visited + n
                  dfs(n, multiplier * w)
                }
              }
            })
        }

        dfs(from, 1)
        output match {
          case None => -1
          case Some(value) => value
        }
      }

      def evaluateQuery(query: List[String]) = {
        val List(n1, n2) = query
        (graph.get(n1), graph.get(n2)) match {
          case (Some(_), Some(_)) if n1 == n2 => 1.0
          case (Some(_), Some(_)) => evaluate(n1, n2)
          case _ => -1.0
        }
      }

      @scala.annotation.tailrec
      def evaluateQueries(currQueries: List[List[String]], result: Seq[Double]): Array[Double] = {
        if(currQueries.isEmpty) {
          result.reverse.toArray
        } else {
          evaluateQueries(currQueries.tail, evaluateQuery(currQueries.head) +: result)
        }
      }

      println(graph)
      evaluateQueries(queries, Nil)

    }
  }
}
