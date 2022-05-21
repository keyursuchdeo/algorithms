package com.algo.arrays

object EvaluateDivision extends App {
  val eq = List(List("a", "b"), List("b", "c"))
  val v = Array(2.0, 3.0)
  val q = List(List("a", "c"), List("b", "a"), List("a", "a"), List("a", "e"), List("x", "x"))

  val res = Solution.calcEquation(eq, v, q)
  println(res.mkString(","))

  object Solution {
    def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {
      val adjMatrix = Array.fill[Double](26, 26)(-1)
      def adjList = Array.fill[Seq[Char]](26)(Nil)

      def charIndex(char: Char): Int = char - 'a'

      def eqVariables(v1: String, v2: String): (Char, Char) = {
        val v1Set = v1.toSet
        val v2Set = v2.toSet
        val uniqueVariables: Set[Char] = v1Set -- v2Set
        (v1Set.intersect(uniqueVariables).head, v2Set.intersect(uniqueVariables).head)
      }

      @scala.annotation.tailrec
      def fillAdjMatrix(currEquations: List[List[String]], index: Int): Unit = {
        if(currEquations.isEmpty) {
          ()
        } else {
          val List(rawV1, rawV2) = currEquations.head
          val (v1, v2) = eqVariables(rawV1, rawV2)
          val v1CharIndex = charIndex(v1)
          val v2CharIndex = charIndex(v2)
          adjMatrix(v1CharIndex)(v1CharIndex) = 1
          adjMatrix(v2CharIndex)(v2CharIndex) = 1
          adjMatrix(v1CharIndex)(v2CharIndex) = values(index)
          adjMatrix(v2CharIndex)(v1CharIndex) = 1 / values(index)
          fillAdjMatrixDerivedValues(v1, v2, adjList(v1CharIndex), values(index))
          adjList(v1CharIndex) = v2 +: adjList(v1CharIndex)
          adjList(v2CharIndex) = v1 +: adjList(v2CharIndex)
          fillAdjMatrix(currEquations.tail, index + 1)
        }
      }

      def fillAdjMatrixDerivedValues(charN: Char, charD: Char, neighbours: Seq[Char], value: Double): Unit = {
        val cDIndex = charIndex(charD)
        val cNIndex = charIndex(charN)
        neighbours.foreach(n => {
          val nIndex = charIndex(n)
          if(adjMatrix(nIndex)(cDIndex) == -1) {
            adjMatrix(nIndex)(cDIndex) = value * adjMatrix(nIndex)(cNIndex)
            adjMatrix(cDIndex)(nIndex) = 1 / adjMatrix(nIndex)(cDIndex)
          }
        })
      }

      @scala.annotation.tailrec
      def evaluateQueries(index: Int, results: List[Double]): Array[Double] = {
        if(index == queries.length) {
          results.reverse.toArray
        } else {
          val List(rawV1, rawV2) = queries(index)
          val (v1, v2) = eqVariables(rawV1, rawV2)
          val v1CharIndex = charIndex(v1)
          val v2CharIndex = charIndex(v2)
          evaluateQueries(index + 1, adjMatrix(v1CharIndex)(v2CharIndex) +: results)
        }
      }

      evaluateQueries(0, Nil)
    }
  }
}
