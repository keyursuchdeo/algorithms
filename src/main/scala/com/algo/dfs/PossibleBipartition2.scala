package com.algo.dfs

import scala.collection.mutable

object PossibleBipartition2 extends App {

  //  val n = 4
  //  val dislikes = Array(Array(1, 2), Array(1, 3), Array(2, 4))

  //  val n = 3
  //  val dislikes = Array(Array(1, 2), Array(2, 3), Array(1, 3))

  //  val n = 5
  //  val dislikes = Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5), Array(1, 5))

//  val n = 6
//  val dislikes = Array(Array(1, 2), Array(1, 3), Array(2, 4), Array(4, 5), Array(5, 6), Array(3, 6))

  val n = 50
  val dislikes = Array(Array(21,47),Array(4,41),Array(2,41),Array(36,42),Array(32,45),Array(26,28),Array(32,44),Array(5,41),Array(29,44),Array(10,46),Array(1,6),Array(7,42),Array(46,49),Array(17,46),Array(32,35),Array(11,48),Array(37,48),Array(37,43),Array(8,41),Array(16,22),Array(41,43),Array(11,27),Array(22,44),Array(22,28),Array(18,37),Array(5,11),Array(18,46),Array(22,48),Array(1,17),Array(2,32),Array(21,37),Array(7,22),Array(23,41),Array(30,39),Array(6,41),Array(10,22),Array(36,41),Array(22,25),Array(1,12),Array(2,11),Array(45,46),Array(2,22),Array(1,38),Array(47,50),Array(11,15),Array(2,37),Array(1,43),Array(30,45),Array(4,32),Array(28,37),Array(1,21),Array(23,37),Array(5,37),Array(29,40),Array(6,42),Array(3,11),Array(40,42),Array(26,49),Array(41,50),Array(13,41),Array(20,47),Array(15,26),Array(47,49),Array(5,30),Array(4,42),Array(10,30),Array(6,29),Array(20,42),Array(4,37),Array(28,42),Array(1,16),Array(8,32),Array(16,29),Array(31,47),Array(15,47),Array(1,5),Array(7,37),Array(14,47),Array(30,48),Array(1,10),Array(26,43),Array(15,46),Array(42,45),Array(18,42),Array(25,42),Array(38,41),Array(32,39),Array(6,30),Array(29,33),Array(34,37),Array(26,38),Array(3,22),Array(18,47),Array(42,48),Array(22,49),Array(26,34),Array(22,36),Array(29,36),Array(11,25),Array(41,44),Array(6,46),Array(13,22),Array(11,16),Array(10,37),Array(42,43),Array(12,32),Array(1,48),Array(26,40),Array(22,50),Array(17,26),Array(4,22),Array(11,14),Array(26,39),Array(7,11),Array(23,26),Array(1,20),Array(32,33),Array(30,33),Array(1,25),Array(2,30),Array(2,46),Array(26,45),Array(47,48),Array(5,29),Array(3,37),Array(22,34),Array(20,22),Array(9,47),Array(1,4),Array(36,46),Array(30,49),Array(1,9),Array(3,26),Array(25,41),Array(14,29),Array(1,35),Array(23,42),Array(21,32),Array(24,46),Array(3,32),Array(9,42),Array(33,37),Array(7,30),Array(29,45),Array(27,30),Array(1,7),Array(33,42),Array(17,47),Array(12,47),Array(19,41),Array(3,42),Array(24,26),Array(20,29),Array(11,23),Array(22,40),Array(9,37),Array(31,32),Array(23,46),Array(11,38),Array(27,29),Array(17,37),Array(23,30),Array(14,42),Array(28,30),Array(29,31),Array(1,8),Array(1,36),Array(42,50),Array(21,41),Array(11,18),Array(39,41),Array(32,34),Array(6,37),Array(30,38),Array(21,46),Array(16,37),Array(22,24),Array(17,32),Array(23,29),Array(3,30),Array(8,30),Array(41,48),Array(1,39),Array(8,47),Array(30,44),Array(9,46),Array(22,45),Array(7,26),Array(35,42),Array(1,27),Array(17,30),Array(20,46),Array(18,29),Array(3,29),Array(4,30),Array(3,46))
  val res = Solution.possibleBipartition(n, dislikes)
  println(res)

  object Solution {
    def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {
      lazy val adjList: Array[Seq[Int]] = Array.fill[Seq[Int]](N + 1)(Nil)
      var verticesColour = mutable.Map[Int, Int]()

      @scala.annotation.tailrec
      def prepAdjList(index: Int): Unit = {
        if (index == dislikes.length) {
          ()
        } else {
          adjList(dislikes(index)(0)) = dislikes(index)(1) +: adjList(dislikes(index)(0))
          adjList(dislikes(index)(1)) = dislikes(index)(0) +: adjList(dislikes(index)(1))
          prepAdjList(index + 1)
        }
      }

      @scala.annotation.tailrec
      def find(index: Int): Boolean = {
        if (index == N + 1) {
          true
        } else {
          val vertex = index
          val neighbours = adjList(index)
          val isColourApplied: Boolean =
            verticesColour.get(vertex) match {
              case Some(colour) if colour == 0 => colourNeighbours(neighbours, 1)
              case Some(_) => colourNeighbours(neighbours, 0)
              case None =>
                verticesColour = verticesColour + (vertex -> 0)
                colourNeighbours(neighbours, 1)
            }
          println(verticesColour)
          if(isColourApplied) find(index + 1) else false
        }
      }

      def colourNeighbours(neighbours: Seq[Int], newColour: Int): Boolean = {
        val numOfNeighbours = neighbours.size

        @scala.annotation.tailrec
        def colourVertex(index: Int): Boolean = {
          if (index == numOfNeighbours) {
            true
          } else {
            verticesColour.get(neighbours(index)) match {
              case None =>
                verticesColour = verticesColour + (neighbours(index) -> newColour)
                colourVertex(index + 1)
              case Some(currColour) if currColour == newColour =>
                colourVertex(index + 1)
              case Some(_) =>
                println(neighbours(index))
                println(newColour)
                false

            }
          }
        }

        colourVertex(0)
      }

      if(dislikes.isEmpty) {
        true
      } else {
        prepAdjList(0)
        println(adjList.mkString(","))
        find(1)
      }
    }
  }

}
