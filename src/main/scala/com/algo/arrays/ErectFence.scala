package com.algo.arrays

object ErectFence extends App {

  val trees = Array(Array(3,7),Array(6,8),Array(7,8),Array(11,10),Array(4,3),Array(8,5),Array(7,13),Array(4,13))
  val res = Solution.outerTrees(trees)
  println(res.map(_.mkString(",")).mkString("|"))

  object Solution {
    def outerTrees(trees: Array[Array[Int]]): Array[Array[Int]] = {
      var west = trees(0)
      var south = trees(0)
      var east = trees(0)
      var north = trees(0)

      @scala.annotation.tailrec
      def cornerTrees(index: Int): Unit = {
        if (index == trees.length) {
          ()
        } else {
          val Array(x, y) = trees(index)
          if (x < west(0)) {
            west = trees(index)
          }
          if (y < south(1)) {
            south = trees(index)
          }
          if (x > east(0)) {
            east = trees(index)
          }
          if (y > north(1)) {
            north = trees(index)
          }

          cornerTrees(index + 1)
        }
      }

      def equation(point1: Array[Int], point2: Array[Int]): Array[Double] = {
        val Array(x1, y1) = point1
        val Array(x2, y2) = point2
        val slope: Double = (y2-y1).toDouble / (x2 - x1)
        val constant = y1 - (slope * x1)
        Array(slope, constant)
      }

      def checkIfTreeFallsOnPerimeter(tree: Array[Int], equation: Array[Double]): Boolean = {
        val Array(x, y) = tree
        val Array(m, c) = equation
        println(s"tree: ${tree.mkString(",")}, eq: ${equation.mkString(",")}, y: $y, ${m*x + c}")
        y.toDouble == (m*x + c)
      }

      @scala.annotation.tailrec
      def perimeterTrees(index: Int, equations: Array[Array[Double]], pTrees: Seq[Array[Int]]): Seq[Array[Int]] = {
        if(index == trees.length) {
          pTrees
        } else {
          equations.find(eq => {
            checkIfTreeFallsOnPerimeter(trees(index), eq)
          }) match {
            case Some(_) =>
              perimeterTrees(index + 1, equations, trees(index) +: pTrees)
            case None =>
              perimeterTrees(index + 1, equations, pTrees)
          }
        }
      }

      cornerTrees(0)

      println(west.mkString(","))
      println(south.mkString(","))
      println(east.mkString(","))
      println(north.mkString(","))

      val eq1: Array[Double] = equation(west, south)
      val eq2 = equation(south, east)
      val eq3 = equation(east, north)
      val eq4 = equation(north, west)

      println(eq1.mkString(","))
      println(eq2.mkString(","))
      println(eq3.mkString(","))
      println(eq4.mkString(","))

      val pTrees = perimeterTrees(0, Array(eq1, eq2, eq3, eq4), Nil)

      println(pTrees.map(_.mkString(",")))

      pTrees.toArray
    }
  }
}
