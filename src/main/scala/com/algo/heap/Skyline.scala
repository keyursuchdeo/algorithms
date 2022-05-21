package com.algo.heap

import scala.collection.immutable.TreeMap


object Skyline extends App {

//    val input = Array(Array(0, 2, 3), Array(2, 5, 3))
  val input = Array(Array(2,9,10),Array(3,7,15),Array(5,12,12),Array(15,20,10), Array(19,24,8))
//  val input = Array(Array(1, 3, 3), Array(2, 4, 4), Array(5, 8, 2), Array(6, 7, 4), Array(8, 9, 4))
//  val input = Array(Array(0, 1, 2), Array(0, 2, 3))
//  val input = Array(Array(3, 5, 3), Array(4, 5, 2))
//  val input = Array(Array(6, 7, 2), Array(7, 8, 3))
  val res = Solution.getSkyline(input)
  println(res)

  object Solution {
    def getSkyline(buildings: Array[Array[Int]]): List[List[Int]] = {
      object BuildingPointOrder extends Ordering[(Int, Int, Boolean)] {
        override def compare(x: (Int, Int, Boolean), y: (Int, Int, Boolean)): Int = {
          val (xs, xh, xIsStart) = x
          val (ys, yh, yIsStart) = y

          if(xs == ys && xIsStart && xIsStart == yIsStart) {
            yh compare xh
          } else if (xs == ys && !xIsStart && xIsStart == yIsStart) {
            xh compare yh
          } else if (xs == ys && xIsStart != yIsStart) {
            yh compare xh
          } else {
            xs compare ys
          }
        }
      }

      @scala.annotation.tailrec
      def prepBuildingStartEndPoints(index: Int = 0, output: Seq[(Int, Int, Boolean)] = Nil): Seq[(Int, Int, Boolean)] = {
        if(index == buildings.length) {
          output
        } else {
          if(buildings(index).isEmpty) {
            prepBuildingStartEndPoints(index + 1, output)
          } else {
            val Array(l, r, h) = buildings(index)
            prepBuildingStartEndPoints(index + 1, (r, h, false) +: ((l, h, true) +: output))
          }
        }
      }

      val sortedStartEndPoints = prepBuildingStartEndPoints().sorted(BuildingPointOrder)

      @scala.annotation.tailrec
      def buildSkyline(points: Seq[(Int, Int, Boolean)], skyline: List[List[Int]], maxHeight: Int, heightQueue: TreeMap[Int, Int]): List[List[Int]] = {
        println(heightQueue)
        if(points.isEmpty) {
          skyline.reverse
        } else {
          val (pos, height, isStart) = points.head
          if (isStart) {
            if(height > maxHeight) {
              buildSkyline(
                points.tail,
                List(pos, height) +: skyline,
                height,
                heightQueue + (height -> (1 + heightQueue.getOrElse(height, 0)))
              )
            } else {
              buildSkyline(points.tail, skyline, maxHeight,
                heightQueue + (height -> (1 + heightQueue.getOrElse(height, 0))))
            }
          } else {
            heightQueue.get(height) match {
              case Some(1) if height == maxHeight =>
                val updatedMap = heightQueue - height
                buildSkyline(
                  points.tail,
                  List(pos, updatedMap.lastKey) +: skyline,
                  updatedMap.lastKey,
                  updatedMap
                )
              case Some(1) =>
                buildSkyline(
                  points.tail,
                  skyline,
                  maxHeight,
                  heightQueue - height
                )
              case Some(value) =>
                buildSkyline(
                  points.tail,
                  skyline,
                  maxHeight,
                  heightQueue + (height -> (value - 1))
                )
              case _ =>
                buildSkyline(
                  points.tail,
                  skyline,
                  maxHeight,
                  heightQueue - height
                )
            }
          }
        }
      }

      println(sortedStartEndPoints)
      buildSkyline(sortedStartEndPoints, Nil, 0, TreeMap(0 -> 1))
    }
  }
}
