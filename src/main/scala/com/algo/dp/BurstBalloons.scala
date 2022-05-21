package com.algo.dp

import scala.collection.mutable
import scala.util.Try

object BurstBalloons extends App {

  val n = Array(3, 1, 5, 8)
  val res = Solution.maxCoins(n)
  println(res)

  object Solution {
    def maxCoins(nums: Array[Int]): Int = {

      var map: mutable.Map[String, Int] = mutable.Map[String, Int]()

      def calculate(currArray: Array[Int]): Int = {
        if(currArray.isEmpty) {
          0
        } else {
          val arrString = currArray.mkString(",")
          map.get(arrString) match {
            case Some(value) => value
            case _ =>
              val value = currArray.indices.map(index => {
                val (product, remainingArray) = prepRemainingArrayAndProduct(index, currArray)
                product + calculate(remainingArray)
              }).max
              map = map + (arrString -> value)
              value
          }
        }
      }

      def prepRemainingArrayAndProduct(index: Int, array: Array[Int]): (Int, Array[Int]) = {
        val product =
          Try(array(index - 1)).toOption.getOrElse(1) * array(index) * Try(array(index + 1)).toOption.getOrElse(1)
        val (before, after) = array.splitAt(index)
        (product, before ++ after.tail)
      }

      val b = calculate(nums)
      println(map)
      b
    }
  }
}
