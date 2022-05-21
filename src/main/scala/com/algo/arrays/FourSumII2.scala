package com.algo.arrays

object FourSumII2 extends App {

  object Solution {
    def fourSumCount(A: Array[Int], B: Array[Int], C: Array[Int], D: Array[Int]): Int = {
      def sumElements(array1: Array[Int], array2: Array[Int]): Array[Int] = {
        array1.flatMap(a1 => array2.map(_ + a1))
      }

      val aPlusB = sumElements(A, B)
      val cPlusD = sumElements(C, D)

      @scala.annotation.tailrec
      def mapElements(index: Int = 0, aPlusBMap: Map[Int, Int] = Map(), cPlusDMap: Map[Int, Int] = Map()): (Map[Int, Int], Map[Int, Int]) = {
        if(index == aPlusB.length) {
          (aPlusBMap, cPlusDMap)
        } else {
          mapElements(
            index + 1,
            aPlusBMap + (aPlusB(index) -> (aPlusBMap.getOrElse(aPlusB(index), 0) + 1)),
            cPlusDMap + (cPlusD(index) -> (cPlusDMap.getOrElse(cPlusD(index), 0) + 1))
          )
        }
      }

      @scala.annotation.tailrec
      def count(map1: Map[Int, Int], map2: Map[Int, Int], total: Int): Int = {
        if(map1.isEmpty) {
          total
        } else {
          val (value, count1) = map1.head
          map2.get(-value) match {
            case Some(count2) =>
              count(map1.tail, map2, total + count1 * count2)
            case _ =>
              count(map1.tail, map2, total)
          }
        }
      }

      val (map1, map2) = mapElements()
      count(map1, map2, 0)
    }
  }

}
