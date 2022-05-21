package com.algo.arrays

object LargestComponentSizeCommonFactor2 extends App {

//  val a = Array(2, 3, 6, 7, 4, 12, 21, 39)
//    val a = Array(20,50,9,63)
    val a = Array(4, 6, 15, 35)
  val res = Solution.largestComponentSize(a)
  println(res)

  object Solution {
    def largestComponentSize(A: Array[Int]): Int = {

      @scala.annotation.tailrec
      def findPrimeFactors(num: Int, set: Set[Int] = Set()): Set[Int] = {
        if (num == 1) {
          set
        } else {
          (2 to Math.ceil(Math.sqrt(num)).toInt).find(factor => {
            num % factor == 0
          }) match {
            case Some(factor) => findPrimeFactors(num / factor, set + factor)
            case None => set + num
          }
        }
      }

      val primeFactors: Array[Int] = A.map(findPrimeFactors(_)).toSet.flatten.toArray
      val primeNumIndexMapping = Array.fill[Set[Int]](primeFactors.length)(Set())


      @scala.annotation.tailrec
      def mapNumsToPrimes(primeIndex: Int, index: Int): Unit = {
        if (primeIndex == primeFactors.length) {
          ()
        } else if (index == A.length) {
          mapNumsToPrimes(primeIndex + 1, 0)
        } else {
          if (A(index) % primeFactors(primeIndex) == 0) {
            primeNumIndexMapping(primeIndex) = primeNumIndexMapping(primeIndex) + index
            mapNumsToPrimes(primeIndex, index + 1)
          } else {
            mapNumsToPrimes(primeIndex, index + 1)
          }
        }
      }

      def commonElementsInSets(set1: Set[Int], set2: Set[Int]): Boolean = {
        if(set1.size < set2.size) {
          set1.exists(set2.contains)
        } else {
          set2.exists(set1.contains)
        }
      }


      @scala.annotation.tailrec
      def mergeSets(index: Int, innerIndex: Int): Unit = {
        if (index == primeNumIndexMapping.length) {
          ()
        } else if (innerIndex == primeNumIndexMapping.length) {
          mergeSets(index + 1, index + 2)
        } else {
          if (commonElementsInSets(primeNumIndexMapping(index), primeNumIndexMapping(innerIndex))) {
            primeNumIndexMapping(innerIndex) = primeNumIndexMapping(index) ++ primeNumIndexMapping(innerIndex)
            primeNumIndexMapping(index) = Set()
            mergeSets(index + 1, index + 2)
          } else {
            mergeSets(index, innerIndex + 1)
          }
        }
      }

      println(primeFactors.mkString(","))
      mapNumsToPrimes(0, 0)
      println(primeNumIndexMapping.mkString(","))
      mergeSets(0, 1)
      println(primeNumIndexMapping.mkString(","))
      primeNumIndexMapping.map(_.size).max
    }
  }

}
