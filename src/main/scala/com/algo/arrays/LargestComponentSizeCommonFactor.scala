package com.algo.arrays

object LargestComponentSizeCommonFactor extends App {

//  val a = Array(2,3,6,7,4,12,21,39)
  val a = Array(20,50,9,63)
//  val a = Array(4, 6, 15, 35)
  val res = Solution.largestComponentSize(a)
  println(res)

  object Solution {
    def largestComponentSize(A: Array[Int]): Int = {

      @scala.annotation.tailrec
      def findCommonFactor(small: Int, large: Int): Int = {
        val modulo = large % small
        if (modulo == 0) {
          small
        } else {
          findCommonFactor(modulo, small)
        }
      }

      def isCommonFactorGreaterThanOne(num1: Int, num2: Int) = {
        val commonFactor =
          if (num1 < num2) {
            findCommonFactor(num1, num2)
          } else {
            findCommonFactor(num2, num1)
          }
        commonFactor > 1
      }

      var elements = A.toSet
      val adjList = Array.fill[Seq[Int]](A.length)(Seq())

      @scala.annotation.tailrec
      def fillAdjList(rootIndex: Int, index: Int): Unit = {
        if(rootIndex >= A.length) {
          ()
        } else if(index == A.length || A(rootIndex) == 1) {
          fillAdjList(rootIndex + 1, rootIndex + 2)
        } else if (A(index) > 1){
          if(isCommonFactorGreaterThanOne(A(rootIndex), A(index))) {
            adjList(rootIndex) = index +: adjList(rootIndex)
            adjList(index) = rootIndex +: adjList(index)
            fillAdjList(rootIndex, index + 1)
          } else {
            fillAdjList(rootIndex, index + 1)
          }
        } else {
          fillAdjList(rootIndex, index + 1)
        }
      }

      @scala.annotation.tailrec
      def calculateComponentSize(index: Int, maxSize: Int): Int = {
        if(index == A.length) {
          maxSize
        } else {
          val size = calculate(index)
          calculateComponentSize(index + 1, Math.max(maxSize, size))
        }
      }

      def calculate(index: Int): Int = {
        if(elements.contains(A(index))) {
          elements = elements - A(index)
          1 + adjList(index).map(elementIndex => {
            calculate(elementIndex)
          }).sum
        } else {
          0
        }
      }

      fillAdjList(0, 1)
      println(adjList.mkString(","))
      calculateComponentSize(0, 0)
    }
  }

}
