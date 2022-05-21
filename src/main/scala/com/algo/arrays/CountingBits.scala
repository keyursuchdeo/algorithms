package com.algo.arrays

object CountingBits extends App {

  val n = 8
  val res = Solution.countBits(n)
  println(res.mkString(","))

  object Solution {
    def countBits(num: Int): Array[Int] = {
      val array = new Array[Int](num + 1)
      @scala.annotation.tailrec
      def count(index: Int, nearestPowTwo: Int): Unit = {
        if(index <= num && index <= 1) {
          array(index) = index
          count(index + 1, nearestPowTwo)
        } else if (index > num){
          ()
        } else {
          val logToTheBase2: Double = Math.log(index) / Math.log(2)
          if(logToTheBase2 == logToTheBase2.toInt) {
            array(index) = 1
            count(index + 1, index)
          } else {
            array(index) = 1 + array(index - nearestPowTwo)
            count(index + 1, nearestPowTwo)
          }
        }
      }
      count(0, 0)
      array
    }
  }
}
