package com.algo.dp

object UglyNumberII2 extends App {

  val res = Solution.nthUglyNumber(10)
  println(res)

  object Solution {
    def nthUglyNumber(n: Int): Int = {
      val uglyNums = new Array[Int](n + 1)
      @scala.annotation.tailrec
      def fill(index: Int, index2: Int, index3: Int, index5: Int): Unit = {
        if(index == n + 1) {
          ()
        } else if (index == 1) {
          uglyNums(index) = 1
          fill(index + 1, index2, index3, index5)
        } else {
          val ugly2 = 2 * uglyNums(index2)
          val ugly3 = 3 * uglyNums(index3)
          val ugly5 = 5 * uglyNums(index5)
          if(ugly2 < ugly3 && ugly2 < ugly5) {
            uglyNums(index) = ugly2
            fill(index + 1, index2 + 1, index3, index5)
          } else if (ugly3 < ugly2 && ugly3 < ugly5) {
            uglyNums(index) = ugly3
            fill(index + 1, index2, index3 + 1, index5)
          } else if (ugly5 < ugly2 && ugly5 < ugly3){
            uglyNums(index) = ugly5
            fill(index + 1, index2, index3, index5 + 1)
          } else if (ugly2 == ugly3 && ugly3 == ugly5) {
            uglyNums(index) = ugly2
            fill(index + 1, index2 + 1, index3 + 1, index5 + 1)
          } else if (ugly2 == ugly3) {
            uglyNums(index) = ugly2
            fill(index + 1, index2 + 1, index3 + 1, index5)
          } else if (ugly2 == ugly5) {
            uglyNums(index) = ugly2
            fill(index + 1, index2 + 1, index3, index5 + 1)
          } else {
            uglyNums(index) = ugly3
            fill(index + 1, index2, index3 + 1, index5 + 1)
          }
        }
      }

      fill(1, 1, 1, 1)
      println(uglyNums.mkString(","))
      uglyNums(n)
    }
  }
}
