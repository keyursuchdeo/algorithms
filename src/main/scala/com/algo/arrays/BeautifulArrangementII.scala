package com.algo.arrays

object BeautifulArrangementII extends App {
  object Solution {
    def constructArray(n: Int, k: Int): Array[Int] = {
      val output = new Array[Int](n)

      @scala.annotation.tailrec
      def construct(index: Int = 2, remainingK: Int): Unit = {
        if(remainingK == 0) {
          ()
        } else {
          if(index % 2 == 1) {
            output(index) = output(index - 2) + 1
          } else {
            output(index) = output(index - 1) - 1
          }
          construct(index + 1, remainingK - 1)
        }
      }

      output(0) = 1
      output(1) = output(0) + k
      construct(remainingK = k - 1)
      (output(1) + 1 to n).foreach(num => {
        output(num - 1) = num
      })
      output
    }
  }
}
