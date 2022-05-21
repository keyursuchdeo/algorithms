package com.algo.dp

object GetMaximumInGeneratedArray extends App {

  object Solution {
    def getMaximumGenerated(n: Int): Int = {
      val array = new Array[Int](n + 1)

      @scala.annotation.tailrec
      def fillArray(index: Int): Unit = {
        if(index == array.length) {
          ()
        } else {
          if(index <= 1) {
            array(index) = index
            fillArray(index + 1)
          } else {
            if(index % 2 == 0) {
              array(index) = array(index / 2)
              fillArray(index + 1)
            } else {
              array(index) = array(index / 2) + array((index / 2) + 1)
              fillArray(index + 1)
            }
          }
        }
      }

      fillArray(0)
      array.max
    }
  }

}
