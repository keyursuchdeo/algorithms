package com.algo.arrays

object BeautifulArrangement extends App {

  println(Solution.countArrangement(6))

  object Solution {
    def countArrangement(n: Int): Int = {

      def buildArrangements(currN: Int): Array[Array[Int]] = {
        if(currN == 1) {
          Array(Array(1))
        } else if (currN == 2) {
          Array(Array(1, 2), Array(2, 1))
        } else {
          val newArrangements = buildArrangements(currN - 1).map(arr => arr :+ currN)
          val output =
            newArrangements.flatMap(arr => {
              arr.zipWithIndex.collect{
                case (element, index) if index < (currN-1) &&
                  (((index + 1) % currN == 0 || currN % (index + 1) == 0) &&
                    (currN % element == 0 || element % currN == 0)) =>
                  val arrayCopy = new Array[Int](arr.length)
                  Array.copy(arr, 0, arrayCopy, 0, arr.length)
                  val temp = arr(index)
                  arrayCopy(index) = currN
                  arrayCopy(currN - 1) = temp
                  arrayCopy
              }
            })
          newArrangements ++ output
        }
      }

      val o = buildArrangements(n)
      println(o.map(_.mkString(",")).mkString("|"))
      o.length
    }
  }
}
