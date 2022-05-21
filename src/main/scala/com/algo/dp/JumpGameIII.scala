package com.algo.dp

object JumpGameIII extends App {
  object Solution {
    def canReach(arr: Array[Int], start: Int): Boolean = {
      val visited = new Array[Boolean](arr.length)
      val reachableFrom = Array.fill[Option[Boolean]](arr.length)(None)

      def check(fromIndex: Int): Boolean = {
        if(fromIndex < 0 || fromIndex >= arr.length) {
          false
        } else if(arr(fromIndex) == 0) {
          true
        } else if (visited(fromIndex)) {
          reachableFrom(fromIndex).contains(true)
        } else {
          visited(fromIndex) = true
          val result = check(fromIndex + arr(fromIndex)) || check(fromIndex - arr(fromIndex))
          reachableFrom(fromIndex) = Option(result)
          result
        }
      }
      check(start)
    }
  }
}
