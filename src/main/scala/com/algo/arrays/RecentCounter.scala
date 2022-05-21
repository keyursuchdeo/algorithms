package com.algo.arrays

object RecentCounter extends App {
  class RecentCounter() {
    var window: Vector[Int] = Vector[Int]()
    def ping(t: Int): Int = {
      window = window :+ t
      window = window.dropWhile(_ < t - 3000)
      window.size
    }
  }
}
