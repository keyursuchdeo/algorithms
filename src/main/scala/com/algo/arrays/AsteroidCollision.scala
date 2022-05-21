package com.algo.arrays

object AsteroidCollision extends App {
  object Solution {
    def asteroidCollision(asteroids: Array[Int]): Array[Int] = {

      @scala.annotation.tailrec
      def calculate(index: Int, positiveAsteroids: Seq[Int], negativeAsteroids: Seq[Int]): Seq[Int] = {
        if(index == asteroids.length) {
          negativeAsteroids.reverse ++ positiveAsteroids.reverse
        } else {
          if(asteroids(index) > 0) {
            calculate(index + 1, asteroids(index) +: positiveAsteroids, negativeAsteroids)
          } else {
            if(positiveAsteroids.isEmpty) {
              calculate(index + 1, positiveAsteroids, asteroids(index) +: negativeAsteroids)
            } else {
              if(-asteroids(index) == positiveAsteroids.head) {
                calculate(index + 1, positiveAsteroids.tail, negativeAsteroids)
              } else if (-asteroids(index) < positiveAsteroids.head) {
                calculate(index + 1, positiveAsteroids, negativeAsteroids)
              } else {
                calculate(index, positiveAsteroids.tail, negativeAsteroids)
              }
            }
          }
        }
      }

      calculate(0, Nil, Nil).toArray
    }
  }
}
