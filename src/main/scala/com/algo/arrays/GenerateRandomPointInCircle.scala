package com.algo.arrays

import scala.util.Random

object GenerateRandomPointInCircle extends App {
  class Solution(_radius: Double, _x_center: Double, _y_center: Double) {

    def randPoint(): Array[Double] = {
      val angle = Math.random() * 2 * Math.PI
      val hyp = Math.sqrt(Math.random()) * _radius
      val adj = Math.cos(angle) * hyp
      val opp = Math.sin(angle) * hyp

      Array(_x_center + adj, _y_center + opp)
    }

  }
}
