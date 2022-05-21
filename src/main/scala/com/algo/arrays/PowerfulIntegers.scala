package com.algo.arrays

object PowerfulIntegers extends App {
  object Solution {
    def powerfulIntegers(x: Int, y: Int, bound: Int): List[Int] = {
      val a = if(x == 1) x else Math.ceil(Math.log(bound)/Math.log(x)).toInt
      val b = if(y == 1) y else Math.ceil(Math.log(bound)/Math.log(y)).toInt

      @scala.annotation.tailrec
      def find(powX: Int, powY: Int, output: Set[Int]): List[Int] = {
        if(powX > a) {
          output.toList
        } else if (powY > b) {
          if(x == 1) {
            output.toList
          } else {
            find(powX + 1, 0, output)
          }
        } else {
          val value = (Math.pow(x, powX) + Math.pow(y, powY)).toInt
          val updatedOutput =
          if(value <= bound) {
            output + value
          } else {
            output
          }
          if(y == 1 && x == 1) {
            updatedOutput.toList
          } else if (y == 1) {
            find(powX + 1, 0, updatedOutput)
          } else {
            find(powX, powY + 1, updatedOutput)
          }
        }
      }

      find(a, b, Set())
    }
  }
}
