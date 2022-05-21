package com.algo.arrays

object MoveZeros extends App {

//  val a = Array(0,1,0,3,12)
//  val a = Array(1, 3, 12)
  val a = Array(0, 0, 0, 1)
  Solution.moveZeroes(a)
  println(a.mkString(","))

  object Solution {
    def moveZeroes(nums: Array[Int]): Unit = {

      @scala.annotation.tailrec
      def move(index: Int, lastNonZeroIndex: Option[Int]): Unit = {
        if (index == nums.length) {
          ()
        } else {
          lastNonZeroIndex match {
            case Some(nonZeroIndex) =>
              if(nums(index) != 0) {
                val temp = nums(nonZeroIndex + 1)
                nums(nonZeroIndex + 1) = nums(index)
                nums(index) = temp
                move(index + 1, Option(nonZeroIndex + 1))
              } else {
                move(index + 1, lastNonZeroIndex)
              }
            case None =>
              if(nums(index) != 0) {
                if(index == 0) {
                  move(index + 1, Option(index))
                } else {
                  nums(0) = nums(index)
                  nums(index) = 0
                  move(index + 1, Option(0))
                }
              } else {
                move(index + 1, lastNonZeroIndex)
              }

          }
        }
      }

      move(0, None)
    }
  }
}
