package com.algo.dp

object HouseRobberNotCircular extends App {
  //  val a = Array(1, 3, 1, 3, 100)
  //  val a = Array(2, 3, 2)
  //    val a = Array(1, 2, 3, 1)
  val a = Array(2, 7, 9, 3, 1)
  //  val a = Array(1,1,3,6,7,10,7,1,8,5,9,1,4,4,3)
  val res = Solution.rob(a)
  println(res)

  object Solution {
    def rob(nums: Array[Int]): Int = {
      val robbedMoney = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def calculatedRobbedMoney(index: Int): Unit = {
        if (index == nums.length) {
          ()
        } else if (index <= 1) {
          robbedMoney(index) = nums(index)
          calculatedRobbedMoney(index + 1)
        } else {
          if (index == 2) {
            robbedMoney(index) = nums(index) + robbedMoney(index - 2)
            calculatedRobbedMoney(index + 1)
          } else {
            robbedMoney(index) = nums(index) + Math.max(robbedMoney(index - 2), robbedMoney(index - 3))
            calculatedRobbedMoney(index + 1)
          }

        }
      }

      if (nums.isEmpty) 0 else {
        calculatedRobbedMoney(0)
        println(robbedMoney.mkString(","))
        robbedMoney.max
      }
    }
  }

}
