package com.algo.dp

object HouseRobberCircular extends App {
//    val a = Array(1, 3, 1, 3, 100)
  //  val a = Array(2, 3, 2)
      val a = Array(1, 2, 3, 1)
//  val a = Array(2, 7, 9, 3, 1)
  //  val a = Array(1,1,3,6,7,10,7,1,8,5,9,1,4,4,3)
  val res = Solution.rob(a)
  println(res)

  object Solution {
    def rob(nums: Array[Int]): Int = {
      val robbedMoneyWithFirstHouse = new Array[Int](nums.length)
      val robbedMoneyWithoutFirstHouse = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def calculatedRobbedMoney(index: Int): Unit = {
        if (index == nums.length) {
          ()
        } else if (index <= 1) {
          robbedMoneyWithFirstHouse(index) = nums(index)
          robbedMoneyWithoutFirstHouse(index) = nums(index)
          calculatedRobbedMoney(index + 1)
        } else {
          if (index == 2) {
            robbedMoneyWithFirstHouse(index) = nums(index) + robbedMoneyWithFirstHouse(index - 2)
            robbedMoneyWithoutFirstHouse(index) = nums(index)
            calculatedRobbedMoney(index + 1)
          } else if (index == 3) {
            robbedMoneyWithFirstHouse(index) = nums(index) + Math.max(robbedMoneyWithFirstHouse(index - 2), robbedMoneyWithFirstHouse(index - 3))
            robbedMoneyWithoutFirstHouse(index) = nums(index) + robbedMoneyWithoutFirstHouse(index - 2)
            calculatedRobbedMoney(index + 1)
          } else {
            robbedMoneyWithFirstHouse(index) = nums(index) + Math.max(robbedMoneyWithFirstHouse(index - 2), robbedMoneyWithFirstHouse(index - 3))
            robbedMoneyWithoutFirstHouse(index) = nums(index) + Math.max(robbedMoneyWithoutFirstHouse(index - 2), robbedMoneyWithoutFirstHouse(index - 3))
            calculatedRobbedMoney(index + 1)
          }

        }
      }

      if (nums.isEmpty) 0 else if (nums.length == 1) {
        nums.head
      } else {
        calculatedRobbedMoney(0)
        println(robbedMoneyWithFirstHouse.mkString(","))
        println(robbedMoneyWithoutFirstHouse.mkString(","))
        val maxWithFirstHouse = robbedMoneyWithFirstHouse.reverse.tail.max
        val amtIfLastHouseIsRobbed = robbedMoneyWithoutFirstHouse.last
        Math.max(maxWithFirstHouse, amtIfLastHouseIsRobbed)
      }
    }
  }

}
