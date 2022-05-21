package com.algo.dp

object HouseRobber2 extends App {
  //  val a = Array(1, 3, 1, 3, 100)
  //  val a = Array(2, 3, 2)
  //  val a = Array(1, 2, 3, 1)
  val a = Array(1,1,3,6,7,10,7,1,8,5,9,1,4,4,3)
  val res = Solution.rob(a)
  println(res)

  object Solution {
    def rob(nums: Array[Int]): Int = {
      val robbedMoney = new Array[(Int, Boolean)](nums.length)

      @scala.annotation.tailrec
      def calculatedRobbedMoney(index: Int): Unit = {
        if (index == nums.length) {
          ()
        } else if (index == 0) {
          robbedMoney(index) = (nums(index), true)
          calculatedRobbedMoney(index + 1)
        } else if (index == 1) {
          robbedMoney(index) = (nums(index), false)
          calculatedRobbedMoney(index + 1)
        } else {
          if (index != nums.length - 1) {
            if (index == 2) {
              robbedMoney(index) = (nums(index) + robbedMoney(index - 2)._1, true)
              calculatedRobbedMoney(index + 1)
            } else {
              val (prevRobbedMoney, isFirstHouseIncluded) =
                if (robbedMoney(index - 2)._1 > robbedMoney(index - 3)._1) {
                  robbedMoney(index - 2)
                } else if (robbedMoney(index - 2)._1 == robbedMoney(index - 3)._1) {
                  if (robbedMoney(index - 2)._2) robbedMoney(index - 3) else robbedMoney(index - 2)
                } else {
                  robbedMoney(index - 3)
                }
              robbedMoney(index) = (nums(index) + prevRobbedMoney, isFirstHouseIncluded)
              calculatedRobbedMoney(index + 1)
            }
          } else {
            if (index == 2) {
              robbedMoney(index) = (nums(index), false)
              calculatedRobbedMoney(index + 1)
            } else if (index == 3) {
              robbedMoney(index) = (nums(index) + robbedMoney(index - 2)._1, false)
              calculatedRobbedMoney(index + 1)
            } else {
              val robbedMoneyMinus2 =
                if(robbedMoney(index - 2)._2) robbedMoney(index - 2)._1 - robbedMoney(0)._1  + addFirstHouseIfNotNeighbour(index - 2) else robbedMoney(index - 2)._1
              val robbedMoneyMinus3 =
                if(robbedMoney(index - 3)._2) robbedMoney(index - 3)._1 - robbedMoney(0)._1 + addFirstHouseIfNotNeighbour(index - 3) else robbedMoney(index - 3)._1

              robbedMoney(index) = (nums(index) + Math.max(robbedMoneyMinus2, robbedMoneyMinus3), false)
              calculatedRobbedMoney(index + 1)
            }
          }
        }
      }

      def addFirstHouseIfNotNeighbour(index: Int) = {
        if(index - 1 == 1) 0 else nums(1)
      }

      if (nums.isEmpty) 0 else {
        calculatedRobbedMoney(0)
        println(robbedMoney.mkString(","))
        robbedMoney.map(_._1).max
      }
    }
  }

}
