package com.algo.dp

object LongestIncreasingSubSeq extends App {

//  val a = Array(10, 9, 2, 5, 3, 7, 101, 18, 6)
//  val a = Array.empty[Int]
  val a = Array(4, 10, 4, 3, 8, 9)
  val res = Solution.lengthOfLIS(a)
  println(res)

  object Solution {

    def lengthOfLIS(nums: Array[Int]): Int = {
      val array = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def len(index: Int): Unit = {
        if (index < 0) {
          ()
        } else if (index == nums.length - 1) {
          array(index) = 1
          len(index - 1)
        } else {
          findSmallestElementLargerThan(index) match {
            case Some((_, lIndex)) =>
              array(index) = array(lIndex) + 1
            case None =>
              array(index) = 1
          }
          len(index - 1)
        }
      }

//      def findLargestElementFrom(index: Int): Option[(Int, Int)] = {
//        @scala.annotation.tailrec
//        def find(l: Int, h: Int, largerElements: Seq[(Int, Int)]): Option[(Int, Int)] = {
//          if (h < l) {
//            largerElements.headOption
//          } else {
//            val mid = (l + h) / 2
//            if (nums(mid) > nums(index)) {
//              find(l, mid - 1, (nums(mid), mid) +: largerElements)
//            } else {
//              find(mid + 1, h, largerElements)
//            }
//          }
//        }
//
//        find(index, nums.length - 1, Nil)
//      }

      def findSmallestElementLargerThan(index: Int): Option[(Int, Int)] = {
        @scala.annotation.tailrec
        def find(currIndex: Int, elementWithIndex: Option[(Int, Int)]): Option[(Int, Int)] = {
          if (currIndex == nums.length) {
            elementWithIndex
          } else {
            elementWithIndex match {
              case None if nums(currIndex) > nums (index) =>
                find(currIndex + 1, Option(nums(currIndex), currIndex))
              case Some((elem, elemIndex)) if nums(currIndex) > nums (index) && nums(currIndex) < elem && array(currIndex) > array(elemIndex) =>
                find(currIndex + 1, Option(nums(currIndex), currIndex))
              case _ =>
                find(currIndex + 1, elementWithIndex)
            }

          }
        }

        find(index + 1, None)
      }

      if(array.isEmpty) 0 else {
        len(nums.length - 1)
        println(array.mkString(","))
        array.max
      }
    }
  }

}
