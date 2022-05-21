package com.algo.dp

object ContiguousArray extends App {

//  val a = Array(0, 0, 1, 0, 0, 0, 1, 1)
  val a = Array(0,1,1,0,1,1,1,0)
  val res = Solution.findMaxLength(a)
  println(res)

  object Solution {
    def findMaxLength(nums: Array[Int]): Int = {
      lazy val lengths = new Array[(Int, Int)](nums.length)
      lazy val totalLen = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def find(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else if (index == 0){
          if (nums(index) == 0) {
            lengths(index) = (1, 0)
          } else {
            lengths(index) = (0, 1)
          }
          totalLen(index) = 0
          find(index + 1)
        } else {
          if (nums(index) == 0) {
            lengths(index) = (lengths(index - 1)._1 + 1, lengths(index - 1)._2)
          } else {
            lengths(index) = (lengths(index - 1)._1, lengths(index - 1)._2 + 1)
          }
          totalLen(index) = Math.min(lengths(index)._1, lengths(index)._2) * 2
          find(index + 1)
        }
      }
      if (nums.length == 0) 0 else {
        find(0)
        println(lengths.mkString(","))
        println(totalLen.mkString(","))
        totalLen.reverse.head
      }
    }
  }
}
