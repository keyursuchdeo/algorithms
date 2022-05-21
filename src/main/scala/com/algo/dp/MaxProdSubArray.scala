package com.algo.dp

object MaxProdSubArray extends App {
  val a = Array(2, -5, -2, -4, 3)
  val res = Solution.maxProduct(a)
  println(res)

  object Solution {
    def maxProduct(nums: Array[Int]): Int = {

      @scala.annotation.tailrec
      def max(index: Int, prevNegIndex: Option[Int], numsArray: Array[Int], array: Array[Int]): Option[Int] = {
        if (index == numsArray.length) {
          prevNegIndex
        } else {
          if (index == 0) {
            if (numsArray(index) < 0) {
              array(index) = 1
              max(index + 1, Option(0), numsArray, array)
            } else {
              array(index) = numsArray(index)
              max(index + 1, prevNegIndex, numsArray, array)
            }
          } else {
            if (numsArray(index) < 0) {
              prevNegIndex match {
                case Some(nIndex) if nIndex == index - 1 =>
                  array(index) = numsArray(index) * numsArray(nIndex) * array(nIndex)
                  max(index + 1, None, numsArray, array)
                case Some(nIndex) =>
                  array(index) = numsArray(index) * numsArray(nIndex) * array(index - 1) * array(nIndex)
                  max(index + 1, None, numsArray, array)
                case None =>
                  array(index) = array(index - 1)
                  max(index + 1, Option(index), numsArray, array)
              }
            } else {
              prevNegIndex match {
                case Some(nIndex) if nIndex == index - 1 =>
                  array(index) = numsArray(index)
                case _ =>
                  array(index) = numsArray(index) * array(index - 1)
              }
              max(index + 1, prevNegIndex, numsArray, array)
            }
          }
        }
      }

      @scala.annotation.tailrec
      def subArrays(index: Int, arrays: Seq[Array[Int]], currSeq: Seq[Int]): Seq[Array[Int]] = {
        if (index == nums.length) {
          if (currSeq.isEmpty) arrays else currSeq.toArray +: arrays
        } else {
          if (nums(index) != 0) {
            subArrays(index + 1, arrays, nums(index) +: currSeq)
          } else {
            if(currSeq.isEmpty) {
              subArrays(index + 1, arrays, Nil)
            } else {
              subArrays(index + 1, currSeq.toArray +: arrays, Nil)
            }
          }
        }
      }

      val allSubArrays = subArrays(0, Nil, Nil)
      allSubArrays match {
        case Nil => 0
        case x :: Nil =>
          if (x.length == 1) x.head else {
            val array = new Array[Int](x.length)
            val optNegIndex = max(0, None, x.reverse, array)
                      println(array.mkString(","))
            optNegIndex match {
              case Some(negIndex) =>
                Math.max(array(negIndex), array(x.length - 1))
              case None =>
                array(x.length - 1)
            }
          }
        case _ =>
          Math.max(0, allSubArrays.map(x => {
            if (x.length == 1) x.head else {
              val array = new Array[Int](x.length)
              val optNegIndex = max(0, None, x.reverse, array)
              //            println(array.mkString(","))
              optNegIndex match {
                case Some(negIndex) =>
                  Math.max(array(negIndex), array(x.length - 1))
                case None =>
                  array(x.length - 1)
              }
            }
          }).max)
      }
    }
  }

}
