package com.algo.heap

import scala.util.Try

object KLargestElement extends App {

  val input = Array(3, 2, 1, 5, 6, 4)
  val k = 2

//  val input = Array(3, 2, 3, 1, 2, 4, 5, 5, 6)
//  val k = 4

  val result = Solution.findKthLargest(input, k)
  println(result)

  object Solution {
    def findKthLargest(nums: Array[Int], k: Int): Int = {
      println(nums.mkString(","))
      val updatedNums = buildHeap(nums)
      println(updatedNums.mkString(","))
      deleteFromHeap(updatedNums, k)
    }

    private def deleteFromHeap(nums: Array[Int], k: Int): Int = {

      @scala.annotation.tailrec
      def delete(count: Int, lastElemIndex: Int, numsToUpdate: Array[Int]): Int = {
        if (count == k - 1) {
          numsToUpdate(0)
        } else {
          swap(0, lastElemIndex, numsToUpdate)
          val optChildIndex: Option[Int] = indexOfChildLargerThanParent(0, numsToUpdate, lastElemIndex)
          val updatedNums = swapTillLeaf(optChildIndex, 0, numsToUpdate, lastElemIndex)
          delete(count + 1, lastElemIndex - 1, updatedNums)
        }
      }

      delete(0, nums.length - 1, nums)
    }

    private def buildHeap(nums: Array[Int]): Array[Int] = {

      @scala.annotation.tailrec
      def build(index: Int, numsToUpdate: Array[Int]): Array[Int] = {
        if (index < 0) {
          numsToUpdate
        } else {
          if (indexHasChildren(index, numsToUpdate.length)) {
            val optChildIndex: Option[Int] = indexOfChildLargerThanParent(index, numsToUpdate, numsToUpdate.length)
            val updatedNums: Array[Int] = swapTillLeaf(optChildIndex, index, numsToUpdate, numsToUpdate.length)
            build(index - 1, updatedNums)
          } else {
            build(index - 1, numsToUpdate)
          }
        }
      }

      build(nums.length - 1, nums)
    }

    private def swapTillLeaf(childIndex: Option[Int], index: Int, nums: Array[Int], maxLength: Int): Array[Int] = {

      @scala.annotation.tailrec
      def swapRepeatedly(optNextChildIndex: Option[Int], currIndex: Int, updatedNums: Array[Int]): Array[Int] = {
        optNextChildIndex match {
          case Some(nextChildIndex) =>
            swap(nextChildIndex, currIndex, updatedNums)
            if(!indexHasChildren(nextChildIndex, maxLength)) {
              updatedNums
            } else {
              swapRepeatedly(indexOfChildLargerThanParent(nextChildIndex, updatedNums, maxLength), nextChildIndex, updatedNums)
            }
          case _ => updatedNums
        }
      }

      swapRepeatedly(childIndex, index, nums)
    }

    private def indexHasChildren(index: Int, maxLength: Int): Boolean =
      leftChildIndex(index) < maxLength || rightChildIndex(index) < maxLength

    private def indexOfChildLargerThanParent(index: Int, nums: Array[Int], maxLength: Int): Option[Int] = {
      val leftIndex = leftChildIndex(index)
      val rightIndex = rightChildIndex(index)
      if (leftIndex < maxLength && rightIndex < maxLength) {
        val (childNum, childNumIndex) =
          (nums(leftIndex), Try(nums(rightIndex)).toOption) match {
            case (left, Some(right)) if left >= right => (left, leftIndex)
            case (left, Some(right)) if right > left => (right, rightIndex)
            case (left, None) => (left, leftIndex)
          }
        if (childNum > nums(index)) Option(childNumIndex) else None
      } else if (leftIndex < maxLength) {
        if (nums(leftIndex) > nums(index)) Option(leftIndex) else None
      } else {
        None
      }
    }

    private def leftChildIndex(index: Int): Int =
      (2 * (index + 1) - 1)

    private def rightChildIndex(index: Int): Int =
      (2 * (index + 1))

    private def swap(index1: Int, index2: Int, nums: Array[Int]): Unit = {
      val temp = nums(index1)
      nums(index1) = nums(index2)
      nums(index2) = temp
    }
  }

}
