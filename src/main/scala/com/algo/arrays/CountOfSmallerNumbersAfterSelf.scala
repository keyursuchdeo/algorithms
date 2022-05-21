package com.algo.arrays

object CountOfSmallerNumbersAfterSelf extends App {
  object Solution {
    def countSmaller(nums: Array[Int]): List[Int] = {
      val ans = new Array[Int](nums.length)
      val sortedNums = new Array[Int](nums.length)

      def insertElement(num: Int, numIndex: Int): Int = {
        @scala.annotation.tailrec
        def findPos(low: Int, high: Int): Int = {
          if(high < low) {
            low
          } else {
            val mid = (high + low) / 2
            if(sortedNums(mid) < num) {
              findPos(mid + 1, high)
            } else {
              findPos(low, mid - 1)
            }
          }
        }

        def insertAt(pos: Int): Unit = {
          @scala.annotation.tailrec
          def insert(index: Int): Unit = {
            sortedNums(index + 1) = sortedNums(index)
            if(index == pos) {
              sortedNums(pos) = num
            } else {
              insert(index - 1)
            }
          }
          insert(numIndex - 1)
        }

        val insertPos = findPos(0, numIndex - 1)
        if(insertPos == numIndex) {
          sortedNums(insertPos) = num
        } else {
          insertAt(insertPos)
        }
        insertPos
      }

      @scala.annotation.tailrec
      def performCount(index: Int): Unit = {
        if (index < 0) {
          ()
        } else {
          ans(index) = insertElement(nums(index), nums.length - 1 - index)
          performCount(index - 1)
        }
      }

      performCount(nums.length - 1)
      ans.toList
    }
  }
}
