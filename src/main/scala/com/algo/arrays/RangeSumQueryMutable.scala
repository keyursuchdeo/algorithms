package com.algo.arrays

object RangeSumQueryMutable extends App {
  class NumArray(_nums: Array[Int]) {

    val cumulativeSums = new Array[Int](_nums.length)
    cumulativeSums(0) = _nums(0)
    populateCumulativeSums(1)

    @scala.annotation.tailrec
    private def populateCumulativeSums(index: Int): Unit = {
      if(index == _nums.length) {
        ()
      } else {
        cumulativeSums(index) = cumulativeSums(index - 1) + _nums(index)
        populateCumulativeSums(index + 1)
      }
    }

    def update(index: Int, `val`: Int) {
      _nums(index) = `val`
      if(index == 0) {
        populateCumulativeSums(1)
      } else {
        populateCumulativeSums(index)
      }
    }

    def sumRange(left: Int, right: Int): Int = {
      if(left == 0) {
        cumulativeSums(right)
      } else {
        cumulativeSums(right) - cumulativeSums(left - 1)
      }
    }

  }

  /**
   * Your NumArray object will be instantiated and called as such:
   * var obj = new NumArray(nums)
   * obj.update(index,`val`)
   * var param_2 = obj.sumRange(left,right)
   */
}
