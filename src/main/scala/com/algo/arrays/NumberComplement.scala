package com.algo.arrays

object NumberComplement extends App {
  val res = Solution.findComplement(0)
  println(res)

  object Solution {
    def findComplement(num: Int): Int = {
      val binNum = num.toBinaryString
      val complement: String = binNum.map(bin => {
        if(bin == '0') '1' else '0'
      })
      Integer.parseInt(complement, 2)
    }
  }
}
