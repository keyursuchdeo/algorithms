package com.algo.stack

object MaximumFrequencyStack extends App {
  class FreqStack() {

    private var maxFreq: Int = 0
    private var freqNums: Map[Int, Seq[Int]] = Map[Int, Seq[Int]]()
    private var numFreq: Map[Int, Int] = Map[Int, Int]()

    def push(x: Int) {
      val newXFreq = numFreq.getOrElse(x, 0) + 1
      val newXFreqNums = freqNums.getOrElse(newXFreq, Nil)
      if(newXFreq > maxFreq) {
        maxFreq = newXFreq
      }
      numFreq = numFreq + (x -> newXFreq)
      freqNums = freqNums + (newXFreq -> (x +: newXFreqNums))
    }
    def pop(): Int = {
      freqNums.get(maxFreq) match {
        case Some(nums) =>
          val output = nums.head
          if(nums.tail.isEmpty) {
            freqNums = freqNums - maxFreq
            maxFreq = maxFreq - 1
            numFreq = numFreq + (output -> maxFreq)
          } else {
            freqNums = freqNums + (maxFreq -> nums.tail)
            numFreq = numFreq + (output -> (maxFreq - 1))
          }
          output
        case _ =>
          -1
      }
    }

  }
}
