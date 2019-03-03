package com.algo.z

import scala.annotation.tailrec
import scala.collection.mutable

object MaxSqRootOpns extends App {
  println(new Solution2().solution(2, 1000000000))
}

class Solution2 {
  def solution(a: Int, b: Int): Int = {
    var numOfSqRootOpnsByNum = mutable.Map.empty[Double, Int]

    def cacheNumOfSqRootOpnsByNumAndReturn(input: Double, numOfOpns: Int): Int = {
      numOfSqRootOpnsByNum += (input -> numOfOpns)
//      println(numOfSqRootOpnsByNum)
      numOfOpns
    }

    def continuousSqRoot(originalInput: Double, input: Double, numOfSqRootOpns: Int = 0): Int = {
      val sqRoot: Double = Math.sqrt(input)
      if (sqRoot.isValidInt) {
        val cachedOutput = numOfSqRootOpnsByNum.get(sqRoot)
//        if(cachedOutput.isDefined) {
//          println(s"using from cache $originalInput $sqRoot ${cachedOutput.get}")
//        }

        cachedOutput.map(numOfOpns => {
          //println(s"originalInput -> $originalInput lastSqRoot -> $input numOfSqRootOpns -> ${numOfOpns + 1}. Cache saved $numOfOpns iterations.")
          cacheNumOfSqRootOpnsByNumAndReturn(originalInput, numOfOpns + 1)
        }).getOrElse(continuousSqRoot(originalInput, sqRoot, numOfSqRootOpns + 1))
      } else {
        if (numOfSqRootOpns >= 1 ) {
          cacheNumOfSqRootOpnsByNumAndReturn(originalInput, numOfSqRootOpns)
        }
        //println(s"originalInput -> $originalInput lastSqRoot -> $input numOfSqRootOpns -> $numOfSqRootOpns")
        numOfSqRootOpns
      }
    }

    @tailrec
    def maxNumOfSqRootOpns(input: Int, maxSqRootOpns: Int, maxSqRootOpnsNum: Int): Int = {
      if (input > b) {
        println(s"maxSqRootOpns -> $maxSqRootOpnsNum $maxSqRootOpns")
        maxSqRootOpns
      } else {
        val numOfSqRootOpns = continuousSqRoot(input, input)
        if (numOfSqRootOpns > maxSqRootOpns) {
//          println(s"New maxSqRootOpns -> $input $numOfSqRootOpns")
          maxNumOfSqRootOpns(input + 1, numOfSqRootOpns, input)
        }
        else
          maxNumOfSqRootOpns(input + 1, maxSqRootOpns, maxSqRootOpnsNum)
      }
    }

    maxNumOfSqRootOpns(a, maxSqRootOpns = 0, a)
  }
}

/**
  *
  * @tailrec
  * def continuousSqRoot(input: Double, numOfSqRootOpns: Int = 0): Int = {
  * val sqRoot = Math.sqrt(input)
  * if (sqRoot.isValidInt) {
  * continuousSqRoot(sqRoot, numOfSqRootOpns + 1)
  * } else {
  * numOfSqRootOpns
  * }
  * }
  * @tailrec
  * def maxNumOfSqRootOpns(input: Int, maxSqRootOpns: Int): Int = {
  * if (input > b) {
  * maxSqRootOpns
  * } else {
  * val numOfSqRootOpns = continuousSqRoot(input)
  * if (numOfSqRootOpns > maxSqRootOpns)
  * maxNumOfSqRootOpns(input + 1, numOfSqRootOpns)
  * else
  * maxNumOfSqRootOpns(input + 1, maxSqRootOpns)
  * }
  * }
  *
  * maxNumOfSqRootOpns(a, maxSqRootOpns = 0)
  *
  */
