package com.algo.arrays

object LDSRabinKarp extends App {
  val s = "banana"
  val res = Solution.longestDupSubstring(s)
  println(res)

  object Solution {
    def longestDupSubstring(S: String): String = {
      lazy val hashMultiplier = 26
      lazy val mod = 1000001

      @scala.annotation.tailrec
      def find(low: Int, high: Int, currOutput: String): String = {
        if (high < low) {
          currOutput
        } else {
          val mid = (low + high) / 2
          val output = check(S, mid)
          if (output == "") {
            find(low, mid - 1, currOutput)
          } else {
            find(mid + 1, high, output)
          }
        }
      }

      def check(str: String, len: Int) = {
        lazy val hash: Long = calculateHash(str, len)

        @scala.annotation.tailrec
        def find(index: Int = len, currHash: Long, strHash: Map[Long, Set[String]]): String = {
          if (index == str.length) {
            ""
          } else {
            val subtract = ((Math.pow(hashMultiplier, len - 1) * ascii(str(index - len))) % mod)
            val hashAfterSubtraction = currHash - subtract
            val hashAfterMultiplication = ((hashAfterSubtraction * hashMultiplier) % mod)
            val hashAfterAddition = ((hashAfterMultiplication + ((ascii(str(index))))) % mod).toLong
            val updatedHash = hashAfterAddition
            val currStr = str.substring(index - len + 1, index + 1)
            strHash.get(updatedHash) match {
              case Some(matchingStrings) =>
                if (matchingStrings.contains(currStr)) {
                  currStr
                } else {
                  find(index + 1, currHash, strHash + (updatedHash -> (matchingStrings + currStr)))
                }
              case _ =>
                find(index + 1, updatedHash, strHash + (updatedHash -> (Set(currStr))))
            }
          }
        }

        find(currHash = hash, strHash = Map(hash -> Set(str)))
      }

      def calculateHash(str: String, len: Int): Long = {
        @scala.annotation.tailrec
        def calculate(index: Int, currHash: Long): Long = {
          if (index == len) {
            currHash
          } else {
            val updatedHash: Double = currHash + ((Math.pow(hashMultiplier, len - 1 - index) * ascii(str(index))) % mod)
            calculate(index + 1, updatedHash.toLong)
          }
        }

        calculate(0, 0)
      }

      def ascii(char: Char): Int = {
        char - 'a'
      }

      find(1, S.length, "")
    }
  }
}
