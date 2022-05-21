package com.algo.arrays

object MaximumLengthConcatenatedStringUniqueCharacters extends App {
  object Solution {
    def maxLength(arr: List[String]): Int = {
      var uniqueCharSet = Set[Char]()

      def containsUniqueChars(str: String): Boolean = {
        str.length == str.toSet.size
      }

      @scala.annotation.tailrec
      def fillUniqueCharSet(index: Int): Unit = {
        if (index == arr.length) {
          ()
        } else {
          val string = arr(index)
          string.foreach(char => {
            uniqueCharSet = uniqueCharSet + char
          })
          fillUniqueCharSet(index + 1)
        }
      }

      fillUniqueCharSet(0)
      uniqueCharSet.size
    }
  }
}
