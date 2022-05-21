package com.algo.arrays

object SearchSuggestionsSystem extends App {
  object Solution {
    def suggestedProducts(products: Array[String], searchWord: String): List[List[String]] = {
      val sortedProducts = products.sorted
      val searchWordChars = searchWord.toCharArray

      def searchStartingFrom(string: String, start: Int): (List[String], Int) = {
        @scala.annotation.tailrec
        def binarySearch(low: Int, high: Int, matchingWordIndex: Int): Int = {
          if(high < low) {
            matchingWordIndex
          } else{
            val mid = (low + high) / 2
            if(sortedProducts(mid).startsWith(string)) {
              binarySearch(low, mid - 1, mid)
            } else {
              if(sortedProducts(mid) > string) {
                binarySearch(low, mid - 1, mid)
              } else {
                binarySearch(mid + 1, high, matchingWordIndex)
              }
            }
          }
        }

        val index = binarySearch(start, sortedProducts.length - 1, matchingWordIndex = -1)
        if(index == -1) {
          (Nil, start)
        } else {
          ((index to index + 2).collect{
            case i if i < sortedProducts.length && sortedProducts(i).startsWith(string) =>
              sortedProducts(i)
          }.toList, index)
        }
      }

      @scala.annotation.tailrec
      def search(index: Int, sb: StringBuilder, searchFrom: Int, output: List[List[String]]): List[List[String]] = {
        if(index == searchWordChars.length) {
          output.reverse
        } else {
          val updatedSb = sb.append(searchWordChars(index))
          val (suggestions, nextStartFrom) = searchStartingFrom(updatedSb.toString(), searchFrom)
          search(index + 1, updatedSb, nextStartFrom, suggestions +: output)
        }
      }

      search(0, new StringBuilder(searchWordChars.length), 0, Nil)
    }
  }
}
