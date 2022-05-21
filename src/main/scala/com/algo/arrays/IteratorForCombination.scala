package com.algo.arrays

class IteratorForCombination {
  class CombinationIterator(_characters: String, _combinationLength: Int) {

    private var combinations = prepCombinations(_characters, _combinationLength)
    private def prepCombinations(currS: String, currCombLen: Int): Seq[String] = {
      if(currS.isEmpty) {
        Nil
      } else if (currCombLen == 1) {
        currS.toCharArray.map(_.toString).toSeq
      } else {
        val head = currS.head
        prepCombinations(currS.tail, currCombLen - 1).map(head +: _) ++
          prepCombinations(currS.tail, currCombLen).map(head +: _)
      }
    }

    def next(): String = {
      val head = combinations.head
      combinations = combinations.tail
      head
    }

    def hasNext(): Boolean = {
      combinations.nonEmpty
    }
  }
}
