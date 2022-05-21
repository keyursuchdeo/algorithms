package com.algo.arrays

object FlattenNestedListIterator extends App {


  trait NestedInteger {

    // Return true if this NestedInteger holds a single integer, rather than a nested list.
    def isInteger: Boolean

    // Return the single integer that this NestedInteger holds, if it holds a single integer.
    def getInteger: Int

    // Set this NestedInteger to hold a single integer.
    def setInteger(i: Int): Unit

    // Return the nested list that this NestedInteger holds, if it holds a nested list.
    def getList: Array[NestedInteger]

    // Set this NestedInteger to hold a nested list and adds a nested integer to it.
    def add(ni: NestedInteger): Unit
  }


  class NestedIterator(_nestedList: List[NestedInteger]) {

    private var flattenedList: Seq[Int] = flatten((_nestedList)).reverse

    def flatten(currList: List[NestedInteger], output: List[Int] = Nil): List[Int] = {
      if(currList.isEmpty) {
        output
      } else {
        if(currList.head.isInteger) {
          flatten(currList.tail, currList.head.getInteger +: output)
        } else {
          flatten(currList.head.getList.toList, output)
          flatten(currList.tail, output)
        }
      }
    }

    def next(): Int = {
      if(hasNext()) {
        val output = flattenedList.head
        flattenedList = flattenedList.tail
        output
      } else {
        throw new IllegalArgumentException("Empty iterator")
      }
    }

    def hasNext(): Boolean = {
      flattenedList.nonEmpty
    }
  }

}
