package com.algo

import scala.io.Source

object QuickSort extends App {
  // val input = mutable.Seq(3, 8, 2, 5, 1, 4, 7, 6)
  // val input = Array(3, 2, 8, 5, 1, 4, 7, 6)
  // val input = Array(8, 2, 3, 5, 1, 4, 7, 6)
  // val input = Array(1, 2, 3, 5, 8, 4, 7, 6)
  // val input = Array(1, 2, 3, 4, 5, 6, 7, 8)
  // val input = Array(8, 7, 6, 5, 4, 3, 2, 1)
  val input: Array[Int] = Source.fromResource("inversioninput.txt").getLines.map(_.toInt).toArray

  val partitionedInput = sort(input, endIndex = input.length - 1)
  println(partitionedInput.mkString(","))

  private lazy val pivotIndexHead: Int = 0 //head's index

  private lazy val pivotIndexLast: Int = input.length - 1 //last element's index

  private def partitionFP(input: Array[Int], pivotElement: Int) = {
    val (lessThanPivot, greaterThanPivot) = input.partition(_ < pivotElement)
    lessThanPivot ++ greaterThanPivot
  }

  private def sort(input: Array[Int], beginIndex: Int = 0, endIndex: Int): Array[Int] = {
    if(endIndex - beginIndex > 1) {
      val (partitionedArray, lessThanPivotIndex) = partitionInPlace(input, beginIndex, beginIndex, endIndex)
      val partitionedLeftArray = sort(partitionedArray, beginIndex, lessThanPivotIndex - 1)
      sort(partitionedLeftArray, lessThanPivotIndex + 1, endIndex)
    } else {
      input
    }
  }

  private def partitionInPlace(input: Array[Int], pivotIndex: Int, beginIndex: Int, endIndex: Int): (Array[Int], Int) = {
    def partition(input: Array[Int], lessThanPivotIndex: Int, partitionedUptoIndex: Int): (Array[Int], Int) = {
      if(partitionedUptoIndex > endIndex) { //everything partitioned. Put pivot at its rightful place
        swap(input, pivotIndex, lessThanPivotIndex)
        (input, lessThanPivotIndex)
      } else {
        if(input(partitionedUptoIndex) < input(pivotIndex)) {
          swap(input, lessThanPivotIndex + 1, partitionedUptoIndex)
          partition(input, lessThanPivotIndex + 1, partitionedUptoIndex + 1)
        } else {
          partition(input, lessThanPivotIndex, partitionedUptoIndex + 1)
        }
      }
    }

    partition(input, lessThanPivotIndex = beginIndex, partitionedUptoIndex = beginIndex + 1)
  }

  private def swap(input: Array[Int], index1: Int, index2: Int): Unit = {
    val temp = input(index1)
    input(index1) = input(index2)
    input(index2) = temp
  }
}
