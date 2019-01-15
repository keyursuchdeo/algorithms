package com.algo

import java.net.URL

import scala.io.Source

object Inversion extends App {
  // val input: Seq[Int] = Seq(1, 3, 5, 7, 2, 4, 6, 8)
  // val input: Seq[Int] = Seq(8, 7, 6, 5, 4, 3, 2, 1)
  val input: Seq[Int] = Source.fromResource("inversioninput.txt").getLines.map(_.toInt).toList
  //val input = input1.take(100)
  //println(input)

  // val input: Seq[Int] = Seq(1, 2, 3, 4, 5, 6)
  // val input = Seq(1, 2)

  val (sortedInput, inversions) = mergeSort(input)
  println(s"sorted $sortedInput")
  println(s"number of inversions $inversions")

  private def mergeSort(input: Seq[Int]): (Seq[Int], Long) = {
    val inputSize = input.size
    if(inputSize > 1) {
      val (input1, input2) = split(input, inputSize)
      val (sortedInput1, inversions1) = mergeSort(input1)
      val (sortedInput2, inversions2) = mergeSort(input2)
      val (sortedOutput, inversions) = merge(sortedInput1, sortedInput2, Nil, inversions1 + inversions2)
      (sortedOutput, inversions)
    } else {
      (input, 0L)
    }
  }

  private def split(input: Seq[Int], inputSize: Int): (Seq[Int], Seq[Int]) = {
    input.splitAt(inputSize / 2)
  }

  private def merge(input1: Seq[Int], input2: Seq[Int], mergedSeq: Seq[Int] = Nil, inversions: Long = 0L): (Seq[Int], Long) = {
    (input1, input2) match {
      case (input1Head :: input1Tail, input2Head :: _) if input1Head <= input2Head =>
        merge(input1Tail, input2, input1Head +: mergedSeq, inversions)
      case (input1Head :: _, input2Head :: input2Tail) if input2Head < input1Head =>
        merge(input1, input2Tail, input2Head +: mergedSeq, inversions + input1.size)
      case (Nil, _ :: _) =>
        ((input2.reverse ++ mergedSeq).reverse, inversions)
      case (_ :: _, Nil) =>
        ((input1.reverse ++ mergedSeq).reverse, inversions)
      case (Nil, Nil) =>
        (mergedSeq.reverse, inversions)
    }
  }
}
