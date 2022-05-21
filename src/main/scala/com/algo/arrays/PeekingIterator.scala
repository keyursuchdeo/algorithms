package com.algo.arrays

import scala.util.Try

class PeekingIterator(_iterator: Iterator[Int]) {

  private var head: Option[Int] = Try(_iterator.next()).toOption

  def peek(): Int = {
    head.getOrElse(0)
  }

  def next(): Int = {
    val output = head.getOrElse(0)
    head = Try(_iterator.next()).toOption
    output
  }

  def hasNext(): Boolean = {
    head.nonEmpty
  }
}
