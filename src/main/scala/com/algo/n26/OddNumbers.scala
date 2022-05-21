package com.algo.n26

object OddNumbers extends App {
  def oddNumbers(l: Int, r: Int): Array[Int] = {
    val odds = for(i <- l to r if i % 2 == 1) yield i
    odds.toArray
  }
}
