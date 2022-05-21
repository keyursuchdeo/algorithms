package com.algo.n26

object FindNumber extends App {
  def findNumber(arr: Array[Int], k: Int): String = {
    arr.find(_ == k).fold("NO")(_ => "YES")
  }
}
