package com.algo.dp

object StoneWeight extends App {
  val a = Array(31, 26, 33, 21, 40)
  val res = Solution.lastStoneWeightII(a)
  println(res)

  object Solution {
    def lastStoneWeightII(stones: Array[Int]): Int = {

      @scala.annotation.tailrec
      def smash(stonesLeft: Seq[Int]): Seq[Int] = {
        if(stonesLeft.length <= 1) {
          stonesLeft
        } else {
          val sortedStones: Seq[Int] = stonesLeft.sorted(Ordering.Int.reverse)
          val (a, b) = sortedStones.splitAt(2)
          if(a.head == a(1)) smash(b) else smash(b :+ Math.abs(a.head - a(1)))
        }
      }

      smash(stones.toSeq).headOption match {
        case Some(head) => head
        case None => 0
      }
    }
  }
}
