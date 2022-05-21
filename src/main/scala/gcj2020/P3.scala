package gcj2020

import gcj2020.P2.Solution

import scala.collection.immutable
import scala.io.StdIn

object P3 extends App {
  new Solution().main()

  class Solution {
    def main(): Unit = {
      val numOfTestCases: Int = StdIn.readInt()
      for (x <- 1 to numOfTestCases) {
        val numOfActivities = StdIn.readInt()
        val activitySchedule: immutable.Seq[(Int, Int)] =
          for (y <- 0 until numOfActivities) yield {
            val schedule: String = StdIn.readLine()
            val times = schedule.split(" ")
            (times(0).toInt, times(1).toInt)
          }
        val allocation: String = allocate(activitySchedule)
        println(s"Case #$x: $allocation")
      }
    }

    private def allocate(activitySchedule: immutable.Seq[(Int, Int)]): String = {
      val sortActivitiesByStartTime: immutable.Seq[((Int, Int), Int)] = activitySchedule.zipWithIndex.sortBy(_._1._1)

      @scala.annotation.tailrec
      def alloc(index: Int, cFreeAt: Int, jFreeAt: Int, allocSeq: Seq[(String, Int)] = Nil): String = {
        if(index == sortActivitiesByStartTime.length) {
          allocSeq.sortBy(_._2).map(_._1).mkString("")
        } else {
          if (cFreeAt <= sortActivitiesByStartTime(index)._1._1) {
            alloc(index + 1, sortActivitiesByStartTime(index)._1._2, jFreeAt, ("C", sortActivitiesByStartTime(index)._2) +: allocSeq)
          } else if (jFreeAt <= sortActivitiesByStartTime(index)._1._1) {
            alloc(index + 1, cFreeAt, sortActivitiesByStartTime(index)._1._2,  ("J", sortActivitiesByStartTime(index)._2) +: allocSeq)
          } else {
            "IMPOSSIBLE"
          }
        }
      }

      alloc(0, -1, -1)
    }
  }

}
