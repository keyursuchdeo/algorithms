package com.algo.greedy

import scala.collection.{immutable, mutable}

object TaskScheduler extends App {

  val t = Array('A', 'A', 'A', 'B', 'B', 'B')
  val res = Solution.leastInterval(t, 2)
  println(res)


  object Solution {
    def leastInterval(tasks: Array[Char], n: Int): Int = {

      val taskFrequencies = new Array[Int](26)
      @scala.annotation.tailrec
      def prepTaskFrequency(index: Int): Unit = {
        if (index == tasks.length) {
          ()
        } else {
          val taskIndex = tasks(index) - 'A'
          taskFrequencies(taskIndex) = taskFrequencies(taskIndex) + 1
          prepTaskFrequency(index + 1)
        }
      }

      object TaskOrdering extends Ordering[(Int, Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          val (xFrequency, xCoolingLen) = x
          val (yFrequency, yCoolingLen) = y

          if (xCoolingLen == yCoolingLen) {
            xFrequency compare yFrequency
          } else {
            yCoolingLen compare xCoolingLen
          }
        }
      }

      val taskQueue: mutable.PriorityQueue[(Int, Int)] = scala.collection.mutable.PriorityQueue.empty(TaskOrdering)

      @scala.annotation.tailrec
      def findTaskOfCoolingLen0(coolingTasks: Seq[(Int, Int)]): Option[(Int, Int)] = {
        if (taskQueue.isEmpty) {
          taskQueue.enqueue(coolingTasks: _*)
          None
        } else {
          val (freq, coolingLen) = taskQueue.dequeue()
          if (coolingLen == 0) {
            val remainingTasks: immutable.Seq[(Int, Int)] = taskQueue.dequeueAll
            val remainingTasksWithReducedCoolingPeriod = remainingTasks.map(queuedTask => {
              val (qFreq, qCoolingLen) = queuedTask
              if (qCoolingLen > 0) {
                (qFreq, qCoolingLen - 1)
              } else {
                (qFreq, qCoolingLen)
              }
            })
            taskQueue.enqueue(coolingTasks ++ remainingTasksWithReducedCoolingPeriod: _*)
            Option((freq, coolingLen))
          } else {
            findTaskOfCoolingLen0((freq, coolingLen - 1) +: coolingTasks)
          }
        }
      }

      @scala.annotation.tailrec
      def schedule(length: Int): Int = {
        if (taskQueue.isEmpty) {
          length
        } else {
          findTaskOfCoolingLen0(Nil) match {
            case None =>
              schedule(length + 1)
            case Some((freq, _)) if freq == 1 =>
              schedule(length + 1)
            case Some((freq, _)) =>
              taskQueue.enqueue((freq - 1, n))
              schedule(length + 1)
          }
        }
      }

      prepTaskFrequency(0)
      val taskFreqWithCoolingPeriod: Seq[(Int, Int)] = taskFrequencies.collect{
        case freq if freq > 0 => (freq, 0)
      }.toSeq
      taskQueue.enqueue(taskFreqWithCoolingPeriod: _*)
      schedule(0)

    }
  }

}
