package com.algo.heap

object CourseScheduleIII extends App {
  object Solution {
    def scheduleCourse(courses: Array[Array[Int]]): Int = {
      object CourseOrderByLastDay extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(xD, xL) = x
          val Array(yD, yL) = y
          if(yL == xL) {
            yD compare xD
          } else {
            yL compare xL
          }
        }
      }

      object CourseOrderByDuration extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(xD, xL) = x
          val Array(yD, yL) = y
          if(xD == yD) {
            yL compare xL
          } else {
            xD compare yD
          }
        }
      }

      val sortedCourses: Array[Array[Int]] = courses.sorted(CourseOrderByLastDay)
      val takenCourseHeap = scala.collection.mutable.PriorityQueue.empty(CourseOrderByDuration)

      @scala.annotation.tailrec
      def takeCourses(index: Int, numOfCourses: Int, completedDays: Int): Int = {
        if(index == sortedCourses.length) {
          numOfCourses
        } else {
          val Array(duration, lastDay) = sortedCourses(index)
          if(duration + completedDays <= lastDay) {
            takenCourseHeap.enqueue(sortedCourses(index))
            takeCourses(index + 1, numOfCourses + 1, completedDays + duration)
          } else {
            if(takenCourseHeap.nonEmpty) {
              val Array(ctrDuration, _) = takenCourseHeap.head
              if(ctrDuration > duration) {
                takenCourseHeap.dequeue()
                takenCourseHeap.enqueue(sortedCourses(index))
                takeCourses(index + 1, numOfCourses, completedDays - ctrDuration + duration)
              } else {
                takeCourses(index + 1, numOfCourses, completedDays)
              }
            } else {
              takeCourses(index + 1, numOfCourses, completedDays)
            }
          }
        }
      }

      takeCourses(0, 0, 0)
    }
  }
}
