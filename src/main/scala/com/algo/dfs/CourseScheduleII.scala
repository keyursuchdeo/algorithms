package com.algo.dfs

object CourseScheduleII extends App {

  val n = 4
  val p = Array(Array(1, 0), Array(2, 0), Array(3, 1), Array(3, 2))

//  val p = Array(Array(1, 0), Array(1, 2), Array(2, 4), Array(0, 4), Array(4, 3), Array(3, 2))

  val res = Solution.findOrder(n, p)
  println(res.mkString(","))

  object Solution {
    def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
      val prerequisiteList = Array.fill[Seq[Int]](numCourses)(Nil)
      val visitedCourses = Array.fill[Boolean](numCourses)(false)
      var order: Seq[Int] = Seq[Int]()

      @scala.annotation.tailrec
      def prepAdjLists(index: Int): Unit = {
        if (index == prerequisites.length) {
          ()
        } else {
          val Array(course, prerequisiteCourse) = prerequisites(index)
          prerequisiteList(prerequisiteCourse) = course +: prerequisiteList(prerequisiteCourse)
          prepAdjLists(index + 1)
        }
      }

      @scala.annotation.tailrec
      def hasCycle(course: Int): Boolean = {
        if (course == numCourses) {
          false
        } else {
          if (hasCycleFrom(course, new Array[Boolean](numCourses))) {
            true
          } else {
            hasCycle(course + 1)
          }
        }
      }

      def hasCycleFrom(course: Int, visited: Array[Boolean]): Boolean = {
        if (visited(course)) {
          true
        } else {
          visited(course) = true
          val cycleFound =
            prerequisiteList(course).exists(downstreamCourse => {
              hasCycleFrom(downstreamCourse, visited)
            })
          if(cycleFound) {
            true
          } else {
            visited(course) = false
            false
          }
        }
      }

      @scala.annotation.tailrec
      def calculateOrder(course: Int): Unit = {
        if (course == numCourses) {
          ()
        } else {
          if (!visitedCourses(course)) {
            calculateOrderStartingFrom(course)
            calculateOrder(course + 1)
          } else {
            calculateOrder(course + 1)
          }
        }
      }

      def calculateOrderStartingFrom(course: Int): Unit = {
        visitedCourses(course) = true
        prerequisiteList(course).collect {
          case downstreamCourse if !visitedCourses(downstreamCourse) =>
            calculateOrderStartingFrom(downstreamCourse)
        }
        order = course +: order
      }

      prepAdjLists(0)
      if (hasCycle(0)) {
        Array()
      } else {
        calculateOrder(0)
        order.toArray
      }
    }
  }

}
