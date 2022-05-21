package com.algo.dp

object BoatsToSavePeople extends App {

  val p = Array(4, 3, 2, 6)
  val l = 7

  val res = Solution.numRescueBoats(p, l)
  println(res)

  object Solution {
    def numRescueBoats(people: Array[Int], limit: Int): Int = {

      object PeopleOrder extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = {
          y compare x
        }
      }

      val sortedPeople = people.sorted(PeopleOrder)

      def calculate(low: Int, high: Int): Int = {
        if(high < low) {
          0
        } else if(high == low) {
          1
        } else {
          if(sortedPeople(high) + sortedPeople(low) <= limit) {
            1 + calculate(low + 1, high - 1)
          } else {
            1 + calculate(low + 1, high)
          }
        }
      }

      calculate(0, people.length - 1)

    }
  }
}
