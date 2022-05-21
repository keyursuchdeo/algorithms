package com.algo.arrays

object MaximizeDistanceToClosestPerson extends App {

  object Solution {
    def maxDistToClosest(seats: Array[Int]): Int = {

      @scala.annotation.tailrec
      def filledSeatsIndex(index: Int, filledSeats: Seq[Int]): Seq[Int] = {
        if (index == seats.length) {
          filledSeats.reverse
        } else {
          if (seats(index) == 0) {
            filledSeatsIndex(index + 1, filledSeats)
          } else {
            filledSeatsIndex(index + 1, index +: filledSeats)
          }
        }
      }

      @scala.annotation.tailrec
      def calculateMaxDistance(index: Int, prevFilledSeat: Option[Int], filledSeats: Seq[Int], maxDistanceTillNow: Int): Int = {
        if (index == seats.length) {
          maxDistanceTillNow
        } else {
          (prevFilledSeat, filledSeats) match {
            case (Some(prev), Nil) =>
              Math.max(maxDistanceTillNow, seats.length - 1 - prev)
            case (Some(prev), (head :: xs)) if head == index =>
              calculateMaxDistance(index + 1, Option(head), xs, maxDistanceTillNow)
            case (Some(prev), (head :: _)) =>
              val minDistance = Math.min(index - prev, head - index)
              if(minDistance > maxDistanceTillNow) {
                calculateMaxDistance(index + 1, prevFilledSeat, filledSeats, minDistance)
              } else {
                calculateMaxDistance(index + 1, prevFilledSeat, filledSeats, maxDistanceTillNow)
              }
            case (None, Nil) => //shouldn't happen
              maxDistanceTillNow
            case (None, (head :: xs)) if head == index =>
              calculateMaxDistance(index + 1, Option(head), xs, maxDistanceTillNow)
            case (None, (head :: _)) =>
              val minDistance = head - index
              if(minDistance > maxDistanceTillNow) {
                calculateMaxDistance(index + 1, prevFilledSeat, filledSeats, minDistance)
              } else {
                calculateMaxDistance(index + 1, prevFilledSeat, filledSeats, maxDistanceTillNow)
              }
          }
        }
      }

      calculateMaxDistance(0, None, filledSeatsIndex(0, Nil), 0)
    }
  }

}
