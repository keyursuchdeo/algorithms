package com.algo.design

object UndergroundSystem extends App {
  class UndergroundSystem() {

    private var openTripsById = Map[Int, (String, Int)]()
    private var tripMetadataByStations = Map[(String, String), (Int, Int)]()

    def checkIn(id: Int, stationName: String, t: Int) {
      openTripsById = openTripsById + (id -> (stationName, t))
    }

    def checkOut(id: Int, stationName: String, t: Int) {
      val (fromStation, fromTime) = openTripsById(id)
      val key = (fromStation, stationName)
      val (totalTravelTime, totalTrips) = tripMetadataByStations.getOrElse(key, (0, 0))
      val currTravelTime = t - fromTime
      openTripsById = openTripsById - id
      tripMetadataByStations = tripMetadataByStations + (key -> (totalTravelTime + currTravelTime, totalTravelTime + 1))
    }

    def getAverageTime(startStation: String, endStation: String): Double = {
      println(tripMetadataByStations)
      val (totalTravelTime, totalTrips) = tripMetadataByStations((startStation, endStation))
      totalTravelTime / totalTrips.toDouble
    }

  }
}
