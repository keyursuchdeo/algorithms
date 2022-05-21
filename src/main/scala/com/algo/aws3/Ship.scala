package com.algo.aws3


import scala.collection.JavaConverters._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

//object Solution {
//  def solution(n: Int, s: String, t: String): String = {
//    val shipPositions: Array[ShipPosition] = buildShipPositions(s)
//    val hitPositions: Array[Position] = buildHitPositions(t)
//    import ShipState._
//    val allShipStates: Array[ShipState] = shipPositions.map(_.calcShipState(hitPositions))
//    val shipStates = allShipStates.groupBy(_.outerEnum.values)
//    val sunkShips = shipStates.getOrElse(ShipState.Sunk.outerEnum.values, "0")
//    val hitShips = shipStates.getOrElse(ShipState.Hit.outerEnum.values, "1")
//
//    s"$sunkShips,$hitShips"
//  }
//
//  private def buildShipPositions(s: String): Array[ShipPosition] = {
//    val ships: Array[String] = s.split(",")
//    val shipPositions: Array[Array[String]] = ships.map(_.split(" "))
//    shipPositions.map(_.map(buildPosition)).map(ShipPosition)
//  }
//
//  private def buildHitPositions(t: String): Array[Position] = {
//    val hits: Array[String] = t.split(" ")
//    hits.map(buildPosition)
//  }
//
//  private def buildPosition(position: String): Position = {
//    val (row, col) = position.splitAt(0)
//    Position(row.toInt, col)
//  }
//
//  object ShipState extends Enumeration {
//    type ShipState = Value
//    val Hit, Sunk, Safe = Value
//  }
//
//  case class Position(row: Int, col: String)
//
//  case class ShipPosition(cornerPositions: Array[Position]) {
//
//    lazy val allPositions: Array[Position] = {
//      val rows: Array[Int] = cornerPositions.map(_.row)
//      val cols: Array[String] = cornerPositions.map(_.col)
//      rows.zip(cols).map(rowAndCol => {
//        val (row, col) = rowAndCol
//        Position(row, col)
//      })
//    }
//    import ShipState._
//    def calcShipState(hitPositions: Array[Position]): ShipState = {
//      val diffPositions: Array[Position] = allPositions.diff(hitPositions)
//      if (diffPositions.length == 0) {
//        ShipState.Safe
//      } else if (diffPositions.length == allPositions.length)  {
//        ShipState.Sunk
//      } else {
//        ShipState.Hit
//      }
//    }
//  }
//}
//
//
//
//object Ship extends App {
//
//}
