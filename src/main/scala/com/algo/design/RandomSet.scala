package com.algo.design

import scala.util.Random

object RandomSet extends App {

  val commands = Array("RandomizedSet","insert","remove","insert","getRandom","remove","insert","getRandom")
  val commandPayloads = "[],[1],[2],[2],[],[1],[2],[]"

  val rs = new RandomizedSet()
  println(rs.insert(1))
  println(rs.remove(2))
  println(rs.insert(2))
  println(rs.getRandom())
  println(rs.remove(1))
  println(rs.insert(2))
  println(rs.getRandom())




  class RandomizedSet() {

    /** Initialize your data structure here. */
    private var randomizedSet = Set[Int]()
    private var randomList = Vector[Int]()
    private var setSize = 0
    private var listSize = 0

    /** Inserts a value to the set. Returns true if the set did not already contain the specified element. */
    def insert(`val`: Int): Boolean = {
      if(randomizedSet.contains(`val`)) {
        false
      } else {
        randomizedSet = randomizedSet + `val`
        randomList = `val` +: randomList
        setSize = setSize + 1
        listSize = listSize + 1
        true
      }
    }

    /** Removes a value from the set. Returns true if the set contained the specified element. */
    def remove(`val`: Int): Boolean = {
      if(randomizedSet.contains(`val`)) {
        randomizedSet = randomizedSet - `val`
        setSize = setSize - 1
        true
      } else {
        false
      }
    }

    /** Get a random element from the set. */
    def getRandom(): Int = {
      @scala.annotation.tailrec
      def findRandomElement(): Int = {
        val randomIndex = Random.nextInt(listSize)
        val randomElement = randomList(randomIndex)
        if(randomizedSet.contains(randomElement)) {
          randomElement
        } else {
          randomList = removeFromList(randomIndex)
          listSize = listSize - 1
          findRandomElement()
        }
      }

      findRandomElement()

    }

    private def removeFromList(index: Int): Vector[Int] = {
      val take = randomList.take(index)
      val drop = randomList.drop(index + 1)
      take ++ drop
    }

  }

}
