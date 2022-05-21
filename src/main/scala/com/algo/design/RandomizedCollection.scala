package com.algo.design

import scala.util.Random

object MainRandomizedCollection extends App {

  val collection = new RandomizedCollection
  collection.insert(0)
  collection.insert(1)
  collection.remove(0)
  collection.insert(2)
  collection.remove(1)
  println(collection.getRandom())



//  // Inserts 1 to the collection. Returns true as the collection did not contain 1.
//  println(collection.insert(1))
//
//  // Inserts another 1 to the collection. Returns false as the collection contained 1. Collection now contains [1,1].
//  println(collection.insert(1))
//
//  // Inserts 2 to the collection, returns true. Collection now contains [1,1,2].
//  println(collection.insert(2))
//
//  // getRandom should return 1 with the probability 2/3, and returns 2 with the probability 1/3.
//  println(collection.getRandom())
//
//  // Removes 1 from the collection, returns true. Collection now contains [1,2].
//  println(collection.remove(1))
//
//  // getRandom should return 1 and 2 both equally likely.
//  println(collection.getRandom())
}

class RandomizedCollection() {

  /** Initialize your data structure here. */
  private var map: Map[Int, Seq[Int]] = Map.empty
  private var vec: Vector[Int] = Vector()
  private var count = 0

  /** Inserts a value to the collection. Returns true if the collection did not already contain the specified element. */
  def insert(`val`: Int): Boolean = {
    map.get(`val`) match {
      case Some(indexSeq) =>
        vec = vec :+ `val`
        map = map + (`val` -> (count +: indexSeq))
        count = count + 1
        false
      case None =>
        vec = vec :+ `val`
        map = map + (`val` -> (count +: Nil))
        count = count + 1
        true
    }
  }

  /** Removes a value from the collection. Returns true if the collection contained the specified element. */
  def remove(`val`: Int): Boolean = {
    map.get(`val`) match {
      case Some(indexSeq) =>
        count = count - 1
        val indexToRemove = indexSeq.head
        vec = vec.patch(indexToRemove, Nil, 1)
        indexSeq.tail match {
          case Nil => map = map - `val`
          case xs => map = map + (`val` -> (count +: xs))
        }
        true
      case None =>
        false
    }
  }

  /** Get a random element from the collection. */
  def getRandom(): Int = {
    val randomIndex = Random.nextInt(count)
    vec(randomIndex)
  }

}
