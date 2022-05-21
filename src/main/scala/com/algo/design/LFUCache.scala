package com.algo.design

import scala.collection.mutable

object MainLFUCache extends App {

//  val cache = new LFUCache(2 /* capacity */)
//
//  cache.put(1, 1)
//  cache.put(2, 2)
//  println(cache.get(1)) // returns 1
//
//  cache.put(3, 3) // evicts key 2
//
//  println(cache.get(2)) // returns -1 (not found)
//
//  println(cache.get(3)) // returns 3.
//
//  cache.put(4, 4) // evicts key 1.
//
//  println(cache.get(1)) // returns -1 (not found)
//  println(cache.get(3)) // returns 3
//
//  println(cache.get(4)) // returns 4

  val cache = new LFUCache(0 /* capacity */)
  cache.put(0, 0)
  println(cache.get(0))

}

class LFUCache(_capacity: Int) {

  private var keyValMap: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  private var keyFreqMap: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  private var keyTsMap: mutable.Map[Int, Long] = mutable.Map[Int, Long]()
  private var count = 0

  def get(key: Int): Int =
    keyValMap.get(key) match {
      case Some(v) =>
        keyFreqMap = keyFreqMap + (key -> (keyFreqMap.getOrElse(key, 0) + 1))
        keyTsMap = keyTsMap + (key -> System.nanoTime())
        v
      case _ => -1
    }


  def put(key: Int, value: Int): Unit = {
    if(_capacity == 0) {
      ()
    } else {
      keyValMap.get(key) match {
        case Some(_) =>
          keyFreqMap = keyFreqMap + (key -> (keyFreqMap.getOrElse(key, 0) + 1))
          keyTsMap.put(key, System.nanoTime())
          keyValMap.put(key, value)
        case None =>
          if (count == _capacity) {
            evictLfuKey()
            count = count + 1
            keyTsMap.put(key, System.nanoTime())
            keyValMap.put(key, value)
          } else {
            count = count + 1
            keyTsMap.put(key, System.nanoTime())
            keyValMap.put(key, value)
          }
      }
    }
  }

  private def evictLfuKey(): Unit = {
    val (key, freq) = findLfuKeyFreq()
    val keysOfFreq = findAllKeysOfFreq(freq)
    val keyToEvict = if (keysOfFreq.length == 1) key else findLruKey(keysOfFreq)
    keyValMap = keyValMap - keyToEvict
    keyFreqMap = keyFreqMap - keyToEvict
    keyTsMap = keyTsMap - keyToEvict
    count = count - 1

  }

  private def findLfuKeyFreq(): (Int, Int) =
    keyFreqMap.minBy(_._2)

  private def findAllKeysOfFreq(freq: Int): Array[Int] =
    keyFreqMap.filter(_._2 == freq).keys.toArray

  private def findLruKey(keys: Array[Int]): Int = {
    keyTsMap.filter(kTs => {
      val (k, _) = kTs
      keys.contains(k)
    }).minBy(_._2)._1
  }

}
