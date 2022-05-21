package com.algo.design

import java.util
import java.util.concurrent.ConcurrentHashMap

object MainLRUCache extends App {
  val cache = new LRUCache(2 /* capacity */)

//  cache.put(1, 1)
//  cache.put(2, 2)
//  println(cache.get(1)) // returns 1
//
//  cache.put(3, 3) // evicts key 2
//
//  println(cache.get(2)) // returns -1 (not found)
//
//  cache.put(4, 4) // evicts key 1
//
//  println(cache.get(1)) // returns -1 (not found)
//  println(cache.get(3)) // returns 3
//
//  println(cache.get(4)) // returns 4

//  cache.put(2, 1)
//  cache.put(2, 2)
//  println(cache.get(2))
//  cache.put(1, 1)
//  cache.put(4, 1)
//  println(cache.get(2))

  val cache1 = new LRUCache(2 /* capacity */)

  println(cache1.put(2, 1))
  println(cache1.put(2, 2))
  println(cache1.get(2))
  println(cache1.put(1, 1))
  println(cache1.put(4, 1))
  println(cache1.get(2))
}

class LRUCache(_capacity: Int) {

  private var keyValMap: ConcurrentHashMap[Int, Int] = new ConcurrentHashMap[Int, Int]()
  private var keyTsMap: ConcurrentHashMap[Int, Long] = new ConcurrentHashMap[Int, Long]()
  private var count = 0

  def get(key: Int): Int = {
    println(keyValMap)
    Option(keyValMap.get(key)) match {
      case Some(v) =>
        keyTsMap.put(key, System.nanoTime())
        v
      case _ => -1
    }
  }

  def put(key: Int, value: Int): Unit = {
    Option(keyValMap.get(key)) match {
      case Some(_) =>
        keyTsMap.put(key, System.nanoTime())
        keyValMap.put(key, value)
      case None =>
        if (count == _capacity) {
          evictLruKey()
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

  private def evictLruKey(): Unit = {
    val lruKey = findLruKey()
    keyValMap.remove(lruKey)
    keyTsMap.remove(lruKey)
    count = count - 1
  }

  private def findLruKey(): Int = {
    val iterator: util.Enumeration[Int] = keyTsMap.keys()

    @scala.annotation.tailrec
    def find(currMinTsKey: Int, currMinTs: Long): Int = {
      if(iterator.hasMoreElements) {
        val key = iterator.nextElement()
        val ts = keyTsMap.get(key)
        if(ts < currMinTs) {
          find(key, ts)
        } else {
          find(currMinTsKey, currMinTs)
        }
      } else {
        currMinTsKey
      }
    }
    find(-1, Long.MaxValue)
  }

}

/**
 * Your LRUCache object will be instantiated and called as such:
 * var obj = new LRUCache(capacity)
 * var param_1 = obj.get(key)
 * obj.put(key,value)
 */
