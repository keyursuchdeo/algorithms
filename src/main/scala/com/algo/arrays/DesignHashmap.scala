package com.algo.arrays

object DesignHashmap extends App {
  class MyHashMap() {

    /** Initialize your data structure here. */
    val prime = 9973
    val array = Array.fill[Seq[(Int, Int)]](prime)(Nil)

    def findPosOfKey(key: Int): Int = key % prime


    /** value will always be non-negative. */
    def put(key: Int, value: Int) {
      val pos = findPosOfKey(key)
      @scala.annotation.tailrec
      def findAndReplace(currSeq: Seq[(Int, Int)], checkedSeq: Seq[(Int, Int)]): Seq[(Int, Int)] = {
        if(currSeq.isEmpty) {
          (key, value) +: checkedSeq
        } else if (currSeq.head._1 == key) {
          (key, value) +: (currSeq.tail ++ checkedSeq)
        } else {
          findAndReplace(currSeq.tail, currSeq.head +: checkedSeq)
        }
      }

      val seq = findAndReplace(array(pos), Nil)
      array(pos) = seq

    }

    /** Returns the value to which the specified key is mapped, or -1 if this map contains no mapping for the key */
    def get(key: Int): Int = {
      val pos = findPosOfKey(key)
      @scala.annotation.tailrec
      def findIn(currSeq: Seq[(Int, Int)]): Int = {
        if(currSeq.isEmpty) {
          -1
        } else if (currSeq.head._1 == key) {
          currSeq.head._2
        } else {
          findIn(currSeq.tail)
        }
      }
      findIn(array(pos))
    }

    /** Removes the mapping of the specified value key if this map contains a mapping for the key */
    def remove(key: Int) {
      val pos = findPosOfKey(key)
      @scala.annotation.tailrec
      def findAndRemove(currSeq: Seq[(Int, Int)], checkedSeq: Seq[(Int, Int)]): Seq[(Int, Int)] = {
        if(currSeq.isEmpty) {
          checkedSeq
        } else if (currSeq.head._1 == key) {
          currSeq.tail ++ checkedSeq
        } else {
          findAndRemove(currSeq.tail, currSeq.head +: checkedSeq)
        }
      }

      val seq = findAndRemove(array(pos), Nil)
      array(pos) = seq
    }

  }
}
