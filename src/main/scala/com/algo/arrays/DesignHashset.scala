package com.algo.arrays

object DesignHashset extends App {
  class MyHashSet() {

    /** Initialize your data structure here. */
    val prime = 7919
    var map: Array[Seq[Int]] = Array.fill[Seq[Int]](prime)(Nil)

    def add(key: Int) {
      val hash = key % prime
      val values = map(hash)
      if(values.contains(key)) {
        ()
      } else {
        map(hash) = key +: map(hash)
      }
    }

    def remove(key: Int) {
      val hash = key % prime
      val values: Seq[Int] = map(hash)

      @scala.annotation.tailrec
      def removeKey(pre: Vector[Int], post: Seq[Int]): Unit = {
        if(post.isEmpty) {
          ()
        } else {
          if(post.head == key) {
            map(hash) = pre ++ post.tail
          } else {
            removeKey(pre :+ post.head, post.tail)
          }
        }
      }

      removeKey(Vector(), values)
    }

    /** Returns true if this set contains the specified element */
    def contains(key: Int): Boolean = {
      val hash = key % prime
      map(hash).contains(key)
    }
  }

  /**
   * Your MyHashSet object will be instantiated and called as such:
   * var obj = new MyHashSet()
   * obj.add(key)
   * obj.remove(key)
   * var param_3 = obj.contains(key)
   */
}
