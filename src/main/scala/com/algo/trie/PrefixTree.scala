package com.algo.trie

import scala.collection.mutable

object PrefixTree extends App {
  val trie = new Trie()
  println(trie.insert("apple"))
  println(trie.search("apple"))
  println(trie.search("app"))
  println(trie.startsWith("app"))
  println(trie.insert("app"))
  println(trie.search("app"))

}

class Trie() {

  /** Initialize your data structure here. */
  private var set = mutable.Set[String]()
  private var prefixSet = mutable.Set[String]()

  /** Inserts a word into the trie. */
  def insert(word: String) {

    @scala.annotation.tailrec
    def prepPrefixSet(index: Int): Unit = {
      if(index > word.length) {
        ()
      } else if(index == 0) {
        prepPrefixSet(index + 1)
      } else {
        prefixSet = prefixSet + word.substring(0, index)
        prepPrefixSet(index + 1)
      }
    }

    if(set.contains(word)) () else prepPrefixSet(1)
    set = set + word
  }

  /** Returns if the word is in the trie. */
  def search(word: String): Boolean = {
    set.contains(word)
  }

  /** Returns if there is any word in the trie that starts with the given prefix. */
  def startsWith(prefix: String): Boolean = {
    prefixSet.contains(prefix)
  }

}
