package com.algo.trie


object PrefixTree2 extends App {
  val trie = new Trie2()
  println(trie.insert("apple"))
  println(trie.search("apple"))
  println(trie.search("app"))
  println(trie.startsWith("app"))
  println(trie.insert("app"))
  println(trie.search("app"))
  println(trie.search("apple"))

//  println(trie.insert("app"))
//  println(trie.insert("apple"))
//  println(trie.search("app"))
//  println(trie.search("apple"))
}

class Trie2() {

  /** Initialize your data structure here. */
  private case class TrieNode(endOfWord: Boolean, childCharNodes: Map[Char, TrieNode])
  private var root: Option[TrieNode] = None


  /** Inserts a word into the trie. */
  def insert(word: String): Unit = {
    def buildTrieNode(index: Int, existingChild: Option[TrieNode]): TrieNode = {
      if(index == word.length) {
        existingChild match {
          case Some(c) =>  c.copy(endOfWord = true)
          case None =>  TrieNode(endOfWord = true, Map.empty)
        }
      } else if (index == 0) {
        root match {
          case Some(r) =>
            val node = r.childCharNodes.get(word(index))
            r.copy(childCharNodes = r.childCharNodes + (word(index) -> buildTrieNode(index + 1, node)))
          case None =>
            TrieNode(endOfWord = false, Map(word(index) -> buildTrieNode(index + 1, existingChild)))
        }
      } else {
        val node = existingChild.flatMap(_.childCharNodes.get(word(index)))
        existingChild match {
          case Some(c) =>
            c.copy(childCharNodes = c.childCharNodes + (word(index) -> buildTrieNode(index + 1, node)))
          case None =>
            TrieNode(endOfWord = false, Map(word(index) -> buildTrieNode(index + 1, node)))
        }
      }
    }

    root = Option(buildTrieNode(0, None))
    //println(root)
  }

  /** Returns if the word is in the trie. */
  def search(word: String): Boolean = {

    @scala.annotation.tailrec
    def sr(index: Int, trieNode: TrieNode): Boolean = {
      if(index == word.length) {
        trieNode.endOfWord
      } else {
        trieNode.childCharNodes.get(word(index)) match {
          case Some(childNode) => sr(index + 1, childNode)
          case None => false
        }
      }
    }

    root match {
      case Some(r) => sr(0, r)
      case None => false
    }
  }

  /** Returns if there is any word in the trie that starts with the given prefix. */
  def startsWith(prefix: String): Boolean = {
    @scala.annotation.tailrec
    def sr(index: Int, trieNode: TrieNode, found: Boolean): Boolean = {
      if(index == prefix.length) {
        found
      } else {
        trieNode.childCharNodes.get(prefix(index)) match {
          case Some(childNode) => sr(index + 1, childNode, found = true)
          case None => false
        }
      }
    }

    root match {
      case Some(r) => sr(0, r, false)
      case None => false
    }
  }

}
