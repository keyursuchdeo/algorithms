package com.algo.trie

import scala.collection.mutable

object MainMagicDict extends App {
  val d = new MagicDictionary()
//  val a = Array("hello", "leetcode")
//  d.buildDict(a)
//  println(d.search("hello"))
//  println(d.search("hhllo"))
//  println(d.search("hell"))
//  println(d.search("leetcoded"))

//  val a = Array("hello", "held")
//  d.buildDict(a)
//  println(d.search("hello"))
//  println(d.search("hhllo"))
//  println(d.search("hell"))
//  println(d.search("help"))

//  val a = Array("a")
//  d.buildDict(a)
//  println(d.search("aa"))
//  println(d.search("a"))
//  println(d.search("b"))

//  val a = Array("hello", "hallo", "leetcode", "judge")
//  d.buildDict(a)
//  println(d.search("hello"))
//  println(d.search("hallo"))
//  println(d.search("leetcodd"))
//  println(d.search("judge"))

  val a = Array("a","b","ab","abc","abcabacbababdbadbfaejfoiawfjaojfaojefaowjfoawjfoawj","abcdefghijawefe","aefawoifjowajfowafjeoawjfaow","cba","cas","aaewfawi","babcda","bcd","awefj")
  d.buildDict(a)
  println(d.search("cba"))
}

class MagicDictionary() {

  /** Initialize your data structure here. */
  private case class TrieNode(endOfWord: Boolean, childCharNodes: Map[Char, TrieNode])

  private var root: Option[TrieNode] = None

  /** Build a dictionary through a list of words */
  def buildDict(dict: Array[String]) {
    dict.foreach(build)
  }

  private def build(word: String) {
    def buildTrieNode(index: Int, existingChild: Option[TrieNode]): TrieNode = {
      if (index == word.length) {
        existingChild match {
          case Some(c) => c.copy(endOfWord = true)
          case None => TrieNode(endOfWord = true, Map.empty)
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
//    println(root)
  }

  /** Returns if there is any word in the trie that equals to the given word after modifying exactly one character */
  def search(word: String): Boolean = {
    val stack: mutable.Stack[(Int, Int, Map[Char, TrieNode])] = new mutable.Stack[(Int, Int, Map[Char, TrieNode])]()

    @scala.annotation.tailrec
    def sr(index: Int, trieNode: TrieNode, mismatchCount: Int, charStack: mutable.Stack[(Int, Int, Map[Char, TrieNode])]): Boolean = {
      if (index == word.length) {
        if(mismatchCount == 1 && trieNode.endOfWord) {
          true
        } else if (mismatchCount == 1 && charStack.isEmpty) {
          trieNode.endOfWord
        } else {
          //backtrack
          if(charStack.nonEmpty) {
            val (backtrackToIndex, btMismatchCount, backTrackToChildNodes) = charStack.pop()
            val childNode = backTrackToChildNodes.head
            val remainingChildNodes: Map[Char, TrieNode] = backTrackToChildNodes - childNode._1
            val updatedCharStack =
              if (remainingChildNodes.nonEmpty) {
                charStack.push((backtrackToIndex, btMismatchCount, remainingChildNodes))
              } else {
                charStack
              }
            if(childNode._1 == word(backtrackToIndex)) {
              sr(backtrackToIndex + 1, childNode._2, btMismatchCount, updatedCharStack)
            } else {
              sr(backtrackToIndex + 1, childNode._2, btMismatchCount + 1, updatedCharStack)
            }
          } else {
            false
          }
        }
      } else {
        trieNode.childCharNodes.get(word(index)) match {
          case Some(childNode) =>
            //add remaining to backtracking stack, and continue
            val remainingChildNodes: Map[Char, TrieNode] = trieNode.childCharNodes - word(index)
            val updatedCharStack =
              if (remainingChildNodes.nonEmpty) {
                charStack.push((index, mismatchCount, remainingChildNodes))
              } else {
                charStack
              }
            sr(index + 1, childNode, mismatchCount, updatedCharStack)
          case None if mismatchCount == 0 =>
            val optChildNode = trieNode.childCharNodes.headOption
            optChildNode match {
              case Some(childNode) =>
                //add remaining to backtracking stack, and continue
                val remainingChildNodes: Map[Char, TrieNode] = trieNode.childCharNodes - childNode._1
                val updatedCharStack =
                  if (remainingChildNodes.nonEmpty) {
                    charStack.push((index, mismatchCount, remainingChildNodes))
                  } else {
                    charStack
                  }
                sr(index + 1, childNode._2, mismatchCount + 1, updatedCharStack)
              case None =>
                if(charStack.nonEmpty) {
                  val (backtrackToIndex, btMismatchCount, backTrackToChildNodes) = charStack.pop()
                  val childNode = backTrackToChildNodes.head
                  val remainingChildNodes: Map[Char, TrieNode] = backTrackToChildNodes - childNode._1
                  val updatedCharStack =
                    if (remainingChildNodes.nonEmpty) {
                      charStack.push((backtrackToIndex, btMismatchCount, remainingChildNodes))
                    } else {
                      charStack
                    }
                  if(childNode._1 == word(backtrackToIndex)) {
                    sr(backtrackToIndex + 1, childNode._2, btMismatchCount, updatedCharStack)
                  } else {
                    sr(backtrackToIndex + 1, childNode._2, btMismatchCount + 1, updatedCharStack)
                  }
                } else {
                  false
                }
            }
          // first mismatch, record mismatch, add remaining to backtracking stack, and continue
          case None =>
            if(charStack.nonEmpty) {
              val (backtrackToIndex, btMismatchCount, backTrackToChildNodes) = charStack.pop()
              val childNode = backTrackToChildNodes.head
              val remainingChildNodes: Map[Char, TrieNode] = backTrackToChildNodes - childNode._1
              val updatedCharStack =
                if (remainingChildNodes.nonEmpty) {
                  charStack.push((backtrackToIndex, btMismatchCount, remainingChildNodes))
                } else {
                  charStack
                }
              if(childNode._1 == word(backtrackToIndex)) {
                sr(backtrackToIndex + 1, childNode._2, btMismatchCount, updatedCharStack)
              } else {
                sr(backtrackToIndex + 1, childNode._2, btMismatchCount + 1, updatedCharStack)
              }
            } else {
              false
            }
          // more mismatches, abandon branch, backtrack
        }
      }
    }

    root match {
      case Some(r) => sr(0, r, 0, stack)
      case None => false
    }
  }

}

