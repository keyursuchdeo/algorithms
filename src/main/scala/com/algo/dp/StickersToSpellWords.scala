package com.algo.dp

import scala.collection.mutable

object StickersToSpellWords extends App {

  val stickers = Array("control","heart","interest","stream","sentence","soil","wonder","them","month","slip")
  val target = "stoodcrease"
//  val stickers = Array("notice", "possible")
//  val target = "basicbasic"
//  val stickers = Array("with", "example", "science")
//  val target = "thehat"
  val res = Solution.minStickers(stickers, target)
  println(res)

  object Solution {
    def minStickers(stickers: Array[String], target: String): Int = {
      var indexMinCount: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
      def find(usedStickers: Set[String], count: Int, index: Int): Int = {
        println(s"index $index count $count usedStickers $usedStickers")
        indexMinCount = indexMinCount + (index - 1 -> count)
        if (index == target.length) {
          //println(count)
          count
        } else {
          indexMinCount.get(index) match {
            case Some(minCount) if minCount < count =>
              //println(minCount)
              minCount
            case _ =>
              val c = target(index)
              stickerContainingChar(c, usedStickers) match {
                case Some((usedSticker, cIndex)) =>
                  val (before, after) = usedSticker.splitAt(cIndex)
                  val updatedUsedSticker = s"$before${after.tail}"
                  val a = find(usedStickers - usedSticker + updatedUsedSticker, count, index + 1)
                  //println(a)
                  a
                case None =>
                  val stickersContainingChar: Array[String] = stickers.filter(_.contains(c))
                  if(stickersContainingChar.isEmpty) {
                    -1
                  } else {
                    val a = stickersContainingChar.map(s => {
                      val indexOfC = s.indexOf(c)
                      val (before, after) = s.splitAt(indexOfC)
                      val updatedNewSticker = s"$before${after.tail}"
                      find(usedStickers + updatedNewSticker, count + 1, index + 1)
                    }).min
                    println(a)
                    a
                  }
              }
          }
        }
      }

      def stickerContainingChar(c: Char, stickers: Set[String]): Option[(String, Int)] = {
        stickers.find(_.contains(c)) match {
          case Some(sticker) =>
            Some(sticker, sticker.indexOf(c))
          case None => None
        }
      }

      val a = find(Set(), 0, 0)
      println(indexMinCount)
      a
    }
  }
}
