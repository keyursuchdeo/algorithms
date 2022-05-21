package com.algo.dp

object StickersToSpellWords2 extends App {

//  val stickers = Array("control","heart","interest","stream","sentence","soil","wonder","them","month","slip","table","miss","boat","speak","figure","no","perhaps","twenty","throw","rich","capital","save","method","store","meant","life","oil","string","song","food","am","who","fat","if","put","path","come","grow","box","great","word","object","stead","common","fresh","the","operate","where","road","mean")
//  val target = "stoodcrease"
//  val stickers = Array("notice", "possible")
//  val target = "basicbasic"
  val stickers = Array("with", "example", "science")
  val target = "thehat"
  val res = Solution.minStickers(stickers, target)
  println(res)

  object Solution {
    def minStickers(stickers: Array[String], target: String): Int = {

      def find(usedStickerCharCount: Map[Char, Int], count: Int, index: Int): Int = {
        if (index == target.length) {
          count
        } else {
          val c = target(index)
          usedStickerCharCount.get(c) match {
            case Some(count) if count > 1 =>
              find(usedStickerCharCount + (c -> (count - 1)), count, index + 1)
            case Some(count) if count == 1 =>
              find(usedStickerCharCount - c, count, index + 1)
            case None =>
              val stickersContainingChar: Array[String] = stickers.filter(_.contains(c))
              if(stickersContainingChar.isEmpty) {
                -1
              } else {
                stickersContainingChar.map(s => {
                  val indexOfC = s.indexOf(c)
                  val (before, after) = s.splitAt(indexOfC)
                  val m1 = before.map(bc => {
                    bc -> (usedStickerCharCount.getOrElse(bc, 0) + 1)
                  })
                  val m2 = after.tail.map(ac => {
                    ac -> (usedStickerCharCount.getOrElse(ac, 0) + 1)
                  })
                  find(usedStickerCharCount ++ m1 ++ m2, count + 1, index + 1)
                }).min
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

      find(Map(), 0, 0)
    }
  }
}
