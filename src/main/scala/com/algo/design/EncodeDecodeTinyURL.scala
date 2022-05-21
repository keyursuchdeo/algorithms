package com.algo.design

object EncodeDecodeTinyURL extends App {
  class Codec {
    var map: Map[String, String] = Map[String, String]()

    private def generateUuid: String = java.util.UUID.randomUUID.toString
    private val baseUrl: String = "http://tinyurl.com/"
    private val baseUrlLen = baseUrl.length
    private val tinyUrlIdLen = 8

    // Encodes a URL to a shortened URL.
    def encode(longURL: String): String = {
      val uuid = generateUuid.take(tinyUrlIdLen)
      map = map + (uuid -> longURL)
      s"$baseUrl$uuid"
    }

    // Decodes a shortened URL to its original URL.
    def decode(shortURL: String): String = {
      val uuid = shortURL.drop(baseUrlLen)
      map(uuid)
    }
  }

  /**
   * Your Codec object will be instantiated and called as such:
   * var obj = new Codec()
   * val s = obj.encode(longURL)
   * val ans = obj.decode(s)
   */
}
