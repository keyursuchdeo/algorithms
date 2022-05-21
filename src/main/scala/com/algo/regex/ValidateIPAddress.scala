package com.algo.regex

object ValidateIPAddress extends App {
  object Solution {
    def validIPAddress(IP: String): String = {
      val ipv4SegPattern: String = "\\b([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\b"
      val ipv4Pattern = s"($ipv4SegPattern(\\.)){3}($ipv4SegPattern)"

      val ipv6SegPattern: String = "\\b([0-9]|[a-f]|[A-F]){1-4}\\b"
      val ipv6Pattern = s"($ipv6SegPattern(:)){7}($ipv6SegPattern)"

      if(IP.matches(ipv4Pattern)) {
        "IPv4"
      } else if (IP.matches(ipv6Pattern)) {
        "IPv6"
      } else {
        "Neither"
      }

    }

//    def validIPAddress(IP: String): String =  IP match {
//      case ipv4(_*) => "IPv4"
//      case ipv6(_*) => "IPv6"
//      case _ => "Neither"
//    }
  }
}
