object Palindrome:
  import scala.util.boundary, boundary.break
  import scala.annotation.tailrec
  object Solution:
    def longestPalindrome(s: String): String =
      val chars = s.toCharArray()
      var max = ""
      (0 until chars.length).foreach: i =>
        val (oddL, oddR, oddS) = expand(i, i, chars)
        val (evenL, evenR, evenS) = expand(i, i + 1, chars)
        if oddS > max.size then max = s.substring(oddL, oddR + 1)
        if evenS > max.size then max = s.substring(evenL, evenR + 1)
      max

    @tailrec
    def expand(l: Int, r: Int, chars: Array[Char]): (Int, Int, Int) =
      if l >= 0 && r < chars.length && chars(l) == chars(r) then
        expand(l - 1, r + 1, chars)
      else (l + 1, r - 1, r - l - 1)

@main def palindromeMain =
  import Palindrome.*
  println(Solution.longestPalindrome(""))
  println(Solution.longestPalindrome("a"))
  println(Solution.longestPalindrome("aaaa"))
  println(Solution.longestPalindrome("aaaaa"))
  println(Solution.longestPalindrome("babad"))
  println(Solution.longestPalindrome("cbbd"))
  println(Solution.longestPalindrome("fooffffoof"))
