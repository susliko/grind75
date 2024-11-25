object LongestSubst:
  import scala.util.boundary, boundary.break


  object Solution:
    def lengthOfLongestSubstring(s: String) =
      val chars = s.toCharArray
      val lastInd = scala.collection.mutable.Map.empty[Char, Int]
      var max = 0
      var start = 0
      (0 to chars.length - 1).foreach: i =>
        val c = chars(i)
        lastInd
          .get(c)
          .foreach: j =>
            start = start.max(j + 1)
        max = max.max(i - start + 1)
        lastInd.update(c, i)
      val opt = chars.length - start
      max

@main def longestsubstr =
  import LongestSubst.*
  println(Solution.lengthOfLongestSubstring("abcabcbb"))
  println(Solution.lengthOfLongestSubstring("bbbbb"))
  println(Solution.lengthOfLongestSubstring("pwwkewks"))
  println(Solution.lengthOfLongestSubstring(" "))
  println(Solution.lengthOfLongestSubstring("au"))
  println(Solution.lengthOfLongestSubstring("cdd"))
  println(Solution.lengthOfLongestSubstring("cddc"))
  println(Solution.lengthOfLongestSubstring("aabaab!bb"))
  println(Solution.lengthOfLongestSubstring("tmmzuxt"))
  println(Solution.lengthOfLongestSubstring("abba"))
  println(Solution.lengthOfLongestSubstring("abba"))

