object RansomNote:

  import scala.util.boundary, boundary.break
  object Solution {
    def canConstruct(ransomNote: String, magazine: String): Boolean =
      val shelf = Array.fill(26)(0)
      magazine
        .toCharArray()
        .foreach(c => shelf(c - 'a') += 1)
      boundary:
        ransomNote.foreach: c =>
          if shelf(c - 'a') > 0 then shelf(c - 'a') -= 1
          else break(false)
        true
  }

@main def randomNoteMain =
  import RansomNote.*
  println(Solution.canConstruct("", "b"))
  println(Solution.canConstruct("a", "b"))
  println(Solution.canConstruct("aa", "ab"))
  println(Solution.canConstruct("aa", "aba"))

