object Atoi:
  case class Foo(a: Int, b: String)

  /*
  Overflow check!!
  result > (Int.MaxValue - digit) / 10
   */
  import scala.util.boundary, boundary.break

  object Solution:
    def myAtoi(s: String): Int =
      val chars = s.trim.toCharArray
      if chars.isEmpty || !List('+', '-').contains(chars(0)) && !chars(
          0
        ).isDigit
      then return 0
      if chars.length == 1 && chars(0).isDigit then return chars(0).asDigit

      val accSign = if chars(0) == '-' then -1 else 1
      val iStart: Int =
        if chars(0).isDigit then 0
        else 1

      // println(s"Sign is $accSign. iStart is $iStart")
      var acc = 0
      val len = boundary:
        for i <- iStart until chars.length do
          if !chars(i).isDigit then break(i - iStart)
        chars.length - iStart
      // println(s"len is $len, istart: $iStart")
      for i <- (iStart until iStart + len).reverse do
        val c = chars(i)
        // println(s"Got $c. I is $i")
        val pow = len - (i - iStart) - 1
        val toAdd =
          if pow > 8 && c.asDigit > 2 then -1
          else
            c.asDigit * Math
              .pow(10, pow)
              .toInt
        val newAcc =
          if toAdd >= 0 then
            // println(Math.abs(acc))
            // println(Math.abs(acc) + toAdd)
            // println(s"Here ${accSign * (toAdd + Math.abs(acc))}")
            accSign * (toAdd + Math.abs(acc))
          else if accSign < 0 then Int.MinValue
          else Int.MaxValue
        // println(s"ToAdd: $toAdd, NewAcc $newAcc")
        if acc < 0 && newAcc > acc then acc = Int.MinValue
        else if acc > 0 && newAcc < acc then acc = Int.MaxValue
        else acc = newAcc
        // println(acc)
      acc

@main def atoiMain =
  import Atoi.*

  assert(Solution.myAtoi(".1") == 0)
  assert(Solution.myAtoi("") == 0)
  assert(Solution.myAtoi("42") == 42)
  assert(Solution.myAtoi("   -42") == -42)
  assert(Solution.myAtoi("4193 with words") == 4193)
  assert(Solution.myAtoi("0-1") == 0)
  assert(Solution.myAtoi("words and 987") == 0)
  assert(Solution.myAtoi("-91283472332") == -2147483648)
  assert(Solution.myAtoi("2147483648") == 2147483647)
  assert(Solution.myAtoi("2147483646") == 2147483646)
  assert(Solution.myAtoi("-2147483649") == -2147483648)
  assert(Solution.myAtoi("-6147483648") == -2147483648)
  assert(Solution.myAtoi("20000000000000000000") == 2147483647)
  assert(Solution.myAtoi("-042") == -42)
  assert(Solution.myAtoi("+1") == 1)
  assert(Solution.myAtoi("+-12") == 0)
  assert(Solution.myAtoi("-2147483647") == -2147483647)
