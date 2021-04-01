import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{log10, pow}

object TestTask extends App{
  def solution1(nums: Array[Int], target: Int) = {
    for (
      i <- nums.indices;
      j <- i + 1 until nums.length if nums(i) + nums(j) == target
    ) yield {
      (i, j)
    }
  }

  //========================================================================//

  def solution2(x: Int): Boolean = {
    def reverseNumber(y: Int): Int = {
      if (y < 10)
        y
      else
        (y % 10) * pow(10, log10(y).toInt).toInt + reverseNumber(y / 10)
    }

    if (reverseNumber(x) == x && x >= 0) true else false
  }

  //========================================================================//

  def solution3(s: String): Int = {
    // обрезаем строку от пробелов с концов, и разбиваем по " " для массива
    val str = (s.trim).split(' ')
    // берем последний элемент(слово) и возвращаем его длинну
    if (str.isEmpty)
      0
    else
      str(str.length-1).length
  }

  def solution33(s: String): Int = {
    val str = s.split(' ')
    if (str.length > 0) str.last.length
    else 0
  }

  def solution333(s: String): Int = {
    s.trim.split(" ").last.length
  }

  //========================================================================//

  def solution4(nums: Array[Int], target: Int): Int = {
    @tailrec
    def binarySearch(nums: Array[Int], key: Int, left: Int, right: Int): Int = {
      val mid = left + (right - left) / 2
      if (left >= right) -(1 + left)
      else if (nums(mid) == key) mid
      else if (nums(mid) > key) binarySearch(nums, key, left, mid)
      else binarySearch(nums, key, mid + 1, right)
    }

    val low = nums(0)
    val mid = nums((nums.length - 1) / 2)

    if (nums.isEmpty || nums.length == 1)
      return -1

    if(mid > low)
      nums.indexOf(binarySearch(nums, target, low, mid))
    else {
      0
    }
  }

  //========================================================================//
  def solution5[T](list: List[T]): List[List[List[T]]] = {
    list match {
      case Nil | _ :: Nil => // содержит пустой или 1 элемент
        List(List(list))
      case head :: tail => // содержит 2 и больше элементов
        solution5(tail).flatMap(part => {
          val joins =
            part.indices.map(i =>
              part.zipWithIndex.map { case (p, j) =>
                if (i == j) {
                  head +: p
                } else {
                  p
                }
              }
            )
          (List(head) +: part) +: joins
        })
    }
  }

  //========================================================================//

  case class ListNode(var x: Int = 0, var next: ListNode = null)

  def solution6(l1: ListNode, l2: ListNode): ListNode = {
    def add(l1: ListNode, l2: ListNode, carryDigit: Boolean): ListNode = {
      val sum: Int =
        (if (l1 == null) 0 else l1.x) +
          (if (l2 == null) 0 else l2.x) +
          (if (carryDigit) 1 else 0)

      val current: ListNode = new ListNode(sum % 10)

      if ((l1 != null && l1.next != null) || (l2 != null && l2.next != null) || sum >= 10) {
        current.next = add(
          l1 = if (l1 != null) l1.next else null,
          l2 = if (l2 != null) l2.next else null,
          carryDigit = sum >= 10
        )
      }

      current
    }

    add(l1, l2, carryDigit = false)
  }

  //========================================================================//

  def solution7(nums: Array[Int]): Boolean = {
    var maxReach = 0 //максимальный возможный прыг до конца массива

    for (i <- nums.indices) {
      // вернем false если прыгнули дальше.
      if (i > maxReach) return false
      //сравниваем текущий шаг с супер прыгом и присваиваем в суперпрыг
      maxReach = Math.max(maxReach, i + nums(i) )
    }
    true
  }

  //========================================================================//

  def solution8(s: String): Int = {
    if (s == null || s.isEmpty) 0
    else {
      val map = new scala.collection.mutable.HashMap[Char, Int]()
      var maxLength = 0
      var start = 0

      (0 until s.length).foreach { i =>
        if (map.contains(s(i)) && start <= map(s(i))) {
          start = map(s(i)) + 1
        }

        val newLength = i - start + 1

        if (newLength > maxLength) {
          maxLength = newLength
        }

        map(s(i)) = i
      }

      maxLength
    }
  }

  //========================================================================//

  abstract class Pattern
  case class SingleChar(c: Char) extends Pattern
  case class MultiChar(c: Char) extends Pattern
  case object Wildcard extends Pattern
  case object MultiWildcard extends Pattern

  def solution9(s: String, p: String): Boolean = {
    def constructPattern (p: Seq[Char]) : Seq[Pattern] = {
      p match {
        case Seq() => Seq()
        case Seq('.', '*', xs@_*) => MultiWildcard +: constructPattern(xs)
        case Seq(c, '*', xs@_*) => MultiChar(c) +: constructPattern(xs)
        case Seq('.', xs@_*) => Wildcard +: constructPattern(xs)
        case Seq(c, xs@_*) => SingleChar(c) +: constructPattern(xs)
      }
    }

    val pattern = constructPattern(p).toArray
    val cache : Array[Array[Option[Boolean]]] = Array.fill (s.length + 1, pattern.length + 1) (None)

    def go(i: Int, j: Int) : Boolean = {
      cache(i)(j) match {
        case Some(result) => result
        case None =>
          val result =
            if (i == s.length && j == pattern.length) {
              true
            } else if (i == s.length) {
              pattern(j) match {
                case MultiWildcard => go(i, j + 1)
                case MultiChar(_) => go(i, j + 1)
                case Wildcard => false
                case SingleChar(_) => false
              }
            } else if (j == pattern.length) {
              false
            } else {
              pattern(j) match {
                case SingleChar(c) => (c == s(i)) && go(i + 1, j + 1)
                case Wildcard => go(i + 1, j + 1)
                case MultiChar(c) => if (c == s(i)) (go(i + 1, j) || go(i, j + 1)) else go(i, j + 1)
                case MultiWildcard => go(i + 1, j) || go(i, j + 1)
              }
            }
          cache (i)(j) = Some(result)
          result
      }
    }
    go(0, 0)
  }

  //========================================================================//

  def solution99(str: String, pattern: String): Boolean = {
    //Проверяем пустоту
    if (pattern.isEmpty) str.isEmpty
    //
    else if (str.nonEmpty && (str.head == pattern.head || pattern.head == '.')) {
      if (pattern.length > 1 && pattern.tail.head == '*')
        solution99(str.tail, pattern) || solution99(str, pattern.tail.tail)
      else
        solution99(str.tail, pattern.tail)
    }
    else if (pattern.length > 1 && pattern.tail.head == '*') solution99(str, pattern.tail.tail)
    else false
  }

  //========================================================================//

  def solution10(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    var wei = matrix.length
    var hei = matrix(0).length
    var value1 = 0
    // транспонируем
    for (r <- 0 until wei){
      for (c <- r until hei){
        value1 = matrix(c)(r);
        matrix(c)(r) = matrix(r)(c)
        matrix(r)(c) = value1
      }
    }

    // реверсируем
    var value = 0
    for (r <- 0 until wei){
      for (c <- 0 until hei/2){
        value = matrix(r)(c)
        matrix(r)(c) = matrix(r)(hei-c-1)
        matrix(r)(hei-c-1) = value
      }
    }
    matrix
  }

  def solution101(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    matrix.reverse.transpose
    }

  //========================================================================//

  def solution11(list: List[Int]): List[List[Int]] = {
    //
    if (list.size == 1 || list.isEmpty) List(list)
    else for {
      x <- list
      y <- solution11(list.filterNot(_ == x))
    } yield x :: y
  }

  //========================================================================//

  def solution120(s: String): Boolean = {
    s match {
      case s if Option(s).isEmpty => false
      case s if s.trim.isEmpty => false
      case s =>
        val str = s.trim
        var digit = false
        var dot = false
        var sign = false
        var e = false
        for (c <- str) {
          if (c == ' ') {
            return false
          }
          if (c == 'e' && e || !digit) {
            return false
          }
          if (c == '.' && dot || e) {
            return false
          }
          if ((c == '+' || c == '-') && sign || digit || dot) {
            return false
          }
          if (c >= '0' && c <= '9') {
            digit = true
          } else if (c == '+' || c == '-') {
            sign = true
          } else if (c == '.') {
            dot = true
          } else if (c == 'e') {
            e = true
            digit = false
            dot = false
            sign = false
          } else {
            return false
          }
        }
        if (e && !digit) {
          return false
        }
        digit
    }
  }

  def solution121(s: String): Boolean = s.matches("\\s*[+-]?(\\d+(\\.\\d*)?|\\.\\d+)(e[+-]?\\d+)?\\s*")

  def solution122(s: String): Boolean = {
    val nPattern = "^([-+]?[0-9]*((?<=[0-9]+)\\.?|\\.?(?=[0-9]+))[0-9]*)?((?<=[0-9.]+)[eE][-+]?[0-9]+)?$"
    val sNew = s.trim
    if (sNew.isEmpty) false
    else sNew.matches(nPattern)
  }

  //========================================================================//

  def solution14(s: String, wordDict: List[String]): List[String] = {
    def breakingWord(str: String, m: mutable.Map[String, List[String]]): List[String] = str match {
      case str if m.contains(str) => m(str)
      case str if str.isEmpty => List("")
      case str =>
        val ans = mutable.ListBuffer[String]()
        for(word <- wordDict) {
          if(str.startsWith(word)) {
            val tmp = breakingWord(str.substring(word.length), m)
            for(sub <- tmp) {
              val space = if(sub.isEmpty) "" else " "
              ans.addOne(word+ space + sub)
            }
          }
        }
        m.put(str, ans.toList)
        ans.toList
    }
    breakingWord(s, mutable.Map[String, List[String]]())
  }
}
