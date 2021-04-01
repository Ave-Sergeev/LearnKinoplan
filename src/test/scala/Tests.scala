import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.wordspec.AnyWordSpecLike
import TestTask._

class Tests extends AnyWordSpecLike {
  "-------Check Task 1-------" should {
    "Samples from task definition" in {
      TestTask.solution1(Array(2, 7, 11, 15), 9) mustBe List((0, 1))
      TestTask.solution1(Array(3, 2, 4), 6) mustBe List((1, 2))
      TestTask.solution1(Array(3, 3), 6) mustBe List((0, 1))
    }
  }

  "-------Check Task 2-------" should {
    "Samples from task definition" in {
      TestTask.solution2(121) mustBe true
      TestTask.solution2(-121) mustBe false
      TestTask.solution2(10) mustBe false
      TestTask.solution2(-101) mustBe false
    }
  }

  "-------Check Task 3-------" should {
    "Samples from task definition" in {
      TestTask.solution3("Hello World") mustBe 5
      TestTask.solution3("") mustBe 0
    }
  }

  "-------Check Task 4-------" should {
    "Samples from task definition" in {
      TestTask.solution4(Array(4, 5, 6, 7, 0, 1, 2), 0) mustBe 4
      TestTask.solution4(Array(4, 5, 6, 7, 0, 1, 2), 3) mustBe -1
      TestTask.solution4(Array(1), 0) mustBe -1
    }
  }

  "-------Check Task 5-------" should {
    "Samples from task definition" in {
      TestTask.solution5(List(1, 2, 3)) mustBe List(List(List(1), List(2), List(3)), List(List(1, 2), List(3)), List(List(2), List(1, 3)), List(List(1), List(2, 3)), List(List(1, 2, 3)))
    }
  }

  "-------Check Task 6-------" should {
    "Samples from task definition" in {
      TestTask.solution6(ListNode(2, ListNode(4, ListNode(3))), ListNode(5, ListNode(6, ListNode(4)))) mustBe ListNode(7,ListNode(0,ListNode(8,null)))
      TestTask.solution6(ListNode(), ListNode()) mustBe ListNode(0, null)
      TestTask.solution6(ListNode(9, ListNode(9, ListNode(9, ListNode(9,ListNode(9,ListNode(9, ListNode(9))))))), ListNode(9, ListNode(9, ListNode(9, ListNode(9))))) mustBe ListNode(8,ListNode(9,ListNode(9,ListNode(9,ListNode(0,ListNode(0,ListNode(0,ListNode(1,null))))))))
    }
  }

  "-------Check Task 7-------" should {
    "Samples from task definition" in {
      TestTask.solution7(Array(2, 3, 1, 1, 4)) mustBe true
      TestTask.solution7(Array(3, 2, 1, 0, 4)) mustBe false
      TestTask.solution7(Array(3, 2, 3, 0, 0, 0)) mustBe true
    }
  }


  "-------Check Task 8-------" should {
    "Samples from task definition" in {
      TestTask.solution8("abcabcbb") mustBe 3
      TestTask.solution8("bbbbb") mustBe 1
      TestTask.solution8("pwwkew") mustBe 3
      TestTask.solution8 ("") mustBe 0
    }
  }

  "-------Check Task 9-------" should {
    "Samples from task definition" in {
      TestTask.solution99("aa", "a") mustBe false
      TestTask.solution99("aa", "a*") mustBe true
      TestTask.solution99("ab", ".*") mustBe true
      TestTask.solution99("aab", "c*a*b") mustBe true
      TestTask.solution99("mississippi", "mis*is*p*.") mustBe false
    }
  }

  "-------Check Task 10-------" should {
    "Samples from task definition" in {
      TestTask.solution10(Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9))) mustBe Array(
        Array(7, 4, 1),
        Array(8, 5, 2),
        Array(9, 6, 3))
    }
  }


  "-------Check Task 11-------" should {
    "Samples from task definition" in {
      TestTask.solution11(List(1, 2, 3)) mustBe List(
        List(1, 2, 3),
        List(1, 3, 2),
        List(2, 1, 3),
        List(2, 3, 1),
        List(3, 1, 2),
        List(3, 2, 1)
      )
      TestTask.solution11(List(1, 2)) mustBe List(
        List(1, 2),
        List(2, 1)
      )
    }
  }


  "-------Check Task 12-------" should {
    "Samples from task definition" in {
      TestTask.solution122("0") mustBe true
      TestTask.solution122(" 0.1 ") mustBe true
      TestTask.solution122("abc") mustBe false
      TestTask.solution122("1 a") mustBe false
      TestTask.solution122("2e10") mustBe true
      TestTask.solution122(" -90e3   ") mustBe true
      TestTask.solution122(" 1e") mustBe false
      TestTask.solution122("e3") mustBe false
      TestTask.solution122(" 6e-1") mustBe true
      TestTask.solution122(" 99e2.5 ") mustBe false
      TestTask.solution122("53.5e93") mustBe true
      TestTask.solution122(" --6 ") mustBe false
      TestTask.solution122("-+3") mustBe false
      TestTask.solution122("95a54e53") mustBe false
    }
  }

  "-------Check Task 14-------" should {
    "Samples from task definition" in {
      TestTask.solution14("catsanddog", List("cat", "cats", "and", "sand", "dog")) mustBe List("cat sand dog",
        "cats and dog")

      TestTask.solution14("pineapplepenapple", List("apple", "pen", "applepen", "pine", "pineapple")) mustBe Array(
        "pine apple pen apple",
        "pine applepen apple",
        "pineapple pen apple"
      )

      TestTask.solution14("catsandog", List("cats", "dog", "sand", "and", "cat")) mustBe Array()

      //Видимо ошибка в описании задачи или выполнении таски
      /*
      TestTask.solution14("abcd", List("a", "abc", "b", "cd")) mustBe List("a", "b", "cd")
      */
    }
  }
}
