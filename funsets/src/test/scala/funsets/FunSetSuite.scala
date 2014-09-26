package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  new TestSets {
    test("singletonSet(1) contains 1") {
      assert(contains(s1, 1))
    }

    test("singletonSet(1) doesn't contain 2") {
      assert(!contains(s1, 2))
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection doesn't contain elements that aren't on both sets") {
    new TestSets {
      val intersection1and2 = intersect(s1, s2)
      assert(!contains(intersection1and2, 1))
      assert(!contains(intersection1and2, 2))
    }
  }

  test("intersection contains the elements on both sets") {
    new TestSets {
      val s = intersect(s1, singletonSet(1))
      assert(contains(s, 1))
    }
  }
}