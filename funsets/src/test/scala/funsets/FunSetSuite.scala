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


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
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
  test("forall test") {
    new TestSets {
      val s = union(s1, s2)
      assert(forall(s, ((x : Int) => (x == 1) || (x == 2))))
      assert(!forall(s, ((x : Int) => (x == 1) || (x == 3))))
    }
  }
  test("exists test") {
    new TestSets {
      val s = union(s1, s2)
      assert(exists(s, ((x : Int) => (x == 1))))
      assert(!exists(s, ((x : Int) => (x == 3))))
    }
  }
  test("map test") {
    new TestSets {
      val s = union(s1, s2)
      val ss = map(s, ((x : Int) => (x + 1)))
      assert(!exists(ss, ((x : Int) => (x == 1))))
      assert(exists(ss, ((x : Int) => (x == 3))))
    }
  }
  test("My tests") {
    new TestSets {
      val s4 = singletonSet(4)
      val s6 = singletonSet(6)
      val set1 = union(s1,s4)
      val set2 = union(s2,s6)
      val set3 = union(Set(1,3,7), Set(3,2,5))
      val set4 = union(set1,set2)

      assert(contains(intersect(set1,set3), 1), "Intersect test")
      assert(contains(diff(set4,set1), 2), "Diff test")
      assert(contains(filter(set4, x => x > 5), 6), "Filter test")
      assert(forall(set3, x => x > 0), "Forall test")
      assert(exists(set3, x => x > 6), "Exists test")
      assert(contains(map(set2, x => x * x), 4), "Map test")

    }
  }
 test("Your tests") {
  new TestSets {
    val a1 = union(s1,s2)
    val a2 = union(s1,s3)
    val b1 = intersect(s1,a1)
    val b2 = intersect(a1,a2)
    assert(contains(b1, 1), "IS1")
    assert(contains(b2, 1), "IS2")
    assert(!contains(b2, 2), "IS3")
    assert(!contains(b2, 3), "IS4")
  }

  new TestSets {
    val a1 = union(s1,s2)
    val a2 = union(s1,s3)
    val b1 = diff(a1,s1)
    val b2 = diff(a1,a2)
    assert(contains(b1, 2), "DF1")
    assert(contains(b2, 2), "DF2")
    assert(!contains(b2, 1), "DF3")
    assert(!contains(b2, 3), "DF4")
  }

  new TestSets {
    val a1 = union(s1,s2)
    val a2 = union(s1,s3)
    val a3 = union(a1,s3)
    val b1 = filter(a3, x => x>1)
    val b2 = filter(a3, x => x>2)
    val b3 = filter(a3, x => x<3)
    assert(contains(b1, 2), "FT1")
    assert(contains(b1, 3), "FT2")
    assert(!contains(b1, 1), "FT3")
    assert(contains(b2, 3), "FT4")
    assert(!contains(b2, 2), "FT5")
    assert(!contains(b3, 3), "FT6")
    assert(contains(b3, 1), "FT7")
  }

  new TestSets {
    val a1 = union(s1,s2)
    val a2 = union(a1,s3)

    assert(forall(a2, x => x > 0 ), "FA1")
    assert(!forall(a2, x => x > 1 ), "FA2")
  }

  new TestSets {
    val a1 = union(s1,s2)
    val a2 = union(a1,s3)

    assert(exists(a2, x => x > 0 ), "EX1")
    assert(exists(a2, x => x > 1 ), "EX2")
    assert(!exists(a2, x => x < 1 ), "EX3")
  }

  new TestSets {
    val a1 = union(s1,s2)
    val a2 = union(a1,s3)
    val a3 = map(a2, x => x + x )
    assert(contains(a3,2), "map1")
    assert(contains(a3,4), "map2")
    assert(contains(a3,6), "map3")
    assert(!contains(a3,1), "map4")
    assert(!contains(a3,3), "map5")
  }
 }

}
