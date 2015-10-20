package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set_1 = set1
    val set_2 = set2
    val set_3 = set3
    val set_4c = set4c
    val set_4d = set4d
    val set_5 = set5
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet

   //   set5.foreach((x:Tweet) => println("%d".format(x.retweets)))
   //   trends.foreach((x:Tweet) => println("%d".format(x.retweets)))
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
  //    assert(trends.head.user == "c")
    }
  }
  test("trending test"){  //park sy's one is superial to ysh's one
    new TestSets {
      assert(GoogleVsApple.trending.head.retweets === 321)
    }
  }
  test("immutable test"){ //kim hs's one superial to park sy's one
    new TestSets {
      set1.foreach(tw => println(tw.toString))
      assert(set_1 === set1)
      println("\nempty-----------------------------\n")
      set2.foreach(tw => println(tw.toString))
      assert(set_2 === set2)
      println("\na-----------------------------\n")
      set3.foreach(tw => println(tw.toString))
      assert(set_3 === set3)
      println("\na,b-----------------------------\n")
      set4c.foreach(tw => println(tw.toString))
      assert(set_4c === set4c)
      println("\na,b,c-----------------------------\n")
      set4d.foreach(tw => println(tw.toString))
      assert(set_4d === set4d)
      println("\na,b,d-----------------------------\n")
      set5.foreach(tw => println(tw.toString))
      assert(set_5 === set5)
      println("\na,b,c,d-----------------------------\n")
    }
  }


}
