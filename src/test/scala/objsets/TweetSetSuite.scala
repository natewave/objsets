package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 321))
    val set3 = set2.incl(new Tweet("b", "b body", 321))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val n = new Tweet("n", "n body", 321)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set1.incl(n).union(set3)
    val setn = set1.incl(new Tweet("b", "b body", 205))
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

  test("filter: 321 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 321)) === 2)
    }
  }

  test("filter: exactly 3 321 on set6") {
    new TestSets {
      assert(size(set6.filter(tw => tw.retweets == 321)) === 3)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("filet and union: 321 and 205") {
    new TestSets {
      assert(
        size(
          set4c.filter( tw => tw.retweets == 321).union(
          setn.filter( tw => tw.retweets == 205)) ) === 2) }
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
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
