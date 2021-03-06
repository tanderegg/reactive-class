package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeSolutionsTwo") {
    val input = Vector(2, 5, 3)
    val determinate = Polynomial.computeDelta(Var(input(0)), Var(input(1)), Var(input(2)))
    assert(determinate() == 1)
    val solutions = Polynomial.computeSolutions(Var(input(0)), Var(input(1)), Var(input(2)), determinate)
    assert(solutions() == Set(-1, -1.5))
  }

  test("computeSolutionsNone") {
    val input = Vector(1, 2, 1)
    val determinate = Polynomial.computeDelta(Var(input(0)), Var(input(1)), Var(input(2)))
    assert(determinate() == -0)
  }

  test("computeValuesCyclic") {
    val v1 = new Literal(3)
    val b = new Ref("b")
    val a = new Ref("a")
    val va: Signal[Expr] = Signal{new Plus(v1, b)}
    val v3 = new Literal(1)
    val vb: Signal[Expr] = Signal{new Plus(a, v3)}
    val m = Map(("a", va), ("b", vb))

    val c = Calculator.computeValues(m)
    assert(c("a").apply() == Double.NaN)
  }

}
