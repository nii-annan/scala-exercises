import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import org.scalacheck.Prop.forAll


class RationalSuite extends FunSuite with ScalaCheckSuite{

  val half = new Rational(1,2)

  /**
    * Gen is part of ScalaCheck, it is a generator class. The below
    * will generate Rational numbers with denominator 0 to 10000 and 1 to 10000
    *
    * Note: The 10000 max here is chosen because the implementation of Rational
    * is simple and doesn't deal with Integer overflow. Remember here also that
    * the overflow can happen in the implementation aswell as these values, so trying to add two
    * rationals with denominator of 10000 for example will use an integer of 10000 * 10000
    */

  val positiveRational: Gen[Rational] = for {
    n <- Gen.choose(0, 10000)
    d <- Gen.choose(1, 10000)
  } yield new Rational(n,d)

  val negativeRational: Gen[Rational] = for {
    n <- Gen.choose(-10000, -1)
    d <- Gen.choose(1, 10000)
  } yield new Rational(n,d)

  val anyRational: Gen[Rational] = for {
    n <- Gen.choose(-10000, 10000)
    m <- Gen.choose(-10000, 10000) if m != 0
  } yield new Rational(n,m)

  val ints:Gen[Int] = for {
    n <- Gen.choose(-10000, 10000)
  } yield n

  test("To String on rational should format the string as numer/denom") {
    assertEquals(half.toString, "1/2")
  }

  test("When a rational number is given it should be converted to its simplest form") {
    val fiveTen = new Rational(5,10)

    assertEquals(fiveTen.toString, "1/2")
    assertNotEquals(fiveTen.toString, "5/10")
  }

  test("Adding should work for two rational numbers") {
    assertEquals(half + half, new Rational(1,1))
  }

  test("Adding should work for a rational and an integer") {
    assertEquals(half + 1, new Rational(3,2))
  }

  /**
    * The tests below use ScalaCheck. What is will do
    * is automatically generate a bunch of test cases (100 by default)
    * and run them all. See the generators towards the top of the file.
    *
    * If it finds failing cases then it can do some clever stuff
    * and simplify the failure to an easy case. It also tells you
    * the parameters it fails on so you can debug it manually.
    *
  */
  test("Adding a positive rational always results in a value bigger than the original") {
    forAll(positiveRational, positiveRational){ (r1, r2) =>
      assert(r1 + r2 > r1)
    }
  }

  test("Adding a negative rational to a positive always results in a value lower than the original") {
    forAll(positiveRational, negativeRational){ (r1, r2) =>
      assert(r1 + r2 < r1)
    }
  }

  test("Adding a negative is the same as subtracting its absolute value") {
    forAll(positiveRational, negativeRational) { (r1, r2) =>
      assert(r1 + r2 == r1 - r2 * -1)
    }
  }

  test("Adding an int to rational should be the same as adding the same rational to the same int ") {
    forAll(anyRational, ints ) { (r, n) =>

      // This import could be at the top of the file but it put here to make it
      // clear that it's needed for 1 + r to work.
      import IntUtils.IntExtensions
      assert(r + 1 == 1 + r)
//      assert(false) // Replace with assert above when implemented.
    }
  }

}
