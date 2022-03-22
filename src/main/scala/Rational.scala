import scala.annotation.tailrec

/**
  * This is a worked example with 'fill in the blanks' and lots of comments to help with understanding some concepts.
  * This is heavily based on Programming in Scala book (chapter 6) with a load of comments added
  */

/*
  A class in Scala is just like a class in Java, it's a blueprint for objects that can then be created with the new keyword (see RationalSuite.scala).
  The class below takes two parameters, n (for numerator) and d (for denominator) and allows for the object to be created
  with these values (i.e. there is a constructor taking those two parameters). So we can do Rational(1,2) for 1/2

  Note: A rational number is any number that can be written in the form n/d where d!= 0
 */
class Rational(n: Int, d:Int) extends Ordered[Rational]{
  // The require keyword is used to check for pre-conditions, we use it here to stop anyone being able to create
  // a rational number with d = 0 as that doesn't make any sense
  require(d != 0)

  /** Create a val holding the greatest common divisor, abs is used to make sure we always get a positive gcd */
  private val g = gcd(n.abs, d.abs)

  // Create fields holding the numerator and denomiator in simplified form.
  val numer: Int = n / g
  val denom: Int = d / g

  /*
   A secondary constructor taking only 1 parameter this is used for whole numbers to allow for doing Rational(2) instead
   of Rational(2,1). It does this by just defaulting the second parameter in the default constructor to 1
   */
  def this(n: Int) = this(n, 1)

  /**
    * An addition method for rational numbers, to add them we use the following formula
    * a/b + c/d = ad + bc / bd
    *
    * As you're inside the class you can use the simpified numer and denom from the fields
    */
  def + (that: Rational): Rational = {
     val numer = (this.numer*that.denom) + (this.denom*that.numer)
    val demon = (this.denom*that.denom)
    new Rational(numer,demon)
  }

  /**
    * This allows for adding an int to a rational without having to convert it to a rational first. It's nicer for our
    * user to be able to do Rational(1,2) + 1 than Rational(1,2) + Rational(1,1)
    */
  def + (i: Int): Rational = this + new Rational(i)

  /**
    * An addition method for rational numbers, to add them we use the following formula
    * a/b - c/d = ad - bc / bd
    *
    * Ananologous to + method
    */
  def - (that: Rational): Rational = {
    val numer = (that.numer*that.denom) - (this.denom*that.numer)
    val denom = this.denom*that.denom
    new Rational(numer,denom)
  }

  /**
    * Analogous to the + method
    */
  def - (i: Int): Rational = this - new Rational(i)

  /**
    * A multiplication method for rational numbers, to multiply them we use the following formula
    * a/b * c/d = ac / bd
    *
    * Analogous to + method
    */
  def * (that: Rational): Rational = {
    val numer = this.numer * that.numer
    val denom = this.denom * that.denom
    new Rational(numer,denom)
  }

  /**
    * Analogous to the + method
    */
  def * (i: Int): Rational = this * new Rational(i)

  /**
    * A division method for rational numbers, to divide them we use the following formula
    * (a/b) / (c/d) = ad / bc
    *
    * Analogous to * method
    */
  def / (that: Rational): Rational = {
   val numer = this.numer*that.denom
  val denom = this.denom*that.numer
    new Rational(numer,denom)
  }

  /**
    * Analogous to the + method
    */
  def / (i: Int): Rational = this / new Rational(i)


  /**
    * gcd returns the greatest common divisor of the values a and b. It's useful here as we can
    * use it to simpify rational numbers to their simplest form, i.e. 3/6 = 1/2
    *
    * The method is 'private' so it isn't available outside of the Rational class.
    *  Hint: Use recursion
   */

  private def gcd(a: Int, b: Int): Int = {
    if (a==0) b
    else gcd(b%a,a)
  }

  /**
    * The toString method is a default method used for outputting objects as strings. The normal default here
    * would give Rational(numer, denom), but this makes it output numer/denom insteaD
    *
    * Use string interpolation to put the parameters in to the string
    */
  override def toString: String = {
    s"${numer}/${denom}"}

  /**
    * We want to be able to compare two rational numbers. Overriding this method
    * along with implementing ordered interface allows us to use <, >, <=, >=, ==
    */
  override def compare(that:Rational): Int = {
//find lcm and compare numer
//    speed up by comparing if they are positive or negative
  }

  /**
    * Override the equals method too, so that we can compare Rational numbers. This is useful in testing
    * as it means that checking if two are equal will compare on value not on object equality
    *
    * Note the use of compare method here. This could also have been implemented
    * directly and may have been very slightly faster (are numerators and denominators the same) but it's done this way to
    * show it can be done this way
    */
  override def equals(that: Any): Boolean = {
    ???
  }
}

/**
  * This is a way of extending the Int class with new methods. Think in terms of SOLID
  * this allows us to extend the class without it being open to modification
  *
  *  Without these we can only do Rational(1/2) + 1 rather than 1 + Rational(1/2)
  *
  * This could be in a separate file and but left here as example
  * that we can have multiple objects/classes in the same file.
  */
object IntUtils {
  implicit class IntExtensions(i: Int) {
    /** Use that adding a rational and an int is associative, i.e i + r == r + i */
    ???
  }
}
