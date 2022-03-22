import scala.annotation.tailrec
import scala.collection.immutable.ListMap

/**
 * Tests for all of the methods in this class are available in StringSuite.scala. Replace
 * the ??? below with your solutions and run the tests to see if they're working. You 
 * don't have to implement them all in one go as the tests will say what passed and failed
 */
object StringFunctions {

/**
  * Example function that just returns the string. This is here to show one test passing
  */
  def returnString(str: String) = {
    str
  }

  /**
    * Capitalise and reverse the supplied string.
    */

  def capitaliseAndReverse(str: String): String = {
    str.reverse.toUpperCase
  }

  /**
    * Return the string with append after the string
    */
  def appendString(str: String, append: String):String = {
    str.appendedAll(append)
  }

  /**
    * Return the string with prepend in front of it
    */
  def prependString(str: String, prepend: String):String = {
    prepend++str
  }

  /**
    * Return the string in alphabetical order.
    * Hint: Watch out for the case where there are capital letters.
    */
  //assumption is the string contains distinct characters
  def convertToAlphabetical(str:String): String = {
    val (upperCase,lowerCase) = str.sorted.partition(_.isUpper)
   if(upperCase.nonEmpty && lowerCase.nonEmpty){
      val stringList = List(upperCase,lowerCase)
      val longerLengthCase = stringList.min // longer length is min because the ASCII character for lower case characters is of a higher value
      val shorterLengthCase = stringList.max // shorter length is max because the ASCII character for upper case characters is of a lower value

      @tailrec
      def helper(longerLengthCase:List[Char], shorterLengthCase:String, results:String):String={

        longerLengthCase match {
          case head+:tail =>
            val headCase = if (head.isUpper)head.toLower
            else head.toUpper
            val(matched,unmatched) = shorterLengthCase.partition(char => char.equals(headCase))
            if(matched.nonEmpty) {
              helper(tail,unmatched,results.appendedAll(head.toString.appendedAll(matched).sorted.reverse))
            }
            else helper(tail,unmatched,results.appendedAll(head.toString))
          case Nil => results
        }
      }
      helper(longerLengthCase.toList,shorterLengthCase,"")
    }else str.sorted
  }

  /**
    * Returns the number of times a particular character occurs in the given string
    */
  def countOfOccurrences(str: String, char: Char) = {
    str.count(ch=> ch.equals(char))
  }

  /**
    * Returns the most common character in the given string, 
    * Hint: There are a number of ways to do this, consider 
    * using map and indexOf from the String library and countOfOccurrences above
    */
  def mostCommonCharacter(str:String):Char = {
    str.groupBy(ch=>ch).values.map(st=> st->st.length).maxBy(_._2)._1.charAt(0)
  }
}
