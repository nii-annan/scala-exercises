import java.time.LocalDate

object IntListFunctions {

  /**
    * Only return the values from the list that are less than five
    */
  def lessThanFive(ints: List[Int]):List[Int] = ints.filter(_<5)

  /**
    * Return the same list but with any duplicated values removed
    */
  def removeDuplicates(ints: List[Int]):List[Int] = ints.toSet.toList

  /**
    * Take two lists and combine them in to one, e.g. List(1,2), List(3,4) should become List(1,2,3,4)
    */
  def combineLists(ints:List[Int], ints2:List[Int]):List[Int] = ints ++ ints2

  /**
    * Take any number of lists and combine them in to one note the * syntax in the signature makes it a variable number
    * of arguments, i.e. 0 to N
    * Hint: Take a look at fold and combining with the method above
    */
  def combineNLists(ints:List[Int]*):List[Int] = ints.foldLeft(List[Int]())((acc,nextList)=>  acc ++ nextList)

  /**
    * Returns the penultimate element of the list
    */
  def penultimateElement(ints:List[Int]): Int = ints(ints.length-2)

  /**
    * Checks whether a list is a palindrome, e.g. is it the same from front to back and back to front
    */
  def isPalindrome(ints:List[Int]): Boolean = ints.reverse
  }

  /**
    * Remove consecutive duplicates from a list, e.g. if we had the List(1,1,2,1,3,3,2,4) we should end up with List(1,2,1,3,2,4)
    * Hint: Use foldRight
    * Hint2: You might need to help the compiler know your start value is definitely an integer you can do that with for example List[Int]()
    */
  def removeConsecutiveDuplicates(ints:List[Int]):List[Int] = {
      ???
  }
}
