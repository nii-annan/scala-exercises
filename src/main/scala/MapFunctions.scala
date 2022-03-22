object MapFunctions {

  /**
    * Returns a map containing only the items where the keys start with the supplied string
    * Hint: Map contains a filter method just like list. Also consider destructuring the key and values with _1 and _2
    */
  def getElementsInMapStartingWith(m:Map[String, String], str: String):Map[String, String] = m.filter(_._1.startsWith(str))


  /**
    * Return the key from the map where the item has the longest value by string length
    * Hint: Don't forget to deal with the case where the map is empty
    * Hint 2: Take a look at reduce, which is very similar to fold
    */
  def getKeyOfLongest(m:Map[String, String]): Option[String] = {
    if (m.isEmpty) None
    else Some(m.maxBy(value => value._2)._1)
  }

  /**
    * This uses generic type parameters and variable number of arguments,
    * meaning it allows for the type to be specified in creation and hence
    * can work across all different maps and for any number of maps
    */
  def combineNMaps[A,B](ms:Map[A,B]*):Map[A,B] = {
 ms.foldLeft(Map[A,B]())((acc,map) => acc ++ map)
  }

}
