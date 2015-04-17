import scala.math.min
import scala.util.Try

object InputHelpers {
  def readLine(reader: java.io.BufferedReader)() = {
    val line = reader.readLine
    if (line == null) None else Some(line)
  }
  def lineIter(reader: java.io.BufferedReader): Iterator[Option[String]] = 
    Iterator.continually(readLine(reader))
  def lineIter(file: String): Option[Iterator[Option[String]]] = Try({
    val freader = new java.io.FileReader(file)
    val breader = new java.io.BufferedReader(freader)
    lineIter(breader)
  }).toOption
}

object arrays {

  /*
     getIntsAsString
   For example: If the Array contains 1, 2, 3, 4, 5
     printInts("my array ", Array(1, 2, 3, 4, 5), " : ") gives
     myarray 1:2:3:4:5
  */

  def arraySize(a: Array[Int]): Int = {
    a.length
  }

  def getIntsAsString(label: String, delimiter: String, a: Array[Int]): String = {
    label + Try(a.map(_.toString).reduceLeft({ (a: String, b: String) =>
      a + delimiter + b
    })).getOrElse("")
  }

  // Read the contents of filename into a.
  // You should only read as many lines as the array can hold (a.length)
  // Each line should be converted to Int (if possible) or 0 otherwise.

  def readFileIntoArray(filename: String, a: Array[Int]) {
    InputHelpers.lineIter(filename).map({ (iter: Iterator[Option[String]]) => 
      iter.take(a.length).toList.zipWithIndex.map({ (s: Option[String], i: Int) => 
        a(i) = s.getOrElse("0").toInt
      }.tupled)
    })
  }

  //Minimum chunk
  ///  Return the minimum value in a.
  ///  Example: If a contains {5, 7, 4, 9}, return 4. 
  ///  Assume a contains at least one value.

  def minimum(a: Array[Int]): Int = {
    require(a.length > 0) // if you delete this, the tests will not pass!
    a.min
  }
  //CountEven chunk
  ///  Return the number of even values in a.
  ///  Example: If a contains {-4, 7, 6, 12, 9}, return 3. 
  def countEven(a: Array[Int]): Int = {
    a.filter(_ % 2 == 0).length
  }

  //CountEven chunk
  ///  Return the number of even values in a.
  ///  Example: If a contains {-4, 7, 6, 12, 9}, return 3. 

  def countOdd(a: Array[Int]): Int = {
    a.filter(_ % 2 == 1).length
  }

  //PairwiseAdd chunk
  ///  Add corresponding elements of a and b and place them in sum.
  ///  Assume all arrays have the same Length.
  ///  Example: If a contains {2, 4, 6} and b contains {7, -1, 8}
  ///  then at the end sum should contain {9, 3, 14}. 

  def pairwiseAdd(a: Array[Int], b: Array[Int], c: Array[Int]) {
    (if (a.length < b.length) a.zip(b) else b.zip(a)).
      zipWithIndex.map({ (tup: (Int, Int), i: Int) =>
        if (i < c.length)
          c(i) = tup._1 + tup._2
    }.tupled)
  }
  //NewPairwiseAdd chunk
  ///  Return a new array whose elements are the sums of the
  ///  corresponding elements of a and b.
  ///  Assume a and b have the same Length.
  ///  Example: If a contains {2, 4, 6} and b contains {3, -1, 5}
  ///  then return an array containing {5, 3, 11}. 
  def newPairwiseAdd(a: Array[Int], b: Array[Int]): Array[Int] = {
    (if (a.length < b.length) a.zip(b) else b.zip(a)).
      map({ (a: Int, b: Int) =>
        a + b
    }.tupled)
  }
  //IsAscending chunk
  ///  Return true if the numbers are sorted in increasing order,
  ///  so that in each pair of consecutive entries,
  ///  the second is always at least as large as the first.
  ///  Return false otherwise.  Assume an array with fewer than
  ///  two elements is ascending.
  ///  Examples: If a contains {2, 5, 5, 8}, return true;
  ///  if a contains {2, 5, 3, 8}, return false. 
  def isAscending(a: Array[Int]): Boolean = {
    a.foldLeft(Some(a.min-1): Option[Int])({ (acc: Option[Int], x: Int) =>
      acc.flatMap({ (y: Int) => if (x >= y) Some(x) else None })
    }).isDefined
  }

  /*
     getAscendingRun(a, position) returns the position where a 
     run (of ascending values) ends. If a run ends at the end of
     the array, the array's length is returned. This function is 
     designed to be called over and over until there are no more 
     runs.

    example:

    If you ahve an array of data:
    val data = Array(2, 5, 8, 3, 9, 9, 8)

    getAscendingRun(data, 0) returns 3 (since 3 < 8)
      run is 2, 5, 8
    getAscendingRun(data, 3) returns 6 (since 8 < 9)
      run is 3, 9, 9
    getAscendingRun(data, 6) returns 7 (since 8 is the last item in the list)
      run is 8

  */

  def getAscendingRun(a: Array[Int], position: Int): Int = {
    require(position < a.length)
    println("\n\n\n")
    position + a.zipWithIndex.dropWhile(_._2 < position).map(_._1).
      zipWithIndex.
      foldLeft((a.min-1, 0))( { (acc: (Int, Int), x: (Int, Int)) =>
        println(s"testing $acc vs $x")
        if (acc._1 <= x._1) (x._1, x._2) else (Int.MaxValue, acc._2)
      })._2 + 1
  }

  /*
    This should use teh getAscendingRun() function to produce a string
    of runs. The runs should be separated by commas with a vertical bar
    between each run. In the above:

    2, 5, 8 | 3, 9, 9 | 8
  */
  
  def _getRunsAsString(a: Array[Int], p: Int = 0): String = {
    if (p >= a.length)
      return ""
    val run = getAscendingRun(a, p)
    val recur = _getRunsAsString(a, run)
    a.slice(p, run).mkString(", ") + (if (recur.length > 0) " | " + recur else "")
  }

  def getRunsAsString(a: Array[Int]): String = {
    _getRunsAsString(a)
  }
  // end PrintRuns chunk   
}
