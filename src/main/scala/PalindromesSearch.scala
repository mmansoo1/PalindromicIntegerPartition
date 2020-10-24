import java.io._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg02 - PalindromesSearch
 * Student(s) Name(s): Mohammad H. Mansoor
 */

object PalindromesSearch {
  val OUTPUT_FILE_NAME = "output.txt"
  var mCount = 0
  var thirdArg = false
  val file = new FileWriter(OUTPUT_FILE_NAME, false)

  def printPartition(target: Int, maxValue: Int, suffix: ListBuffer[Byte], partList: ListBuffer[ListBuffer[Byte]]): Unit = {
    if (target == 0) {
      partList += suffix
    }
    else {
      if (maxValue > 1) {
        printPartition(target, maxValue - 1, suffix, partList)
      }
      if (maxValue <= target) {
        val tmp = new ListBuffer[Byte]()
        tmp.addOne(maxValue.toByte)
        tmp.addAll(suffix)
        printPartition(target - maxValue, maxValue, tmp, partList)
      }
    }
  }

  def callPrintPartition(n: Int, m: Int): Unit = {
    val partList = new ListBuffer[ListBuffer[Byte]]()
    printPartition(n, 10, new ListBuffer[Byte], partList)

    for (i <- 0 until partList.length) {
      val listPermutation = partList(i).permutations.toList

      for (j <- 0 until listPermutation.length) {

        val palindromeList = listPermutation(j).reverse

        if (listPermutation(j).equals(palindromeList)) {

          if (palindromeList.contains(m)) {

            countM(m, palindromeList)

            if (thirdArg == true) {
              writeToFile(palindromeList.mkString(","))
            }
          }
        }
      }
    }
    println("Number of palindromic sequences: " + mCount)
  }

  def countM(m: Int, y: ListBuffer[Byte]): Unit = {
    if (y.contains(m)) {
      mCount += 1
    }
  }

  def writeToFile(y: String): Unit = {
    file.write(y + "\n")
  }

  def main(args: Array[String]): Unit = {
    val startTimeMillis = System.currentTimeMillis()
    val n = args(0).toInt
    val m = args(1).toInt

    println("Welcome to the palindromic sequence project!")
    println("parameter n: " + n + "\n"
      + "parameter m: " + m)

    if (args.length < 2 || args.length > 3) {
      println("Use: java PalindromesSearch n m [y] \n" +
        "[y]: when informed, all palindromic sequences must be saved to a file")
      System.exit(1)
    }
    if (args.length == 3) {
      if (args(2) == "y") {
        thirdArg = true
        println("generating palindromic sequences...")
      }
    }

    callPrintPartition(n, m)
    file.close()
    if (thirdArg == true) {
      println("Done!")
    }

    val endTimeMillis = System.currentTimeMillis()
    val durationSeconds = (endTimeMillis - startTimeMillis) / 1000
    println("It took me: " + durationSeconds + " seconds")

  }
}