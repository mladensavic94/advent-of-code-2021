package io.github.mladensavic94
package completed

object Day8 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day8.txt")
    val digits = list.map(_.split(" \\| ")).map(_.last).map(_.split(" "))

    val sum = digits.map(_.count(str => str.length == 2 || str.length == 3 || str.length == 4 || str.length == 7)).sum
    println(sum)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day8.txt")
    val digitsList = list.map(_.split(" \\| ")).map(_.last).map(_.split(" "))
    val codesList = list.map(_.split(" \\| ")).map(_.head).map(_.split(" "))
    var sum = 0
    for (i <- codesList.indices) {
      val codes = codesList(i)
      val digits = digitsList(i)
      val out = decode(codes, digits)
      sum += out.toInt
    }
    println(sum)
  }

  def decode(codes: Array[String], digits: Array[String]): String = {
    var out = ""
    val one = codes.filter(_.length == 2).head.split("").toSet
    val seven = codes.filter(_.length == 3).head.split("").toSet
    val four = codes.filter(_.length == 4).head.split("").toSet
    val eight = codes.filter(_.length == 7).head.split("").toSet
    val zero = codes.filter(_.length == 6).map(_.split("").toSet).filter(s => four.diff(s).size == 1 && one.diff(s).isEmpty).head
    val nine = codes.filter(_.length == 6).map(_.split("").toSet).filter(s => s.intersect(seven).size == 3 && s != zero).head
    val six = codes.filter(_.length == 6).map(_.split("").toSet).filter(s => seven.intersect(s).size == 2).head
    val three = codes.filter(str => str.length == 5).map(_.split("").toSet).filter(s => one.subsetOf(s)).head
    val five = codes.filter(_.length == 5).map(_.split("").toSet).filter(_ != three).filter(s => nine.diff(s).size == 1).head
    val two = codes.filter(_.length == 5).map(_.split("").toSet).filter(s => nine.diff(s).size == 2).head
    for (d <- digits) {
      val set = d.split("").toSet
      if (set == one)
        out += "1"
      else if (set == two)
        out += "2"
      else if (set == three)
        out += "3"
      else if (set == four)
        out += "4"
      else if (set == five)
        out += "5"
      else if (set == six)
        out += "6"
      else if (set == seven)
        out += "7"
      else if (set == eight)
        out += "8"
      else if (set == nine) {
        out += "9"
      } else if (set == zero) {
        out += "0"
      } else {
        println("duvaj ga")
      }
    }
    out
  }
}
