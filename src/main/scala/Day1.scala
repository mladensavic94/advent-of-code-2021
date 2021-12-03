package io.github.mladensavic94

object Day1 {

  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day1.txt").map(f => f.toInt)
    var counter = 0
    for (i <- 0 until list.length - 1) {
      if (list(i) < list(i + 1)) {
        counter += 1
      }
    }
    println(counter)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day1.txt").map(f => f.toInt)
    var counter = 0
    var prevSum = list.head + list(1) + list(2)
    for (i <- 1 until list.length - 2) {
      val sum = list(i) + list(i + 1) + list(i + 2)
      if(sum > prevSum)
        counter += 1
      prevSum = sum
    }
    println(counter)
  }

}
