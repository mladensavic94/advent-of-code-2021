package io.github.mladensavic94

object Day2 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day2.txt")
    var depth = 0
    var horizontal = 0
    for (row <- list) {
      val commands = row.split(" ")
      val int = commands.last.toInt
      val command = commands.head
      command match {
        case "forward" => horizontal += int
        case "down" => depth += int
        case "up" => depth -= int
      }
    }
    println(depth * horizontal)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day2.txt")
    var depth = 0
    var horizontal = 0
    var aim = 0
    for (row <- list) {
      val commands = row.split(" ")
      val int = commands.last.toInt
      val command = commands.head
      command match {
        case "forward" =>
          horizontal += int
          depth += aim * int
        case "down" => aim += int
        case "up" => aim -= int
      }
    }
    println(depth * horizontal)
  }
}
