package io.github.mladensavic94

object Template {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/dummy.txt")

  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/dummy.txt")
  }
}
