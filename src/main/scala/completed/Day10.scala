package io.github.mladensavic94
package completed

import scala.collection.mutable
import scala.util.control.Breaks

object Day10 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day10.txt")
    var sum = 0
    val incomplete = Array[String]()

    for (line <- list) {
      val stack = mutable.Stack[Char]()
      Breaks.breakable({
        for (ch <- line) {
          if (isOpenChar(ch))
            stack.push(ch)
          else {
            val c = stack.pop()
            if (!isOpposite(c, ch)) {
              sum += mapToVal(ch)
              Breaks.break()
            }
          }
        }
      })
    }
    println(sum)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day10.txt")
    var sum = 0
    var incomplete = Array[mutable.Stack[Char]]()

    for (line <- list) {
      val stack = mutable.Stack[Char]()
      Breaks.breakable({
        for (ch <- line) {
          if (isOpenChar(ch))
            stack.push(ch)
          else {
            val c = stack.pop()
            if (!isOpposite(c, ch)) {
              sum += mapToVal(ch)
              Breaks.break()
            }
          }
        }
        incomplete = incomplete.appended(stack)
      })
    }
    val sorted = incomplete.map(calculateScore).sorted
    println(sorted(sorted.length / 2))
  }

  def isOpenChar(char: Char): Boolean = {
    char == '[' || char == '{' || char == '(' || char == '<'
  }

  def isOpposite(open: Char, close: Char): Boolean = {
    if (open == '{' && close == '}')
      return true
    if (open == '[' && close == ']')
      return true
    if (open == '<' && close == '>')
      return true
    if (open == '(' && close == ')')
      return true
    false
  }

  def mapToVal(char: Char): Int = {
    char match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }
  }

  def calculateScore(stack: mutable.Stack[Char]): Long = {
    var score = 0L
    while (stack.nonEmpty) {
      val point = stack.pop() match {
        case '[' => 2
        case '(' => 1
        case '{' => 3
        case '<' => 4
      }
      score = score * 5 + point
    }
    score
  }
}
