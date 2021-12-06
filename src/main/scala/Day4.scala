package io.github.mladensavic94

import scala.util.control.Breaks.break
import scala.collection.mutable.Set

object Day4 {
  def main(args: Array[String]): Unit = {
    //    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day4.txt")
    val numbers = list.head.split(",").map(_.toInt)
    var boards = List[Array[(Int, Boolean)]]()
    var board = Array[(Int, Boolean)]()
    for (row <- list.tail) {
      if (!row.isBlank) {
        val ints = row.split(" ").filter(str => !str.isBlank).map(str => (str.toInt, false))
        board = board.appendedAll(ints)
      } else {
        if (board != null)
          boards = boards.appended(board)
        board = Array[(Int, Boolean)]()
      }
    }

    for (number <- numbers) {
      markNumber(boards, number)
      for (board <- boards) {
        if (checkBoard(board)) {
          printBoard(board)
          println(board.filter(t => t._2 == false).map(t => t._1).sum * number)
          break
        }
      }
    }
    //    boards.foreach(printBoard(_))
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day4.txt")
    val numbers = list.head.split(",").map(_.toInt)
    var boards = List[Array[(Int, Boolean)]]()
    var board = Array[(Int, Boolean)]()
    for (row <- list.tail) {
      if (!row.isBlank) {
        val ints = row.split(" ").filter(str => !str.isBlank).map(str => (str.toInt, false))
        board = board.appendedAll(ints)
      } else {
        if (board != null)
          boards = boards.appended(board)
        board = Array[(Int, Boolean)]()
      }
    }
    var completeBoards = Set[Int]()
    for (number <- numbers) {
      markNumber(boards, number)
      for (i <- 0 until boards.length) {
        if (checkBoard(boards(i))) {
          completeBoards.add(i)
          if (completeBoards.size == boards.length) {
            printBoard(boards(i))
            println(boards(i).filter(t => t._2 == false).map(t => t._1).sum * number)
            break
          }
        }
      }
    }
  }

  def printBoard(board: Array[(Int, Boolean)]): Unit = {
    for (i <- 0 to 24) {
      print(board(i))
      print(" ")
      if ((i + 1) % 5 == 0)
        println()
    }
    println(checkBoard(board))
    println("***")
  }

  def checkBoard(board: Array[(Int, Boolean)]): Boolean = {
    for (i <- 0 to 24 by 5) {
      if (board(i)._2 && board(i + 1)._2 && board(i + 2)._2 && board(i + 3)._2 && board(i + 4)._2)
        return true
    }
    for (i <- 0 to 4) {
      if (board(i)._2 && board(i + 5)._2 && board(i + 10)._2 && board(i + 15)._2 && board(i + 20)._2)
        return true
    }
    false
  }

  def markNumber(boards: List[Array[(Int, Boolean)]], value: Int): Unit = {
    boards.foreach(board => {
      for (i <- 0 to 24) {
        if (board(i)._1 == value)
          board(i) = (value, true)
      }
    })
  }
}
