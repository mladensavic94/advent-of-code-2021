package io.github.mladensavic94

import scala.io.Source

object Util {

  def readFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    val lines = source.getLines().toList
    source.close()
    lines
  }
}
