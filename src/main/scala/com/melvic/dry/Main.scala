package com.melvic.dry

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => Run.prompt()
      case Array(path) => Run.path(path)
      case _           => println("Usage: dry [script]")
    }
}
