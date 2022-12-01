package com.melvic.dry.interpreter

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

final case class ModuleLoader(mainModule: Path) {
  def fullPathOf(fullName: String): Path = {
    val sibling = mainDirectory.resolve(pathOf(fullName))
    if (Files.exists(sibling)) sibling
    else sourceRoot
  }

  val mainDirectory: Path = mainModule.getParent

  def pathOf(fullName: String): Path =
    Paths.get(fullName.replace(".", "/") + ".dry")

  def parentDirectory(path: String): Path =
    Paths.get(path).getParent

  val sourceRoot: Path = {
    val sourcePath = mainDirectory.resolve(".dry_config")
    if (Files.exists(sourcePath)) {
      val source = Source.fromFile(sourcePath.toFile)
      val config = source.getLines().filter(_.nonEmpty).toList
      source.close

      config.headOption.flatMap(parseSourceRoot(_).map(Paths.get(_))).getOrElse(mainDirectory)
    } else mainDirectory
  }

  def parseSourceRoot(row: String): Option[String] =
    row.split("=").map(_.trim).pipe { parts =>
      if (parts.length == 2) Some(parts(1))
      else None
    }
}
