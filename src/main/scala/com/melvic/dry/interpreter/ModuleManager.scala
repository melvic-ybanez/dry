package com.melvic.dry.interpreter

import java.nio.file.{Files, Path, Paths}
import scala.io.Source

object ModuleManager {
  def getSourcePaths(mainModule: Path): List[Path] = {
    val mainDir = mainModule.getParent
    val dryPathsPath = mainDir.resolve(".dry_paths")
    val paths = if (Files.exists(dryPathsPath)) {
      val source = Source.fromFile(dryPathsPath.toFile)
      val paths = source.getLines().filter(_.nonEmpty).map(_.trim).toList
      source.close()
      paths
    } else Nil
    (mainDir :: paths.map(Paths.get(_))).distinct
  }

  def pathOf(pathComponents: List[String]): Path =
    Paths.get(pathComponents.mkString("/") + ".dry")

  def fullPathOf(pathComponents: List[String], sourcePaths: List[Path]): Option[Path] =
    sourcePaths.map(_.resolve(pathOf(pathComponents))).find(Files.exists(_))
}
