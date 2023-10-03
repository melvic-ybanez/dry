package com.melvic.dry.interpreter

import com.melvic.dry.resolver.Scopes
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.{Failure, Result}

import java.nio.file.{Path, Paths}
import scala.io.Source

object Run {
  def repl(env: Env, scopes: Scopes): Unit =
    Repl.live.start(env, scopes)

  def mainModule(path: String): Unit = {
    val result = Run.path(path, ModuleManager.getSourcePaths(Paths.get(path)))
    Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
    System.exit(if (result.isLeft) -1 else 0)
  }

  def path(path: String, sourcePaths: List[Path]): Result[Env] = {
    val env = Env.empty

    val source = Source.fromFile(path)
    val code = source.getLines().mkString("\n")
    source.close

    val result = Interpret.script(code, env, Scopes.empty, sourcePaths)
    result.map(_ => env)
  }
}
