package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.interpreter.eval.Evaluate
import com.melvic.dry.lexer.Lexer
import com.melvic.dry.parsers.Parser
import com.melvic.dry.resolver.{Context, Locals, Resolve}
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.{Failure, Result}

import java.nio.file.{Path, Paths}
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

object Run {
  def repl(env: Env, locals: Locals): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      def runScript(): Unit =
        Run
          .script(input, env, locals, Nil)
          .map { case (value, locals) =>
            if (value != Value.Unit)
              println(Value.show(value))
            locals
          }
          .pipe { result =>
            Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
            result match {
              case Left(_)          => repl(env, locals)
              case Right(newLocals) => repl(env, newLocals)
            }
          }

      Run.expression(input, env) match {
        case Left(_) => runScript()
        case Right(value) =>
          println(Value.show(value))
          repl(env, locals)
      }
    }
  }

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

    val result = Run.script(code, env, Locals.empty, sourcePaths)
    result.map(_ => env)
  }

  def script(source: String, env: Env, oldLocals: Locals, sourcePaths: List[Path]): Result[(Value, Locals)] =
    for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      locals <- Resolve.resolveAll(decls)(Context.default).map(_.locals ++ oldLocals)
      value  <- Interpreter.interpret(decls, env, locals, sourcePaths)
    } yield (value, locals)

  def expression(source: String, env: Env): Result[Value] =
    for {
      tokens <- Lexer.scanTokens(source)
      expr   <- Parser.fromTokens(tokens).expression.result
      value  <- Evaluate.expr(eval.Context(expr, env, Locals.empty, Nil))
    } yield value
}
