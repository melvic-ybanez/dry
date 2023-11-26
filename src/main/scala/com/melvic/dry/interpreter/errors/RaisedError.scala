package com.melvic.dry.interpreter.errors

import com.melvic.dry.Show
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Value
import com.melvic.dry.interpreter.values.{DException, DInstance}
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Failure.showLine

final case class RaisedError(instance: DInstance) extends Failure with Value

object RaisedError {
  def show: Show[RaisedError] = { case RaisedError(instance) =>
    val maybeShow = for {
      message <- DException.messageOf(instance)
      line    <- DException.lineOf(instance)
    } yield show"Runtime Exception: $message\n${showLine(line)}."
    maybeShow.getOrElse("Runtime Exception")
  }
}
