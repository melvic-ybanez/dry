package com.melvic.dry.interpreter.values

trait DSequence extends DObject with Countable {
  def getByIndex(index: Int): Value
}
