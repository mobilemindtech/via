package io.via.types

/** Pipe operator
  */
extension [A, B](a: A) infix def |>(f: A => B): B = f(a)
