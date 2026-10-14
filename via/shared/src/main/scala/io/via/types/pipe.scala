package io.via.types

/**
 * Pipe operator
 */
extension [A, B](a: A) inline infix def |>(f: A => B): B = f(a)
