package io.micro.router.core

extension [A, B](a: A)
  infix def |> (f: A => B): B = f(a)
