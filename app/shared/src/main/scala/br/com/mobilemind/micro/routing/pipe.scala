package br.com.mobilemind.micro.routing

extension [A, B](a: A)
  infix def |> (f: A => B): B = f(a)
