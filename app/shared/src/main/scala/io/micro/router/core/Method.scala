package io.micro.router.core

enum Method(val verb: String):
  case Get extends Method("GET")
  case Post extends Method("POST")
  case Put extends Method("PUT")
  case Delete extends Method("DELETE")
  case Patch extends Method("PATCH")
  case Head extends Method("HEAD")
  case Options extends Method("OPTIONS")
  case Any extends Method("ANY")
