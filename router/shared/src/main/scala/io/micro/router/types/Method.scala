package io.micro.router.types

enum Method(val verb: String):
  case GET extends Method("GET")
  case POST extends Method("POST")
  case PUT extends Method("PUT")
  case DELETE extends Method("DELETE")
  case PATCH extends Method("PATCH")
  case HEAD extends Method("HEAD")
  case OPTIONS extends Method("OPTIONS")
  case TRACE extends Method("TRACE")
  case CONNECT extends Method("CONNECT")
  case ANY extends Method("ANY")
