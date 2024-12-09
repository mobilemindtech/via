package io.micro.router

import io.micro.router.core.*
import io.micro.router.core.Path.*
import io.micro.router.core.Route.route
import io.micro.router.core.RouteChain.{RouteFound, RouteNotFound}
import io.micro.router.core.RouteQuery.*

import org.scalatest.funsuite.AnyFunSuite

class RoutingSpec extends AnyFunSuite:

  def testPathParams(uri: String, routes: Seq[Route], expects: Param*) =
    RouteChain.chain(uri, routes) match

      case RouteFound(_, _, params, _, _) =>
        assert(
          expects.size == params.size,
          s"wrong expected params count: ${expects.size} != ${params.size}"
        )

        for i <- params.indices do
          val found = params.at(i)
          val expected = expects(i)
          assert(
            found == expected,
            s"wong param ${expected.name} expected ${expected}  != found ${found}"
          )

      case RouteNotFound() =>
        fail(s"route $uri not found")

  def testQuery(uri: String, routes: Seq[Route], expects: (String, Any)*) =
    RouteChain.chain(uri, routes) match
      case RouteFound(route, _, _, query, _) =>
        assert(
          expects.size == query.size,
          s"wrong expected params count: ${expects.size} != ${query.size}"
        )

        for tp <- expects do
          query.tuple.find(_._1 == tp._1) match
            case Some(found) =>
              val expected = tp._2
              assert(
                found._2 == expected,
                s"wong param ${found._1} expected ${expected} != found ${found}"
              )
            case None => fail(s"query ${tp._1} not found")

      case RouteNotFound() =>
        fail(s"route $uri not found")

  def testNotFound(uri: String, routes: Route*) =
    val r = RouteChain.chain(uri, routes)
    assert(r == RouteNotFound(), s"expected not found route to uri ${uri}")

  test("route compile") {
    val home = root |> route
    val test = root / "test" |> route
    val testGet = test / int("id") |> route
    assert(home.pattern == "^/$", "wrong compile home")
    assert(test.pattern == "^/test$", "wrong compile test")
    assert(test.pure, "route should be pure")
    assert(testGet.pattern == "^/test/([0-9]+)$", "wrong compile testGet")
    assert(!testGet.pure, "route cannot be pure")
  }

  test("route process found param int v1") {
    val r = root / "test" / int("id") |> route
    testPathParams("/test/22", r :: Nil, ParamInt("id", 22))
  }

  test("route process found param int v2") {
    val r = root / "test" / int("id") |> route
    testPathParams("/test/22", r :: Nil, ParamInt("id", 22))
  }

  test("route process found param two int") {
    val test = root / "test" / int("id") / int("id2") |> route
    assert(
      test.pattern == "^/test/([0-9]+)/([0-9]+)$",
      "wrong compile test two path params"
    )
    testPathParams(
      "/test/22/100",
      test :: Nil,
      ParamInt("id", 22),
      ParamInt("id2", 100)
    )
  }

  test("route process found param string") {
    val test = root / "test" / param("name") |> route
    assert(test.pattern == "^/test/(.+)$", "wrong compile test param string")
    testPathParams("/test/john", test :: Nil, ParamStr("name", "john"))
  }

  test("route process found param string + path") {
    val test = root / "test" / param("name") / "show" |> route
    assert(
      test.pattern == "^/test/(.+)/show$",
      "wrong compile test param string"
    )
    testPathParams("/test/john/show", test :: Nil, ParamStr("name", "john"))
  }

  test("route process found param int string") {
    val test = root / "test" / int("id") / param("name") |> route
    assert(
      test.pattern == "^/test/([0-9]+)/(.+)$",
      "wrong compile test two path params"
    )
    testPathParams(
      "/test/22/john",
      test :: Nil,
      ParamInt("id", 22),
      ParamStr("name", "john")
    )
  }

  test("route process found param tail") {
    val test = root / "test" / int("id") / tail("paths") |> route
    assert(
      test.pattern == "^/test/([0-9]+)/(.+)$",
      "wrong compile test two path params"
    )
    testPathParams(
      "/test/22/john/do/ok",
      test :: Nil,
      ParamInt("id", 22),
      ParamPaths("paths", "john" :: "do" :: "ok" :: Nil)
    )
  }

  test("route chain regex") {
    val test =
      root / "test" / regex(
        "date",
        "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}"
      ) |> route
    assert(
      test.pattern == "^/test/([0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2})$",
      "wrong route"
    )
    testPathParams(
      "/test/2024-02-01",
      test :: Nil,
      ParamStr("date", "2024-02-01")
    )
    testNotFound("/test/02-01-2024", test)
  }

  test("route chain query int") {
    val test = root / "test" /? q_int("id") |> route
    assert(test.pattern == "^/test/$", "wrong route")
    testQuery("/test/?id=22", test :: Nil, ("id", 22))
  }

  test("route chain query int and bool") {
    val test = root / "test" /? q_int("id") & q_bool("enabled") |> route
    assert(test.pattern == "^/test/$", "wrong route")
    testQuery(
      "/test/?id=90&enabled=true",
      test :: Nil,
      ("id", 90),
      ("enabled", true)
    )
  }

  test("route chain query int and bool str") {
    val test =
      root / "test" /? q_int("id") & q_bool("enabled") & q_str(
        "name"
      ) |> route
    assert(test.pattern == "^/test/$", "wrong route")
    testQuery(
      "/test/?id=90&enabled=true&name=ricardo",
      test :: Nil,
      ("id", 90),
      ("enabled", true),
      ("name", "ricardo")
    )
  }

  test("route chain query list") {
    val test = root / "test" /? q_list_int("ids") |> route
    assert(test.pattern == "^/test/$", "wrong route")
    testQuery("/test/?ids=1,2,3,4,5", test :: Nil, ("ids", List(1, 2, 3, 4, 5)))
  }

  test("route chain query list option") {
    val test = root / "test" /? q_list_int_opt("ids") |> route
    assert(test.pattern == "^/test/$", "wrong route")
    testQuery(
      "/test/?ids=1,2,3,4,5",
      test :: Nil,
      ("ids", Some(List(1, 2, 3, 4, 5)))
    )
    testQuery("/test/", test :: Nil, ("ids", None))
  }

  test("route chain query get values") {

    val chain: (String, Route) => (Query => Unit) => Unit = {
      (uri, route) => f =>
        RouteChain.chain(uri, route :: Nil) match
          case RouteFound(route, _, _, query, _) =>
            f(query)
          case RouteNotFound() => fail("not found")

    }

    val test1 = root / "test" /? q_list_int_opt("ids") |> route
    chain("/test/", test1) { q =>
      assert(q.listInt("ids") == Nil, "ids should be Nil")
    }

    val test2 = root / "test" /? q_list_int_opt("ids") |> route
    chain("/test/?ids=1,2,3", test2) { q =>
      assert(q.listInt("ids") == List(1, 2, 3), "ids should be 1,2,3")
    }

    val test3 = root / "test" /? q_list_int("ids") |> route
    chain("/test/?ids=1,2,3", test3) { q =>
      assert(q.listInt("ids") == List(1, 2, 3), "ids should be 1,2,3")
    }

    val test4 = root / "test" /? q_bool("test") |> route
    chain("/test/?test=true", test4) { q =>
      assert(q.bool("test").contains(true), "ids should be true")
    }

    val test5 = root / "test" /? q_str("name") |> route
    chain("/test/?name=ricardo", test5) { q =>
      assert(q.str("name").contains("ricardo"), "name should be true")
    }

    val test6 = root / "test" /? q_str_opt("name") |> route
    chain("/test/?name=ricardo", test6) { q =>
      assert(q.str("name").contains("ricardo"), "name should be true")
    }

  }
