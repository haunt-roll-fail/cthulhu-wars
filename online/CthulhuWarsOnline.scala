package cws.online

import slick.jdbc.HsqldbProfile.api._
import slick.jdbc.HsqldbProfile.api.DBIO.seq

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

object CthulhuWarsOnline {
    def main(args : Array[String]) : Unit = {
        if (args.size != 4) {
            println("cwo <create|drop|run|drop-create-run> <database-name> <server-url> <port>")
            return
        }

        val mode = args(0).split("-")
        val database = args(1)
        val url = args(2)
        val port = args(3).toInt

        def readFile(path : String) = {
            import java.nio.charset.StandardCharsets._
            import java.nio.file.{Files, Paths}

            new String(Files.readAllBytes(Paths.get(path)), UTF_8)
        }

        def full = readFile("../solo/index.html").replace("###SERVER-URL###", url)

        implicit class Ascii(val s : String) {
            def ascii = s.filter(c => c >= 32 && c < 128)
        }

        case class Game(name : String, id : Option[Int] = None)

        class Games(tag : Tag) extends Table[Game](tag, "Games") {
            def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
            def name = column[String]("name")
            def * = (name, id.?).mapTo[Game]
        }

        val games = TableQuery[Games]
        val gamesId = games.returning(games.map(_.id))

        case class Role(gameId : Int, name : String, secret : String)

        class Roles(tag : Tag) extends Table[Role](tag, "Roles") {
            def gameId = column[Int]("gameId")
            def name = column[String]("name")
            def secret = column[String]("secret")
            def * = (gameId, name, secret).mapTo[Role]
            def pk = primaryKey("Roles" + "Key", (gameId, name))
            def game = foreignKey("Roles" + "Games", gameId, games)(_.id)
        }

        val roles = TableQuery[Roles]

        case class Log(gameId : Int, index : Int, role : String, value : String)

        class Logs(tag : Tag) extends Table[Log](tag, "Logs") {
            def gameId = column[Int]("gameId")
            def index = column[Int]("index")
            def role = column[String]("role")
            def value = column[String]("value")
            def * = (gameId, index, role, value).mapTo[Log]
            def pk = primaryKey("Logs" + "Key", (gameId, index))
            def game = foreignKey("Logs" + "Games", gameId, games)(_.id)
        }

        val logs = TableQuery[Logs]

        val db = Database.forURL("jdbc:hsqldb:file:" + database, driver="org.hsqldb.jdbcDriver")

        object q {
            import scala.concurrent.Await
            import scala.concurrent.duration.Duration

            def apply[E <: Effect](actions : DBIOAction[_, NoStream, E]*) = Await.result(db.run(DBIO.seq(actions : _*).withPinnedSession), Duration.Inf)
            def apply[R](action : DBIOAction[R, NoStream, Effect.Read]) : R = Await.result(db.run(action.withPinnedSession), Duration.Inf)
        }

        if (mode.contains("drop")) {
            q(roles.schema.dropIfExists, logs.schema.dropIfExists, games.schema.dropIfExists)
        }

        if (mode.contains("create")) {
            q(games.schema.create, logs.schema.create, roles.schema.create)
        }

        if (!mode.contains("run")) {
            return
        }

        def secret = {
            val random = new scala.util.Random()

            0.until(16).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(random.nextInt(26))).mkString("")
        }

        implicit val system = ActorSystem()
        implicit val executionContext = system.dispatcher

        def htm(s : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s))
        def jsx(s : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s))
        def txt(s : String) = complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, s))
        def rdr(s : String) = redirect(s, StatusCodes.TemporaryRedirect)

        val route =
            (get & path("")) {
                htm(full)
            } ~
            pathPrefix("hrf") {
                getFromDirectory("../solo")
            } ~
            (get & path("play" / Segment)) { name =>
                rdr("/#" + name)
            } ~
            (post & path("create")) {
                decodeRequest {
                    entity(as[String]) { body =>
                        val ss = body.split("\n").toList.map(_.ascii)
                        val rls = List("$", "#") ++ ss(0).split(" ").toList
                        val name = ss(2)
                        val lgs = ss.drop(1)
                        val srs = rls.map(r => r -> secret).toMap
                        q((gamesId += Game(name)).flatMap(id => seq(
                            roles ++= rls.map(r => Role(id, r, srs(r))),
                            logs ++= lgs.zipWithIndex.map { case (l, n) => Log(id, n, "", l) }
                        )))
                        txt(srs("$"))
                    }
                }
            } ~
            (get & path("roles" / Segment)) { role =>
                val list = q(roles.filter(_.secret === role).filter(_.name === "$").map(_.gameId).result.head.flatMap { id =>
                    roles.filter(_.gameId === id).result
                })
                txt(list.map(r => r.name + " " + r.secret).mkString("\n"))
            } ~
            (get & path("role" / Segment)) { role =>
                val name = q(roles.filter(_.secret === role).map(_.name).result.head)
                txt(name)
            } ~
            (get & path("read" / Segment / IntNumber)) { (role, from) =>
                val log = q(roles.filter(_.secret === role).map(_.gameId).result.head.flatMap { id =>
                        logs.filter(_.gameId === id).filter(_.index >= from).map(_.value).result
                })
                txt(log.mkString("\n"))
            } ~
            (post & path("write" / Segment / IntNumber)) { (role, index) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val ss = body.split("\n").toList.map(_.ascii)

                        try {
                            q(roles.filter(_.secret === role).filter(_.name =!= "#").map(r => (r.name, r.gameId)).result.head.flatMap { case (name, id) =>
                                logs ++= 0.until(ss.size).map(n => Log(id, index + n, name, ss(n)))
                            })
                            complete(StatusCodes.Accepted)
                        }
                        catch {
                            case e : java.sql.SQLIntegrityConstraintViolationException => complete(StatusCodes.Conflict)
                        }
                    }
                }
            }

        val bindingFuture = Http().newServerAt("0.0.0.0", port).bind(route)

        while (true) Thread.sleep(1000)

        bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
    }
}
