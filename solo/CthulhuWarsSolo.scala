package cws

import scala.scalajs._
import scala.scalajs.js.timers._

import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import Html._

import util.canvas._

sealed trait UIAction
case object UIStop extends UIAction
case class UILog(s : String) extends UIAction
case class UIPerform(game : Game, action : Action) extends UIAction
case class UIQuestion(faction : Faction, game : Game, actions : List[Action]) extends UIAction
case class UIQuestionDebug(faction : Faction, game : Game, actions : List[Action]) extends UIAction

case class UIRead(game : Game) extends UIAction
case class UIParse(game : Game, recorded : List[String]) extends UIAction

case class UIRollD6(game : Game, question : Game => String, roll : Int => Action) extends UIAction
case class UIRollBattle(game : Game, question : Game => String, n : Int, o : List[BattleRoll] => Action) extends UIAction
case class UIDrawES(game : Game, question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => Action) extends UIAction

@EnableReflectiveInstantiation
sealed trait Difficulty {
    def html : String
}
case object Off extends Difficulty { def html = "Off".hl }
case object Recorded extends Difficulty { def html = "Recorded".hl }
case object Human extends Difficulty { def html = "Human".hl }
case object Debug extends Difficulty { def html = "Debug".hl }
case object Easy extends Difficulty { def html = "Easy".styled("miss") }
case object Normal extends Difficulty { def html = "Normal".styled("pain") }
case object AllVsHuman extends Difficulty { def html = "AllVsHuman".styled("pain") }

class Setup(factions : $[Faction], diff : Difficulty) {
    var seating : $[Faction] = factions
    var options : $[GameOption] = $(factions.num match {
        case 3 => MapEarth33
        case 4 => MapEarth35
        case 5 => MapEarth55
    })

    def toggle(go : GameOption) {
        options = if (options.contains(go))
            options.but(go)
        else
            go +: options
    }

    def get(go : GameOption) = options.contains(go)

    var difficulty : Map[Faction, Difficulty] = seating.map(_ -> diff).toMap

    var dice = true
    var es = true
    var confirm = false
}

class CachedBitmap(val node : dom.Element) {
    private var bitmap : Bitmap = null

    def get(w : Int, h : Int) = {
        if (bitmap == null || bitmap.width != w || bitmap.height != h) {
            if (bitmap != null)
                if (bitmap.canvas.parentNode != null)
                    bitmap.canvas.parentNode.removeChild(bitmap.canvas)

            bitmap = new Bitmap(w, h)
        }

        node.appendChild(bitmap.canvas)

        bitmap
    }
}

case class GameOverAction(winners : List[Faction], msg : String) extends Action with Soft {
    def question = (game : Game) => winners.none.?("Winner is Humanity").|((winners.num == 1).?("Winner is " + winners.head).|("Winners are " + winners.mkString(", ")))
    def option = (game : Game) => msg
}

object CthulhuWarsSolo {
    val original = dom.document.documentElement.outerHTML

    def main(args : Array[String]) {
        if (dom.document.readyState == dom.DocumentReadyState.complete)
            setupUI()
        else
            dom.window.onload = (e) => setupUI()
    }

    def getElem(k : String) = dom.document.getElementById(k).asInstanceOf[html.Element]

    def getAsset(k : String) = dom.document.getElementById(k).asInstanceOf[html.Image]

    def newDiv(cl : String, content : String, click : () => Unit = null) = {
        val p = dom.document.createElement("div").asInstanceOf[html.Div]
        p.className = cl
        p.innerHTML = content
        if (click != null)
            p.onclick = (e) => click()
        p
    }

    def clear(e : dom.Element) {
        while (e.hasChildNodes())
            e.removeChild(e.lastChild)
    }

    def hide(e : html.Element) {
        e.style.display = "none"
    }

    def show(e : html.Element) {
        e.style.display = ""
    }

    def fail(url : String) {
        println("fail " + url)
    }

    def post(url : String, data : String)(then : String => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => then(xhr.response.asInstanceOf[String])
        xhr.open("POST", url, true)
        xhr.responseType = "text"
        xhr.send(data)
    }

    def postF(url : String, data : String)(then : => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => then
        xhr.onload = (e : dom.Event) => then
        xhr.open("POST", url, true)
        xhr.responseType = "text"
        xhr.send(data)
    }

    def get(url : String)(then : String => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => then(xhr.response.asInstanceOf[String])
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }

    def getF(url : String)(then : String => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => then("")
        xhr.onload = (e : dom.Event) => then(xhr.response.asInstanceOf[String])
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }

    def clipboard(text : String) : Boolean = {
        println(text)

        val cb = getElem("clipboard").asInstanceOf[html.TextArea]
        cb.value = text
        cb.focus()
        cb.select()
        try {
            dom.document.execCommand("copy")
        } catch {
            case e : Throwable => false
        }
    }



    def setupUI() {
        val (hash, quick) = dom.window.location.hash.drop(1) @@ {
            case "quick" => ("", true)
            case h => (h, false)
        }

        val location = dom.window.location.href.take(dom.window.location.href.length - dom.window.location.hash.length)
        val cwsOptions = Option(getElem("cws-options"))
        val delay = cwsOptions./~(_.getAttribute("data-delay").?)./~(_.toIntOption).|(30)
        val menu = cwsOptions./~(_.getAttribute("data-menu").?)./~(_.toIntOption).|(5)
        //val menu = cwsOptions./~(_.getAttribute("data-menu").?)./~(_.toIntOption).|(6) // Temp for test (comment out before deploying)
        val scroll = cwsOptions./~(_.getAttribute("data-scroll").?)./(_ == "true").|(false)
        val server = cwsOptions./~(_.getAttribute("data-server").?).|("###SERVER-URL###")
        //val server = cwsOptions./~(_.getAttribute("data-server").?).|("http://localhost:999/")
        //val redirect = location != server
        val redirect = false // making online games work with AN, as per hauntrollfail's advice

        val logDiv = getElem("log")

        def log(s : String) = {
            val nd = logDiv
            val isScrolledToBottom = nd.scrollHeight - nd.clientHeight <= nd.scrollTop + 1

            val p = newDiv("p", s)

            logDiv.appendChild(p)

            if (isScrolledToBottom || scroll)
                nd.scrollTop = nd.scrollHeight - nd.clientHeight
        }

        val msday = 24 * 60 * 60 * 1000
        val diff = (System.currentTimeMillis - Info.time) % msday / 1000
        val secs = diff % 60
        val mins = diff / 60

        val version = Info.name + " " + Info.version

        log(version)

        val actionDiv = getElem("action")

        def askTop() {
            actionDiv.scrollTop = 0
        }

        def ask(question : String, options : List[String], onResult : Int => Unit, style : Option[String] = None) : None.type = {
            clear(actionDiv)

            actionDiv.appendChild(newDiv("", question))

            options.zipWithIndex.foreach { case(o, n) =>
                actionDiv.appendChild(newDiv("option" + style./(" " + _).|(""), o, () => if (!o.contains("<a ")) { clear(actionDiv); onResult(n) }))
            }

            None
        }

        var scrollTop = 0.0

        def askM(qos : List[(String, String)], onResult : Int => Unit, style : Option[String] = None) : None.type = {
            clear(actionDiv)

            var prev : String = null

            qos.zipWithIndex.foreach { case((qq, o), n) =>
                var q = qq

                if (q == null)
                    q = qos.take(n - 1).reverse.lefts.dropWhile(_ == null).headOption.|(null)

                if (q != null && q != prev) {
                    actionDiv.appendChild(newDiv("", q))

                    prev = q
                }

                var s = style

                actionDiv.appendChild(newDiv("option" + s./(" " + _).|(""), o, () => { scrollTop = actionDiv.scrollTop; clear(actionDiv); onResult(n) }))
            }

            actionDiv.scrollTop = scrollTop

            None
        }

        var statuses = List(getElem("status-1"), getElem("status-2"), getElem("status-3"), getElem("status-4"), getElem("status-5"))

        val mapWest = getElem("map-west")
        val mapEast = getElem("map-east")

        val cw = getElem("to-cw")
        val ccw = getElem("to-ccw")

        val mapSmall = getElem("map-small")
        val mapBig = getElem("map-big")

        hide(mapBig.parentNode.parentNode.asInstanceOf[html.Element])
        hide(cw)
        hide(ccw)

        def processStatus(strings : List[String], ps : String) = strings
            ./(_.replace("------", "<br/>"))
            ./(s =>
                (if (s.startsWith("    "))
                    ("<div class='indent1'>" + s.drop(4))
                else
                    ("<div class='" + ps + "'>" + s))
                + "</div>")
           .mkString("\n")

        def onlineGameName = {
            val n = List("Power", "Doom", "Glory", "Destiny", "Might", "Fight", "Betrayal", "Fate", "Eternity", "Existance", "Time", "Space", "Agony", "Pain", "Torment", "Anything", "Sacrifice", "Death", "Despair").sortBy(_ => math.random())
            val c = List("for", "against", "versus", "through", "and", "of", "in", "as").sortBy(_ => math.random())
            n.head + " " + c.head + " " + n.last
        }

        def startOnlineGame(setup : Setup, recorded : List[String] = Nil) {
            val roles = setup.seating.%(f => setup.difficulty(f) == Human).map(_.short).mkString(" ")
            val stp = (setup.seating.%(f => setup.difficulty(f) != Off)./(f => f.short + ":" + setup.difficulty(f)).mkString("/") + " " + setup.options./(_.toString).mkString(" "))
            val name = onlineGameName
            post(server + "create", List(roles, version, name, stp).mkString("\n")) { master =>
                get(server + "roles/" + master) { ras =>
                    val rs = ras.split("\n").toList.map(_.split(" ")).map(s => s(0) -> s(1)).filter(_._1 != "$")
                    var ca = "Copy all"
                    def linkMenu() {
                        val op = ca +: rs.map {
                            case ("#", s) => "<a target=\"_blank\" rel=\"noopener\" href=\"" + server + "play/" + s + "\"><div>" + "Spectator".hl + "</div></a>"
                            case (f, s) => "<a target=\"_blank\" rel=\"noopener\" href=\"" + server + "play/" + s + "\"><div>" + "Play as".hl + " " + Serialize.parseFaction(f).get + "</div></a>"
                        }
                        ask(name.hl, op, { n =>
                            if (n == 0)
                                ca = clipboard(name + "\n" + rs.map {
                                    case ("#", s) => "Spectate " + server + "play/" + s
                                    case (f, s) => Serialize.parseFaction(f).get.short + " " + server + "play/" + s
                                }.mkString("\n")).?("Copied links to clipboard").|("Error copying to clipboard").hl

                            linkMenu()
                        })
                    }

                    linkMenu()
                }
            }
        }

        def startGame(setup : Setup, recorded : List[String] = Nil, self : Option[Faction] = None) {
            val seating = setup.seating.%(f => setup.difficulty(f) != Off)

            if (seating.num == 5)
                statuses = statuses.take(3) ++ statuses.drop(4).take(1) ++ statuses.drop(3).take(1)

            statuses.take(seating.num)./(_.parentNode.parentNode.asInstanceOf[html.Element]).foreach(show)
            statuses.drop(seating.num)./(_.parentNode.parentNode.asInstanceOf[html.Element]).foreach(hide)

            if (seating.num <= 4) {
                hide(getElem("to-cw6"))
                hide(getElem("to-ccw6"))
            }
            else {
                hide(getElem("to-cw4"))
                hide(getElem("to-ccw4"))
            }

            statuses.lazyZip(setup.seating).foreach { (s, f) =>
                s.as[html.Element].get.style.backgroundImage = "url(info/" + f.style + "-header.png)"
                s.as[html.Element].get.style.backgroundImage = "url(info/" + f.style + "-background.jpg)"
                s.as[html.Element].get.style.backgroundSize = "cover"
            }

            val board = setup.options.of[MapOption].starting match {
                case Some(MapEarth33) => EarthMap3
                case Some(MapEarth35) | None => EarthMap4v35
                case Some(MapEarth53) => EarthMap4v53
                case Some(MapEarth55) => EarthMap5
            }

            val track = seating.num @@ {
                case 3 => RitualTrack.for3
                case 4 => RitualTrack.for4
                case 5 => RitualTrack.for5
            }

            var game = new Game(board, track, seating, true, setup.options)

            var actions : List[Action] = Nil
            var queue : List[UIAction] = Nil
            var paused = recorded.any && hash == ""

            val serializer = new Serialize(game)

            def askFaction(game : Game, c : Continue) : UIAction = {
                def dontAttack(factions : List[Faction])(a : Action) = factions.map(f => !Explode.isOffense(game, f)(a)).reduce(_ && _)

                def filterAttack(actions : List[Action], factions : List[Faction]) = actions.%(dontAttack(factions)).some.|(actions)

                c match {
                    case Force(action) =>
                        UIPerform(game, action)

                    case DelayedContinue(_, continue) =>
                        askFaction(game, continue)

                    case RollD6(question, roll) if setup.dice =>
                        UIPerform(game, roll((1::2::3::4::5::6).maxBy(_ => random())))

                    case RollD6(question, roll) =>
                        UIRollD6(game, question, roll)

                    case RollBattle(_, 0, roll) =>
                        UIPerform(game, roll(Nil))

                    case RollBattle(_, n, roll) if setup.dice =>
                        UIPerform(game, roll(List.fill(n)(BattleRoll.roll())))

                    case RollBattle(question, n, roll) =>
                        UIRollBattle(game, question, n, roll)

                    case DrawES(_, 0, 0, 0, draw) =>
                        UIPerform(game, draw(0, true))

                    case DrawES(_, es1, es2, es3, draw) if setup.es =>
                        UIPerform(game, draw((List.fill(es1)(1) ++ List.fill(es2)(2) ++ List.fill(es3)(3)).maxBy(_ => random()), false))

                    case DrawES(question, es1, es2, es3, draw) =>
                        UIDrawES(game, question, es1, es2, es3, draw)

                    case GameOver(winners) =>
                        UIQuestion(null, game, GameOverAction(winners, "Hooray!") :: GameOverAction(winners, "Meh...") :: GameOverAction(winners, "Save replay"))

                    case Ask(faction, actions) =>
                        if (actions(0).isInstanceOf[PlayDirectionAction] || actions(0).isInstanceOf[StartingRegionAction]) {
                            hide(cw)
                            hide(ccw)
                        }
                        else {
                            if (game.order.dropWhile(_ != game.factions(0)) ++ game.order.takeWhile(_ != game.factions(0)) == game.factions) {
                                show(cw)
                                hide(ccw)
                            }
                            else {
                                hide(cw)
                                show(ccw)
                            }
                        }

                        val confirm = setup.confirm && setup.difficulty(faction) == Human

                        if (!confirm && actions.size == 1)
                            UIPerform(game, actions(0))
                        else
                        if (!confirm && actions(0).isInstanceOf[SpellbookAction] && actions.num == game.of(faction).unclaimedSB)
                            UIPerform(game, actions(0))
                        else {
                            setup.difficulty(faction) match {
                                case Human =>
                                    UIQuestion(faction, game, actions)
                                case Debug =>
                                    UIQuestionDebug(faction, game, actions)
                                case Easy =>
                                    UIPerform(game, faction match {
                                        case GC => Bot3(GC).ask(game, actions, 0.5)
                                        case CC => Bot3(CC).ask(game, actions, 0.2)
                                        case BG => Bot3(BG).ask(game, actions, 0.6)
                                        case YS => Bot3(YS).ask(game, actions, 0.3)
                                        case SL => BotSL   .ask(game, actions, 0.2)
                                        case WW => BotWW   .ask(game, actions, 0.2)
                                        case OW => BotOW   .ask(game, actions, 0.2)
                                        case AN => BotAN   .ask(game, actions, 0.2)
                                    })
                                case Normal =>
                                    UIPerform(game, faction match {
                                        case GC => BotGC   .ask(game, actions, 0.03)
                                        case CC => BotCC   .ask(game, actions, 0.03)
                                        case BG => Bot3(BG).ask(game, actions, 0.03)
                                        case YS => BotYS   .ask(game, actions, 0.03)
                                        case SL => BotSL   .ask(game, actions, 0.03)
                                        case WW => BotWW   .ask(game, actions, 0.03)
                                        case OW => BotOW   .ask(game, actions, 0.03)
                                        case AN => BotAN   .ask(game, actions, 0.03)
                                    })
                                case AllVsHuman =>
                                    val aa = Explode.explode(game, actions)
                                    val fr = setup.seating.but(faction).filter(f => setup.difficulty(f) == AllVsHuman)
                                    val as = filterAttack(aa, fr)
                                    UIPerform(game, faction match {
                                        case GC => BotGC   .ask(game, as, 0.03)
                                        case CC => BotCC   .ask(game, as, 0.03)
                                        case BG => Bot3(BG).ask(game, as, 0.03)
                                        case YS => BotYS   .ask(game, as, 0.03)
                                        case SL => BotSL   .ask(game, as, 0.03)
                                        case WW => BotWW   .ask(game, as, 0.03)
                                        case OW => BotOW   .ask(game, as, 0.03)
                                        case AN => BotAN   .ask(game, as, 0.03)
                                    })


                                case d => throw new Error("Unknown difficulty " + d)
                            }

                        }
                }
            }

            val mapBitmapSmall = new CachedBitmap(mapSmall)
            val mapBitmapBig = new CachedBitmap(mapBig)
            var map = mapBitmapSmall

            val findAnother = {
                var mplace = getAsset(board.id + "-place")

                val placeb = new Bitmap(mplace.width, mplace.height)
                placeb.context.drawImage(mplace, 0, 0)
                val placed = placeb.context.getImageData(0, 0, placeb.width, placeb.height).data
                val place = Array.tabulate(placeb.width, placeb.height)((x, y) => placed((y * placeb.width + x) * 4) * 0x010000 + placed((y * placeb.width + x) * 4 + 1) * 0x0100 + placed((y * placeb.width + x) * 4 + 2))

                (x : Int, y : Int) => {
                    val p = place(x)(y)
                    var xx = 0
                    var yy = 0
                    do {
                        xx = (placeb.width * math.random()).toInt
                        yy = (placeb.height * math.random()).toInt
                    }
                    while (place(xx)(yy) != p)
                    (xx, yy)
                }
            }

            case object DesecrationToken extends FactionUnitClass(YS, "Desecration", Token, 0)
            case object IceAgeToken extends FactionUnitClass(WW, "Ice Age", Token, 0)
            case object Cathedral extends FactionUnitClass(AN, "Cathedral", Token, 0)
            case object Gate extends UnitClass("Gate", Token, 3)
            case object FactionGlyph extends UnitClass("Faction Glyph", Token, 0)

            case class DrawRect(key : String, x : Int, y : Int, width : Int, height : Int, cx : Int = 0, cy : Int = 0)

            case class DrawItem(region : Region, faction : Faction, unit : UnitClass, health : UnitHealth, x : Int, y : Int) {
                val rect : DrawRect = unit match {
                    case Gate => { DrawRect("gate", x - 38, y - 38, 76, 76) }

                    case Acolyte => faction match {
                        case BG => DrawRect("bg-acolyte", x - 17, y - 54, 39, 60)
                        case CC => DrawRect("cc-acolyte", x - 17, y - 54, 38, 60)
                        case GC => DrawRect("gc-acolyte", x - 17, y - 54, 40, 59)
                        case YS => DrawRect("ys-acolyte", x - 17, y - 54, 39, 61)
                        case SL => DrawRect("sl-acolyte", x - 17, y - 54, 38, 60)
                        case WW => DrawRect("ww-acolyte", x - 17, y - 52, 40, 58)
                        case OW => DrawRect("ow-acolyte", x - 17, y - 54, 38, 60)
                        case AN => DrawRect("an-acolyte", x - 17, y - 54, 39, 60)
                        case _ => null
                    }

                    case FactionGlyph => faction match {
                        case BG => DrawRect("bg-glyph", x - 50, y - 50, 100, 100)
                        case CC => DrawRect("cc-glyph", x - 50, y - 50, 100, 100)
                        case GC => DrawRect("gc-glyph", x - 50, y - 50, 100, 100)
                        case YS => DrawRect("ys-glyph", x - 51, y - 50, 102, 100)
                        case SL => DrawRect("sl-glyph", x - 50, y - 50, 100, 102)
                        case WW => DrawRect("ww-glyph", x - 50, y - 50, 100, 100)
                        case OW => DrawRect("ow-glyph", x - 50, y - 50, 100, 100)
                        case AN => DrawRect("an-glyph", x - 50, y - 50, 100, 101)
                        case _ => null
                    }

                    case Ghoul         => DrawRect("bg-ghoul", x - 20, y - 40, 39, 47)
                    case Fungi         => DrawRect("bg-fungi", x - 40, y - 73, 72, 80)
                    case DarkYoung     => DrawRect("bg-dark-young", x - 53, y - 122, 83, 131)
                    case ShubNiggurath => DrawRect("bg-shub", x - 69, y - 173, 132, 185, 0, 10)

                    case Nightgaunt    => DrawRect("cc-nightgaunt", x - 36, y - 82, 69, 90, -1, 0)
                    case FlyingPolyp   => DrawRect("cc-flying-polyp", x - 36, y - 81, 73, 90, 10, 0)
                    case HuntingHorror => DrawRect("cc-hunting-horror", x - 86, y - 70, 166, 77)
                    case Nyarlathotep  => DrawRect("cc-nyarly", x - 50, y - 155, 106, 163)

                    case DeepOne       => DrawRect("gc-deep-one", x - 16, y - 25, 36, 31, 0, -5)
                    case Shoggoth      => DrawRect("gc-shoggoth", x - 31, y - 62, 63, 69)
                    case Starspawn     => DrawRect("gc-starspawn", x - 35, y - 63, 69, 70)
                    case Cthulhu       => DrawRect("gc-cthulhu", x - 65, y - 209, 117, 225, 0, 50)

                    case Undead        => DrawRect("ys-undead", x - 27, y - 49, 44, 54, -5, 0)
                    case Byakhee       => DrawRect("ys-byakhee", x - 32, y - 64, 57, 70)
                    case KingInYellow  => DrawRect("ys-king-in-yellow", x - 44, y - 111, 85, 116)
                    case Hastur        => DrawRect("ys-hastur", x - 87, y - 163, 150, 170)

                    case Wizard        => DrawRect("sl-wizard", x - 23, y - 33, 45, 41)
                    case SerpentMan    => DrawRect("sl-serpent-man", x - 34, y - 76, 70, 85, 3, 0)
                    case FormlessSpawn => DrawRect("sl-formless-spawn", x - 38, y - 85, 78, 94)
                    case Tsathoggua    => DrawRect("sl-tsathoggua", x - 75, y - 133, 152, 146)

                    case Wendigo       => DrawRect("ww-wendigo", x - 26, y - 62, 56, 68)
                    case GnophKeh      => DrawRect("ww-gnoph-keh", x - 30, y - 88, 61, 95)
                    case RhanTegoth    => DrawRect("ww-rhan-tegoth", x - 74, y - 128, 153, 135)
                    case Ithaqua       => DrawRect("ww-ithaqua", x - 112, y - 192, 164, 202)

                    case Mutant        => DrawRect("ow-mutant", x - 20, y - 52, 40, 58)
                    case Abomination   => DrawRect("ow-abomination", x - 30, y - 76, 62, 82)
                    case SpawnOW       => DrawRect("ow-spawn-of-yog-sothoth", x - 49, y - 94, 91, 100, 3, 3)
                    case YogSothoth    => DrawRect("ow-yog-sothoth", x - 82, y - 162, 132, 174)

                    case UnMan         => DrawRect("an-un-man", x - 24, y - 60, 48, 65)
                    case Reanimated    => DrawRect("an-reanimated", x - 28, y - 62, 57, 65)
                    case Yothan        => DrawRect("an-yothan", x - 61, y - 85, 122, 90)

                    case DesecrationToken => DrawRect("ys-desecration", x - 20, y - 20, 41, 40)
                    case IceAgeToken      => DrawRect("ww-ice-age", x - 44, y - 67, 91, 75)
                    case Cathedral        => DrawRect("an-cathedral", x - 39, y - 90, 78, 110)

                    case Ghast         => DrawRect("n-ghast", x - 17, y - 53, 35, 59)
                    case Gug           => DrawRect("n-gug", x - 36, y - 78, 73, 90)
                    case Shantak       => DrawRect("n-shantak", x - 39, y - 89, 79, 100)
                    case StarVampire   => DrawRect("n-star-vampire", x - 35, y - 75, 70, 85)

                    case GhastIcon        => DrawRect("ghast-icon", x - 17, y - 55, 50, 50)
                    case GugIcon          => DrawRect("gug-icon", x - 17, y - 55, 50, 50)
                    case ShantakIcon      => DrawRect("shantak-icon", x - 17, y - 55, 50, 50)
                    case StarVampireIcon  => DrawRect("star-vampire-icon", x - 17, y - 55, 50, 50)

                    case _ => null
                }

                def icon = if (health == Killed) Some(DrawRect("kill", x + rect.cx - 30, (rect.y + y) / 2 + rect.cy - 30, 60, 60)) else if (health == Pained) Some(DrawRect("pain", x - 29 + rect.cx, (rect.y + y) / 2 + rect.cy - 30, 60, 60)) else None
            }

            var oldPositions : List[DrawItem] = Nil
            var oldGates : List[Region] = Nil
            var horizontal = true

            def drawMap() {
                val upscale = 2

                val width = map.node.clientWidth * dom.window.devicePixelRatio
                val height = map.node.clientHeight * dom.window.devicePixelRatio

                val bitmap = map.get(width.~ * upscale, height.~ * upscale)

                bitmap.canvas.style.width = "100%"
                bitmap.canvas.style.height = "100%"

                if (bitmap.height <= bitmap.width != horizontal) {
                    horizontal = !horizontal
                    oldPositions = $
                    oldGates = $
                }

                var mp = getAsset(board.id)

                val gateXYO : Region => (Int, Int) = board.gateXYO

                def gateXY(r : Region) =
                    if (horizontal)
                        gateXYO(r)
                    else {
                        val (x, y) = gateXYO(r)
                        (mp.height - y, x)
                    }

                def find(ox : Int, oy : Int) =
                    if (horizontal)
                        findAnother(ox, oy)
                    else {
                        val (x, y) = findAnother(oy, mp.height - ox)
                        (mp.height - y, x)
                    }

                val g = bitmap.context

                g.setTransform(1, 0, 0, 1, 0, 0)

                g.clearRect(0, 0, bitmap.width, bitmap.height)

                if (horizontal)
                {
                    val dw = 12
                    val dh = 12
                    if ((dw + mp.width + dw) * bitmap.height < bitmap.width * (dh + mp.height + dh)) {
                        g.translate((bitmap.width - (dw + mp.width + dw) * bitmap.height / (dh + mp.height + dh)) / 2, 0)
                        g.scale(1.0 * bitmap.height / (dh + mp.height + dh), 1.0 * bitmap.height / (dh + mp.height + dh))
                    }
                    else {
                        g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                        g.scale(1.0 * bitmap.width / (dw + mp.width + dw), 1.0 * bitmap.width / (dw + mp.width + dw))
                    }
                    g.translate(dw, dh)
                    g.drawImage(mp, 0, 0)
                }
                else {
                    g.translate(bitmap.width, 0)
                    g.rotate(math.Pi / 2)

                    val dw = 12
                    val dh = 12
                    if ((dw + mp.width + dw) * bitmap.width < bitmap.height * (dh + mp.height + dh)) {
                        g.translate((bitmap.height - (dw + mp.width + dw) * bitmap.width / (dh + mp.height + dh)) / 2, 0)
                        g.scale(1.0 * bitmap.width / (dh + mp.height + dh), 1.0 * bitmap.width / (dh + mp.height + dh))
                    }
                    else {
                        g.translate(0, (bitmap.width - (dh + mp.height + dh) * bitmap.height / (dw + mp.width + dw)) / 2)
                        g.scale(1.0 * bitmap.height / (dw + mp.width + dw), 1.0 * bitmap.height / (dw + mp.width + dw))
                    }
                    g.translate(dw, dh)
                    g.drawImage(mp, 0, 0)
                    g.rotate(-math.Pi / 2)
                    g.translate(-mp.width/2, 0)
                }

                var saved = oldPositions
                oldPositions = Nil

                var draws : List[DrawItem] = Nil

                game.board.regions.foreach { r =>
                    val (px, py) = gateXY(r)
                    val gated = game.gates.contains(r)

                    val controler = game.factions.%(f => game.of(f).gates.contains(r)).single
                    val keeper = controler.flatMap(f => game.of(f).at(r).%(_.health == Alive).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && game.of(f).has(RedSign))).headOption)
                    val others = game.factions.%(f => !game.of(f).gates.contains(r)).%(game.of(_).at(r).num > 0).sortBy(f => f.strength(game, game.of(f).at(r), f))

                    var fixed : List[DrawItem] = Nil
                    var all : List[DrawItem] = Nil
                    var sticking : List[DrawItem] = Nil
                    var free : List[DrawItem] = Nil

                    if (gated)
                        fixed +:= DrawItem(r, null, Gate, Alive, px, py)

                    keeper match {
                        case Some(uf) => fixed +:= DrawItem(r, uf.faction, uf.uclass, uf.health, px, py)
                        case _ =>
                    }

                    game.factions.foreach { f =>
                        game.of(f).at(r).diff(keeper.toList).foreach { u =>
                            all +:= DrawItem(r, f, u.uclass, u.health, 0, 0)
                        }
                    }

                    if (game.desecrated.contains(r))
                        all +:= DrawItem(r, null, DesecrationToken, Alive, 0, 0)

                    if (game.cathedrals.contains(r))
                        all +:= DrawItem(r, null, Cathedral, Alive, 0, 0)

                    if (game.factions.%(game.of(_).iceage./(_ == r).|(false)).any)
                        all +:= DrawItem(r, null, IceAgeToken, Alive, 0, 0)

                    all.foreach { d =>
                        saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit && d.health == o.health) match {
                            case Some(o) =>
                                sticking +:= o.copy(health = d.health)
                                saved = saved.diff(List(o))
                            case None =>
                            saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit) match {
                                case Some(o) =>
                                    sticking +:= o.copy(health = d.health)
                                    saved = saved.diff(List(o))
                                case None =>
                                    free +:= d
                            }
                        }
                    }

                    if (free.num > sticking.num * 0 + 3 || free.%(_.unit.utype == GOO).any || (!oldGates.contains(r) && game.gates.contains(r))) {
                        free = free ++ sticking
                        sticking = Nil
                    }

                    def rank(d : DrawItem) = d.unit.utype match {
                        case Token => 0
                        case Cultist => 1
                        case Monster => 2
                        case Terror => 3
                        case GOO => 4
                    }

                    free.sortBy(d => -rank(d)).foreach { d =>
                        sticking +:= Array.tabulate(40)(n => find(px, py)).sortBy { case (x, y) => ((x - px).abs * 5 + (y - py).abs) }.map { case (x, y) => DrawItem(d.region, d.faction, d.unit, d.health, x, y) }.minBy { dd =>
                            (draws ++ fixed ++ sticking).map { oo =>
                                val d = dd.rect
                                val o = oo.rect
                                val w = min(o.x + o.width, d.x + d.width) - max(o.x, d.x)
                                val h = min(o.y + o.height, d.y + d.height) - max(o.y, d.y)
                                val s = (w > 0 && h > 0).?(w * h).|(0)
                                s * (1.0 / (o.width * o.height) + 1.0 / (d.width * d.height))
                            }.sum
                        }

                    }

                    draws ++= fixed
                    draws ++= sticking
                    oldPositions ++= sticking
                }

                oldGates = game.gates

                draws.sortBy(d => d.y + (d.unit == Gate).?(-2000).|(0) + (d.unit == DesecrationToken).?(-1000).|(0)).foreach { d =>
                    g.drawImage(getAsset(d.rect.key), d.rect.x, d.rect.y)
                }

                draws.sortBy(d => d.y + (d.unit == Gate).?(-2000).|(0) + (d.unit == DesecrationToken).?(-1000).|(0)).foreach { d =>
                    if (d.icon.any)
                        g.drawImage(getAsset(d.icon.get.key), d.icon.get.x, d.icon.get.y)
                }
            }

            mapSmall.onclick = (e) => {
                hide(mapSmall.parentNode.parentNode.asInstanceOf[html.Element])
                show(mapBig.parentNode.parentNode.asInstanceOf[html.Element])

                map = mapBitmapBig
                drawMap()
            }

            mapBig.onclick = (e) => {
                hide(mapBig.parentNode.parentNode.asInstanceOf[html.Element])
                show(mapSmall.parentNode.parentNode.asInstanceOf[html.Element])

                map = mapBitmapSmall
                drawMap()
            }

            drawMap()

            val statusBitmaps = statuses.take(seating.num)./(s => new CachedBitmap(s))

            def factionStatus(f : Faction, b : CachedBitmap) {
                if (!game.factions.contains(f))
                    return

                val p = game.of(f)

                def div(styles : String*)(content : String) = if (styles.isEmpty) "<div>" + content + "</div>" else "<div class=\"" + styles.mkString(" ") + "\">" + content + "</div>"
                def r(content : String) = div("right")(content)

                val name = div("name")("" + f + "")
                val nameS = div("name")(f.styled(f.short) + "")
                val power = div()(p.hibernating.?(("" + p.power + " Power").styled("hibernate")).|((p.power > 0).?(p.power.power).|("0 Power")))
                val powerS = div()(p.hibernating.?(("" + p.power + "P").styled("hibernate")).|((p.power > 0).?(("" + p.power + "P").styled("power")).|("0P")))
                val doom = div()(("" + p.doom + " Doom").styled("doom") + p.es.any.?(" + " + (p.es.num == 1).?("ES").|("" + p.es.num + " ES").styled("es")).|(""))
                val doomL = div()(("" + p.doom + " Doom").styled("doom") + p.es.any.?(" + " + (p.es.num == 1).?("Elder Sign").|("" + p.es.num + " Elder Signs").styled("es")).|(""))
                val doomS = div()(("" + p.doom + "D").styled("doom") + p.es.any.?("+" + ("" + p.es.num + "ES").styled("es")).|(""))

                val sb = p.spellbooks./{ sb =>
                    val full = sb.full
                    val s = sb.name.replace("\\", "\\\\").replace("'", "&#39") // "
                    // val d = "<div class='spellbook' onclick=\"onExternalClick('" + f.short + "', '" + s + "')\" onpointerover=\"onExternalOver('" + f.short + "', '" + s + "')\" onpointerout=\"onExternalOut('" + f.short + "', '" + s + "')\" >" + full + "</div>"
                    val d = s"""<div class='spellbook' onclick='onExternalClick("${f.short}", "${s}")' onpointerover='onExternalOver("${f.short}", "${s}")' onpointerout='onExternalOut("${f.short}", "${s}")' >${full}</div>"""
                    p.can(sb).?(d).|(d.styled("used"))
                }.mkString("") +
                (1.to(6 - p.spellbooks.num - p.requirements.num).toList./(x => f.styled("?")))./(div("spellbook", f.style + "-background")).mkString("") +
                p.requirements./{ r =>
                    val s = r.text.replace("\\", "\\\\") // "
                    val d = s"""<div class='spellbook' onclick='onExternalClick("${f.short}", "${s}")' onpointerover='onExternalOver("${f.short}", "${s}")' onpointerout='onExternalOut("${f.short}", "${s}")' >${r.text}</div>"""
                    // "<div class='spellbook' onclick=\"onExternalClick('" + f.short + "', '" + s + "')\" onpointerover=\"onExternalOver('" + f.short + "', '" + s + "')\" onpointerout=\"onExternalOut('" + f.short + "', '" + s + "')\" >" + r.text + "</div>"
                    d
                }.mkString("")

                val iconSpacing = 30
                val baseRightOffset = 3

                val lcis = p.loyaltyCards.zipWithIndex.map { case (lc, i) =>
                    val icon = lc.name match {
                        case "Ghast" => GhastIcon
                        case "Gug"   => GugIcon
                        case "Shantak"   => ShantakIcon
                        case "Star Vampire"   => StarVampireIcon
                    }

                    val d = DrawItem(null, f, icon, Alive, 0, 0)
                    val unitName = lc.name.replace("\\", "\\\\").replace("\"", "&quot;")
                    val factionShort = f.short.replace("\"", "&quot;")

                    val right = baseRightOffset + i * iconSpacing

                    s"""<img class='loyalty-card-icon'
                        src='info/n-${lc.name.toLowerCase.replace(" ", "-")}.svg'
                        style='right:${right}px;'
                        onclick='onExternalClick("${unitName}")'
                        onpointerover='onExternalOver("${unitName}")'
                        onpointerout='onExternalOut("${unitName}")' />"""
                }.mkString("")

                val h = 450
                val scale = b.node.clientHeight / h.toDouble
                val w = (b.node.clientWidth / scale).round.toInt

                val s =
                if (w > 580)
                    name + power + doomL
                else
                if (w > 420)
                    name + power + doom
                else
                if (w > 300)
                    nameS + powerS + doomS
                else
                    r(nameS) + r(powerS) + r(doomS)

                b.node.innerHTML = div("top")("<div onclick=onExternalClick('" + f.short + "') onpointerover=onExternalOver('" + f.short + "') onpointerout=onExternalOut('" + f.short + "')>" + s + "</div>") + sb + lcis

                val bitmap = b.get(w, h)

                bitmap.canvas.style.pointerEvents = "none"
                bitmap.canvas.style.width = "" + b.node.clientWidth + "px"
                bitmap.canvas.style.height = "" + b.node.clientHeight + "px"

                val g = bitmap.context
                g.setTransform(1, 0, 0, 1, 0, 0)
                g.clearRect(0, 0, bitmap.width, bitmap.height)

                def dd(d : DrawRect) = g.drawImage(getAsset(d.key), d.x, d.y)

                dd(DrawItem(null, f, FactionGlyph, Alive, 55, 55).rect)

                if (p.gates.contains(SL.slumber)) {
                    dd(DrawItem(null, f, Gate, Alive, w - 46, 56).rect)
                    dd(DrawItem(null, p.at(SL.slumber, Cultist).head.faction, p.at(SL.slumber, Cultist).head.uclass, Alive, w - 46, 56).rect)
                }

                var smx = 0
                game.factions.%(_ != f).foreach { e =>
                    if (game.of(e).borrowed.contains(f.abilities.head)) {
                        dd(DrawItem(null, e, SerpentMan, Alive, w - 46 + smx, 86).rect)
                        smx -= 20
                    }
                }

                val deep = if (p.at(GC.deep).any) {
                    var draws = List(DrawItem(null, f, Cthulhu, Alive, 64, h - 12 - 6))

                    while (draws.num < p.at(GC.deep).num) {
                        val last = draws.last
                        draws :+= ((last.unit, p.at(GC.deep)(draws.num).uclass) match {
                            case (Cthulhu, Starspawn) => DrawItem(null, f, Starspawn, Alive, 75 + last.x, 6 + last.y)
                            case (Starspawn, Starspawn) => DrawItem(null, f, Starspawn, Alive, 70 + last.x, last.y)

                            case (Cthulhu, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, 76 + last.x, 6 + last.y)
                            case (Starspawn, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, 66 + last.x, last.y)
                            case (Shoggoth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, 62 + last.x, last.y)

                            case (Cthulhu, DeepOne) => DrawItem(null, f, DeepOne, Alive, 64 + last.x, 6 + last.y)
                            case (Starspawn, DeepOne) => DrawItem(null, f, DeepOne, Alive, 51 + last.x, last.y)
                            case (Shoggoth, DeepOne) => DrawItem(null, f, DeepOne, Alive, 48 + last.x, last.y)
                            case (DeepOne, DeepOne) if last.health == Alive => DrawItem(null, f, DeepOne, Pained, last.x, last.y - 31)
                            case (DeepOne, DeepOne) if last.health == Pained => DrawItem(null, f, DeepOne, Alive, 35 + last.x, last.y + 31)

                            case (Cthulhu, Acolyte) => DrawItem(null, f, Acolyte, Alive, 57 + last.x, 6 + last.y)
                            case (Starspawn, Acolyte) => DrawItem(null, f, Acolyte, Alive, 52 + last.x, last.y)
                            case (Shoggoth, Acolyte) => DrawItem(null, f, Acolyte, Alive, 48 + last.x, last.y)
                            case (DeepOne, Acolyte) if last.health == Alive => DrawItem(null, f, Acolyte, Alive, 36 + last.x, last.y)
                            case (DeepOne, Acolyte) if last.health == Pained => DrawItem(null, f, Acolyte, Alive, 36 + last.x, last.y + 31)
                            case (Acolyte, Acolyte) => DrawItem(null, f, Acolyte, Alive, 35 + last.x, last.y)
                            
                            case (Cthulhu, Ghast) => DrawItem(null, f, Ghast, Alive, 62 + last.x, 6 + last.y)
                            case (Starspawn, Ghast) => DrawItem(null, f, Ghast, Alive, 54 + last.x, last.y)
                            case (Shoggoth, Ghast) => DrawItem(null, f, Ghast, Alive, 52 + last.x, last.y)
                            case (DeepOne, Ghast) if last.health == Alive => DrawItem(null, f, Ghast, Alive, 39 + last.x, last.y)
                            case (DeepOne, Ghast) if last.health == Pained => DrawItem(null, f, Ghast, Alive, 39 + last.x, last.y + 31)
                            case (Acolyte, Ghast) => DrawItem(null, f, Ghast, Alive, 37 + last.x, last.y)
                            case (Ghast, Ghast) => DrawItem(null, f, Ghast, Alive, 35 + last.x, last.y)

                            case (Cthulhu, Gug) => DrawItem(null, f, Gug, Alive, 78 + last.x, 6 + last.y)
                            case (Starspawn, Gug) => DrawItem(null, f, Gug, Alive, 70 + last.x, last.y)
                            case (Shoggoth, Gug) => DrawItem(null, f, Gug, Alive, 66 + last.x, last.y)
                            case (DeepOne, Gug) if last.health == Alive => DrawItem(null, f, Gug, Alive, 56 + last.x, last.y)
                            case (DeepOne, Gug) if last.health == Pained => DrawItem(null, f, Gug, Alive, 56 + last.x, last.y + 31)
                            case (Acolyte, Gug) => DrawItem(null, f, Gug, Alive, 54 + last.x, last.y)
                            case (Ghast, Gug) => DrawItem(null, f, Gug, Alive, 55 + last.x, last.y)
                            case (Gug, Gug) => DrawItem(null, f, Gug, Alive, 72 + last.x, last.y)
                            
                            case (Cthulhu, Shantak) => DrawItem(null, f, Shantak, Alive, 83 + last.x, 6 + last.y)
                            case (Starspawn, Shantak) => DrawItem(null, f, Shantak, Alive, 66 + last.x, last.y)
                            case (Shoggoth, Shantak) => DrawItem(null, f, Shantak, Alive, 66 + last.x, last.y)
                            case (DeepOne, Shantak) if last.health == Alive => DrawItem(null, f, Shantak, Alive, 49 + last.x, last.y)
                            case (DeepOne, Shantak) if last.health == Pained => DrawItem(null, f, Shantak, Alive, 49 + last.x, last.y + 31)
                            case (Acolyte, Shantak) => DrawItem(null, f, Shantak, Alive, 50 + last.x, last.y)
                            case (Ghast, Shantak) => DrawItem(null, f, Shantak, Alive, 48 + last.x, last.y)
                            case (Gug, Shantak) => DrawItem(null, f, Shantak, Alive, 63 + last.x, last.y)
                            case (Shantak, Shantak) => DrawItem(null, f, Shantak, Alive, 74 + last.x, last.y)

                            case (Cthulhu, StarVampire) => DrawItem(null, f, StarVampire, Alive, 79 + last.x, 6 + last.y)
                            case (Starspawn, StarVampire) => DrawItem(null, f, StarVampire, Alive, 61 + last.x, last.y)
                            case (Shoggoth, StarVampire) => DrawItem(null, f, StarVampire, Alive, 60 + last.x, last.y)
                            case (DeepOne, StarVampire) if last.health == Alive => DrawItem(null, f, StarVampire, Alive, 50 + last.x, last.y)
                            case (DeepOne, StarVampire) if last.health == Pained => DrawItem(null, f, StarVampire, Alive, 50 + last.x, last.y + 31)
                            case (Acolyte, StarVampire) => DrawItem(null, f, StarVampire, Alive, 52 + last.x, last.y)
                            case (Ghast, StarVampire) => DrawItem(null, f, StarVampire, Alive, 53 + last.x, last.y)
                            case (Gug, StarVampire) => DrawItem(null, f, StarVampire, Alive, 64 + last.x, last.y)
                            case (Shantak, StarVampire) => DrawItem(null, f, StarVampire, Alive, 70 + last.x, last.y)
                            case (StarVampire, StarVampire) => DrawItem(null, f, StarVampire, Alive, 65 + last.x, last.y)

                            case (a, b) => { println("GC DEEP:" + a + " -> " + b); null }
                        })
                    }

                    draws./(_.rect)
                } else Nil

                val captured = {
                    var draws : List[DrawItem] = Nil

                    game.factions.%(_ != f)./~(e => game.of(e).at(f.prison)).foreach { u =>
                        val d = DrawItem(null, u.faction, u.uclass, Alive, 0, 0)

                        val x = draws./(_.rect.width).sum - d.rect.x

                        draws :+= DrawItem(null, u.faction, u.uclass, Alive, x, h - 12)
                    }

                    val t = draws./(_.rect.width).sum

                    draws./(_.rect)
                }

                val dw = deep.any.?(deep./(r => r.x + r.width).max - deep./(_.x).min).|(0)
                val cw = captured.any.?(captured./(r => r.x + r.width).max - captured./(_.x).min).|(0)
                val draws =
                    if (dw + cw > w - 20)
                        (deep ++ captured./(r => r.copy(x = r.x + dw)))./(r => r.copy(x = r.x * (w - 20) / (dw + cw)))
                    else
                    if (dw > 0)
                        deep ++ captured./(r => r.copy(x = r.x + w - cw - 20))
                    else
                        captured./(r => r.copy(x = r.x + (w - cw) / 2))

                draws.reverse.foreach(dd)
            }

            def updateStatus() {
                0.until(seating.num).foreach { n =>
                    factionStatus(game.factions(n), statusBitmaps(n))
                }

                mapWest.innerHTML = (board.west :+ GC.deep)./(r => processStatus(game.regionStatus(r), "p8")).mkString("")
                mapEast.innerHTML = board.east./(r => processStatus(game.regionStatus(r), "p8")).mkString("")

                drawMap()
            }

            dom.window.onresize = e => updateStatus()

            def perform(action : Action) {
                queue :+= UIPerform(game, action)

                if (!paused)
                    processUI()
            }

            def finishUI() {
                updateUI() match {
                    case Some(_) => finishUI()
                    case None =>
                }
            }

            def processUI() {
                updateUI() match {
                    case Some((s, n)) =>
                        if (s) {
                            updateStatus()
                            if (recorded.any && hash == "")
                                replayMenu()
                        }

                        setTimeout(n * delay) { processUI() }
                    case None =>
                }
            }

            def updateUI() : Option[(Boolean, Int)] = {
                queue.@@ {
                    case head :: rest =>
                        queue = rest
                        head match {
                            case UILog(l) => {
                                log(l)

                                Some((false, l.contains(".........").?(16).|(5)))
                            }
                            case UIPerform(g, a : GameOverAction) if a.msg == "Save replay" => {
                                val name = version + " replay from " + (new js.Date()).toLocaleString()
                                val data = (name +: (g.factions./(_.short).mkString("-") + " " + g.options./(_.toString).mkString(" ")) +: actions.reverse./(serializer.write)).mkString("\n")
                                val result = original.replace("<div id=\"replay\"></div>", "<div id=\"replay\">\n" + data + "\n</div>")

                                val blob = {
                                    import scala.scalajs.js.typedarray._
                                    import scala.scalajs.js.JSConverters._

                                    new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
                                }

                                val link = dom.document.createElement("a").asInstanceOf[html.Anchor]
                                link.href = dom.URL.createObjectURL(blob)
                                link.asInstanceOf[js.Dynamic].download = name.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
                                dom.document.body.appendChild(link)
                                link.click()

                                val winners = a.winners
                                queue :+= UIQuestion(null, game, GameOverAction(winners, "Hooray!") :: GameOverAction(winners, "Meh...") :: GameOverAction(winners, "Save replay"))

                                Some((false, 10))
                            }
                            case UIPerform(g, a) if hash != "" && self == None => {
                                queue :+= UIRead(g)

                                Some((false, 30))
                            }
                            case UIPerform(g, a) if hash != "" && Explode.isRecorded(a) => {
                                postF(server + "write/" + hash + "/" + (actions.num + 3), serializer.write(a)) {
                                    queue :+= UIRead(g)

                                    processUI()
                                }

                                None
                            }
                            case UIRead(g) => {
                                getF(server + "read/" + hash + "/" + (actions.num + 3)) { ll =>
                                    queue :+= UIParse(g, ll.split("\n").toList.filter(_ != ""))

                                    processUI()
                                }

                                None
                            }
                            case UIParse(g, recorded) if recorded.none => {
                                queue :+= UIRead(g)

                                Some((false, 30))
                            }
                            case UIParse(g, recorded) => {
                                var cc : Continue = null

                                recorded.foreach { aa =>
                                    val a = serializer.parseAction(aa)

                                    actions +:= a

                                    val (l, c) = game.perform(a)

                                    l.foreach(log)

                                    cc = c
                                }

                                queue :+= askFaction(game, cc)

                                Some((true, 0))
                            }

                            case UIPerform(g, aa) => {
                                val a = aa match {
                                    case esa : ElderSignAction if recorded.any => esa.copy(public = true)
                                    case a => a
                                }

                                if (Explode.isRecorded(a))
                                    actions +:= a

                                val (l, c) = g.perform(a)

                                l.foreach { s =>
                                    queue :+= UILog(s)
                                }

                                val t = c match {
                                    case Ask(f, _) if setup.difficulty(f) == Human => 2
                                    case Ask(_, actions) if actions.distinct.num <= 2 => 4
                                    case DelayedContinue(n, _) => n
                                    case _ => 30
                                 }

                                if (recorded.any && hash == "") {
                                    if (recorded.num > actions.num && !paused)
                                        queue :+= UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">")))
                                }
                                else
                                    queue :+= askFaction(game, c)

                                Some((true, t))
                            }
                            case UIRollD6(g, q, roll) => {
                                ask(q(g), (1::2::3::4::5::6)./("[" + _.styled("power") + "]"), x => perform(roll(x)))
                            }
                            case UIRollBattle(g, q, n, roll) if n <= 3 => {
                                def apr(br : BattleRoll) = 0.to(n).toList./(n => List.fill(n)(br))
                                val results = apr(Kill)./~(k => apr(Pain)./~(p => apr(Miss)./(m => k ++ p ++ m))).%(_.num == n)
                                val os = results./(roll)
                                ask(q(g), results./(_.mkString(" ")), v => perform(roll(results(v))))
                            }
                            case UIRollBattle(g, q, n, roll) => {
                                val osK = 0.to(n).toList./(k => List.fill(k)(Kill))./(x => x.any.?(x.mkString(" ")).|("None"))
                                ask(q(g) + "<br/>" + "Number of " + "Kills".styled("kill"), osK, kills => {
                                    if (kills == n) {
                                        perform(roll(List.fill(kills)(Kill)))
                                    }
                                    else {
                                        val osP = 0.to(n - kills).toList./(p => List.fill(p)(Pain))./(x => x.any.?(x.mkString(" ")).|("None"))
                                        ask(q(g) + "<br/>" + "Number of " + "Pains".styled("pain"), osP, pains => {
                                            perform(roll(List.fill(kills)(Kill) ++ List.fill(pains)(Pain) ++ List.fill(n - kills - pains)(Miss)))
                                        })
                                    }
                                })
                            }
                            case UIDrawES(g, q, es1, es2, es3, draw) =>
                                val options = ((1 -> es1) :: (2 -> es2) :: (3 -> es3)).%>(_ > 0)
                                ask(q(g), options./((e, q) => "[" + e.styled("es") + "]" + " of " + q), n => perform(draw(options(n)._1, true)))

                            case UIQuestion(f, g, actions) if f != null && hash != "" && Some(f) != self => {
                                ask("Waiting for " + f, Nil, n => {})
                                queue :+= UIRead(g)
                                Some((false, 50))
                            }
                            case UIQuestion(f, g, actions) => {
                                askM(actions./(a => a.question(g) -> a.option(g)), n => perform(actions(n)), Option(f)./(_.style + "-border"))
                            }
                            case UIQuestionDebug(f, g, actions) => {
                                val aa = Explode.explode(g, actions)

                                val sorted = if (f == BG)
                                    Bot3(BG).eval(g, aa).sortBy(-_.evaluations.map(_.weight).sum)
                                else
                                if (f == null) {
                                    (BotYS.eval(g, aa).sortWith(BotYS.compare).take(1) ++ BotYSOld.eval(g, aa).sortWith(BotYSOld.compare).take(1))./(ae => ae.copy(evaluations = ae.evaluations.%(_.desc != "random"))).distinct
                                }
                                else {
                                    val bot = (f match {
                                        case GC => BotGC
                                        case CC => BotCC
                                        case YS => BotYS
                                        case SL => BotSL
                                        case WW => BotWW
                                        case OW => BotOW
                                        case AN => BotAN
                                    })
                                    bot.eval(g, aa).sortWith(bot.compare)
                                }

                                askM(sorted./(wa => wa.action.question(g).some.|(" ") -> (wa.action.option(g) + " (" + wa.evaluations.headOption./(_.weight)./(v => v.styled((v > 0).?("power").|("doom"))).|("0") + ")" + "<br/>" +
                                    wa.evaluations./(e =>
                                        ("(" + e.weight.styled((e.weight > 0).?("power").|("doom")) + " -> " + e.desc + ")").styled("expl")
                                    ).mkString("<br/>"))),
                                    n => {
                                        println((sorted(n).action.question(g) + " -> " + sorted(n).action.option(g)).replaceAll("<[^>]*>", ""))
                                        sorted(n).evaluations.foreach { e =>
                                            println("  (" + e.weight + " -> " + e.desc + ")")
                                        }
                                        perform(sorted(n).action)
                                    }
                                )
                            }
                        }
                    case Nil =>
                        None
                }
            }

            def replayMenu() {
                def action = recorded.lift(actions.num).map(_.replace("&gt;", ">")).map(serializer.parseAction)
                ask("Replay (" + actions.num + " / " + recorded.num + ")", List(paused.?("Play").|("Pause"), "Start", "End", "Next"), {
                    case 0 =>
                        if (paused) {
                            paused = false
                            if (queue.none)
                                action.map(perform)
                        }
                        else {
                            paused = true
                        }
                        replayMenu()
                    case 1 =>
                        paused = true
                        game = new Game(game.board, game.ritualTrack, game.factions, game.logging, Nil)
                        actions = Nil
                        clear(logDiv)
                        updateStatus()
                        replayMenu()
                    case 2 =>
                        if (paused) {
                            paused = false
                            action.map(perform)
                        }
                        finishUI()
                        updateStatus()
                        paused = true
                        replayMenu()
                    case 3 =>
                        paused = true
                        if (queue.none) {
                            action.map(perform)
                            processUI()
                        }
                        replayMenu()
                })
            }

            if (hash != "") {
                if (recorded.any || self == None) {
                    queue :+= UIParse(game, recorded)

                    processUI()
                }
                else
                    perform(StartAction)
            }
            else {
                if (recorded.any)
                    replayMenu()
                else
                    perform(StartAction)
            }
        }

        def smaller(s : String) = "<span class=\"smaller\">" + s + "</span>"

        def allSeatings(factions : List[Faction]) = factions.permutations.toList.%(s => s.contains(GC).?(s(0) == GC).|(s(0) != WW))
        def randomSeating(factions : List[Faction]) = allSeatings(factions).sortBy(s => random()).head

        def startOnlineSetup(factions : List[Faction]) {
            val all = allSeatings(factions)

            val seatings = all.%(s => all.indexOf(s) <= all.indexOf(s.take(1) ++ s.drop(1).reverse))

            val setup = new Setup(seatings(0), Human)

            def setupQuestions() {
                askM(
                    factions.map(f => "Factions" -> ("" + f + " (" + setup.difficulty(f).html + ")")) ++
                    seatings.map(ff => ("Seating" + factions.contains(GC).not.??(" and first player")) -> ((ff == setup.seating).?(ff.map(_.ss)).|(ff.map(_.short)).mkString(" -> "))) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " spellbooks (" + setup.get(NeutralSpellbooks).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " monsters (" + setup.get(NeutralMonsters).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + GhastCard.short + " (" + setup.get(UseGhast).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + GugCard.short + " (" + setup.get(UseGug).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + ShantakCard.short + " (" + setup.get(UseShantak).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + StarVampireCard.short + " (" + setup.get(UseStarVampire).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL) && factions.has(WW))
                        .$("Variants" -> (IceAge.full + " affects " + Lethargy.full + " (" + setup.get(IceAgeAffectsLethargy).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW) && factions.num == 4)
                        .$("Variants" -> (OW.full + " needs 10 Gates in 4-Player (" + setup.get(Opener4P10Gates).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL))
                        .$("Variants" -> (DemandSacrifice.full + " requires " + Tsathoggua.toString + " (" + setup.get(DemandTsathoggua).?("yes").|("no").hl + ")")) ++
                    $("Map" -> ("Map Configuration (" + setup.options.of[MapOption].lastOption.?(_.toString.hl) + ")")) ++
                    $("Done" -> "Start game".styled("power")),
                    nn => {
                        var n = nn
                        if (n >= 0 && n < factions.num) {
                            setup.difficulty += factions(n) -> List(Human, Easy, Normal, Human).dropWhile(_ != setup.difficulty(factions(n))).drop(1).head
                            setupQuestions()
                        }
                        n -= factions.num
                        if (n >= 0 && n < seatings.num) {
                            setup.seating = seatings(n)
                            setupQuestions()
                        }
                        n -= seatings.num
                        if (n == 0) {
                            setup.toggle(NeutralSpellbooks)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralMonsters)
                            setupQuestions()
                        }
                        if (setup.options.contains(NeutralMonsters)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseGhast)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseGug)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseShantak)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseStarVampire)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL) && factions.has(WW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(IceAgeAffectsLethargy)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW) && factions.num == 4) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(Opener4P10Gates)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(DemandTsathoggua)
                                setupQuestions()
                            }
                        }
                        n -= 1
                        if (n == 0) {
                            val all = $(MapEarth33, MapEarth35, MapEarth53, MapEarth55)
                            setup.options = setup.options.notOf[MapOption] :+ (all.dropWhile(setup.options.of[MapOption].lastOption.has(_).not).drop(1) ++ all).first
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0)
                            startOnlineGame(setup)
                    }
                )
            }

            setupQuestions()
            askTop()
        }

        def startSetup(factions : List[Faction]) {
            val all = allSeatings(factions)

            val seatings = all.%(s => all.indexOf(s) <= all.indexOf(s.take(1) ++ s.drop(1).reverse))

            val setup = new Setup(seatings(0), Human)

            def setupQuestions() {
                askM(
                    factions.map(f => "Factions" -> ("" + f + " (" + setup.difficulty(f).html + ")")) ++
                    seatings.map(ff => ("Seating" + factions.contains(GC).not.??(" and first player")) -> ((ff == setup.seating).?(ff.map(_.ss)).|(ff.map(_.short)).mkString(" -> "))) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " spellbooks (" + setup.get(NeutralSpellbooks).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " monsters (" + setup.get(NeutralMonsters).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + GhastCard.short + " (" + setup.get(UseGhast).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + GugCard.short + " (" + setup.get(UseGug).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + ShantakCard.short + " (" + setup.get(UseShantak).?("yes").|("no").hl + ")")) ++
                    (setup.options.contains(NeutralMonsters))
                        .$("Variants" -> ("Use " + StarVampireCard.short + " (" + setup.get(UseStarVampire).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL) && factions.has(WW))
                        .$("Variants" -> (IceAge.full + " affects " + Lethargy.full + " (" + setup.get(IceAgeAffectsLethargy).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW) && factions.num == 4)
                        .$("Variants" -> (OW.full + " needs 10 Gates in 4-Player (" + setup.get(Opener4P10Gates).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL))
                        .$("Variants" -> (DemandSacrifice.full + " requires " + Tsathoggua.toString + " (" + setup.get(DemandTsathoggua).?("yes").|("no").hl + ")")) ++
                    $("Map" -> ("Map Configuration (" + setup.options.of[MapOption].lastOption.?(_.toString.hl) + ")")) ++
                    $("Options" -> ("Dice rolls (" + setup.dice.?("auto").|("manual").hl + ")")) ++
                    $("Options" -> ("Elder Signs (" + setup.es.?("auto").|("manual").hl + ")")) ++
                    $("Options" -> ("Forced moves (" + setup.confirm.?("confirm").|("perform").hl + ")")) ++
                    $("Done" -> "Start game".styled("power")),
                    nn => {
                        var n = nn
                        if (n >= 0 && n < factions.num) {
                            setup.difficulty += factions(n) -> List(Human, Easy, Normal, Human).dropWhile(_ != setup.difficulty(factions(n))).drop(1).head
                            setupQuestions()
                        }
                        n -= factions.num
                        if (n >= 0 && n < seatings.num) {
                            setup.seating = seatings(n)
                            setupQuestions()
                        }
                        n -= seatings.num
                        if (n == 0) {
                            setup.toggle(NeutralSpellbooks)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralMonsters)
                            setupQuestions()
                        }
                        if (setup.options.contains(NeutralMonsters)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseGhast)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseGug)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseShantak)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseStarVampire)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL) && factions.has(WW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(IceAgeAffectsLethargy)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW) && factions.num == 4) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(Opener4P10Gates)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(DemandTsathoggua)
                                setupQuestions()
                            }
                        }
                        n -= 1
                        if (n == 0) {
                            val all = $(MapEarth33, MapEarth35, MapEarth53, MapEarth55)
                            setup.options = setup.options.notOf[MapOption] :+ (all.dropWhile(setup.options.of[MapOption].lastOption.has(_).not).drop(1) ++ all).first
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.dice = !setup.dice
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.es = !setup.es
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.confirm = !setup.confirm
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0)
                            startGame(setup)
                    }
                )
            }

            setupQuestions()
            askTop()
        }

        val allFactions = List(GC, CC, BG, YS, SL, WW, OW, AN)

        val replay = getElem("replay").innerHTML

        if (replay.trim != "") {
            val entries = replay.split("\n").toList./(_.trim).%(_ != "")
            val nf = entries(0)
            val of = entries(1)
            val factions = of.split(" ")(0).split("-").toList./(Serialize.parseFaction)./(_.get)
            val options = of.split(" ").toList.drop(1)./(Serialize.parseGameOption)./(_.get)
            val setup = new Setup(factions, Recorded)
            setup.options = options
            log(nf)
            startGame(setup, entries.drop(2))
        }
        else
        if (hash != "") {
            get(server + "role/" + hash) { role =>
                val self = Serialize.parseFaction(role)

                self match {
                    case Some(f) => getElem("icon").asInstanceOf[html.Link].href = getAsset(f.style + "-glyph").src
                    case None =>
                }

                dom.window.history.pushState("initilaize", "", "/play/" + hash)

                if (role != "$") {
                    get(server + "read/" + hash + "/0") { read =>
                        val logs = read.split("\n").toList

                        if (logs(0) != version)
                            log("Incorrect game version: " + logs(0).hl)

                        log(logs(1).styled("nt"))

                        log(self./("Playing as " + _).|("Spectating"))

                        val factions = logs(2).split(" ")(0).split("/").toList./(_.split(":"))./(s => Serialize.parseFaction(s(0)).get -> Serialize.parseDifficulty(s(1)).get)
                        val options = logs(2).split(" ").toList.drop(1)./(Serialize.parseGameOption)./(_.get)

                        val setup = new Setup(factions.lefts, Recorded)
                        factions.foreach { case (f, d) => setup.difficulty += f -> d }
                        setup.options = options

                        startGame(setup, logs.drop(3), self)
                    }
                }
            }
        }
        else {
            def topMenu() {
                ask("Cthulhu Wars", List("Quick game".hl, "Hotseat game".hl, redirect.?("<a href='https://cwo.im/' target='_blank'><div>" + "Online game".hl + "</div></a>").|("Online game".hl), "Extra", "About", "Test").take(menu), {
                    case 999_0 =>
                        val n = 1
                        val pn = n + 3
                        ask("Play as", allFactions./(_.toString) :+ "Back", nf => {
                            if (nf < allFactions.num) {
                                val faction = allFactions(nf)
                                val combinations = allFactions.but(faction).combinations(pn - 1).toList
                                ask("Choose opponents", combinations./(_.mkString(", ")) :+ "Back", no => {
                                    if (no < combinations.num) {
                                        val opponents = combinations(no)
                                        val setup = new Setup(randomSeating(faction +: opponents), Normal)
                                        setup.difficulty += faction -> Human
                                        startGame(setup)
                                    }
                                    else
                                        topMenu()
                                })
                            }
                            else
                                topMenu()
                        })
                    case 0 =>
                        val faction = allFactions.shuffle.first
                        val combinations = allFactions.but(faction).combinations(3).$
                        val opponents = combinations.shuffle.first
                        val setup = new Setup(randomSeating(faction +: opponents), Normal)
                        setup.difficulty += faction -> Human
                        startGame(setup)
                    case 1 =>
                        ask("Players", ("3 Players" :: "4 Players" :: "5 Players") :+ "Back", n => {
                            if (n < 3) {
                                val pn = n + 3
                                val combinations = allFactions.combinations(pn).toList
                                ask("Choose factions", combinations./(_.mkString(", "))./(smaller) :+ "Back", n => {
                                    if (n < combinations.num)
                                        startSetup(combinations(n))
                                    else
                                        topMenu()
                                })
                            }
                            else
                                topMenu()
                        })
                    case 2 =>
                        ask("Players", ("3 Players" :: "4 Players" :: "5 Players") :+ "Back", n => {
                            if (n < 3) {
                                val pn = n + 3
                                val combinations = allFactions.combinations(pn).toList
                                ask("Choose factions", combinations./(_.mkString(", "))./(smaller) :+ "Back", n => {
                                    if (n < combinations.num)
                                        startOnlineSetup(combinations(n))
                                    else
                                        topMenu()
                                })
                            }
                            else
                                topMenu()
                        })
                    case 3 => ask("Cthulhu Wars Extra", List("Survival mode".hl, "<a href='https://necronomicon.app/' target='_blank'><div>Necronomicon</div></a>", "<a href='https://cthulhuwars.fandom.com/' target='_blank'><div>Cthulhu Wars Strategy Wiki</div></a>", "Back"), {
                        case 0 =>
                            val base = allFactions.take(4)
                            ask("Choose faction", base./(f => f.toString) :+ "Back", nf => {
                                if (nf < base.num) {
                                    val setup = new Setup(randomSeating(base), AllVsHuman)
                                    setup.difficulty += base(nf) -> Human
                                    startGame(setup)
                                }
                                else
                                    topMenu()
                            })
                        case 1 =>
                            topMenu()
                        case 2 =>
                            topMenu()
                        case 3 =>
                            topMenu()
                    })
                    case 4 => ask("Cthulhu Wars Solo", List("<a href='https://boardgamegeek.com/filepage/152635/cthulhu-wars-solo-hrf-19' target='_blank'><div>Project Homepage</div></a>", "Developed by " + "Haunt Roll Fail".hl, "Additional AI programming by " + "ricedwlit".hl, "Ancients"/*.styled("AN")*/ + " faction developed by " + "Legrasse81".hl, "Board game by " + "Peterson Games".hl, "All graphics in the app belong to Petersen Games.<br>Used with permission.", "Back"), {
                        case 0 =>
                            val base = allFactions.take(4)
                            ask("Choose faction", base./(f => f.toString) :+ "Back", nf => {
                                if (nf < base.num) {
                                    val setup = new Setup(randomSeating(base), AllVsHuman)
                                    setup.difficulty += base(nf) -> Human
                                    startGame(setup)
                                }
                                else
                                    topMenu()
                            })
                        case 1 =>
                            topMenu()
                        case 2 =>
                            topMenu()
                        case 3 =>
                            topMenu()
                        case 4 =>
                            topMenu()
                        case 5 =>
                            topMenu()
                        case 6 =>
                            topMenu()
                    })
                    case 5 =>
                        val setup = new Setup(randomSeating($(GC, CC, BG)), Normal)
                        setup.difficulty += GC -> Human
                        setup.difficulty += CC -> Debug
                        setup.difficulty += BG -> Human
                        // setup.options = $(MapEarth53)
                        setup.options = $(NeutralMonsters, UseGhast, UseGug, UseShantak, UseStarVampire)
                        startGame(setup)
                    case 666 =>
                        val base = allFactions.take(4)
                        ask("Choose faction", base./(f => f.toString) :+ "Back", nf => {
                            if (nf < base.num) {
                                val setup = new Setup(randomSeating(base), AllVsHuman)
                                setup.difficulty += base(nf) -> Human
                                startGame(setup)
                            }
                            else
                                topMenu()
                        })
                    case 777 =>
                        val setup = new Setup(randomSeating(List(YS, GC, BG, AN)), Normal)
                        startGame(setup)
                    case 888 =>
                        val setup = new Setup(randomSeating(List(GC, CC, YS, OW)), Normal)
                        setup.difficulty += OW -> Debug
                        startGame(setup)
                })
            }

            topMenu()

            if (quick) {
                val faction = allFactions.shuffle.first
                val combinations = allFactions.but(faction).combinations(3).$
                val opponents = combinations.shuffle.first
                val setup = new Setup(randomSeating(faction +: opponents), Normal)
                setup.difficulty += faction -> Human
                startGame(setup)
            }

        }
    }
}
