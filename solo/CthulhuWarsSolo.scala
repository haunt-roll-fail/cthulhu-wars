package cws

import scala.scalajs._
import scala.scalajs.js.timers._

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import util.canvas._

import hrf.BuildInfo

import cws.html._


sealed trait UIAction
case object UIStop extends UIAction
case class UILog(s : String) extends UIAction
case class UIPerform(game : Game, action : Action) extends UIAction
case class UIQuestion(faction : Faction, game : Game, actions : $[Action], waiting : $[Faction] = $) extends UIAction
case class UIQuestionDebug(faction : Faction, game : Game, actions : $[Action]) extends UIAction

case class UIRead(game : Game) extends UIAction
case class UIProcess(game : Game, recorded : $[Action]) extends UIAction

case class UIRollD6(game : Game, question : Game => String, roll : Int => Action) extends UIAction
case class UIRollBattle(game : Game, question : Game => String, n : Int, o : $[BattleRoll] => Action) extends UIAction
case class UIDrawES(game : Game, question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => Action) extends UIAction

sealed trait Difficulty extends Record {
    def elem : String
}
case object Off extends Difficulty { def elem = "Off".hl }
case object Recorded extends Difficulty { def elem = "Recorded".hl }
case object Human extends Difficulty { def elem = "Human".hl }
case object Debug extends Difficulty { def elem = "Debug".hl }
case object Easy extends Difficulty { def elem = "Easy".styled("miss") }
case object Normal extends Difficulty { def elem = "Normal".styled("pain") }
case object AllVsHuman extends Difficulty { def elem = "AllVsHuman".styled("pain") }

class Setup(factions : $[Faction], diff : Difficulty) {
    var seating : $[Faction] = factions
    var options : $[GameOption] = $(factions.num match {
        case 3 => MapEarth33
        case 4 => MapEarth35
        case 5 => MapEarth55
    })

    def toggle(go : GameOption) {
        options = if (options.has(go))
            options.but(go)
        else
            go +: options
    }

    def get(go : GameOption) = options.has(go)

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

case class GameOverAction(winners : $[Faction], msg : String) extends Action with NoClear with Soft {
    def question(implicit game : Game) = winners.none.?("Winner is Humanity").|((winners.num == 1).?("Winner is " + winners.head).|("Winners are " + winners.mkString(", ")))
    def option(implicit game : Game) = msg
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

    def getAsset(k : String) : html.Image = dom.document.getElementById(k).asInstanceOf[html.Image]

    case class Processing(tint : |[String], screen : |[String], overlay : |[String]) extends GoodMatch

    def getTintedAsset(k : String, processing : Processing) : html.Canvas = {
        val source = dom.document.getElementById(k).asInstanceOf[html.Image]

        val result = new Bitmap(source.width, source.height)
        result.context.drawImage(source, 0, 0)

        processing.tint.foreach { tint =>
            result.context.fillStyle = tint
            result.context.globalCompositeOperation = "color"
            result.context.fillRect(0, 0, source.width, source.height)
        }

        processing.screen.foreach { screen =>
            result.context.fillStyle = screen
            result.context.globalCompositeOperation = "screen"
            result.context.fillRect(0, 0, source.width, source.height)
        }

        processing.overlay.foreach { overlay =>
            result.context.fillStyle = overlay
            result.context.globalCompositeOperation = "overlay"
            result.context.fillRect(0, 0, source.width, source.height)
        }

        result.context.globalCompositeOperation = "destination-in"
        result.context.drawImage(source, 0, 0)
        result.canvas
    }

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

    val DottedLine = "............................................................................................................................................................................................................................................"
    val DoubleLine = "======================================================================================================================="

    def setupUI() {
        val (hash, quick) = dom.window.location.hash.drop(1) @@ {
            case "quick" => ("", true)
            case h if h != "" => (h, false)
            case _ =>
                val path = dom.window.location.pathname

                if (path.startsWith("/play/quick"))
                    ("", true)
                else
                if (path.startsWith("/play/"))
                    (path.drop("/play/".length), false)
                else
                    ("", false)
        }

        val origin = dom.window.location.origin + "/"
        val cwsOptions = Option(getElem("cws-options"))
        val delay = cwsOptions./~(_.getAttribute("data-delay").?)./~(_.toIntOption).|(30)
        val menu = cwsOptions./~(_.getAttribute("data-menu").?)./~(_.toIntOption).|(5)
        // val menu = cwsOptions./~(_.getAttribute("data-menu").?)./~(_.toIntOption).|(6) // Temp for test (comment out before deploying)
        val scroll = cwsOptions./~(_.getAttribute("data-scroll").?)./(_ == "true").|(false)
        val server = cwsOptions./~(_.getAttribute("data-server").?).|("###SERVER-URL###")
        val redirect = origin != server
        // val redirect = false // making online games work with AN, as per hauntrollfail's advice
        val localReplay = false

        val logDiv = getElem("log")

        def log(s : String, onClick : () => Unit = () => {}) = {
            val nd = logDiv
            val isScrolledToBottom = nd.scrollHeight - nd.clientHeight <= nd.scrollTop + 1

            val p = newDiv("p", s, onClick)

            logDiv.appendChild(p)

            if (isScrolledToBottom || scroll)
                nd.scrollTop = nd.scrollHeight - nd.clientHeight
        }

        val msday = 24 * 60 * 60 * 1000
        val diff = (System.currentTimeMillis - BuildInfo.time) % msday / 1000
        val secs = diff % 60
        val mins = diff / 60

        val version = "Cthulhu Wars HRF " + BuildInfo.version

        log(version)

        val actionDiv = getElem("action")
        val undoDiv = getElem("undo")

        def ask(question : String, options : $[String], onResult : Int => Unit, style : Option[String] = None) : None.type = {
            clear(actionDiv)

            actionDiv.appendChild(newDiv("", question))

            options.zipWithIndex.foreach { case(o, n) =>
                actionDiv.appendChild(newDiv("option" + style./(" " + _).|(""), o, () => if (o.contains("<a ").not) { clear(actionDiv); onResult(n) }))
            }

            None
        }

        var scrollTop = 0.0

        def askTop() {
            actionDiv.scrollTop = 0
        }

        def askM(headers : $[String], qos : $[(String, String)], onResult : Int => Unit, style : |[String] = None, extra : Int => |[String] = _ => None) : None.type = {
            clear(actionDiv)

            headers.foreach(h => actionDiv.appendChild(newDiv("", h)))

            var prev : |[String] = None

            qos.zipWithIndex.foreach { case((qq, oo), n) =>
                val q = |(qq).but("")
                val o = |(oo).but("")

                if (q.any && q != prev) {
                    if (prev.any)
                        actionDiv.appendChild(newDiv("", "&nbsp;"))

                    actionDiv.appendChild(newDiv("", q.get))

                    prev = q
                }

                if (o.any)
                    actionDiv.appendChild(newDiv("option" + style./(" " + _).|("") + extra(n)./(" " + _).|(""), o.get, () => {
                        if (extra(n).none) {
                            scrollTop = actionDiv.scrollTop
                            clear(actionDiv)
                            onResult(n)
                        }
                    }))
            }

            actionDiv.scrollTop = scrollTop

            None
        }

        var lastScrollTop : |[Double] = None

        case class AskLine(group : String, option : String, styles : $[String], clear : Boolean, onClick : () => Unit = () => ())

        def askN(headers : $[String], lines : $[AskLine]) : None.type = {
            clear(actionDiv)

            headers.foreach(h => actionDiv.appendChild(newDiv("", h)))

            var prev : |[String] = None

            lines.foreach { line =>
                val q = |(line.group).but("")
                val o = |(line.option).but("")

                if (q.any && q != prev) {
                    if (prev.any)
                        actionDiv.appendChild(newDiv("", "&nbsp;"))

                    actionDiv.appendChild(newDiv("", q.get))

                    prev = q
                }

                if (o.any)
                    actionDiv.appendChild(newDiv("option" + line.styles./(" " + _).join(""), o.get, () => {
                        if (line.onClick != null) {
                            scrollTop = actionDiv.scrollTop

                            if (line.clear) {
                                clear(actionDiv)
                                lastScrollTop = None
                            }
                            else {
                                lastScrollTop = |(actionDiv.scrollTop)
                            }

                            line.onClick()
                        }
                    }))
            }

            actionDiv.scrollTop = lastScrollTop.|(0.0)

            None
        }

        var statuses = $(getElem("status-1"), getElem("status-2"), getElem("status-3"), getElem("status-4"), getElem("status-5"))

        val mapWest = getElem("map-west")
        val mapEast = getElem("map-east")

        val cw = getElem("to-cw")
        val ccw = getElem("to-ccw")

        val mapSmall = getElem("map-small")
        val mapBig = getElem("map-big")

        hide(mapBig.parentElement.parentElement)
        hide(cw)
        hide(ccw)

        def processStatus(strings : $[String], ps : String) = strings
            ./(_.replace("------", "<br/>"))
            ./(s =>
                (if (s.startsWith("    "))
                    ("<div class='indent1'>" + s.drop(4))
                else
                    ("<div class='" + ps + "'>" + s))
                + "</div>")
           .mkString("\n")

        def onlineGameName = {
            val n = $(
                "Power",
                "Doom",
                "Glory",
                "Destiny",
                "Might",
                "Fight",
                "Betrayal",
                "Fate",
                "Eternity",
                "Existence",
                "Time",
                "Space",
                "Agony",
                "Pain",
                "Torment",
                "Anything",
                "Sacrifice",
                "Death",
                "Despair",
                "Rage",
                "Curse",
                "Fear",
                "Undefined",
                "Shift",
                "Colour",
                "Gate",
                "Break",
                "Desperation",
                "Ritual",
                "Dread",
                "Discord",
                "Slaughter",
                "Horror",
                "Omen",
                "Insanity",
                "Rupture",
                "Decay",
                "Blindness",
                "Continuum",
                "Catastrophe",
                "Disaster",
                "Hazard",
                "Devastation",
                "Failure",
                "Tentacles",
                "Maw",
                "Void",
                "Blood",
            ).sortBy(_ => math.random())
            val c = $("for", "against", "versus", "through", "and", "of", "in", "as").sortBy(_ => math.random())
            n.head + " " + c.head + " " + n.last
        }

        def startOnlineGame(setup : Setup, recorded : $[String] = $) {
            val roles = setup.seating.%(f => setup.difficulty(f) == Human).map(_.short).mkString(" ")
            val stp = (setup.seating.%(f => setup.difficulty(f) != Off)./(f => f.short + ":" + setup.difficulty(f)).mkString("/") + " " + setup.options./(_.toString).mkString(" "))
            val name = onlineGameName

            val urlV = (dom.window.location.search + "&version=").splt("version=")(1).splt("&")(0)
            val v = (urlV != "").??("?version=" + urlV)

            post(server + "create", $(roles, version, name, stp).mkString("\n")) { master =>
                get(server + "roles/" + master) { ras =>
                    val rs = ras.split("\n").toList.map(_.split(" ")).map(s => s(0) -> s(1)).filter(_._1 != "$")
                    var ca = "Copy all"
                    def linkMenu() {
                        val op = ca +: rs.map {
                            case ("#", s) => "<a target=\"_blank\" rel=\"noopener\" href=\"" + server + "play/" + s + v + "\"><div>" + "Spectator".hl + "</div></a>"
                            case (f, s) => "<a target=\"_blank\" rel=\"noopener\" href=\"" + server + "play/" + s + v + "\"><div>" + "Play as".hl + " " + Serialize.parseFaction(f).get + "</div></a>"
                        }
                        ask(name.hl, op, { n =>
                            if (n == 0)
                                ca = clipboard(name + "\n" + rs.map {
                                    case ("#", s) => "Spectate " + server + "play/" + s + v
                                    case (f, s) => Serialize.parseFaction(f).get.short + " " + server + "play/" + s + v
                                }.mkString("\n")).?("Copied links to clipboard").|("Error copying to clipboard").hl

                            linkMenu()
                        })
                    }

                    linkMenu()
                }
            }
        }

        def startGame(setup : Setup, recorded : $[String] = $, self : |[Faction] = None) {
            setup.options = GameOptions.all.intersect(setup.options)
            val seating = setup.seating.%(f => setup.difficulty(f) != Off)

            if (seating.num == 5)
                statuses = statuses.take(3) ++ statuses.drop(4).take(1) ++ statuses.drop(3).take(1)

            statuses.take(seating.num)./(_.parentElement.parentElement).foreach(show)
            statuses.drop(seating.num)./(_.parentElement.parentElement).foreach(hide)

            if (seating.num <= 4) {
                hide(getElem("to-cw6"))
                hide(getElem("to-ccw6"))
            }
            else {
                hide(getElem("to-cw4"))
                hide(getElem("to-ccw4"))
            }

            statuses.lazyZip(setup.seating).foreach { (s, f) =>
                // s.as[html.Element].get.style.backgroundImage = "url(info/" + f.style + "-header.png)"
                s.as[html.Element].get.style.backgroundImage = "url(" + Overlays.imageSource("info:" + f.style + "-background") + ")"
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
            var overrideGame : |[Game] = None
            def displayGame = overrideGame.|(game)

            var actions : $[Action] = $
            var queue : $[UIAction] = $
            var paused = recorded.any && hash == ""

            val serializer = new Serialize(game)

            def askFaction(c : Continue)(implicit game : Game) : UIAction = {
                def dontAttack(factions : $[Faction])(a : Action) = factions.map(f => Explode.isOffense(f)(a)(game).not).reduce(_ && _)

                def filterAttack(actions : $[Action], factions : $[Faction]) = actions.%(dontAttack(factions)).some.|(actions)

                c match {
                    case Force(action) =>
                        throw new Error("force escaped " + action)

                    case Then(action) =>
                        UIPerform(game, action)

                    case DelayedContinue(_, continue) =>
                        askFaction(continue)

                    case RollD6(question, roll) if setup.dice =>
                        UIPerform(game, roll((1::2::3::4::5::6).maxBy(_ => random())))

                    case RollD6(question, roll) =>
                        UIRollD6(game, question, roll)

                    case RollBattle(_, 0, roll) =>
                        UIPerform(game, roll($))

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
                        UIQuestion(self.||(winners.starting).|(game.setup.first), game, GameOverAction(winners, "Hooray!") :: GameOverAction(winners, "Meh...") :: GameOverAction(winners, "Save replay"))

                    case MultiAsk(asks) =>
                        val a = asks.sortBy(ask =>
                            if (self.has(ask.faction))
                                0
                            else
                                setup.difficulty(ask.faction) match {
                                    case Debug => 100 + random(10)
                                    case Human | Recorded => 200 + random(10)
                                    case Easy => 300 + random(10)
                                    case Normal => 400 + random(10)
                                    case AllVsHuman => 500 + random(10)
                                }
                        ).first

                        askFaction(a) match {
                            case q : UIQuestion => q.copy(waiting = asks./(_.faction))
                            case ui => ui
                        }

                    case Ask(faction, actions) =>
                        if (actions(0).isInstanceOf[PlayDirectionAction] || actions(0).isInstanceOf[StartingRegionAction]) {
                            hide(cw)
                            hide(ccw)
                        }
                        else {
                            if ((game.factions ++ game.factions).containsSlice(game.setup)) {
                                show(cw)
                                hide(ccw)
                            }
                            else {
                                hide(cw)
                                show(ccw)
                            }
                        }

                        val confirm = setup.confirm && setup.difficulty(faction) == Human

                        if (confirm.not && actions.num == 1)
                            UIPerform(game, actions(0))
                        else
                        if (confirm.not && actions.%!(_.isInfo).num == 1 && actions.has(NeedOk).not)
                            UIPerform(game, actions.%!(_.isInfo).only)
                        else
                        if (confirm.not && actions(0).isInstanceOf[SpellbookAction] && actions.num == faction.unclaimedSB)
                            UIPerform(game, actions(0))
                        else {
                            setup.difficulty(faction) match {
                                case Human | Recorded =>
                                    UIQuestion(faction, game, actions)
                                case Debug =>
                                    UIQuestionDebug(faction, game, actions)
                                case Easy =>
                                    UIPerform(game, faction match {
                                        case GC => Bot3(GC).ask(actions, 0.5)(game)
                                        case CC => Bot3(CC).ask(actions, 0.2)(game)
                                        case BG => Bot3(BG).ask(actions, 0.6)(game)
                                        case YS => Bot3(YS).ask(actions, 0.3)(game)
                                        case SL => BotSL   .ask(actions, 0.2)(game)
                                        case WW => BotWW   .ask(actions, 0.2)(game)
                                        case OW => BotOW   .ask(actions, 0.2)(game)
                                        case AN => BotAN   .ask(actions, 0.2)(game)
                                    })
                                case Normal =>
                                    UIPerform(game, faction match {
                                        case GC => BotGC   .ask(actions, 0.03)(game)
                                        case CC => BotCC   .ask(actions, 0.03)(game)
                                        case BG => Bot3(BG).ask(actions, 0.03)(game)
                                        case YS => BotYS   .ask(actions, 0.03)(game)
                                        case SL => BotSL   .ask(actions, 0.03)(game)
                                        case WW => BotWW   .ask(actions, 0.03)(game)
                                        case OW => BotOW   .ask(actions, 0.03)(game)
                                        case AN => BotAN   .ask(actions, 0.03)(game)
                                    })
                                case AllVsHuman =>
                                    val aa = Explode.explode(game, actions)
                                    val fr = setup.seating.but(faction).filter(f => setup.difficulty(f) == AllVsHuman)
                                    val as = filterAttack(aa, fr)
                                    UIPerform(game, faction match {
                                        case GC => BotGC   .ask(as, 0.03)(game)
                                        case CC => BotCC   .ask(as, 0.03)(game)
                                        case BG => Bot3(BG).ask(as, 0.03)(game)
                                        case YS => BotYS   .ask(as, 0.03)(game)
                                        case SL => BotSL   .ask(as, 0.03)(game)
                                        case WW => BotWW   .ask(as, 0.03)(game)
                                        case OW => BotOW   .ask(as, 0.03)(game)
                                        case AN => BotAN   .ask(as, 0.03)(game)
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

            case class DrawRect(key : String, tint : |[Processing], x : Int, y : Int, width : Int, height : Int, cx : Int = 0, cy : Int = 0, alpha : Double = 1.0)

            case class DrawItem(region : Region, faction : Faction, unit : UnitClass, health : UnitHealth, tags : $[UnitState], x : Int, y : Int) {
                val defaultProcessing = Processing(None, None, None)

                val tint = faction @@ {
                    case GC => Processing(|("#77a055"), |("#222222"), None)
                    case CC => Processing(|("#4977b3"), |("#111111"), None)
                    case BG => Processing(|("#cd3233"), None, |("#555555"))
                    case YS => Processing(|("#ffd000"), |("#663344"), None)
                    case WW => Processing(|("#88a9be"), |("#5577aa"), None)
                    case SL => Processing(|("#db6a33"), |("#4a1a1a"), None)
                    case OW => Processing(|("#6c4296"), None, |("#4c4c4c"))
                    case AN => Processing(|("#47a5bc"), |("#333333"), None)
                    case _  => defaultProcessing
                }

                def proto : DrawRect = unit match {
                    case Gate => DrawRect("gate", None, x - 38, y - 38, 76, 76)

                    case Acolyte => faction match {
                        case BG => DrawRect("bg-acolyte", None, x - 17, y - 54, 39, 60)
                        case CC => DrawRect("cc-acolyte", None, x - 17, y - 54, 38, 60)
                        case GC => DrawRect("gc-acolyte", None, x - 17, y - 54, 40, 59)
                        case YS => DrawRect("ys-acolyte", None, x - 17, y - 54, 39, 61)
                        case SL => DrawRect("sl-acolyte", None, x - 17, y - 54, 38, 60)
                        case WW => DrawRect("ww-acolyte", None, x - 17, y - 52, 40, 58)
                        case OW => DrawRect("ow-acolyte", None, x - 17, y - 54, 38, 60)
                        case AN => DrawRect("an-acolyte", None, x - 17, y - 54, 39, 60)
                        case _ => null
                    }

                    case HighPriest => faction match {
                        case BG => DrawRect("bg-high-priest", None, x - 35, y - 60, 70, 68)
                        case CC => DrawRect("cc-high-priest", None, x - 35, y - 60, 70, 69)
                        case GC => DrawRect("gc-high-priest", None, x - 35, y - 60, 70, 67)
                        case YS => DrawRect("ys-high-priest", None, x - 35, y - 60, 70, 66)
                        case SL => DrawRect("sl-high-priest", None, x - 35, y - 60, 70, 69)
                        case WW => DrawRect("ww-high-priest", None, x - 35, y - 60, 70, 67)
                        case OW => DrawRect("ow-high-priest", None, x - 35, y - 60, 70, 66) // Left to do
                        case AN => DrawRect("an-high-priest", None, x - 35, y - 60, 70, 66) // Left to do
                        case _ => null
                    }

                    case FactionGlyph => faction match {
                        case BG => DrawRect("bg-glyph", None, x - 50, y - 50, 100, 100)
                        case CC => DrawRect("cc-glyph", None, x - 50, y - 50, 100, 100)
                        case GC => DrawRect("gc-glyph", None, x - 50, y - 50, 100, 100)
                        case YS => DrawRect("ys-glyph", None, x - 51, y - 50, 102, 100)
                        case SL => DrawRect("sl-glyph", None, x - 50, y - 50, 100, 102)
                        case WW => DrawRect("ww-glyph", None, x - 50, y - 50, 100, 100)
                        case OW => DrawRect("ow-glyph", None, x - 50, y - 50, 100, 100)
                        case AN => DrawRect("an-glyph", None, x - 50, y - 50, 100, 101)
                        case _ => null
                    }

                    case Ghoul         => DrawRect("bg-ghoul", None, x - 20, y - 40, 39, 47)
                    case Fungi         => DrawRect("bg-fungi", None, x - 40, y - 73, 72, 80)
                    case DarkYoung     => DrawRect("bg-dark-young", None, x - 53, y - 122, 83, 131)
                    case ShubNiggurath => DrawRect("bg-shub", None, x - 69, y - 173, 132, 185, 0, 10)

                    case Nightgaunt    => DrawRect("cc-nightgaunt", None, x - 36, y - 82, 69, 90, -1, 0)
                    case FlyingPolyp   => DrawRect("cc-flying-polyp", None, x - 36, y - 81, 73, 90, 10, 0)
                    case HuntingHorror => DrawRect("cc-hunting-horror", None, x - 86, y - 70, 166, 77)
                    case Nyarlathotep  => DrawRect("cc-nyarly", None, x - 50, y - 155, 106, 163)

                    case DeepOne       => DrawRect("gc-deep-one", None, x - 16, y - 25, 36, 31, 0, -5)
                    case Shoggoth      => DrawRect("gc-shoggoth", None, x - 31, y - 62, 63, 69)
                    case Starspawn     => DrawRect("gc-starspawn", None, x - 35, y - 63, 69, 70)
                    case Cthulhu       => DrawRect("gc-cthulhu", None, x - 65, y - 209, 117, 225, 0, 50)

                    case Undead        => DrawRect("ys-undead", None, x - 27, y - 49, 44, 54, -5, 0)
                    case Byakhee       => DrawRect("ys-byakhee", None, x - 32, y - 64, 57, 70)
                    case KingInYellow  => DrawRect("ys-king-in-yellow", None, x - 44, y - 111, 85, 116)
                    case Hastur        => DrawRect("ys-hastur", None, x - 87, y - 163, 150, 170)

                    case Wizard        => DrawRect("sl-wizard", None, x - 23, y - 33, 45, 41)
                    case SerpentMan    => DrawRect("sl-serpent-man", None, x - 34, y - 76, 70, 85, 3, 0)
                    case FormlessSpawn => DrawRect("sl-formless-spawn", None, x - 38, y - 85, 78, 94)
                    case Tsathoggua    => DrawRect("sl-tsathoggua", None, x - 75, y - 133, 152, 146)

                    case Wendigo       => DrawRect("ww-wendigo", None, x - 26, y - 62, 56, 68)
                    case GnophKeh      => DrawRect("ww-gnoph-keh", None, x - 30, y - 88, 61, 95)
                    case RhanTegoth    => DrawRect("ww-rhan-tegoth", None, x - 74, y - 128, 153, 135)
                    case Ithaqua       => DrawRect("ww-ithaqua", None, x - 112, y - 192, 164, 202)

                    case Mutant        => DrawRect("ow-mutant", None, x - 20, y - 52, 40, 58)
                    case Abomination   => DrawRect("ow-abomination", None, x - 30, y - 76, 62, 82)
                    case SpawnOW       => DrawRect("ow-spawn-of-yog-sothoth", None, x - 49, y - 94, 91, 100, 3, 3)
                    case YogSothoth    => DrawRect("ow-yog-sothoth", None, x - 82, y - 162, 132, 174)

                    case UnMan         => DrawRect("an-un-man", None, x - 24, y - 60, 48, 65)
                    case Reanimated    => DrawRect("an-reanimated", None, x - 28, y - 62, 57, 65)
                    case Yothan        => DrawRect("an-yothan", None, x - 61, y - 85, 122, 90)

                    case DesecrationToken => DrawRect("ys-desecration", None, x - 20, y - 20, 41, 40)
                    case IceAgeToken      => DrawRect("ww-ice-age", None, x - 44, y - 67, 91, 75)
                    case Cathedral        => DrawRect("an-cathedral", None, x - 39, y - 90, 78, 110)

                    case Ghast         => DrawRect("n-ghast", |(tint), x - 17, y - 53, 35, 59)
                    case Gug           => DrawRect("n-gug", |(tint), x - 36, y - 78, 73, 90)
                    case Shantak       => DrawRect("n-shantak", |(tint), x - 39, y - 89, 79, 100)
                    case StarVampire   => DrawRect("n-star-vampire", |(tint), x - 35, y - 75, 70, 85)
                    case Byatis        => DrawRect("n-byatis", |(tint), x - 47, y - 90, 95, 90)
                    case Abhoth        => DrawRect("n-abhoth", |(tint), x - 47, y - 110, 95, 120)
                    case Filth         => DrawRect("n-filth", |(tint), x - 20, y - 20, 40, 40)
                    case Daoloth       => DrawRect("n-daoloth", |(tint), x - 59, y - 91, 118, 99)
                    case Nyogtha       => DrawRect("n-nyogtha", |(tint), x - 40, y - 69, 81, 80)

                    case GhastIcon        => DrawRect("ghast-icon", None, x - 17, y - 55, 50, 50)
                    case GugIcon          => DrawRect("gug-icon", None, x - 17, y - 55, 50, 50)
                    case ShantakIcon      => DrawRect("shantak-icon", None, x - 17, y - 55, 50, 50)
                    case StarVampireIcon  => DrawRect("star-vampire-icon", None, x - 17, y - 55, 50, 50)
                    case ByatisIcon       => DrawRect("byatis-icon", None, x - 17, y - 55, 50, 50)
                    case AbhothIcon       => DrawRect("abhoth-icon", None, x - 17, y - 55, 50, 50)
                    case DaolothIcon      => DrawRect("daoloth-icon", None, x - 17, y - 55, 50, 50)
                    case NyogthaIcon      => DrawRect("nyogtha-icon", None, x - 17, y - 55, 50, 50)
                    case HighPriestIcon   => DrawRect("high-priest-icon", None, x - 17, y - 55, 50, 50)

                    case _ => null
                }

                def rect = proto
                    .useIf(tags.has(Hidden))(_.copy(alpha = 0.5))
                    .useIf(tags.has(Absorbed)) { r =>
                        val k = math.sqrt(1 + tags.count(Absorbed))
                        r.copy(x = r.x + (r.width * 0.5 + r.cx) * (1 - k) ~, y = r.y + (r.height * 0.5 + r.cy) * (1 - k) ~, width = r.width * k ~, height = r.height * k ~, cx = r.cx * k ~, cy = r.cy * k ~)
                    }

                def icon =
                    if (health == Killed)
                        |(DrawRect("kill", None, x + rect.cx - 30, (rect.y + y) / 2 + rect.cy - 30, 60, 60))
                    else
                    if (health == Pained)
                        |(DrawRect("pain", None, x - 29 + rect.cx, (rect.y + y) / 2 + rect.cy - 30, 60, 60))
                    else
                        None
            }

            var oldPositions : $[DrawItem] = $
            var oldGates : $[Region] = $
            var horizontal = true

            def drawMap(implicit game : Game) {
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
                oldPositions = $

                var draws : $[DrawItem] = $

                areas.foreach { r =>
                    val (px, py) = gateXY(r)
                    val gated = game.gates.has(r)

                    val controler = game.setup.%(_.gates.has(r)).single
                    val keeper = controler./~(_.at(r).%(_.onGate).%(_.health == Alive).starting)

                    var fixed : $[DrawItem] = $
                    var all : $[DrawItem] = $
                    var sticking : $[DrawItem] = $
                    var free : $[DrawItem] = $

                    if (gated)
                        fixed +:= DrawItem(r, null, Gate, Alive, $, px, py)

                    keeper match {
                        case Some(u) => fixed +:= DrawItem(r, u.faction, u.uclass, u.health, u.state, px, py)
                        case _ =>
                    }

                    factionlike.foreach { f =>
                        f.at(r).diff(keeper.$).foreach { u =>
                            all +:= DrawItem(r, f, u.uclass, u.health, u.state, 0, 0)
                        }
                    }

                    if (game.desecrated.has(r))
                        all +:= DrawItem(r, null, DesecrationToken, Alive, $, 0, 0)

                    if (game.cathedrals.has(r))
                        all +:= DrawItem(r, null, Cathedral, Alive, $, 0, 0)

                    if (game.setup.%(_.iceAge.?(_ == r)).any)
                        all +:= DrawItem(r, null, IceAgeToken, Alive, $, 0, 0)

                    all.foreach { d =>
                        saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit && d.tags == o.tags && d.health == o.health) match {
                            case Some(o) =>
                                sticking +:= o
                                saved :-= o
                            case None =>
                                saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit && d.tags == o.tags) match {
                                    case Some(o) =>
                                        sticking +:= o.copy(health = d.health)
                                        saved :-= o
                                    case None =>
                                        saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit) match {
                                            case Some(o) =>
                                                sticking +:= o.copy(tags = d.tags, health = d.health)
                                                saved :-= o
                                            case None =>
                                                free +:= d
                                        }
                                }
                        }
                    }

                    if (free.num > sticking.num * 0 + 3 || free.%(_.unit.utype == GOO).any || (oldGates.has(r).not && game.gates.has(r))) {
                        free = free ++ sticking
                        sticking = $
                    }

                    def rank(d : DrawItem) = d.unit.utype match {
                        case Token => 0
                        case Cultist => 1
                        case Monster => 2
                        case Terror => 3
                        case GOO => 4
                    }

                    free.sortBy(d => -rank(d)).foreach { d =>
                        sticking +:= Array.tabulate(40)(n => find(px, py)).sortBy { case (x, y) => ((x - px).abs * 5 + (y - py).abs) }.map { case (x, y) => DrawItem(d.region, d.faction, d.unit, d.health, d.tags, x, y) }.minBy { dd =>
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

                draws.sortBy(d => d.y + (d.unit == Gate).?(-2000).|(0) + (d.unit == DesecrationToken).?(-1000).|(0))./(_.rect).foreach { d =>
                    g.globalAlpha = d.alpha
                    g.drawImage(d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key)), d.x, d.y, d.width, d.height)
                    g.globalAlpha = 1.0
                }

                draws.sortBy(d => d.y + (d.unit == Gate).?(-2000).|(0) + (d.unit == DesecrationToken).?(-1000).|(0)).foreach { d =>
                    if (d.icon.any)
                        g.drawImage(getAsset(d.icon.get.key), d.icon.get.x, d.icon.get.y)
                }
            }

            mapSmall.onclick = (e) => {
                hide(mapSmall.parentElement.parentElement)
                show(mapBig.parentElement.parentElement)

                map = mapBitmapBig
                drawMap(displayGame)
            }

            mapBig.onclick = (e) => {
                hide(mapBig.parentElement.parentElement)
                show(mapSmall.parentElement.parentElement)

                map = mapBitmapSmall
                drawMap(displayGame)
            }

            drawMap(displayGame)

            val statusBitmaps = statuses.take(seating.num)./(s => new CachedBitmap(s))

            def factionStatus(f : Faction, b : CachedBitmap)(implicit game : Game) {
                if (!game.setup.has(f))
                    return

                def div(styles : String*)(content : String) = if (styles.none) "<div>" + content + "</div>" else "<div class=\"" + styles.mkString(" ") + "\">" + content + "</div>"
                def r(content : String) = div("right")(content)

                val current = game.factions.starting.has(f)

                val name = div("name")("" + f + "")
                val nameS = div("name")(f.styled(f.short) + "")
                val power = div()(f.hibernating.?(("" + f.power + " Power").styled("hibernate")).|((f.power > 0).?(f.power.power).|("0 Power")))
                val powerS = div()(f.hibernating.?(("" + f.power + "P").styled("hibernate")).|((f.power > 0).?(("" + f.power + "P").styled("power")).|("0P")))
                val doom = div()(("" + f.doom + " Doom").styled("doom") + f.es.any.?(" + " + (f.es.num == 1).?("ES").|("" + f.es.num + " ES").styled("es")).|(""))
                val doomL = div()(("" + f.doom + " Doom").styled("doom") + f.es.any.?(" + " + (f.es.num == 1).?("Elder Sign").|("" + f.es.num + " Elder Signs").styled("es")).|(""))
                val doomS = div()(("" + f.doom + "D").styled("doom") + f.es.any.?("+" + ("" + f.es.num + "ES").styled("es")).|(""))

                val sb = f.spellbooks./{ sb =>
                    val full = sb.full
                    val s = sb.name.replace("\\", "\\\\").replace("'", "&#39") // "
                    val d = s"""<div class='spellbook'
                        onclick='event.stopPropagation(); onExternalClick("${f.short}", "${s}")'
                        onpointerover='event.stopPropagation(); onExternalOver("${f.short}", "${s}")'
                        onpointerout='event.stopPropagation(); onExternalOut("${f.short}", "${s}")'
                        >${full}</div>"""
                    f.can(sb).?(d).|(d.styled("used"))
                }.mkString("") +
                (1.to(6 - f.spellbooks.num - f.unfulfilled.num).toList./(x => f.styled("?")))./(div("spellbook", f.style + "-background")).mkString("") +
                f.unfulfilled./{ r =>
                    val s = r.text.replace("\\", "\\\\") // "
                    val d = s"""<div class='spellbook'
                        onclick='event.stopPropagation(); onExternalClick("${f.short}", "${s}")'
                        onpointerover='event.stopPropagation(); onExternalOver("${f.short}", "${s}")'
                        onpointerout='event.stopPropagation(); onExternalOut("${f.short}", "${s}")'
                        >${r.text}</div>"""
                    d
                }.mkString("")

                val iconSpacing = 30
                val baseRightOffset = 3

                val lcis = f.loyaltyCards.zipWithIndex.map { case (lc, i) =>
                    val spellbook = lc match {
                        case ByatisCard => |(f.upgrades.has(GodOfForgetfulness))
                        case AbhothCard => |(f.upgrades.has(TheBrood))
                        case DaolothCard => |(f.upgrades.has(Interdimensional))
                        case NyogthaCard => |(f.upgrades.has(NightmareWeb))
                        case _ => None
                    }

                    val sb = spellbook @@ {
                        case Some(true) => ", true"
                        case Some(false) => ", false"
                        case None => ""
                    }

                    val d = DrawItem(null, f, lc.icon, Alive, $, 0, 0)
                    val unitName = lc.name.replace("\\", "\\\\").replace("\"", "&quot;")
                    val factionShort = f.short.replace("\"", "&quot;")

                    val right = baseRightOffset + i * iconSpacing

                    s"""<img class='loyalty-card-icon'
                        src='${Overlays.imageSource("info:" + "n-" + lc.name.toLowerCase.replace(" ", "-"))}'
                        style='right:${right}px;'
                        onclick='event.stopPropagation(); onExternalClick("${unitName}"${sb})'
                        onpointerover='event.stopPropagation(); onExternalOver("${unitName}"${sb})'
                        onpointerout='event.stopPropagation(); onExternalOut("${unitName}"${sb})' />"""
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

                b.node.innerHTML = s"""<div class='full-height'
                    onclick='event.stopPropagation(); onExternalClick("${f.short}")'
                    onpointerover='event.stopPropagation(); onExternalOver("${f.short}")'
                    onpointerout='event.stopPropagation(); onExternalOut("${f.short}")'
                    >${div("top")(s) + sb + lcis}</div>"""

                val bitmap = b.get(w, h)

                bitmap.canvas.style.pointerEvents = "none"
                bitmap.canvas.style.width = "" + b.node.clientWidth + "px"
                bitmap.canvas.style.height = "" + b.node.clientHeight + "px"

                val g = bitmap.context
                g.setTransform(1, 0, 0, 1, 0, 0)
                g.clearRect(0, 0, bitmap.width, bitmap.height)

                def dd(d : DrawRect) = {
                    val img = d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key))
                    g.drawImage(img, d.x, d.y)
                }

                dd(DrawItem(null, f, FactionGlyph, Alive, $, 55, 55).rect.copy(tint = |(Processing(None, game.factions.starting.has(f).?("#444444"), None))))

                if (f == SL && game.gates.has(SL.slumber)) {
                    dd(DrawItem(null, f, Gate, Alive, $, w - 46, 56).rect)
                    val cultistOrHP = f.at(SL.slumber, Cultist) ++ f.at(SL.slumber, HighPriest)
                    if (cultistOrHP.any) {
                        val unit = cultistOrHP.head
                        dd(DrawItem(null, unit.faction, unit.uclass, Alive, $, w - 46, 56).rect)
                    }
                }

                var smx = 0
                game.setup.but(f).foreach { e =>
                    if (e.borrowed.has(f.abilities.head)) {
                        dd(DrawItem(null, e, SerpentMan, Alive, $, w - 46 + smx, 86).rect)
                        smx -= 20
                    }
                }

                val deep = f.at(GC.deep).any.?? {
                    var draws = $(DrawItem(null, f, Cthulhu, Alive, $, 64, h - 12 - 6))

                    val sortedDeep = f.at(GC.deep).filterNot(_.uclass == Cthulhu).sortBy(_.uclass @@ {
                        case Cthulhu =>      0
                        case Abhoth =>       1
                        case Daoloth =>      2
                        case Nyogtha =>      3
                        case Starspawn =>    4
                        case Shoggoth =>     5
                        case DeepOne =>      6
                        case Acolyte =>      7
                        case HighPriest =>   8
                        case Ghast =>        9
                        case Gug =>         10
                        case Shantak =>     11
                        case StarVampire => 12
                        case Filth =>       13
                    })

                    while (draws.num - 1 < sortedDeep.num) {
                        val last = draws.last
                        val next = sortedDeep(draws.num - 1).uclass
                        draws :+= ((last.unit, next) match {
                            case (Cthulhu, Abhoth) => DrawItem(null, f, Abhoth, Alive, $, 90 + last.x, 6 + last.y)

                            case (Cthulhu, Daoloth) => DrawItem(null, f, Daoloth, Alive, $, 92 + last.x, 6 + last.y)
                            case (Abhoth, Daoloth) => DrawItem(null, f, Daoloth, Alive, $, 88 + last.x, last.y)

                            case (Cthulhu, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 86 + last.x, 6 + last.y)
                            case (Abhoth, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 81 + last.x, last.y)
                            case (Daoloth, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 86 + last.x, last.y)
                            case (Nyogtha, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 80 + last.x, last.y)

                            case (Cthulhu, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 75 + last.x, 6 + last.y)
                            case (Abhoth, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 70 + last.x, last.y)
                            case (Daoloth, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 82 + last.x, last.y)
                            case (Nyogtha, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 77 + last.x, last.y)
                            case (Starspawn, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 70 + last.x, last.y)

                            case (Cthulhu, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 76 + last.x, 6 + last.y)
                            case (Abhoth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 65 + last.x, last.y)
                            case (Daoloth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 79 + last.x, last.y)
                            case (Nyogtha, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 73 + last.x, last.y)
                            case (Starspawn, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 66 + last.x, last.y)
                            case (Shoggoth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 62 + last.x, last.y)

                            case (Cthulhu, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 64 + last.x, 6 + last.y)
                            case (Abhoth, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 60 + last.x, last.y)
                            case (Daoloth, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 74 + last.x, last.y)
                            case (Nyogtha, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 62 + last.x, last.y)
                            case (Starspawn, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 51 + last.x, last.y)
                            case (Shoggoth, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 48 + last.x, last.y)
                            case (DeepOne, DeepOne) if last.health == Alive => DrawItem(null, f, DeepOne, Pained, $, last.x, last.y - 31)
                            case (DeepOne, DeepOne) if last.health == Pained => DrawItem(null, f, DeepOne, Alive, $, 35 + last.x, last.y + 31)

                            case (Cthulhu, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 57 + last.x, 6 + last.y)
                            case (Abhoth, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 54 + last.x, last.y)
                            case (Daoloth, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 68 + last.x, last.y)
                            case (Nyogtha, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 60 + last.x, last.y)
                            case (Starspawn, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 52 + last.x, last.y)
                            case (Shoggoth, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 48 + last.x, last.y)
                            case (DeepOne, Acolyte) if last.health == Alive => DrawItem(null, f, Acolyte, Alive, $, 36 + last.x, last.y)
                            case (DeepOne, Acolyte) if last.health == Pained => DrawItem(null, f, Acolyte, Alive, $, 36 + last.x, last.y + 31)
                            case (Acolyte, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 35 + last.x, last.y)

                            case (Cthulhu, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 75 + last.x, 6 + last.y)
                            case (Abhoth, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 68 + last.x, last.y)
                            case (Daoloth, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 82 + last.x, last.y)
                            case (Nyogtha, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 77 + last.x, last.y)
                            case (Starspawn, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 70 + last.x, last.y)
                            case (Shoggoth, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 66 + last.x, last.y)
                            case (DeepOne, HighPriest) if last.health == Alive => DrawItem(null, f, HighPriest, Alive, $, 54 + last.x, last.y)
                            case (DeepOne, HighPriest) if last.health == Pained => DrawItem(null, f, HighPriest, Alive, $, 54 + last.x, last.y + 31)
                            case (Acolyte, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 53 + last.x, last.y)

                            case (Cthulhu, Ghast) => DrawItem(null, f, Ghast, Alive, $, 62 + last.x, 6 + last.y)
                            case (Abhoth, Ghast) => DrawItem(null, f, Ghast, Alive, $, 53 + last.x, last.y)
                            case (Daoloth, Ghast) => DrawItem(null, f, Ghast, Alive, $, 67 + last.x, last.y)
                            case (Nyogtha, Ghast) => DrawItem(null, f, Ghast, Alive, $, 61 + last.x, last.y)
                            case (Starspawn, Ghast) => DrawItem(null, f, Ghast, Alive, $, 54 + last.x, last.y)
                            case (Shoggoth, Ghast) => DrawItem(null, f, Ghast, Alive, $, 52 + last.x, last.y)
                            case (DeepOne, Ghast) if last.health == Alive => DrawItem(null, f, Ghast, Alive, $, 39 + last.x, last.y)
                            case (DeepOne, Ghast) if last.health == Pained => DrawItem(null, f, Ghast, Alive, $, 39 + last.x, last.y + 31)
                            case (Acolyte, Ghast) => DrawItem(null, f, Ghast, Alive, $, 37 + last.x, last.y)
                            case (HighPriest, Ghast) => DrawItem(null, f, Ghast, Alive, $, 52 + last.x, last.y)
                            case (Ghast, Ghast) => DrawItem(null, f, Ghast, Alive, $, 35 + last.x, last.y)

                            case (Cthulhu, Gug) => DrawItem(null, f, Gug, Alive, $, 78 + last.x, 6 + last.y)
                            case (Abhoth, Gug) => DrawItem(null, f, Gug, Alive, $, 70 + last.x, last.y)
                            case (Daoloth, Gug) => DrawItem(null, f, Gug, Alive, $, 87 + last.x, last.y)
                            case (Nyogtha, Gug) => DrawItem(null, f, Gug, Alive, $, 77 + last.x, last.y)
                            case (Starspawn, Gug) => DrawItem(null, f, Gug, Alive, $, 70 + last.x, last.y)
                            case (Shoggoth, Gug) => DrawItem(null, f, Gug, Alive, $, 66 + last.x, last.y)
                            case (DeepOne, Gug) if last.health == Alive => DrawItem(null, f, Gug, Alive, $, 56 + last.x, last.y)
                            case (DeepOne, Gug) if last.health == Pained => DrawItem(null, f, Gug, Alive, $, 56 + last.x, last.y + 31)
                            case (Acolyte, Gug) => DrawItem(null, f, Gug, Alive, $, 54 + last.x, last.y)
                            case (HighPriest, Gug) => DrawItem(null, f, Gug, Alive, $, 68 + last.x, last.y)
                            case (Ghast, Gug) => DrawItem(null, f, Gug, Alive, $, 55 + last.x, last.y)
                            case (Gug, Gug) => DrawItem(null, f, Gug, Alive, $, 72 + last.x, last.y)

                            case (Cthulhu, Shantak) => DrawItem(null, f, Shantak, Alive, $, 83 + last.x, 6 + last.y)
                            case (Abhoth, Shantak) => DrawItem(null, f, Shantak, Alive, $, 64 + last.x, last.y)
                            case (Daoloth, Shantak) => DrawItem(null, f, Shantak, Alive, $, 90 + last.x, last.y)
                            case (Nyogtha, Shantak) => DrawItem(null, f, Shantak, Alive, $, 70 + last.x, last.y)
                            case (Starspawn, Shantak) => DrawItem(null, f, Shantak, Alive, $, 66 + last.x, last.y)
                            case (Shoggoth, Shantak) => DrawItem(null, f, Shantak, Alive, $, 66 + last.x, last.y)
                            case (DeepOne, Shantak) if last.health == Alive => DrawItem(null, f, Shantak, Alive, $, 49 + last.x, last.y)
                            case (DeepOne, Shantak) if last.health == Pained => DrawItem(null, f, Shantak, Alive, $, 49 + last.x, last.y + 31)
                            case (Acolyte, Shantak) => DrawItem(null, f, Shantak, Alive, $, 50 + last.x, last.y)
                            case (HighPriest, Shantak) => DrawItem(null, f, Shantak, Alive, $, 61 + last.x, last.y)
                            case (Ghast, Shantak) => DrawItem(null, f, Shantak, Alive, $, 48 + last.x, last.y)
                            case (Gug, Shantak) => DrawItem(null, f, Shantak, Alive, $, 63 + last.x, last.y)
                            case (Shantak, Shantak) => DrawItem(null, f, Shantak, Alive, $, 74 + last.x, last.y)

                            case (Cthulhu, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 79 + last.x, 6 + last.y)
                            case (Abhoth, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 63 + last.x, last.y)
                            case (Daoloth, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 77 + last.x, last.y)
                            case (Nyogtha, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 69 + last.x, last.y)
                            case (Starspawn, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 61 + last.x, last.y)
                            case (Shoggoth, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 60 + last.x, last.y)
                            case (DeepOne, StarVampire) if last.health == Alive => DrawItem(null, f, StarVampire, Alive, $, 50 + last.x, last.y)
                            case (DeepOne, StarVampire) if last.health == Pained => DrawItem(null, f, StarVampire, Alive, $, 50 + last.x, last.y + 31)
                            case (Acolyte, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 52 + last.x, last.y)
                            case (HighPriest, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 59 + last.x, last.y)
                            case (Ghast, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 53 + last.x, last.y)
                            case (Gug, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 64 + last.x, last.y)
                            case (Shantak, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 70 + last.x, last.y)
                            case (StarVampire, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 65 + last.x, last.y)

                            case (Cthulhu, Filth) => DrawItem(null, f, Filth, Alive, $, 62 + last.x, last.y - 10)
                            case (Abhoth, Filth) => DrawItem(null, f, Filth, Alive, $, 50 + last.x, last.y - 15)
                            case (Daoloth, Filth) => DrawItem(null, f, Filth, Alive, $, 70 + last.x, last.y - 15)
                            case (Nyogtha, Filth) => DrawItem(null, f, Filth, Alive, $, 65 + last.x, last.y - 15)
                            case (Starspawn, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Shoggoth, Filth) => DrawItem(null, f, Filth, Alive, $, 52 + last.x, last.y - 15)
                            case (DeepOne, Filth) if last.health == Alive => DrawItem(null, f, Filth, Alive, $, 42 + last.x, last.y - 15)
                            case (DeepOne, Filth) if last.health == Pained => DrawItem(null, f, Filth, Alive, $, 42 + last.x, last.y + 16)
                            case (Acolyte, Filth) => DrawItem(null, f, Filth, Alive, $, 38 + last.x, last.y - 15)
                            case (HighPriest, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Ghast, Filth) => DrawItem(null, f, Filth, Alive, $, 40 + last.x, last.y - 15)
                            case (Gug, Filth) => DrawItem(null, f, Filth, Alive, $, 57 + last.x, last.y - 15)
                            case (Shantak, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (StarVampire, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Filth, Filth) => DrawItem(null, f, Filth, Alive, $, 40 + last.x, last.y)

                            case (a, b) => throw new RuntimeException(s"GC DEEP missing draw case: $a -> $b")
                        })
                    }

                    draws./(_.rect)
                }

                val captured = {
                    var draws : $[DrawItem] = $

                    game.setup.but(f)./~(_.at(f.prison)).foreach { u =>
                        val (prisonXOffset, prisonYOffset) = u.uclass match {
                            case Filth        => (8, -15)
                            case _            => (0, 0)
                        }

                        val x = draws./(_.rect.width).sum - 0 + prisonXOffset
                        val y = h - 12 + prisonYOffset

                        draws :+= DrawItem(null, u.faction, u.uclass, Alive, $, x, y)
                    }

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
                    factionStatus(displayGame.setup(n), statusBitmaps(n))(displayGame)
                }

                mapWest.innerHTML = (board.west :+ GC.deep)./(r => processStatus(displayGame.regionStatus(r), "p8")).mkString("")
                mapEast.innerHTML = board.east./(r => processStatus(displayGame.regionStatus(r), "p8")).mkString("")

                drawMap(displayGame)
            }

            dom.window.onresize = e => updateStatus()

            var token : Double = 0.0

            var savedUIAction : |[(Faction, $[Action])] = None
            var savedContinue : |[Continue] = None

            class BackgroundCheckToken(random : Double)

            var backgroundCheckThread : |[BackgroundCheckToken] = None

            def startBackgroundCheck() {
                if (backgroundCheckThread.none) {
                    val t = new BackgroundCheckToken(math.random())
                    backgroundCheckThread = |(t)
                    executeBackgroundCheck(t)
                }
            }

            def executeBackgroundCheck(token : BackgroundCheckToken) {
                if (backgroundCheckThread.has(token)) {
                    setTimeout(500) {
                        if (backgroundCheckThread.has(token)) {
                            getF(server + "read/" + hash + "/" + (actions.num + 3)) { ll =>
                                if (backgroundCheckThread.has(token)) {
                                    if (ll.splt("\n").but("").any) {
                                        backgroundCheckThread = None

                                        // ask("Waiting for update >" + ll + "<", $, n => {})
                                        ask("Waiting for update", $, n => {})

                                        perform(UpdateAction)
                                    }
                                    else {
                                        executeBackgroundCheck(token)
                                    }
                                }
                            }
                        }
                    }
                }
            }

            def stopBackgroundCheck() {
                backgroundCheckThread = None
            }

            def perform(action : Action) {
                queue :+= UIPerform(game, action)

                if (!paused)
                    startUI()
            }

            def finishUI() {
                updateUI() match {
                    case Some(_) => finishUI()
                    case None =>
                }
            }

            def startUI() {
                token = math.random()

                processUI(token)
            }

            def processUI(t : Double) {
                if (t == token) {
                    updateUI() match {
                        case Some((s, n)) =>
                            if (s) {
                                updateStatus()
                                if (recorded.any && hash == "")
                                    replayMenu()
                            }

                            setTimeout(n * delay) { processUI(t) }
                        case None =>
                    }
                }
            }

            def cancelUndo() {
                clear(undoDiv)
                hide(undoDiv.parentElement.parentElement)

                if (overrideGame.any) {
                    overrideGame = None
                    updateStatus()
                }
            }

            def showUndo(n : Int) : () => Unit = () => {
                show(undoDiv.parentElement.parentElement)

                clear(undoDiv)

                undoDiv.appendChild(newDiv("", "Game state after " + n + " actions"))

                val style = None

                if (hash != "")
                    undoDiv.appendChild(newDiv("option" + style./(" " + _).|(""), "Undo to here".hl, () => { clear(undoDiv); performUndoOnline(n) }))
                else
                    undoDiv.appendChild(newDiv("option" + style./(" " + _).|(""), "Undo to here".hl, () => { clear(undoDiv); performUndoLocal(n) }))

                undoDiv.appendChild(newDiv("option" + style./(" " + _).|(""), "Cancel", () => { cancelUndo() }))

                val g = new Game(board, track, seating, true, setup.options)

                actions.reverse.take(n).foreach { a =>
                    if (a.isVoid.not)
                        g.perform(a.unwrap)
                }

                overrideGame = |(g)

                updateStatus()

                ()
            }

            def performUndoLocal(n : Int) : Unit = {
                actions = actions.takeLast(n)

                clear(logDiv)

                log(version)

                var cc : Continue = StartContinue

                val g = new Game(board, track, seating, true, setup.options)

                actions.reverse.indexed./ { (a, i) =>
                    if (a.isVoid.not) {
                        val (l, c) = g.perform(a.unwrap)

                        l.foreach(s => log(s, showUndo(i + 1)))

                        if (a.isOutOfTurn.not)
                            cc = c
                    }
                }

                game = g
                overrideGame = None

                queue = $(askFaction(cc)(game))

                startUI()
            }

            def performUndoOnline(n : Int) : Unit = {
                hrf.web.postF(server + "rollback-v2/" + hash + "/" + (n + 3), "") { _ =>
                    dom.document.location.assign(dom.document.location.href)
                } {
                    log("Failed to undo, reloading...")

                    setTimeout(3000) {
                        dom.document.location.assign(dom.document.location.href)
                    }
                }
            }


            def updateUI() : Option[(Boolean, Int)] = {
                queue.@@ {
                    case head :: rest =>
                        queue = rest
                        // println(head)
                        head match {
                            case UILog(l) => {
                                log(l, showUndo(actions.num))

                                Some((false, (l == DottedLine).?(16).|(5)))
                            }
                            case UIPerform(g, a : GameOverAction) if a.msg == "Save replay" => {
                                val ir = hrf.html.ImageResources(Map(), Map(), hrf.HRF.imageCache)
                                val resources = hrf.html.Resources(ir, () => Map())
                                val title = "Cthulhu Wars " + BuildInfo.version + " Replay"
                                val filename = "cthulhu-wars-" + BuildInfo.version + "-replay-" + hrf.HRF.startAt.toISOString().take(16).replace("T", "-")

                                hrf.quine.Quine.save(title, g.setup, g.options, resources, actions.reverse, new Serialize(g), filename, true, "", {
                                    val winners = a.winners
                                    queue :+= UIQuestion(null, game, GameOverAction(winners, "Hooray!") :: GameOverAction(winners, "Meh...") :: GameOverAction(winners, "Save replay"))
                                })

                                Some((false, 10))
                            }
                            case UIPerform(g, UpdateAction) => {
                                queue :+= UIRead(g)

                                Some((false, 0))
                            }
                            case UIPerform(g, OutOfTurnReturn) => {
                                savedUIAction.$./{ (f, l) =>
                                    if (l.of[OutOfTurnRefresh].any)
                                        queue = UIPerform(g, l.of[OutOfTurnRefresh].only.action) +: queue
                                    else
                                        queue = UIQuestion(f, g, l) +: queue
                                }
                                savedUIAction = None
                                Some((false, 0))
                            }
                            case UIPerform(g, OutOfTurnRepeat(f, action)) => {
                                val (l, c) = g.perform(action.unwrap)

                                val cc = c match {
                                    case Ask(f, l) => Ask(f, l./(a => a.useIf(_.unwrap == CancelAction)(_ => OutOfTurnDone.as("Done")(" "))))
                                    case c => c
                                 }

                                queue :+= askFaction(cc)(game)

                                Some((false, 0))
                            }
                            case UIPerform(g, a) if hash != "" && self == None && localReplay.not => {
                                queue :+= UIRead(g)

                                Some((false, 30))
                            }
                            case UIPerform(g, a) if hash != "" && a.isRecorded && localReplay.not => {
                                val position = actions.num
                                var n = 0

                                def retry() {
                                    n += 1

                                    if (n < 12) {
                                        if (n > 1)
                                            askN($(n.times(".").mkString("")), $)

                                        setTimeout(n * 100) { post() }
                                    }
                                    else {
                                        hrf.web.getF(server + "alive") { s =>
                                            if (s == "1")
                                                askN($("Synchronization Error"), $(AskLine("", "Reload", $, false, () => dom.document.location.assign(dom.document.location.href))))
                                            else
                                                askN($("Server Error"), $(AskLine("", "Reload", $, false, () => dom.document.location.assign(dom.document.location.href))))
                                        } {
                                            askN($("Server Down"), $(AskLine("", "Please Reload Later", $("thin"), false)))
                                        }
                                    }
                                }

                                def post() {
                                    if (position == actions.num) {
                                        hrf.web.postF(server + "write/" + hash + "/" + (position + 3), serializer.write(a.unwrap)) { _ =>
                                            queue :+= UIRead(g)

                                            startUI()
                                        } {
                                            if (position == actions.num) {
                                                hrf.web.getF(server + "read/" + hash + "/" + (position + 3)) { ll =>
                                                    if (ll.splt("\n").but("").any) {
                                                        queue :+= UIRead(g)

                                                        startUI()
                                                    }
                                                    else
                                                        retry()
                                                } {
                                                    retry()
                                                }
                                            }
                                        }
                                    }
                                }

                                post()

                                None
                            }
                            case UIRead(g) => {
                                getF(server + "read/" + hash + "/" + (actions.num + 3)) { ll =>
                                    queue :+= UIProcess(g, ll.splt("\n").but("")./(serializer.parseAction))

                                    startUI()
                                }

                                None
                            }
                            case UIProcess(g, recorded) if recorded.none => {
                                queue :+= UIRead(g)

                                Some((false, 30))
                            }
                            case UIProcess(g, recorded) => {
                                savedUIAction = None

                                var cc : |[Continue] = savedContinue

                                val initial = actions.none

                                recorded.indexed./ { (a, n) =>
                                    actions +:= a

                                    if (a.is[ReloadAction.type] && initial.not) {
                                        println("reloading...")
                                        dom.document.location.assign(dom.document.location.href)
                                        return None
                                    }

                                    if (a.isVoid.not) {
                                        val (l, c) = game.perform(a.unwrap)

                                        l.foreach(s => log(s, showUndo(actions.num)))

                                        if (a.isOutOfTurn.not)
                                            cc = |(c)
                                        else
                                            c @@ {
                                                case Then(OutOfTurnRepeat(f, action)) if self.has(f) => cc = |(c)
                                                case Then(OutOfTurnRepeat(f, action)) if self.has(f).not =>
                                                case c => cc = |(c)
                                            }
                                    }
                                }

                                savedContinue = cc

                                queue :+= askFaction(cc.get)(game)

                                Some((true, 0))
                            }

                            case UIPerform(g, action) if action.isVoid =>
                                if (action.isRecorded || recorded.any)
                                    actions +:= action

                                if (recorded.any && hash == "" && localReplay.not) {
                                    if (recorded.num > actions.num && paused.not)
                                        queue :+= UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">")))
                                }

                                Some((true, 0))

                            case UIPerform(g, action) => {
                                val a = action match {
                                    case esa : ElderSignAction if recorded.any => esa.copy(public = true)
                                    case a => a
                                }

                                if (a.isRecorded)
                                    actions +:= a

                                val (l, c) = g.perform(a.unwrap)

                                l.foreach { s =>
                                    queue :+= UILog(s)
                                }

                                val t = c match {
                                    case Ask(f, _) if setup.difficulty(f) == Human => 2
                                    case Ask(_, actions) if actions.distinct.num <= 2 => 4
                                    case DelayedContinue(n, _) => n
                                    case Then(_) => 0
                                    case _ => 30
                                 }

                                if (recorded.any && hash == "" && localReplay.not) {
                                    if (recorded.num > actions.num && !paused)
                                        queue :+= UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">")))
                                }
                                else
                                    queue :+= askFaction(c)(game)

                                Some((true, t))
                            }
                            case UIRollD6(g, q, roll) => {
                                ask(q(g), (1::2::3::4::5::6)./("[" + _.styled("power") + "]"), x => perform(roll(x)))
                            }
                            case UIRollBattle(g, q, n, roll) if n <= 3 => {
                                def apr(br : BattleRoll) = 0.to(n)./(_.times(br))
                                val results = apr(Kill)./~(k => apr(Pain)./~(p => apr(Miss)./(m => k ++ p ++ m))).%(_.num == n)
                                val os = results./(roll)
                                ask(q(g), results./(_.mkString(" ")), v => perform(roll(results(v))))
                            }
                            case UIRollBattle(g, q, n, roll) => {
                                val osK = 0.to(n)./(_.times(Kill))./(x => x.any.?(x.mkString(" ")).|("None"))
                                ask(q(g) + "<br/>" + "Number of " + "Kills".styled("kill"), osK, kills => {
                                    if (kills == n) {
                                        perform(roll(kills.times(Kill)))
                                    }
                                    else {
                                        val osP = 0.to(n - kills)./(_.times(Pain))./(x => x.any.?(x.mkString(" ")).|("None"))
                                        ask(q(g) + "<br/>" + "Number of " + "Pains".styled("pain"), osP, pains => {
                                            perform(roll(kills.times(Kill) ++ pains.times(Pain) ++ (n - kills - pains).times(Miss)))
                                        })
                                    }
                                })
                            }
                            case UIDrawES(g, q, es1, es2, es3, draw) =>
                                val options = ((1 -> es1) :: (2 -> es2) :: (3 -> es3)).%>(_ > 0)
                                ask(q(g), options./((e, q) => "[" + e.styled("es") + "]" + " of " + q), n => perform(draw(options(n)._1, true)))

                            case UIQuestion(e, game, actions, waiting) if hash != "" && e != null && self.none && localReplay.not => {
                                startBackgroundCheck()

                                scrollTop = 0

                                askN($("Waiting for " + waiting.some.|($(e)).mkString(", ") + "<br/>“" + actions.first.safeQ(game) + "”<br/><br/>"), $)
                            }
                            case UIQuestion(e, game, actions, waiting) if hash != "" && e != null && self.any && self.has(e).not && localReplay.not => {
                                val extra = self./~(f => game.extraActions(f, true, actions.has(SacrificeHighPriestAllowedAction)))
                                val f = self.get

                                startBackgroundCheck()

                                scrollTop = 0

                                askN($("Waiting for " + waiting.some.|($(e)).mkString(", ") + "<br/>“" + actions.first.safeQ(game) + "”<br/><br/>"),
                                    extra./(a => AskLine(a.question(game), a.option(game), false.$(f.style + "-border") ++ a.isInfo.$("thin"), a.isNoClear.not, a.isInfo.not.??(() => {
                                        stopBackgroundCheck()

                                        savedUIAction = |((e, actions))

                                        perform(a)
                                    })))
                                )
                            }
                            case UIQuestion(f, game, actions, waiting) => {
                                cancelUndo()

                                if (hash != "")
                                    startBackgroundCheck()

                                val extra = actions.unwrap.use(l => game.extraActions(f, false, actions.has(SacrificeHighPriestAllowedAction)).%{
                                    case _ if l.has(CancelAction) => false
                                    case _ if l.has(OutOfTurnDone) => false
                                    case _ : DragonAscendingOutOfTurnAction if l.of[DragonAscendingPromptAction].any => false
                                    case _ : SacrificeHighPriestOutOfTurnMainAction if l.of[SacrificeHighPriestPromptAction].any => false
                                    case _ => true
                                })

                                scrollTop = 0

                                val aa = actions.useIf(game.options.has(AsyncActions))(_
                                    .%!(_.unwrap.is[RevealESMainAction])
                                    .%!(_.unwrap.is[SacrificeHighPriestDoomAction])
                                    .%!(_.unwrap.is[SacrificeHighPriestMainAction])
                                    .%!(_.unwrap.is[DragonAscendingMainAction])
                                    .%!(_.unwrap.is[DragonAscendingDoomAction])
                                )

                                askN($,
                                    aa./(a => AskLine(a.question(game), a.option(game), $(f.style + "-border") ++ a.isInfo.$("thin"), a.isNoClear.not, a.isInfo.not.??(() => {
                                        stopBackgroundCheck()

                                        perform(a)
                                    }))) ++
                                    extra./(a => AskLine(a.question(game), a.option(game), false.$(f.style + "-border") ++ a.isInfo.$("thin"), a.isNoClear.not, a.isInfo.not.??(() => {
                                        stopBackgroundCheck()

                                        savedUIAction = |((f, actions))

                                        perform(a)
                                    })))
                                )
                            }
                            case UIQuestionDebug(f, g, actions) => {
                                cancelUndo()

                                val aa = Explode.explode(g, actions)

                                val sorted = if (f == BG)
                                    Bot3(BG).eval(aa)(g).sortBy(-_.evaluations.map(_.weight).sum)
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

                                askM($, sorted./(wa => wa.action.question(g).some.|(" ") -> (wa.action.option(g) + " " + wa.action.toString + " (" + wa.evaluations.starting./(_.weight)./(v => v.styled((v > 0).?("power").|("doom"))).|("0") + ")" + "<br/>" +
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
                ask("Replay (" + actions.num + " / " + recorded.num + ")", $(paused.?("Play").|("Pause"), "Start", "End", "Next"), {
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
                        game = new Game(game.board, game.ritualTrack, game.factions, game.logging, $)
                        actions = $
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
                            startUI()
                        }
                        replayMenu()
                })
            }

            if (hash != "") {
                if (recorded.any || self == None) {
                    queue :+= UIProcess(game, recorded./(serializer.parseAction))

                    startUI()
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

        def allSeatings(factions : $[Faction]) = factions.permutations.toList.%(s => s.has(GC).?(s(0) == GC).|(s(0) != WW))
        def randomSeating(factions : $[Faction]) = allSeatings(factions).sortBy(s => random()).head

        def startOnlineSetup(factions : $[Faction]) {
            val all = allSeatings(factions)

            val seatings = all.%(s => all.indexOf(s) <= all.indexOf(s.take(1) ++ s.drop(1).reverse))

            val setup = new Setup(seatings(0), Human)

            def setupQuestions() {
                askM($,
                    factions.map(f => "Factions" -> ("" + f + " (" + setup.difficulty(f).elem + ")")) ++
                    seatings.map(ff => ("Seating" + factions.has(GC).not.??(" and first player")) -> ((ff == setup.seating).?(ff.map(_.ss)).|(ff.map(_.short)).mkString(" -> "))) ++
                    $("Gameplay" -> ("Gate Diplomacy (" + setup.get(GateDiplomacy).?("yes").|("no").hl + ")")) ++
                    $("Gameplay" -> ("Async Options (" + setup.get(AsyncActions).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("High Priests (" + setup.get(HighPriests).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " spellbooks (" + setup.get(NeutralSpellbooks).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " monsters (" + setup.get(NeutralMonsters).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + GhastCard.short + " (" + setup.get(UseGhast).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + GugCard.short + " (" + setup.get(UseGug).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + ShantakCard.short + " (" + setup.get(UseShantak).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + StarVampireCard.short + " (" + setup.get(UseStarVampire).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Independent Great Old Ones (" + setup.get(IGOOs).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + ByatisCard.short + " (" + setup.get(UseByatis).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + AbhothCard.short + " (" + setup.get(UseAbhoth).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + DaolothCard.short + " (" + setup.get(UseDaoloth).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + NyogthaCard.short + " (" + setup.get(UseNyogtha).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL) && factions.has(WW))
                        .$("Variants" -> (IceAge.full + " affects " + Lethargy.full + " (" + setup.get(IceAgeAffectsLethargy).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW) && factions.num == 4)
                        .$("Variants" -> (OW.full + " needs 10 Gates in 4-Player (" + setup.get(Opener4P10Gates).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL))
                        .$("Variants" -> (DemandSacrifice.full + " requires " + SL.styled(Tsathoggua) + " (" + setup.get(DemandTsathoggua).?("yes").|("no").hl + ")")) ++
                    $("Map" -> ("Map Configuration (" + setup.options.of[MapOption].lastOption.?(_.toString.hl) + ")")) ++
                    $("Done" -> "Start game".styled("power")),
                    nn => {
                        var n = nn
                        if (n >= 0 && n < factions.num) {
                            setup.difficulty += factions(n) -> $(Human, Easy, Normal, Human).dropWhile(_ != setup.difficulty(factions(n))).drop(1).head
                            setupQuestions()
                        }
                        n -= factions.num
                        if (n >= 0 && n < seatings.num) {
                            setup.seating = seatings(n)
                            setupQuestions()
                        }
                        n -= seatings.num
                        if (n == 0) {
                            setup.toggle(GateDiplomacy)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(AsyncActions)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(HighPriests)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralSpellbooks)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralMonsters)

                            if (setup.options.has(NeutralMonsters))
                                setup.options ++= $(UseGhast, UseGug, UseShantak, UseStarVampire)
                            else
                                setup.options = setup.options.notOf[NeutralMonsterOption]

                            setupQuestions()
                        }
                        if (setup.options.has(NeutralMonsters)) {
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
                        n -= 1
                        if (n == 0) {
                            setup.toggle(IGOOs)

                            if (setup.options.has(IGOOs))
                                setup.options ++= $(UseByatis, UseAbhoth, UseDaoloth, UseNyogtha)
                            else
                                setup.options = setup.options.notOf[IGOOOption]

                            setupQuestions()
                        }
                        if (setup.options.has(IGOOs)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseByatis)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseAbhoth)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseDaoloth)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseNyogtha)
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

        def startSetup(factions : $[Faction]) {
            val all = allSeatings(factions)

            val seatings = all.%(s => all.indexOf(s) <= all.indexOf(s.take(1) ++ s.drop(1).reverse))

            val setup = new Setup(seatings(0), Human)

            def setupQuestions() {
                askM($,
                    factions.map(f => "Factions" -> ("" + f + " (" + setup.difficulty(f).elem + ")")) ++
                    seatings.map(ff => ("Seating" + factions.has(GC).not.??(" and first player")) -> ((ff == setup.seating).?(ff.map(_.ss)).|(ff.map(_.short)).mkString(" -> "))) ++
                    $("Gameplay" -> ("Gate Diplomacy (" + setup.get(GateDiplomacy).?("yes").|("no").hl + ")")) ++
                    $("Gameplay" -> ("Async Options (" + setup.get(AsyncActions).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("High Priests (" + setup.get(HighPriests).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " spellbooks (" + setup.get(NeutralSpellbooks).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " monsters (" + setup.get(NeutralMonsters).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + GhastCard.short + " (" + setup.get(UseGhast).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + GugCard.short + " (" + setup.get(UseGug).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + ShantakCard.short + " (" + setup.get(UseShantak).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(NeutralMonsters))
                        .$("Variants" -> ("Use " + StarVampireCard.short + " (" + setup.get(UseStarVampire).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Independent Great Old Ones (" + setup.get(IGOOs).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + ByatisCard.short + " (" + setup.get(UseByatis).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + AbhothCard.short + " (" + setup.get(UseAbhoth).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + DaolothCard.short + " (" + setup.get(UseDaoloth).?("yes").|("no").hl + ")")) ++
                    (setup.options.has(IGOOs))
                        .$("Variants" -> ("Use " + NyogthaCard.short + " (" + setup.get(UseNyogtha).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL) && factions.has(WW))
                        .$("Variants" -> (IceAge.full + " affects " + Lethargy.full + " (" + setup.get(IceAgeAffectsLethargy).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW) && factions.num == 4)
                        .$("Variants" -> (OW.full + " needs 10 Gates in 4-Player (" + setup.get(Opener4P10Gates).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL))
                        .$("Variants" -> (DemandSacrifice.full + " requires " + SL.styled(Tsathoggua) + " (" + setup.get(DemandTsathoggua).?("yes").|("no").hl + ")")) ++
                    $("Map" -> ("Map Configuration (" + setup.options.of[MapOption].lastOption.?(_.toString.hl) + ")")) ++
                    $("Options" -> ("Dice rolls (" + setup.dice.?("auto").|("manual").hl + ")")) ++
                    $("Options" -> ("Elder Signs (" + setup.es.?("auto").|("manual").hl + ")")) ++
                    $("Options" -> ("Forced moves (" + setup.confirm.?("confirm").|("perform").hl + ")")) ++
                    $("Done" -> "Start game".styled("power")),
                    nn => {
                        var n = nn
                        if (n >= 0 && n < factions.num) {
                            setup.difficulty += factions(n) -> $(Human, Easy, Normal, Human).dropWhile(_ != setup.difficulty(factions(n))).drop(1).head
                            setupQuestions()
                        }
                        n -= factions.num
                        if (n >= 0 && n < seatings.num) {
                            setup.seating = seatings(n)
                            setupQuestions()
                        }
                        n -= seatings.num
                        if (n == 0) {
                            setup.toggle(GateDiplomacy)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(AsyncActions)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(HighPriests)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralSpellbooks)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralMonsters)

                            if (setup.options.has(NeutralMonsters))
                                setup.options ++= $(UseGhast, UseGug, UseShantak, UseStarVampire)
                            else
                                setup.options = setup.options.notOf[NeutralMonsterOption]

                            setupQuestions()
                        }
                        if (setup.options.has(NeutralMonsters)) {
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
                        n -= 1
                        if (n == 0) {
                            setup.toggle(IGOOs)

                            if (setup.options.has(IGOOs))
                                setup.options ++= $(UseByatis, UseAbhoth, UseDaoloth, UseNyogtha)
                            else
                                setup.options = setup.options.notOf[IGOOOption]

                            setupQuestions()
                        }
                        if (setup.options.has(IGOOs)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseByatis)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseAbhoth)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseDaoloth)
                                setupQuestions()
                            }
                            n -= 1
                            if (n == 0) {
                                setup.toggle(UseNyogtha)
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

        val allFactions = $(GC, CC, BG, YS, SL, WW, OW, AN)

        val replay = getElem("replay").?./(_.innerHTML).|("")

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

                if (dom.window.location.pathname.startsWith("/play/").not)
                    dom.window.history.pushState("initilaize", "", "/play/" + hash)

                if (role != "$") {
                    get(server + "read/" + hash + "/0") { read =>
                        val logs = read.split("\n").toList

                        val oldVersion = logs(0)
                        if (oldVersion != version)
                            log("Incorrect game version: " + oldVersion.hl)

                        oldVersion @@ {
                            case "Cthulhu Wars Solo HRF 1.8"
                               | "Cthulhu Wars Solo HRF 1.9"
                               | "Cthulhu Wars Solo HRF 1.10"
                               | "Cthulhu Wars Solo HRF 1.11"
                               | "Cthulhu Wars Solo HRF 1.12"
                               | "Cthulhu Wars Solo HRF 1.13"
                               | "Cthulhu Wars Solo HRF 1.14"
                               | "Cthulhu Wars Solo HRF 1.15"
                               | "Cthulhu Wars Solo HRF 1.16"
                               | "Cthulhu Wars Solo HRF 1.17"
                            =>
                                val l = dom.document.location
                                val search = ("version=" + "retro") +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                                val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                                log("Reload: " + url)
                                dom.document.location.assign(url)
                                return

                            case _ =>
                        }

                        val title = dom.document.createElement("div")
                        title.innerHTML = s"""
                            <div style="
                                position: absolute;
                                left: 0%;
                                top: 0%;
                                width: 100%;
                                height: 100%;
                                z-index: 1;
                                pointer-events: none;
                            ">
                                <div style="
                                    color: rgb(255, 255, 255);
                                    font-size: 100%;
                                    font-weight: bold;
                                    filter: drop-shadow(rgb(0, 0, 0) 0px 0px 6px) drop-shadow(rgb(0, 0, 0) 0px 0px 6px) drop-shadow(rgb(0, 0, 0) 0px 0px 6px);
                                    text-align: left;
                                ">
                                    <span data-elem="text">
                                        ${logs(1)}
                                    </span>
                                </div>
                            </div>"""

                        mapSmall.appendChild(title)

                        dom.document.title = logs(1) + " - Cthulhu Wars HRF"

                        def parseDifficulty(s : String) : |[Difficulty] = Serialize.parseSymbol(s)./~(_.as[Difficulty])

                        val factions = logs(2).split(" ")(0).split("/").toList./(_.split(":"))./(s => Serialize.parseFaction(s(0)).get -> parseDifficulty(s(1)).get)
                        val options = logs(2).split(" ").toList.drop(1)./(Serialize.parseGameOption)./(_.get)

                        log(logs(1).styled("nt"))
                        log(self./("Playing as " + _).|("Spectating"))

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
                ask("Cthulhu Wars", $("Quick Game".hl, "Local Game".hl, redirect.?("<a href='https://cwo.im/' target='_blank'><div>" + "Online game".hl + "</div></a>").|("Online Game".hl), "Extra", "About", "Test").take(menu), {
                    case 998_0 =>
                        val setup = new Setup(randomSeating($(GC, BG, WW, OW)), Normal)
                        setup.difficulty += OW -> Debug
                        setup.options = $(MapEarth35, GateDiplomacy)
                        startGame(setup)
                    case 999_0 =>
                        val n = 1
                        val pn = n + 3
                        ask("Play as", allFactions./(_.full) :+ "Back", nf => {
                            if (nf < allFactions.num) {
                                val faction = allFactions(nf)
                                val combinations = allFactions.but(faction).combinations(pn - 1).toList
                                ask("Choose opponents", combinations./(_.mkString(", ")) :+ "Back", no => {
                                    if (no < combinations.num) {
                                        val opponents = combinations(no)
                                        val setup = new Setup(randomSeating(faction +: opponents), Normal)
                                        setup.difficulty += faction -> Human
                                        setup.options ++= $(QuickGame, MapEarth35)
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
                        setup.options ++= $(QuickGame) ++ $(MapEarth35, MapEarth53).shuffle.take(1)
                        startGame(setup)
                    case 1 =>
                        ask("Players", ("3 Players".hl :: "4 Players".hl :: "5 Players".hl) :+ "Back", n => {
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
                        ask("Players", ("3 Players".hl :: "4 Players".hl :: "5 Players".hl) :+ "Back", n => {
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
                    case 3 => ask("Cthulhu Wars Extra", $("Survival mode".styled("kill"), "Download Offline Version".hl, "<a href='https://necronomicon.app/' target='_blank'><div>Necronomicon</div></a>", "<a href='https://cthulhuwars.fandom.com/' target='_blank'><div>Cthulhu Wars Strategy Wiki</div></a>", "Back"), {
                        case 0 =>
                            val base = allFactions.take(4)
                            ask("Choose faction", base./(f => f.full) :+ "Back", nf => {
                                if (nf < base.num) {
                                    val setup = new Setup(randomSeating(base), AllVsHuman)
                                    setup.difficulty += base(nf) -> Human
                                    startGame(setup)
                                }
                                else
                                    topMenu()
                            })
                        case 1 =>
                            val ir = hrf.html.ImageResources(Map(), Map(), hrf.HRF.imageCache)
                            val resources = hrf.html.Resources(ir, () => Map())
                            var game = new Game(EarthMap3, RitualTrack.for3, $(GC, CC, BG), true, $)

                            hrf.quine.Quine.save("cthulhu-wars-solo-" + BuildInfo.version, $, $, resources, $, new Serialize(game), "cthulhu-wars-solo-" + BuildInfo.version, false, "", topMenu())
                        case 2 | 3 | 4 =>
                            topMenu()
                    })
                    case 4 =>
                        ask("Cthulhu Wars Solo", $(
                            "<a href='https://boardgamegeek.com/filepage/152635/cthulhu-wars-solo-hrf-19' target='_blank'><div>Project Homepage</div></a>",
                            "Developed by " + "Haunt Roll Fail".hl,
                            "Additional AI programming by " + "ricedwlit".hl,
                            "Ancients, High Priests, Neutral Monsters, Independent Great Old Ones developed by " + "Legrasse81".hl,
                            "Board game by " + "Peterson Games".hl,
                            "All graphics in the app belong to Petersen Games.<br>Used with permission.",
                            "Back"
                        ), { _ => topMenu() })
                    case 5 =>
                        val setup = new Setup(randomSeating($(GC, BG, WW)), Normal)
                        setup.difficulty += GC -> Human
                        setup.difficulty += BG -> Normal
                        setup.difficulty += WW -> Debug
                        // setup.options = $(MapEarth53)
                        // setup.options = $(NeutralMonsters, UseGhast, UseGug, UseShantak, UseStarVampire)
                        setup.options = $(IGOOs, UseAbhoth, UseDaoloth, UseNyogtha)
                        //setup.options = $(HighPriests)
                        startGame(setup)
                    case 666 =>
                        val base = allFactions.take(4)
                        ask("Choose faction", base./(f => f.full) :+ "Back", nf => {
                            if (nf < base.num) {
                                val setup = new Setup(randomSeating(base), AllVsHuman)
                                setup.difficulty += base(nf) -> Human
                                startGame(setup)
                            }
                            else
                                topMenu()
                        })
                    case 777 =>
                        val setup = new Setup(randomSeating($(YS, GC, BG, AN)), Normal)
                        startGame(setup)
                    case 888 =>
                        val setup = new Setup(randomSeating($(GC, CC, YS, OW)), Normal)
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
