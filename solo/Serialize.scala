package cws

import hrf.colmat._

import scala.scalajs.reflect._

import fastparse._, NoWhitespace._

class Serialize(val g : Game) {
    import Serialize._

    def className(o : AnyRef) : String = o.getClass.getName.split("\\.").toList.last.split("\\$").toList.last

    def write(o : Any) : String = o match {
        case b : Boolean => b.toString
        case n : Int => n.toString
        case s : String => "\"" + s + "\""
        case r : Region => r.name.split(" ").mkString("")
        case f : Faction => f.short
        case sb : Spellbook => className(sb)
        case uc : UnitClass => className(uc)
        case ur : UnitRef => write(ur.faction) + "/" + write(ur.uclass) + "/" + write(ur.index)
        case br : BattleRoll => className(br)
        case bf : BattlePhase => className(bf)
        case o : Offer => write(o.f) + "->" + write(o.n)
        case a : Action => className(a) + a.productIterator.toList./(write).mkString("(", ", ", ")")
        case es : ElderSign => "$" + es.value

        case Some(x) => "Some(" + write(x) + ")"
        case None => "None"

        case ss : List[_] => ss./(write).mkString("[", ", ", "]")

        case x => x.getClass.getSimpleName.stripSuffix("$")
    }

    trait Expr
    case class ESymbol(value : String) extends Expr
    case class EInt(value : Int) extends Expr
    case class EDouble(value : Double) extends Expr
    case class EBool(value : Boolean) extends Expr
    case class EString(value : String) extends Expr
    case object ENone extends Expr
    case class ESome(value : Expr) extends Expr
    case class EList(values : List[Expr]) extends Expr
    case class EPair(pair : (Expr, Expr)) extends Expr
    case class EMap(values : List[EPair]) extends Expr
    case class EApply(f : String, params : List[Expr]) extends Expr

    case class EElderSign(value : Int) extends Expr
    case class EOffer(a : String, b : Int) extends Expr
    case class EUnitRef(a : String, b : String, c : Int) extends Expr

    def space[* : P] = P{ CharsWhileIn(" \r\n", 0) }

    def symbol[* : P] = P{ (CharIn("A-Z") ~ CharsWhileIn("A-Za-z0-9").?).! }.map(ESymbol)

    def number[* : P] = P{ ("-".? ~ CharsWhileIn("0-9")).! }.map(_.toInt).map(EInt)

    def fractional[* : P] = P{ ("-".? ~ CharsWhileIn("0-9") ~ "." ~ CharsWhileIn("0-9")).! }.map(_.toDouble).map(EDouble)

    def string[* : P] = P{ "\"" ~/ CharsWhile(c => c != '\"' && c != '\\', 0).! ~ "\"" }.map(EString)

    def pfalse[* : P] = P{ "false" }.map(_ => EBool(false))

    def ptrue[* : P] = P{ "true" }.map(_ => EBool(true))

    def list[* : P] = P{ "[" ~/ params ~ "]" }.map(EList)

    def map[* : P] = P{ "{" ~/ pairs ~ "}" }.map(EMap)

    def pairs[* : P] = P{ pair.rep(sep = ","./) }.map(_.$)

    def pair[* : P] = P{ expr ~ "->" ~ expr }.map(EPair)

    def some[* : P] = P{ "Some" ~ space ~ "(" ~/ expr ~ ")" }.map(o => ESome(o))

    def none[* : P] = P{ "None" }.map(o => ENone)

    def base[* : P] : P[Expr] = P{ some | none | action | symbol | fractional | number | pfalse | ptrue | list | map | string }

    def main[* : P] : P[Expr] = P{ action | symbol }

    def action[* : P] = P{ space ~ symbol ~ space ~ "(" ~/ space ~ params ~ space ~ ")" ~ space }.map(o => EApply(o._1.value, o._2))

    def params[* : P] = P{ expr.rep(sep = ","./) }.map(_.$)


    def unitref[* : P] = P( symbol ~ "/" ~ symbol ~ "/" ~ number ).map(o => EUnitRef(o._1.value, o._2.value, o._3.value))

    def es[* : P] = P( "$" ~ ("0" | "1" | "2" | "3").! ).map(_.toInt).map(EElderSign)

    def offer[* : P] = P( symbol ~ "->" ~ number ).map(o => EOffer(o._1.value, o._2.value))

    def expr[* : P] : P[Expr] = P{ space ~ ( unitref | es | offer | base ) ~ space }


    def parseAction(ss : String) : Action = {
        val s = ss.replace("&gt;", ">")

        if (s.startsWith("// "))
            CommentAction(s.drop("// ".length))
        else {
            val sss =
                if (s.startsWith("AwakenAction(") && s.split(",").length == 3)
                    s.replace(")", ", -1)")
                else
                if (s.startsWith("MainDoneFertilityAction("))
                    s.replace("MainDoneFertilityAction(", "EndAction(")
                else
                    s

            parse(sss, main(_)) match {
                case Parsed.Success(a, _) => parseExpr(a).asInstanceOf[Action]
                case Parsed.Failure(label, index, extra) => throw new Error(s + "\n" + label + " " + index + " " + extra)
            }
        }
    }

    def parseExpr(e : Expr) : Any = e match {
        case ESymbol(s) =>
            parseFaction(s).map(_.asInstanceOf[Any])
                .orElse(parseRegion(s).map(_.asInstanceOf[Any]))
                .orElse(parseLoyaltyCard(s))
                .orElse(parseSymbol(s))
                .getOrElse(throw new IllegalArgumentException(s"Unknown symbol: $s"))

        case EInt(n) => n
        case EElderSign(v) => ElderSign(v)
        case EBool(b) => b
        case EString(s) => s
        case EOffer(a, b) => Offer(parseFaction(a).get, b)
        case EUnitRef(a, b, c) => UnitRef(parseFaction(a).get, parseSymbol(b).get.asInstanceOf[UnitClass], c)
        case ESome(e) => Some(parseExpr(e))
        case ENone => None
        case EList(l) => l.map(parseExpr)
        case EApply(f, params) => params.none.?(parseSymbol(f).get).|(parseActionConstructor(f, params.num).get.newInstance(params.map(parseExpr) : _*))
    }

    //def parseRegion(s : String) : Option[Region] = g.board.regions.%(_.name.split(" ").mkString("") == s).single
    def parseRegion(s : String) : Option[Region] = {
        val normalized = s.replaceAll(" ", "")
        val allRegions = g.board.regions :+ SL.slumber
        allRegions.find(r => r.name.replaceAll(" ", "") == normalized)
    }
}

object Serialize {
    val factions = $(GC, CC, BG, YS, SL, WW, OW, AN)

    val loyaltyCards = List(GhastCard, GugCard, ShantakCard, StarVampireCard, HighPriestCard, ByatisCard, AbhothCard, DaolothCard, NyogthaCard)

    def parseDifficulty(s : String) : Option[Difficulty] = parseSymbol(s).map(_.asInstanceOf[Difficulty])

    def parseFaction(s : String) : Option[Faction] = factions.%(_.short == s).single

    def parseGameOption(s : String) : Option[GameOption] = GameOptions.all.%(_.toString == s).single

    def parseLoyaltyCard(s : String) : Option[LoyaltyCard] = loyaltyCards.find(_.productPrefix == s)

    def parseSymbol(s : String) : Option[Any] = Reflect.lookupLoadableModuleClass("cws." + s + "$").map(_.loadModule())

    def parseActionConstructor(s : String, n : Int) : Option[InvokableConstructor] = Reflect.lookupInstantiatableClass("cws." + s).toList.flatMap(_.declaredConstructors).%(_.parameterTypes.num == n).single
}
