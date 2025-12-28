package cws

import hrf.colmat._

import Html._

import scala.util._


@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
trait Record extends Product with GoodMatch

sealed abstract class Glyph(val inPlay : Boolean, val onMap : Boolean)

case object Ocean extends Glyph(true, true)
case object GlyphAA extends Glyph(true, true)
case object GlyphOO extends Glyph(true, true)
case object GlyphWW extends Glyph(true, true)
case object NoGlyph extends Glyph(true, true)
case object Pool extends Glyph(false, false)
case object Prison extends Glyph(false, false)
case object Deep extends Glyph(true, false)
case object Slumber extends Glyph(true, false)
case object Sorcery extends Glyph(true, false)
case object Extinct extends Glyph(false, false)

trait GameImplicits {
    implicit def factionToState(f : Faction)(implicit game : Game) : Player = f match {
        case f : NeutralFaction => game.neutrals(f)
        case f : Faction => game.players.get(f).|(game.noPlayer)
    }

    def options(implicit game : Game) = game.options
    def factions(implicit game : Game) = game.factions
    def nfactions(implicit game : Game) = game.nfactions
    def board(implicit game : Game) = game.board


    def log(m : Any*)(implicit game : Game) = game.appendLog(m.$)

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def neutral = f.is[NeutralFaction]
        def real = f.is[NeutralFaction].not
        def enemies = game.nfactions.but(f)
        def factionGOOs = f.all.factionGOOs
        def log(m : Any*) = game.appendLog(f +: m.$)
        def any = game.players.contains(f)
    }

    implicit class UnitFigureEx(u : UnitFigure) {
        def goo = u.uclass.utype == GOO
        def factionGOO = u.uclass.utype == GOO && u.uclass.is[IGOO].not
        def independentGOO = u.uclass.utype == GOO && u.uclass.is[IGOO]
        def monster = u.uclass.utype == Monster
        def monsterly = u.uclass.utype == Monster || u.uclass.utype == Terror
        def terror = u.uclass.utype == Terror
        def cultist = u.uclass.utype == Cultist
        def inPlay = u.region.glyph.inPlay
        def onMap = u.region.glyph.onMap
    }

    implicit class RegionEx(r : Region) {
        def inPlay = r.glyph.inPlay
        def onMap = r.glyph.onMap
    }

    implicit class RegionListEx(l : $[Region]) {
        def inPlay = l.%(_.glyph.inPlay)
        def onMap = l.%(_.glyph.onMap)
        def nex(implicit game : Game) = game.nexed.some./(x => l.%(x.has)).|(l)
    }

    implicit class UnitFigureGameEx(u : UnitFigure)(implicit game : Game) {
        def canMove = u.uclass.canMove(u)
        def canBeMoved = u.uclass.canBeMoved(u)
        def canCapture = u.uclass.canCapture(u)
        def canBattle = u.uclass.canBattle(u)
        def canControlGate = u.uclass.canControlGate(u) && u.health != Pained
    }

    implicit class UnitFigureListEx(l : $[UnitFigure]) {
        def apply(uc : UnitClass) = l.%(_.uclass == uc)
        def not(uc : UnitClass) = l.%(_.uclass != uc)
        def one(uc : UnitClass) = l.%(_.uclass == uc).first
        def one(ut : UnitType) = l.%(_.uclass.utype == ut).first
        def goos = l.%(_.uclass.utype == GOO)
        def factionGOOs = l.%(u => u.uclass.utype == GOO && u.uclass.is[IGOO].not)
        def independentGOOs = l.%(u => u.uclass.utype == GOO && u.uclass.is[IGOO])
        def cultists = l.%(_.uclass.utype == Cultist)
        def vulnerable = l.%(u => u.uclass.utype == Cultist || u.uclass.utype == Monster)
        def monsters = l.%(_.uclass.utype == Monster)
        def monsterly = l.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror)
        def terrors = l.%(_.uclass.utype == Terror)
        def notGOOs = l.%(_.uclass.utype != GOO)
        def notMonsters = l.%(_.uclass.utype != Monster)
        def notTerrors = l.%(_.uclass.utype != Terror)
        def notCultists = l.%(_.uclass.utype != Cultist)
        def onMap = l.%(_.region.glyph.onMap)
        def inPlay = l.%(_.region.glyph.inPlay)
        def nex(implicit game : Game) = game.nexed.some./(x => l.%(u => x.has(u.region))).|(l)
    }

    implicit class UnitFigureListGameEx(l : $[UnitFigure])(implicit game : Game) {
        def sort = l.sortWith(game.compareUnits)
        // def canMove = l.%(_.canMove)
        // def canBeMoved = l.%(_.canBeMoved)
        // def canBattle(r : Region) = l.%(_.uclass.canBattle(r))
        // def canCapture(r : Region) = l.%(_.uclass.canCapture(r))
        // def canControlGate(r : Region) = l.%(_.uclass.canControlGate(r))
        // def canBeSummoned = l.%(_.uclass.canBeSummoned)
        // def canBeRecruited = l.%(_.uclass.canBeRecruited)
    }

    implicit def string2desc(s : String) : Game => String = (g : Game) => s
    implicit def region2desc(r : Region) : Game => String = (g : Game) => r.toString
    implicit def faction2desc(f : Faction) : Game => String = (g : Game) => f.full
    implicit def spellbook2desc(b : Spellbook) : Game => String = (g : Game) => b.full
    implicit def option2desc(n : |[String]) : Game => String = (g : Game) => n.|(null)
    implicit def unitrefshort2desc(ur : UnitRefShort) : Game => String = (g : Game) => g.unit(ur.r).short
    implicit def unitreffull2desc(ur : UnitRefFull) : Game => String = (g : Game) => g.unit(ur.r).toString

    implicit def action2force(fa : ForcedAction) : Continue = Force(fa)

    implicit def unitRefToFigure(ur : UnitRef)(implicit game : Game) : UnitFigure = game.unit(ur)
    implicit def unitRefToFigureEx(ur : UnitRef)(implicit game : Game) : UnitFigureEx = UnitFigureEx(ur)
    implicit def figureToUnitRef(u : UnitFigure)(implicit game : Game) : UnitRef = u.ref
}

case class Region(name : String, glyph : Glyph) extends GoodMatch {
    def elem =
        glyph @@ {
            case Ocean => name.styled("sea")
            case Deep => GC.styled(name)
            case _ => name.styled("region")
        }

    def +(ia : IceAges) = elem + ia.toString

    override def toString = elem
}

trait Board {
    def id : String
    def name : String
    def regions : $[Region]
    def connected(region : Region) : $[Region]
    def starting(faction : Faction) : $[Region]
    def distance(a : Region, b : Region) : Int
    def gateXYO(r : Region) : (Int, Int)
    val nonFactionRegions : $[Region]
    val west : $[Region]
    val east : $[Region]
}

case class ElderSign(value : Int) {
    def short = "[" + value.styled("es") + "]"
}


abstract class LoyaltyCard(val name : String, val doom : Int, val power : Int, val quantity : Int, val cost : Int, val combat : Int, val unit : UnitClass, val icon : UnitClass) {
    def short = name.styled("nt")
}


sealed trait UnitType {
    def name = toString
    def plural = name + "s"
    val priority : Int
}
case object Cultist extends UnitType { val priority = 10 }
case object Monster extends UnitType { val priority = 20 }
case object Terror extends UnitType { val priority = 30 }
case object GOO extends UnitType { val priority = 40 }
case object Token extends UnitType { val priority = 2 }
case object Building extends UnitType { val priority = 5 }

trait IGOO

abstract class UnitClass(val name : String, val utype : UnitType, val cost : Int) extends Record {
    def plural = name + "s"
    val priority = utype.priority * 1_00_00_00 + cost * 1_00 + this.is[NeutralMonster].??(1_00_00)

    def canMove(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = utype == Cultist
    def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = utype == Monster
}

case object Acolyte extends UnitClass("Acolyte", Cultist, 1) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(r : UnitFigure)(implicit game : Game) : Boolean = true
}

abstract class FactionUnitClass(val faction : Faction, name : String, utype : UnitType, cost : Int) extends UnitClass(name, utype, cost) {
    def elem = faction.styled(name)
    override def toString = elem
}

sealed class UnitState(val text : String) extends Ordered[UnitState]  {
    def elem = text
    override def toString = elem

    def compare(that : UnitState) = text.compare(that.text)
}

case object Moved extends UnitState("moved")
case object Retreated extends UnitState("retreated")
case object Absorbed extends UnitState("absorbed")
case object Harbinged extends UnitState("harbinged")
case object Invised extends UnitState("invised")
case object Hidden extends UnitState("hidden")
case object Zeroed extends UnitState("zeroed")
case object MovedForFree extends UnitState("moved-for-free")
case object MovedForDouble extends UnitState("moved-for-double")

class UnitFigure(val faction : Faction, val uclass : UnitClass, val index : Int, var region : Region, var state : $[UnitState] = $, var health : UnitHealth = Alive) {
    override def toString = short

    def dbg = faction.short + "/" + uclass.name + "/" + index

    def short = faction.styled(uclass.name)

    def full = faction.styled(uclass.name) + state.some./(_.mkString(" (", "/", ")")).|("") + (health != Alive && health != DoubleHP(Alive, Alive)).??(" (" + health + ")")

    def has(s : UnitState) = state.contains(s)
    def add(s : UnitState) { state :+= s }
    def add(l : $[UnitState]) { state ++= l }
    def remove(s : UnitState) { state = state.but(s) }
    def count(s : UnitState) = state.count(_ == s)
    def ref = UnitRef(faction, uclass, index)
}

case class UnitRef(faction : Faction, uclass : UnitClass, index : Int) {
    def short = faction.styled(uclass)
    def full = UnitRefFull(this)
}

case class UnitRefShort(r : UnitRef)
case class UnitRefFull(r : UnitRef)

sealed abstract class Spellbook(val name : String) extends Record {
    def full : String
    override def toString = full
}

abstract class NeutralSpellbook(name : String) extends Spellbook(name) {
    override def full = name.styled("nt")
}

abstract class FactionSpellbook(val faction : Faction, name : String) extends Spellbook(name) {
    override def full = faction.styled(name)
}

abstract class Requirement(val text : String, val es : Int = 0)

trait Faction {
    def name : String
    def short : String
    def style : String
    def abbr : String = style.toUpperCase.styled(style)
    def styled(s : String) = s.styled(style)
    def styled(ut : UnitType) = ut.name.styled(style)
    def styled(uc : UnitClass) = uc.name.styled(style)
    def styled(sb : Spellbook) = sb.name.styled(style)
    def reserve : Region
    def prison : Region

    def full = name.styled(style, "inline-block")
    def ss = short.styled(style)

    override def toString = full

    def allUnits : $[UnitClass]
    def abilities : $[Spellbook]
    def library : $[Spellbook]
    def requirements(options : $[GameOption]) : $[Requirement]
    def recruitCost(u : UnitClass, r : Region)(implicit game : Game) = u.cost
    def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u.cost
    def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None
    def awakenDesc(u : UnitClass) : |[String] = None
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int
    def neutralStrength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) =
        units(Ghast).num * 0 +
        units(Gug).num * 3 +
        units(Shantak).num * 2 +
        units(StarVampire).num * 1 +
        units(Byatis).%!(_.has(Zeroed)).num * 4 +
        units(Abhoth).%!(_.has(Zeroed)).num * this.all(Filth).num +
        units(Daoloth).%!(_.has(Zeroed)).num * 0 +
        units(Nyogtha).%!(_.has(Zeroed)).num * game.battle./(_.attacker).has(this).?(4).|(1)
}

object NoFaction extends Faction {
    def name = "No Faction"
    def short = "NF"
    def style = "nt"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def abilities = $
    def library = $
    def requirements(options : $[GameOption]) = $

    val allUnits = $

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) = 0
}

trait Action extends Record {
    def question(implicit game : Game) : String
    def safeQ(implicit game : Game) = question(game)
    def option(implicit game : Game) : String
    def unwrap : Action = this

    def isMore : Boolean = this match {
        case a : Wrapped => unwrap.isMore
        case a : More => true
        case _ => false
    }

    def isCancel : Boolean = this match {
        case a : Wrapped => unwrap.isCancel
        case a : Cancel => true
        case _ => false
    }

    def isSoft : Boolean = this match {
        case a : Wrapped => unwrap.isSoft
        case a : Soft => true
        case _ => false
    }

    def isInfo : Boolean = this match {
        case a : Wrapped => unwrap.isInfo
        case a : Info => true
        case _ => false
    }

    def isVoid : Boolean = this match {
        case a : Wrapped => unwrap.isVoid
        case a : Void => true
        case _ => false
    }

    def isOutOfTurn : Boolean = this match {
        case a : Wrapped => unwrap.isOutOfTurn
        case a : OutOfTurn => true
        case _ => false
    }

    def isRecorded = (isMore || isCancel || isSoft || isVoid).not
}

trait FactionAction extends Action {
    def self : Faction
}

trait Soft extends Action
trait Cancel extends Action
trait Info extends Action
trait More extends Soft
trait PowerNeutral extends Action
trait Wrapped extends Action

trait Continue

case class Ask(faction : Faction, actions : $[Action] = $) extends Continue {
    def add(a : Action) = Ask(faction, actions :+ a)
    def when(c : Boolean)(a : Action) = c.?(add(a)).|(this)
    def list(l : $[Action]) = Ask(faction, actions ++ l)
    def prepend(a : Action) = Ask(faction, a +: actions)
    def each[T](l : IterableOnce[T])(a : T => Action) = list(l.iterator.map(a).$)
    def each[T, U](l : IterableOnce[(T, U)])(a : (T, U) => Action) = list(l.iterator.map { case (t, u) => a(t, u) }.$)
    def some[T](l : IterableOnce[T])(a : T => IterableOnce[Action]) = list(l.iterator.flatMap(a).$)
    def group(t : Any*) = add(GroupAction(t.$.but("").mkString(" ")))
    def done(a : ForcedAction) = add(a.as("Done"))
    def skip(a : ForcedAction) = add(a.as("Skip"))
    def skipIf(c : Boolean)(a : ForcedAction) = c.?(add(a.as("Skip"))).|(this)
    def cancel = add(CancelAction)
    def cancelIf(c : Boolean) = c.?(add(CancelAction)).|(this)
}

case object StartContinue extends Continue
case class Force(action : Action) extends Continue
case class DelayedContinue(delay : Int, continue : Continue) extends Continue
case class RollD6(question : Game => String, roll : Int => ForcedAction) extends Continue
case class RollBattle(question : Game => String, n : Int, roll : $[BattleRoll] => ForcedAction) extends Continue
case class DrawES(question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => ForcedAction) extends Continue
case class GameOver(winners : $[Faction]) extends Continue
case object UnknownContinue extends Continue
case object TryAgain extends Continue


object RollBattle {
    def apply(faction : Faction, side : String, n : Int, roll : $[BattleRoll] => ForcedAction) : RollBattle = RollBattle("" + faction + " rolls " + (n == 0).?(" no dice").|((n == 1).?(" one die").|("" + n + " dice")) + " for " + side, n, roll)
}

trait SelfPerform { self : Action =>
    def perform(soft : VoidGuard)(implicit game : Game) : Continue
}

abstract class ForcedAction extends Action {
    def question(implicit game : Game) = "N/A"
    def option(implicit game : Game) = "N/A"
    def as(o : Any*) = new WrappedForcedAction(this, o.$)
}

case object CancelAction extends Action with Cancel {
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = "Cancel"
}

case class GroupAction(t : String) extends Action with Info {
    def question(implicit game : Game) = t
    def option(implicit game : Game) = ""
}


case class WrappedForcedAction(action : ForcedAction, o : $[Any]) extends Action with Wrapped {
    override def unwrap = action.unwrap
    def apply(q : Any*) = new WrappedQForcedAction(action, q.$, o)
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = o.but("").mkString(" ")
}

case class WrappedQForcedAction(action : ForcedAction, q : $[Any], o : $[Any]) extends Action with Wrapped {
    override def unwrap = action.unwrap
    def question(implicit game : Game) = q.but("").mkString(" ")
    def option(implicit game : Game) = o.but("").mkString(" ")
}

trait Void { self : ForcedAction => }
trait OutOfTurn { self : Action => }
trait OutOfTurnReturn extends Soft { self : Action => }


case object ReloadAction extends ForcedAction with Void
case object UpdateAction extends ForcedAction with Void
case class CommentAction(comment : String) extends ForcedAction with Void


class Player(private val f : Faction)(implicit game : Game) {
    var gates : $[Region] = $
    var cathedrals : $[Region] = $

    var spellbooks : $[Spellbook] = $
    var upgrades : $[Spellbook] = $
    var borrowed : $[Spellbook] = $

    var oncePerBattle : $[Spellbook] = $
    var oncePerAction : $[Spellbook] = $
    var oncePerRound : $[Spellbook] = $
    var oncePerTurn : $[Spellbook] = $
    var oncePerGame : $[Spellbook] = $

    var ignorePerInstant : $[Spellbook] = $
    var ignorePerTurn : $[Spellbook] = $

    var ignorePerGame : $[Spellbook] = $
    var ignorePerGameNew : $[Spellbook] = $

    var ignoreOptions : $[IgnoreOption] = $
    var ignoreOptionsNew : $[IgnoreOption] = $

    var unfulfilled : $[Requirement] = f.requirements(game.options)

    var power : Int = 8
    var doom : Int = 0
    var es : $[ElderSign] = $
    var revealed : $[ElderSign] = $
    var loyaltyCards : $[LoyaltyCard] = $
    var hired : Boolean = false

    var battled : $[Region] = $
    var acted : Boolean = false

    var units : $[UnitFigure] = f.allUnits.indexed./((uc, i) => new UnitFigure(f, uc, f.allUnits.take(i).count(uc) + 1, f.reserve))

    var hibernating = false
    var iceAge : |[Region] = None
    var unitGate : |[UnitFigure] = None

    var ignoredSacrificeHighPriest = false

    def active = power > 0 && !hibernating
    def allGates = gates ++ unitGate./(_.region).$
    def needs(rq : Requirement) = unfulfilled.contains(rq)
    def has(sb : Spellbook) = f.abilities.contains(sb) || spellbooks.contains(sb) || upgrades.contains(sb) || borrowed.contains(sb)
    def used(sb : Spellbook) = oncePerGame.contains(sb) || oncePerTurn.contains(sb) || oncePerRound.contains(sb) || oncePerAction.contains(sb)
    def can(sb : Spellbook) = has(sb) && !used(sb)
    def ignored(sb : Spellbook) = ignorePerGame.contains(sb) || ignorePerTurn.contains(sb) || ignorePerInstant.contains(sb)
    def option(io : IgnoreOption) = ignoreOptions.contains(io)
    def want(sb : Spellbook) = can(sb) && !ignored(sb)
    def hasAllSB = unfulfilled.none
    def unclaimedSB = f.library.num - spellbooks.num - unfulfilled.num
    def present(region : Region) = units.exists(_.region == region)
    def at(region : Region) = units.%(_.region == region)
    def at(region : Region, uclass : UnitClass) = units.%(_.region == region).%(_.uclass == uclass)
    def at(region : Region, utype : UnitType) = units.%(_.region == region).%(_.uclass.utype == utype)
    def at(region : Region, utype : UnitType, utype2 : UnitType) = units.%(_.region == region).%(u => u.uclass.utype == utype || u.uclass.utype == utype2)
    def pool = units.%(_.region == f.reserve)
    def onMap = units.%(_.region.glyph.onMap)
    def all = units.%(_.region.glyph.inPlay)
    def goos = units.%(_.region.glyph.inPlay).%(_.uclass.utype == GOO)
    def goo(uclass : UnitClass) = all(uclass).single.get
    def has(uclass : UnitClass) = all(uclass).any

    def satisfy(rq : Requirement, text : String, es : Int = 0) {
        if (f.needs(rq)) {
            f.unfulfilled = f.unfulfilled.but(rq)
            f.log("achieved", f.styled(text), ((es + rq.es) > 0).??("and gained " + (es + rq.es).es))
            f.takeES(es + rq.es)
        }
    }

    def satisfyIf(rq : Requirement, text : String, c : => Boolean, p : Int = 0) {
        if (f.needs(rq)) {
            if (c) {
                f.satisfy(rq, text, 0)
                if (p != 0) {
                    f.power += p
                    f.log("got", p.power)
                }
            }
        }
    }

    def takeES(n : Int) {
        val total = factions./(f => f.es.num + f.revealed.num).sum
        var count = n

        if (total + count > 36) {
            f.log("got", (total + count - 36).doom, "instead of", (total + count - 36).es)
            f.doom += (total + count - 36)
            count = 36 - total
        }

        f.es ++= count.times(ElderSign(0))
    }

    def place(uc : UnitClass, r : Region) {
        val u = f.pool(uc).first
        u.region = r
    }

    def canAccessGate(r : Region) = gates.contains(r) || f.unitGate.?(_.region == r) || (has(TheyBreakThrough) && game.gates.contains(r))

    def canAttack(r : Region)(e : Faction) = e.at(r).any && f.strength(at(r), e) > 0 && at(r).%(_.canBattle).any

    def canCapture(r : Region)(e : Faction) : Boolean =
        if (f == e)
            false
        else
        if (e.at(r, Cultist).none)
            false
        else
        if (e.at(r, GOO).any)
            false
        else
        if (f.at(r, GOO).any)
            true
        else
        if (e.at(r, Terror).any)
            false
        else
        if (e.at(r, Monster).any)
            e.at(r, Monster).none
        else
        if (e.has(Ferox) && e.has(Ithaqua))
            false
        else
        if (f.at(r, Terror).any)
            true
        else {
            f.at(r, Monster).%(u => u.uclass.canCapture(u)).any
    }

    def summonRegions : $[Region] = (f.allGates.onMap ++ f.has(TheyBreakThrough).??(f.enemies./~(_.allGates) ++ game.abandonedGates)).nex.distinct

    def iced(r : Region) : IceAges = {
        if (game.anyIceAge.not)
            NoIceAges
        else {
            val movedHere = f.at(r).%(_.has(Moved)).any
            // Show Ice Age unless explicitly moved here this turn *and* it's a normal (paid) move.
            if (movedHere && !f.oncePerAction.contains(FromBelow))
                NoIceAges
            else
                IceAges(factions.%(_.iceAge./(_ == r).|(false)).but(f))
        }
    }

    def taxIn(r : Region) : Int = f.iced(r).tax

    def affords(n : Int)(r : Region) = f.power >= f.taxIn(r) + n

    def payTax(r : Region) : Int = {
        val s = iced(r)
        if (s.any) {
            f.power -= s.tax
            f.log("lost", s.tax.power, "due to", s.list./(_.styled(IceAge)).mkString(", "))
            s.tax
        }
        else
            0
    }
}

object RitualTrack {
    val for3 = 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 999
    val for4 = 5 :: 6 :: 7 :: 7 :: 8 :: 8 :: 9 :: 10 :: 999
    val for5 = 5 :: 6 :: 6 :: 7 :: 7 :: 8 :: 8 :: 9 :: 9 :: 10 :: 999
}

case class IceAges(list : $[Faction]) {
    def any = list.any
    def tax = list.num
    def elem = list./(" (" + _.styled(IceAge) + ")").mkString("")
    override def toString = elem
}

object NoIceAges extends IceAges($)


sealed trait GameOption extends Record

case object GateDiplomacy extends GameOption
case object AsyncActions extends GameOption

case object HighPriests extends GameOption
case object NeutralSpellbooks extends GameOption
case object NeutralMonsters extends GameOption
case object IGOOs extends GameOption

case object IceAgeAffectsLethargy extends GameOption
case object Opener4P10Gates extends GameOption
case object DemandTsathoggua extends GameOption


trait MapOption extends GameOption
case object MapEarth33 extends MapOption
case object MapEarth35 extends MapOption
case object MapEarth53 extends MapOption
case object MapEarth55 extends MapOption

abstract class LoyaltyCardGameOption(val lc : LoyaltyCard) extends GameOption

sealed trait NeutralMonsterOption extends LoyaltyCardGameOption
case object UseGhast extends LoyaltyCardGameOption(GhastCard) with NeutralMonsterOption
case object UseGug extends LoyaltyCardGameOption(GugCard) with NeutralMonsterOption
case object UseShantak extends LoyaltyCardGameOption(ShantakCard) with NeutralMonsterOption
case object UseStarVampire extends LoyaltyCardGameOption(StarVampireCard) with NeutralMonsterOption

sealed trait IGOOOption extends LoyaltyCardGameOption
case object UseByatis extends LoyaltyCardGameOption(ByatisCard) with IGOOOption
case object UseAbhoth extends LoyaltyCardGameOption(AbhothCard) with IGOOOption
case object UseDaoloth extends LoyaltyCardGameOption(DaolothCard) with IGOOOption
case object UseNyogtha extends LoyaltyCardGameOption(NyogthaCard) with IGOOOption

case class PlayerCount(n : Int) extends GameOption

object GameOptions {
    val all = $(
        HighPriests,
        NeutralSpellbooks,
        NeutralMonsters,
        IGOOs,
        IceAgeAffectsLethargy,
        Opener4P10Gates,
        DemandTsathoggua,
        MapEarth33,
        MapEarth35,
        MapEarth53,
        MapEarth55,
        UseGhast,
        UseGug,
        UseShantak,
        UseStarVampire,
        UseByatis,
        UseAbhoth,
        UseDaoloth,
        UseNyogtha,
        PlayerCount(3),
        PlayerCount(4),
        PlayerCount(5),
    )
}

trait Expansion {
    def perform(a : Action, soft : VoidGuard)(implicit game : Game) : Continue

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}


case object StartAction extends ForcedAction
case object SetupFactionsAction extends ForcedAction
case class CheckSpellbooksAction(then : Action) extends ForcedAction
case object AfterPowerGatherAction extends ForcedAction
case object FirstPlayerDeterminationAction extends ForcedAction
case object PlayOrderAction extends ForcedAction
case class PowerGatherAction(next : Faction) extends ForcedAction
case object DoomPhaseAction extends ForcedAction
case object ActionPhaseAction extends ForcedAction
case object GameOverPhaseAction extends ForcedAction

abstract class BaseFactionAction(private val q : Game => String, private val o : Game => String) extends FactionAction {
    def question(implicit game : Game) = q(game)
    def option(implicit game : Game) = o(game)
}
abstract class OptionFactionAction(private val o: Game => String) extends FactionAction {
    def option(implicit game : Game) = o(game)
}

case class StartingRegionAction(self : Faction, r : Region) extends ForcedAction
case class FirstPlayerAction(self : Faction, f : Faction) extends BaseFactionAction("First player", f)
case class PlayDirectionAction(self : Faction, order : $[Faction]) extends BaseFactionAction("Order of play", order.mkString(" > "))

trait MainQuestion extends FactionAction {
    def question(implicit game : Game) = game.nexed.some./(n => "" + EnergyNexus + " in " + n.mkString(", ")).|("" + self + " action") + " (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
    override def safeQ(implicit game : Game) = self @@ {
        case f if game.nexed.any => "" + EnergyNexus + " in " + game.nexed.mkString(", ")
        case f if f.acted => "Unlimited actions"
        case f => "Main action"
    }
}

trait DoomQuestion extends FactionAction {
    def question(implicit game : Game) = "" + self + " doom phase (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
    override def safeQ(implicit game : Game) = "" + self + " doom phase"
}

case class SpellbookAction(self : Faction, sb : Spellbook, next : Action) extends BaseFactionAction(implicit g => (self.unclaimedSB == 1).?("Receive spellbook").|("Receive " + self.unclaimedSB + " spellbooks"), {
    val p = s""""${self.short}", "${sb.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    val qm = Overlays.imageSource("question-mark")
    "<div class=sbdiv>" +
        sb.full +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
})

case class ElderSignAction(f : Faction, n : Int, value : Int, public : Boolean, next : Action) extends ForcedAction

case object OutOfTurnDoneAction extends ForcedAction with OutOfTurnReturn
case class OutOfTurnCancelAction(self : Faction) extends BaseFactionAction("&nbsp;", "Cancel") with Cancel with OutOfTurnReturn

case class DoomAction(faction : Faction) extends ForcedAction
case class DoomNextPlayerAction(faction : Faction) extends ForcedAction
case class DoomDoneAction(self : Faction) extends BaseFactionAction("&nbsp;", "Done".styled("power")) with PowerNeutral

case class MainAction(faction : Faction) extends ForcedAction
case class MainNextPlayerAction(faction : Faction) extends ForcedAction
case object ProceedBattlesAction extends ForcedAction
case class MainDoneAction(self : Faction) extends BaseFactionAction("&nbsp;", "Done".styled("power")) with PowerNeutral

case class EndAction(self : Faction) extends ForcedAction
case class AfterAction(self : Faction) extends ForcedAction

case class RitualAction(self : Faction, cost : Int, k : Int) extends OptionFactionAction(g => "Perform " + "Ritual of Annihilation".styled("doom") + " for " + cost.power) with DoomQuestion

case class RevealESDoomAction(self : Faction) extends OptionFactionAction("View " + "Elder Signs".styled("es")) with DoomQuestion with Soft with PowerNeutral
case class RevealESMainAction(self : Faction) extends OptionFactionAction("View " + "Elder Signs".styled("es")) with MainQuestion with Soft with PowerNeutral
case class RevealESOutOfTurnAction(self : Faction) extends BaseFactionAction("Elder Signs", "View " + "Elder Signs".styled("es")) with Soft
case class RevealESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(implicit g => "Elder Signs".styled("es") + " " + self.es./(_.short).mkString(" "), (es.num == 1).?("Reveal " + es(0).short).|("Reveal all for " + es./(_.value).sum.doom)) with OutOfTurn

case class PassAction(self : Faction) extends OptionFactionAction("Pass and lose remaining power") with MainQuestion

case class MoveMainAction(self : Faction) extends OptionFactionAction("Move") with MainQuestion with Soft
case class MoveContinueAction(self : Faction, moved : Boolean) extends ForcedAction with Soft
case class MoveSelectAction(self : Faction, uc : UnitClass, r : Region) extends ForcedAction with Soft
case class MoveAction(self : Faction, uc : UnitClass, from : Region, to : Region) extends ForcedAction
case class MoveDoneAction(self : Faction) extends ForcedAction

case class AttackMainAction(self : Faction, l : $[Region], effect : |[Spellbook]) extends OptionFactionAction("Battle") with MainQuestion with Soft
case class AttackAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends BaseFactionAction(implicit g => "Battle in " + r  + effect./(" with " + _).|("") + self.iced(r), f)

case class BuildGateMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Build Gate") with MainQuestion with Soft
case class BuildGateAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Build gate" + g.forNPowerWithTax(r, self, 3 - self.has(UmrAtTawil).??(1)) + " in", r)

case class CaptureMainAction(self : Faction, l : $[Region], effect : |[Spellbook]) extends OptionFactionAction("Capture") with MainQuestion with Soft
case class CaptureAction(self : Faction, r : Region, f : Faction, uc : UnitClass, effect : |[Spellbook]) extends BaseFactionAction(implicit g => "Capture" + g.for1PowerWithTax(r, self) + " in " + r + self.iced(r), g => f.styled(uc))

case class RecruitMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Recruit " + self.styled(uc)) with MainQuestion with Soft
case class RecruitAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Recruit " + self.styled(uc) + g.forNPowerWithTax(r, self, self.recruitCost(uc, r)) + " in", implicit g => r + self.iced(r))

case class SummonMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Summon " + self.styled(uc)) with MainQuestion with Soft
case class SummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Summon " + self.styled(uc) + g.forNPowerWithTax(r, self, self.summonCost(uc, r)) + " in", implicit g => r + self.iced(r))
case class ContinueFreeSummonAction(self : Faction, uc : UnitClass, l : $[Region]) extends ForcedAction with Soft
case class FreeSummonAction(self : Faction, uc : UnitClass, r : Region, l : $[Region]) extends BaseFactionAction(g => "Summon " + self.styled(uc) + " for free in", implicit g => r + self.iced(r))

case class AwakenMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Awaken " + self.styled(uc)) with MainQuestion with Soft
case class AwakenAction(self : Faction, uc : UnitClass, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + self.styled(uc) + g.forNPowerWithTax(r, self, cost) + " in", implicit g => r + self.iced(r))
case class AwakenEliminate2CultistsAction(self : Faction, uc : UnitClass, l : $[Region], a : Region, b : Region) extends BaseFactionAction("Eliminate two " + self.styled(Cultist.plural) + " to awaken " + self.styled(uc), (a == b).?("Two from " + a)|("From " + a + " and " + b))

case class Offer(f : Faction, n : Int)

case class SacrificeHighPriestDoomAction(self : Faction) extends OptionFactionAction("Sacrifice " + self.styled("High Priest")) with DoomQuestion with Soft with PowerNeutral
case class SacrificeHighPriestMainAction(self : Faction, then : Action) extends OptionFactionAction("Sacrifice " + self.styled("High Priest")) with MainQuestion with Soft
case class SacrificeHighPriestAction(self : Faction, r : Region, then : Action) extends BaseFactionAction("Sacrifice to gain " + 2.power, self.styled(HighPriest) + " in " + r)
case class SacrificeHighPriestDoneAction(self : Faction, then : Action) extends BaseFactionAction(None, "Done".styled("power"))


case class ToggleUnlimitedBattleAction(self : Faction, value : UnlimitedBattleOption) extends BaseFactionAction("&nbsp;&nbsp;", "Unlimited Battle: " + value.label) with Cancel
case class ToggleUnlimitedSummonAction(self : Faction, value : UnlimitedSummonOption) extends BaseFactionAction("&nbsp;&nbsp;", "Unlimited Summon: " + value.label) with Cancel
case class ToggleOutOfTurnDevolveAction(self : Faction, value : OutOfTurnDevolveOption) extends BaseFactionAction("&nbsp;&nbsp;", "Out of turn " + self.styled("Devolve") + ": " + value.label) with Cancel
case class ToggleOutOfTurnSacrificeHighPriestAction(self : Faction, value : OutOfTurnSacrificeHighPriestOption) extends BaseFactionAction("&nbsp;&nbsp;", "Out of turn sacrifice " + self.styled("High Priest") + ": " + value.label) with Cancel

trait IgnoreOption

sealed abstract class UnlimitedBattleOption(val label : String) extends IgnoreOption
case object UnlimitedBattleOff extends UnlimitedBattleOption("Off")
case object UnlimitedBattleOn extends UnlimitedBattleOption("On".hl)
case object UnlimitedBattleOnlyWithGOO extends UnlimitedBattleOption("Only with " + "GOO".hl)

sealed abstract class OutOfTurnDevolveOption(val label : String) extends IgnoreOption
case object OutOfTurnDevolveOff extends OutOfTurnDevolveOption("Off")
case object OutOfTurnDevolveOn extends OutOfTurnDevolveOption("On".hl)
case object OutOfTurnDevolveAvoidCapture extends OutOfTurnDevolveOption("Avoid " + "Capture".hl)

sealed abstract class OutOfTurnSacrificeHighPriestOption(val label : String) extends IgnoreOption
case object OutOfTurnSacrificeHighPriestOff extends OutOfTurnSacrificeHighPriestOption("Off")
case object OutOfTurnSacrificeHighPriestOn extends OutOfTurnSacrificeHighPriestOption("On".hl)
case object OutOfTurnSacrificeHighPriestAvoidCapture extends OutOfTurnSacrificeHighPriestOption("Avoid " + "Capture".hl)

sealed abstract class UnlimitedSummonOption(val label : String) extends IgnoreOption
case object UnlimitedSummonOff extends UnlimitedSummonOption("Off")
case object UnlimitedSummonOn extends UnlimitedSummonOption("On".hl)
case object UnlimitedSummonEnemyGOO extends UnlimitedSummonOption("Enemy " + "GOO".hl)


class Game(val board : Board, val ritualTrack : $[Int], val factions : $[Faction], val logging : Boolean, val providedOptions : $[GameOption]) extends Expansion {
    private implicit val game : Game = this

    val options = providedOptions ++ $(PlayerCount(factions.num))

    var expansions : $[Expansion] =
        factions./~ {
            case GC => $(GCExpansion)
            case CC => $(CCExpansion)
            case BG => $(BGExpansion)
            case YS => $(YSExpansion)
            case SL => $(SLExpansion)
            case WW => $(WWExpansion)
            case OW => $(OWExpansion)
            case AN => $(ANExpansion)
        } ++
        options.has(NeutralSpellbooks).$(NeutralSpellbooksExpansion) ++
        options.of[NeutralMonsterOption].any.$(NeutralMonstersExpansion) ++
        options.of[IGOOOption].any.$(IGOOsExpansion) ++
        $(this)

    val players = factions./(f => f -> new Player(f)).toMap

    val noPlayer = new Player(NoFaction)

    var neutrals : Map[NeutralFaction, Player] = Map()
    def nfactions = factions ++ neutrals.keys

    var starting = Map[Faction, Region]()
    var turn = 1
    var round = 1
    var doomPhase = false
    var order : $[Faction] = $
    var starter : Faction = factions(0)
    var gates : $[Region] = $
    def unitGates = factions./~(_.unitGate)./(_.region)
    var cathedrals : $[Region] = $
    var desecrated : $[Region] = $
    var ritualMarker = 0
    var battle : |[Battle] = None
    var nexed : $[Region] = $
    var queue : $[Battle] = $
    var anyIceAge : Boolean = false
    var neutralSpellbooks : $[Spellbook] = options.contains(NeutralSpellbooks).$(MaoCeremony, Recriminations, Shriveling, StarsAreRight, UmrAtTawil, Undimensioned)
    var loyaltyCards : $[LoyaltyCard] = options.of[LoyaltyCardGameOption]./(_.lc)

    // Solution for keeping track of use cases for dematerialization, for the AN bot.
    var demCaseMap : Map[Region, Int] = board.regions.map(r => r -> 0).toMap

    def forNPowerWithTax(r : Region, f : Faction, n : Int) : String = { val p = n + f.taxIn(r) ; " for " + p.power }
    def for1PowerWithTax(r : Region, f : Faction) : String = { val p = 1 + f.taxIn(r) ; if (p != 1) " for " + p.power else "" }

    def unit(ur : UnitRef) = ur.faction.units.%(u => u.uclass == ur.uclass && u.index == ur.index).only

    /*
    def clone(logging : Boolean) = {
        val g = new Game(board, ritualTrack, factions, logging, options)
        g.starting = starting
        g.turn = turn
        g.round = round
        g.order = order
        g.first = first
        g.gates = gates
        g.cathedrals = cathedrals
        g.desecrated = desecrated
        g.battled = battled
        g.acted = acted
        g.reveal = reveal
        g.ritualMarker = ritualMarker
        g.battle = battle // TODO clone
        g.nexus = nexus
        g.nexusExtra = nexusExtra
        g.anyIceAge = anyIceAge
        g.neutralSpellbooks = neutralSpellbooks
        g.loyaltyCards = loyaltyCards

        (factions ++ neutrals.keys).foreach { f =>
            val o = f.as[NeutralFaction]./(f => neutrals(f)).|(players(f))

            val p = f.as[NeutralFaction]./ { f =>
                val state = new Player(f)(options)
                g.neutrals += f -> state
                state
            }.|(g.players(f))

            p.gates = o.gates
            p.spellbooks = o.spellbooks
            p.borrowed = o.borrowed
            p.oncePerAction = o.oncePerAction
            p.oncePerRound = o.oncePerRound
            p.oncePerTurn = o.oncePerTurn
            p.oncePerGame = o.oncePerGame
            p.ignorePerInstant = o.ignorePerInstant
            p.ignorePerTurn = o.ignorePerTurn
            p.ignorePerGame = o.ignorePerGame
            p.unfulfilled = o.unfulfilled
            p.power = o.power
            p.doom = o.doom
            p.es = o.es
            p.revealed = o.revealed
            p.units = o.units.map(u => new UnitFigure(u.faction, u.uclass, u.index, u.region, u.state, u.health))
            p.hibernating = o.hibernating
            p.iceAge = o.iceAge
            p.unitGate = o.unitGate
            p.loyaltyCards = o.loyaltyCards
            p.hired = o.hired
        }

        g
    }
    */

    var mlog : $[String] = $

    def appendLog(s : $[Any]) {
        if (logging)
            mlog = s.mkString(" ") +: mlog
    }

    def ritualCost = min(10, ritualTrack(ritualMarker))

    def abandonedGates = gates.%(g => factions.%(_.gates.contains(g)).none)

    def capture(f : Faction, u : UnitFigure) {
        eliminate(u)
        u.region = f.prison
    }

    def eliminate(u : UnitFigure) {
        val f = u.faction

        if (u.uclass.utype == Cultist && f.has(Passion) && u.region.glyph.onMap)
            f.oncePerAction :+= Passion

        if (u.uclass.utype == GOO) {
            if (f.all(u.uclass).num <= 1) {
                factions.foreach { e =>
                    if (e.has(Daoloth) && e.upgrades.has(Interdimensional).not) {
                        e.upgrades :+= Interdimensional

                        e.log("gained", e.styled(Interdimensional), "for", e.styled(Daoloth))
                    }
                }
            }
        }

        u.uclass @@ {
            case Byatis =>
                f.units :-= u
                f.upgrades :-= GodOfForgetfulness
                f.loyaltyCards :-= ByatisCard

                loyaltyCards :+= ByatisCard

            case Abhoth =>
                f.units :-= u
                f.upgrades :-= TheBrood
                f.loyaltyCards :-= AbhothCard

                loyaltyCards :+= AbhothCard

                f.oncePerAction :+= LostAbhoth

            case Daoloth =>
                f.units :-= u
                f.upgrades :-= CosmicUnity
                f.upgrades :-= Interdimensional
                f.loyaltyCards :-= DaolothCard

                loyaltyCards :+= DaolothCard

            case Nyogtha if f.all(Nyogtha).but(u).any =>
                u.region = f.reserve
                f.oncePerAction :+= NyogthaMourning

            case Nyogtha =>
                f.units = f.units.%!(_.uclass == Nyogtha)
                f.upgrades :-= FromBelow
                f.upgrades :-= NightmareWeb
                f.loyaltyCards :-= NyogthaCard

                loyaltyCards :+= NyogthaCard

            case Yothan if f.has(Extinction) =>
                u.region = AN.extinct
                log(f.styled(Yothan), "was removed from the game permanently due to", f.styled(Extinction))

            case YogSothoth =>
                f.unitGate = None
                u.region = f.reserve

            case _ =>
                u.region = f.reserve
        }

        u.state = $
        u.health = Alive
    }

    def compareUnits(a : UnitFigure, b : UnitFigure) : Boolean = {
        if (a.uclass.priority != b.uclass.priority)
            a.uclass.priority > b.uclass.priority
        else
            board.regions.indexOf(a.region) < board.regions.indexOf(b.region)
    }

    def compareUnitClasses(a : UnitClass, b : UnitClass) : Int = {
        if (a == b)
            0
        else
        if (a.utype != b.utype)
            a.utype.priority - b.utype.priority
        else
            a.priority - b.priority
    }

    def regionStatus(r : Region) : $[String] = {
        val gate = gates.contains(r)
        val cathedral = cathedrals.contains(r)
        val ds = desecrated.contains(r)
        val controler = factions.%(f => f.gates.contains(r)).single
        val keeper = controler./~(f => f.at(r).%(_.health == Alive).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && f.has(RedSign))).starting)
        val others = factions.%(f => !f.gates.contains(r)).%(_.at(r).num > 0).sortBy(f => f.strength(f.at(r), f))
        if (gate || !others.none || ds) {
            $("" + r + ":" + gate.??(" " + keeper./(u => ("[[[".styled(u.faction.style) + " " + u + " " + "]]]".styled(u.faction.style))).|("[[[ GATE ]]]".styled("power"))) + ds.??(" " + YS.styled(")|("))) ++
            controler./(f => "    " + f.at(r).diff(keeper.$)./(u => u.short).mkString(", ")).$ ++
            others.sortBy(_.units.%(_.region == r).num)./ { f =>  "    " + f.at(r)./(u => u.short).mkString(", ") } ++ $("&nbsp;")
        }
        else
            $
    }

    def targetDragonAscending(f : Faction) = (f.power > f.enemies./(_.power).max).??(f.enemies.%(_.want(DragonAscending)))

    def showROAT() {
        def vv(v : Int) = (v == 999).?("Instant Death").|(v)
        log("Ritual of Annihilation".styled("doom"), "track", ritualTrack.zipWithIndex./{ case (v, n) => (n == ritualMarker).?(("[" + vv(v) + "]").styled("str")).|("[".styled("xxxhighlight") + vv(v) + "]".styled("xxxhighlight")) }.mkString("-".styled("highlight")))
    }

    def checkGatesLost() {
        factions.foreach { f =>
            f.gates.foreach { r =>
                if (f.at(r).%(_.canControlGate).none) {
                    f.gates = f.gates.but(r)
                    f.log("lost control of the gate in", r)
                }
            }
        }
    }

    def checkPowerReached() {
        factions.foreach { f =>
            f.satisfyIf(Gates3Power12, "Have 12 Power", f.power >= 12)
            f.satisfyIf(Gates4Power15, "Have 15 Power", f.power >= 15)
        }
    }

    def checkAbhothSpellbook() {
        factions.foreach { f =>
            if (f.has(Abhoth) && f.upgrades.has(TheBrood).not) {
                val monsters = f.units.monsters.inPlay

                if (monsters./(_.uclass).distinct.num >= 4 || monsters.num >= 8) {
                    f.upgrades :+= TheBrood
                    f.log("gained", f.styled(TheBrood), "for", f.styled(Abhoth))
                }
            }
        }
    }

    def checkInterdimensional() {
        factions.foreach { f =>
            if (f.has(Interdimensional)) {
                val r = f.goo(Daoloth).region

                if (r.onMap && !gates.contains(r)) {
                    gates :+= r
                    log(f.styled(Daoloth), "placed a Gate in", r, "with", f.styled(Interdimensional))
                }
            }
        }
    }

    def checkGatesOwnership(self : Faction) {
        checkPowerReached()

        checkGatesLost()

        checkAbhothSpellbook()

        checkInterdimensional()

        gates.foreach { r =>
            if (factions.%(f => f.gates.has(r)).none && self.at(r).%(_.canControlGate).any) {
                self.gates :+= r
                self.log("gained control of the gate in", r)
            }
        }

        factions.foreach { f =>
            f.satisfyIf(OceanGates, "Control three Gates in Ocean areas", f.gates.%(_.glyph == Ocean).num >= 3)
            f.satisfyIf(OceanGates, "Four Gates exist in Ocean areas", (gates ++ unitGates).%(_.glyph == Ocean).num >= 4)

            f.satisfyIf(Gates3Power12, "Control three Gates", f.gates.num >= 3)
            f.satisfyIf(Gates4Power15, "Control four Gates", f.gates.num >= 4)

            f.satisfyIf(Spread4, "Have Units in four Areas", board.regions.%(r => f.at(r).any).num >= 4)
            f.satisfyIf(Spread6, "Have Units in six Areas", board.regions.%(r => f.at(r).any).num >= 6)
            f.satisfyIf(Spread8, "Have Units in eight Areas", board.regions.%(r => f.at(r).any).num >= 8)
            f.satisfyIf(SpreadSocial, "Share Areas with all enemies", f.enemies.forall(e => board.regions.exists(r => f.at(r).any && e.at(r).any)), f.enemies.num)

            if (board.starting(f).num == 2) {
                val o = board.starting(f).but(starting(f)).only
                f.satisfyIf(OppositeGate, "Gate exists in " + o.name, (gates ++ unitGates).contains(o))
            }

            f.satisfyIf(EightGates, "Eight Gates on the map", (gates ++ unitGates).%(_.glyph.onMap).num >= 8)
            f.satisfyIf(TenGates, "Ten Gates on the map", (gates ++ unitGates).%(_.glyph.onMap).num >= 10)
            f.satisfyIf(TwelveGates, "Twelve Gates on the map", (gates ++ unitGates).%(_.glyph.onMap).num >= 12)

            f.satisfyIf(GooMeetsGoo, "GOO shares Area with another GOO", board.regions.%(r => f.at(r, GOO).any && f.enemies.%(_.at(r, GOO).any).any).any)
            f.satisfyIf(UnitsAtEnemyGates, "Units at two enemy Gates", board.regions.%(r => f.at(r).any && f.enemies.%(_.gates.has(r)).any).num >= 2)
        }
    }

    def getCathedralCost(r : Region) : Int = 1 + board.connected(r).intersect(cathedrals).any.??(2)

    def outOfTurn(f : Faction) : $[Action] = {
        $ ++
        f.es.some./(l => RevealESOutOfTurnAction(f))
    }

    var continue : Continue = StartContinue

    def perform(action : Action) : ($[String], Continue) = {
        val c = performContinue(action)

        val l = mlog.reverse

        mlog = $

        (l, c)
    }

    def performContinue(action : Action) : Continue = {
        if (action == CancelAction)
            return continue

        internalPerform(action, NoVoid) match {
            case Force(a : OutOfTurnReturn) => continue
            case Force(a) => internalPerform(a, NoVoid)
            case c =>
                if (action.isRecorded)
                    continue = c

                c
        }
    }

    def internalPerform(action : Action, soft : VoidGuard) : Continue = {
        expansions.foreach { e =>
            e.perform(action, soft) @@ {
                case UnknownContinue =>
                case Force(a : OutOfTurnReturn) => return continue
                case Force(another) =>
                    if (action.isSoft.not && another.isSoft)
                        soft()

                    return another.as[SelfPerform]./(_.perform(soft)).|(internalPerform(another, soft))
                case TryAgain => return internalPerform(action, soft)
                case c => return c
            }
        }

        throw new Error("unknown continue on " + action)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {
        // INIT
        case StartAction =>
            if (options.has(HighPriests)) {
                factions.foreach { f =>
                    f.loyaltyCards = f.loyaltyCards :+ HighPriestCard

                    f.units :+= new UnitFigure(f, HighPriest, 1, f.reserve)
                }

                loyaltyCards = loyaltyCards.but(HighPriestCard)
            }

            SetupFactionsAction

        case SetupFactionsAction if factions.forall(starting.contains) =>
            PlayOrderAction

        case SetupFactionsAction =>
            val f = factions.%!(starting.contains).minBy(board.starting(_).num)

            /*
            if (f == CC)
            board.regions.foreach { r =>
                var destinations = board.connected(r)

                destinations = destinations ++ destinations.flatMap(board.connected)

                destinations = destinations.distinct//.but(region).diff(destinations)

                log("From " + r, "cant fly to " + board.regions.diff(destinations).mkString(", "))
            }
            */

            Ask(f).each(board.starting(f).diff(starting.values.$))(r => StartingRegionAction(f, r).as(r)(f, "starts in"))

        case StartingRegionAction(self, r) =>
            starting += self -> r

            1.to(6).foreach(_ => self.place(Acolyte, r))

            // Temp starting setup (for debug)
            // if (self.has(Immortal)) {
            //     self.place(Cthulhu, r)
            //     self.satisfy(FirstDoomPhase, "Debug")
            //     self.satisfy(KillDevour1, "Debug")
            //     self.satisfy(KillDevour2, "Debug")
            //     self.satisfy(AwakenCthulhu, "Debug")
            //     self.satisfy(OceanGates, "Debug")
            //     self.satisfy(FiveSpellbooks, "Debug")
            // }

            gates :+= r

            self.gates :+= r

            self.log("started in", r)

            SetupFactionsAction

        case PowerGatherAction(last) =>
            // last minute Dragon Ascending check
            if (factions.%!(_.hibernating).%(_.power > 0).any)
                return MainNextPlayerAction(last)

            turn += 1

            factions.foreach { f =>
                f.oncePerTurn = $
                f.ignorePerTurn = $
            }

            log("POWER GATHER")

            factions.foreach { f =>
                val hibernate = f.power
                val cultists = f.all.cultists.num
                val captured = factions./~(w => w.at(f.prison)).num
                val ownGates = f.gates.num + f.unitGate.any.??(1)
                val oceanGates = (f.has(YhaNthlei) && f.has(Cthulhu)).??(f.enemies./(f => f.allGates.%(_.glyph == Ocean).num).sum)
                val darkYoungs = f.has(RedSign).??(f.all(DarkYoung).num)
                val feast = f.has(Feast).??(desecrated.%(r => f.at(r).any).num)
                val abandoned = abandonedGates.num
                var worship = 0

                if (f.has(WorshipServices))
                    f.enemies.foreach { e =>
                        board.regions.%(cathedrals.contains).%(e.gates.has).some.foreach { l => worship += l.num }
                    }
                else
                if (factions.%(_.has(WorshipServices)).num > 0)
                    board.regions.%(cathedrals.contains).%(f.gates.has).some.foreach { l => worship += l.num }

                f.power = hibernate + ownGates * 2 + abandoned + cultists + captured + oceanGates + darkYoungs + feast + worship
                f.hibernating = false

                val fromHibernate = (hibernate > 0).?(hibernate.styled("region") + " hibernate")
                val fromGates = (ownGates > 0).?((("2 x " + ownGates).styled("region") + " gate".s(ownGates)))
                val fromAbandoned = (abandoned > 0).?(abandoned.styled("region") + " abandoned")
                val fromCultist = (cultists > 0).?(cultists.styled("region") + " cultist".s(cultists))
                val fromCaptured = (captured > 0).?(captured.styled("region") + " captured")
                val fromYhaNthlei = (oceanGates > 0).?(oceanGates.styled("region") + " enemy controlled ocean gate".s(oceanGates))
                val fromDarkYoungs = (darkYoungs > 0).?(darkYoungs.styled("region") + " Dark Young".s(darkYoungs))
                val fromFeast = (feast > 0).?(feast.styled("region") + " desecrated")
                val fromWorship = (worship > 0).?(worship.styled("region") + " cathedral".s(worship))

                f.log("got", f.power.power, "(" + $(fromHibernate, fromGates, fromAbandoned, fromCultist, fromCaptured, fromYhaNthlei, fromDarkYoungs, fromFeast, fromWorship).flatten.mkString(" + ") + ")")
            }

            factions.foreach { f =>
                val captured = factions./~(_.at(f.prison))

                if (captured.any) {
                    captured.foreach(eliminate)

                    f.log("released", captured.mkString(", "))
                }
            }

            val max = factions./(_.power).max
            val min = (max + 1) / 2

            if (min == 0) {
                log("Humanity won")

                return GameOver($)
            }

            factions.foreach { f =>
                if (f.power < min) {
                   f.log("power increased to", min.power)
                   f.power = min
                }
            }

            checkPowerReached()

            AfterPowerGatherAction

        case AfterPowerGatherAction =>
            factions.foreach { f =>
                if (f.want(MaoCeremony)) {
                    f.onMap.cultists.some.foreach { l =>
                        return Ask(f).each(l)(c => MaoCeremonyAction(f, c.region, c.uclass)).add(MaoCeremonyDoneAction(f))
                    }
                }
            }

            factions.foreach(_.ignorePerInstant = $)

            DragonAscendingInstantAction(DragonAscendingUpAction("first player determination", FirstPlayerDeterminationAction))

        case DoomPhaseAction =>
            doomPhase = true

            factions.foreach { f =>
                val brood = f.enemies.%(_.has(TheBrood))
                val gates = f.gates ++ f.unitGate./(_.region)
                val valid = gates.%!(r => brood.exists(_.at(r)(Filth).any))

                f.doom += valid.num
                f.log("got", valid.num.doom)

                if (f.has(Byatis)) {
                    val r = f.goo(Byatis).region
                    if (f.enemies.exists(_.present(r)).not) {
                        f.log("gained", 1.es, "from", f.styled(Byatis), "and", "Toad of Berkeley".styled("nt"))
                        f.takeES(1)
                    }
                }
            }

            log(CthulhuWarsSolo.DottedLine)
            showROAT()

            CheckSpellbooksAction(DoomNextPlayerAction(starter))

        case ActionPhaseAction =>
            if (factions.%(_.doom >= 30).any || ritualTrack(ritualMarker) == 999)
                return GameOverPhaseAction

            doomPhase = false

            log("=======================================================================================================================")
            log("Turn", turn)
            log("ACTIONS")

            round = 0

            CheckSpellbooksAction(MainAction(starter))

        case GameOverPhaseAction =>
            factions.%(_.needs(AnytimeGainElderSigns)).foreach { f =>
                f.satisfy(AnytimeGainElderSigns, "Anytime Spellbook", min(3, f.enemies.%(_.hasAllSB).num))
                return CheckSpellbooksAction(GameOverPhaseAction)
            }

            factions.%(_.es.any).foreach { f =>
                f.log("revealed", f.es.num.es, "for", f.es./(_.value).sum.doom)
                f.doom += f.es./(_.value).sum
                f.revealed ++= f.es
                f.es = $
            }

            val contenders = factions.%(_.hasAllSB)
            val winners = contenders.%(_.doom == contenders./(_.doom).max)

            if (winners.none)
                log("Humanity won")
            else {
                log(winners.mkString(", "), "won")
            }

            GameOver(winners)

        case FirstPlayerDeterminationAction =>
            val max = factions./(_.power).max
            val fs = factions.%(f => f.power == max)

            if (fs.num > 1) {
                Ask(starter).each(fs)(f => FirstPlayerAction(starter, f))
            }
            else {
                val old = starter

                starter = fs.only

                if (old != starter)
                    starter.log("became the first player")

                PlayOrderAction
            }

        case FirstPlayerAction(self, f) =>
            starter = f

            if (self == f)
                self.log("decided to remain the first player")
            else
                self.log("chose", f, "as the first player")

            PlayOrderAction

        case PlayOrderAction =>
            starter.satisfy(FirstPlayer, "Become Starting Player")

            val forward = factions.dropWhile(_ != starter) ++ factions.takeWhile(_ != starter)
            val backward = forward.take(1) ++ forward.drop(1).reverse

            Ask(starter)
                .add(PlayDirectionAction(starter, forward))
                .add(PlayDirectionAction(starter, backward))

        case PlayDirectionAction(self, fs) =>
            order = fs

            log("Play order", order.mkString(", "))

            if (turn == 1)
                ActionPhaseAction
            else {
                log(CthulhuWarsSolo.DottedLine)
                log("DOOM PHASE")

                factions.foreach(f => f.satisfyIf(FirstDoomPhase, "The first Doom phase", turn == 2))
                factions.foreach(f => f.satisfyIf(FiveSpellbooks, "Have five spellbooks", f.unfulfilled.num == 1))

                CheckSpellbooksAction(DoomPhaseAction)
            }

        // SPELLBOOK
        case CheckSpellbooksAction(next) =>
            val fs = factions.%(f => f.unfulfilled.num + f.spellbooks.num < f.library.num)
            val fe = factions.%(f => f.es.%(_.value == 0).any)

            if (fs.any) {
                val f = fs(0)
                val bs = (f.library.%!(f.has) ++ neutralSpellbooks).diff(f.ignorePerInstant)
                Ask(f).each(bs)(SpellbookAction(f, _, next))
            }
            else
            if (fe.any) {
                val f = fe(0)
                val n = f.es.%(_.value == 0).num
                val es = factions./~(f => f.es ++ f.revealed)

                DrawES("" + f + " gets " + n.es, 18 - es.%(_.value == 1).num, 12 - es.%(_.value == 2).num, 6 - es.%(_.value == 3).num, (x, public) => ElderSignAction(f, n, x, public, next))
            }
            else {
                val end = factions.%(_.doom >= 30).any || ritualTrack(ritualMarker) == 999

                if (!end && next.is[MainAction])
                    log(CthulhuWarsSolo.DottedLine)

                Force(next)
            }

        case SpellbookAction(self, sb, next) =>
            self.spellbooks = self.spellbooks :+ sb

            self.log("received", sb.full)

            neutralSpellbooks = neutralSpellbooks.but(sb)

            if (self.hasAllSB)
                factions.foreach(f => f.satisfy(AnotherFactionAllSpellbooks, "Another faction has all spellbooks"))

            self.ignorePerInstant = $

            CheckSpellbooksAction(next)

        case ElderSignAction(f, _, v, public, next) =>
            if (v == 0) {
                val n = f.es.%(_.value == 0).num
                f.es = f.es.%(_.value > 0)
                f.doom += n
                log("No more", "Elder Signs".styled("es") + ",", f, "got", n.doom, "instead")
            }
            else {
                f.es = f.es.%(_.value > 0) ++ f.es.%(_.value == 0).drop(1) :+ ElderSign(v)
                if (public)
                    f.log("got", 1.es, "worth", v.doom)
            }
            CheckSpellbooksAction(next)


        // REVEAL
        case RevealESMainAction(self) =>
            Ask(self).each(self.es +: self.es.sortBy(_.value)./(e => $(e)))(RevealESAction(self, _ , false, MainAction(self))).cancel

        case RevealESDoomAction(self) =>
            Ask(self).each(self.es +: self.es.sortBy(_.value)./(e => $(e)))(RevealESAction(self, _ , self.has(StarsAreRight), DoomAction(self))).cancel

        case RevealESOutOfTurnAction(self) =>
            Ask(self).each(self.es +: self.es.sortBy(_.value)./(e => $(e)))(RevealESAction(self, _ , doomPhase && self.has(StarsAreRight), OutOfTurnDoneAction)).cancel

        case RevealESAction(self, es, power, next) =>
            val sum = es./(_.value).sum
            self.doom += sum

            self.revealed ++= es
            self.es = self.es.diff(es)

            if (power)
                self.power += sum

            self.log("revealed", es./(_.short).mkString(" "), "for", sum.doom, power.??("and " + sum.power))

            Force(next)

        // DOOM
        case DoomAction(self) =>
            checkGatesLost()

            var options : $[FactionAction] = $

            val cost = self.has(Herald).?(5).|(ritualCost)

            if (self.want(DragonAscending) && factions.%(_.power > self.power).any)
                options :+= DragonAscendingDoomAction(self)

            if (self.power >= cost && self.acted.not) {
                options :+= RitualAction(self, cost, 1)

                if (self.can(DragonDescending))
                    options :+= DragonDescendingDoomAction(self, cost)
            }

            if (self.can(BloodSacrifice) && self.has(ShubNiggurath) && self.all.cultists.any)
                options :+= BloodSacrificeDoomAction(self)

            if (self.can(DeathFromBelow) && self.pool.monsters.any)
                options :+= DeathFromBelowDoomAction(self)

            if (self.can(Dematerialization))
                options :+= DematerializationDoomAction(self)

            if (self.es.num > 0)
                options :+= RevealESDoomAction(self)

            if (self.all(HighPriest).any)
                options :+= SacrificeHighPriestDoomAction(self)

            if (!self.hired && loyaltyCards.%(_.doom > 0).exists(c => self.doom >= c.doom && self.power >= c.power))
                options :+= LoyaltyCardDoomAction(self)

            if (self.needs(AnytimeGainElderSigns))
                options :+= AnytimeGainElderSignsDoomAction(self)

            if (self.has(AncientSorcery) && self.at(SL.slumber, SerpentMan).any)
                options :+= AncientSorceryDoomAction(self)
            else
                options :+= DoomDoneAction(self)

            Ask(self).list(options)

        case DoomNextPlayerAction(self) =>
            CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingDownAction(self, "doom action", DoomAction(self))))

        case DoomDoneAction(self) =>
            self.acted = false

            self.hired = false

            val next = (order ++ order).dropWhile(_ != self).drop(1).first

            if (next != starter)
                DoomNextPlayerAction(next)
            else {
                factions.foreach(_.borrowed = $)
                CheckSpellbooksAction(ActionPhaseAction)
            }

        // RITUAL
        case RitualAction(f, cost, k) =>
            f.power -= cost

            val brood = f.enemies.%(_.has(TheBrood))
            val gates = f.gates ++ f.unitGate./(_.region)
            val valid = gates.%!(r => brood.exists(_.at(r)(Filth).any))

            val doom = valid.num * k

            val es = f.all.factionGOOs.num + f.has(Consecration).??($(0, 1, 1, 1, 2)(cathedrals.num))

            f.doom += doom

            f.log("performed the ritual", "for", cost.power, "and gained", doom.doom, (es > 0).??("and " + es.es))

            f.takeES(es)

            f.acted = true

            if (ritualTrack(ritualMarker) != 999)
                ritualMarker += 1

            showROAT()

            f.satisfy(PerformRitual, "Perform Ritual of Annihilation")

            CheckSpellbooksAction(DoomAction(f))

        // MAIN
        case MainAction(self) =>
            if (factions.%(f => f.unfulfilled.num + f.spellbooks.num < f.spellbooks.num).any)
                return CheckSpellbooksAction(MainAction(self))

            val others = self.enemies

            if (self.active) {
                others.%(_.can(Devolve)).%(_.pool(DeepOne).any).foreach { f =>
                    board.regions.%(f.at(_, Acolyte).any).foreach { r =>
                        if (self.acted.not)
                            if (self.canCapture(r)(f))
                                if (!f.option(OutOfTurnDevolveOff))
                                    return Force(DevolveMainAction(f, DevolveDoneAction(f, MainAction(self))))

                        if (f.ignored(Devolve).not)
                            if (self.acted || (self.hasAllSB && self.battled.has(r).not))
                                if (self.canAttack(r)(f))
                                    if (!f.option(OutOfTurnDevolveOff) && !f.option(OutOfTurnDevolveAvoidCapture))
                                        return Force(DevolveMainAction(f, DevolveDoneAction(f, MainAction(self))))
                    }
                }

                checkGatesOwnership(self)
            }

            if (self.active) {
                others.%(_.all(HighPriest).any).foreach { f =>
                    board.regions.%(f.at(_, HighPriest).any).foreach { r =>
                        if (!f.ignoredSacrificeHighPriest) {
                            if (self.acted.not)
                                if (self.canCapture(r)(f))
                                    if (!f.option(OutOfTurnSacrificeHighPriestOff))
                                        return Force(SacrificeHighPriestMainAction(f, SacrificeHighPriestDoneAction(f, MainAction(self))))

                            if (self.acted.not || (self.hasAllSB && self.battled.has(r).not))
                                if (self.canAttack(r)(f))
                                    if (!f.option(OutOfTurnSacrificeHighPriestOff) && !f.option(OutOfTurnSacrificeHighPriestAvoidCapture))
                                        return Force(SacrificeHighPriestMainAction(f, SacrificeHighPriestDoneAction(f, MainAction(self))))
                        }
                        else {
                            f.ignoredSacrificeHighPriest = false
                        }
                    }
                }

                checkGatesOwnership(self)
            }

            object afford {
                def apply(n : Int)(r : Region) = self.affords(n)(r)
                def apply(c : Region => Int)(r : Region) = self.affords(c(r))(r)
            }

            var options : $[Action] = $

            if (self.has(Lethargy) && self.has(Tsathoggua) && nexed.none && others.%(f => f.power > 0 && !f.hibernating).any)
                if (game.options.has(IceAgeAffectsLethargy).not || self.affords(0)(self.goo(Tsathoggua).region))
                    options :+= LethargyMainAction(self)

            if (self.has(Hibernate))
                options :+= HibernateMainAction(self, min(self.power, others./~(_.goos.distinctBy(_.uclass)).num))

            if (self.want(DragonAscending) && self.power < others./(_.power).max)
                options :+= DragonAscendingMainAction(self)

            if (self.units.nex.onMap.%!(_.has(Moved)).%(_.canMove).any)
                options :+= MoveMainAction(self)

            if (self.has(BeyondOne) && gates.num < board.regions.num && board.regions.diff(gates).%(afford(1)).any)
                gates.%(r => others.%(_.at(r, GOO).any).none).%(r => self.at(r).%(_.uclass.cost >= 3).%(_.canMove).any).some.foreach {
                    options :+= BeyondOneMainAction(self, _)
                }

            board.regions.nex.%(afford(1)).%(r => others.%(self.canCapture(r)).any).some.foreach { l =>
                options :+= CaptureMainAction(self, l, None)
            }

            if (self.has(CaptureMonster) && board.regions.nex.%(afford(1)).%(r => self.at(r, Tsathoggua).any && (others.exists(f => f.at(r).goos.none && f.at(r).monsters.any))).any)
                options :+= CaptureMonsterMainAction(self)

            val cs = self.pool.cultists./(_.uclass).distinct.reverse

            cs.foreach { uc =>
                board.regions.%(self.at(_).any).some.|(board.regions).nex.%(afford(r => self.recruitCost(uc, r))).some.foreach { l =>
                    options :+= RecruitMainAction(self, uc, l)
                }
            }

            val enough = nexed.any.?(queue.%(_.attacker == self).%(_.effect.has(EnergyNexus))./(_.arena)).|(self.battled)

            board.regions.nex.%(afford(1)).diff(enough).%(r => others.exists(self.canAttack(r))).some.foreach { r =>
                options :+= AttackMainAction(self, r, nexed.any.?(EnergyNexus))
            }

            board.regions.nex.%(afford(3 - self.has(UmrAtTawil).??(1))).%!(gates.has).%(r => self.at(r).%(_.canControlGate).any).some.foreach { r =>
                options :+= BuildGateMainAction(self, r)
            }

            if (self == AN && cathedrals.num < 4) {
                 val existingGlyphs = cathedrals.map(_.glyph).toSet

                 val validRegions = board.regions.filter { r =>
                     !cathedrals.has(r) &&
                     afford(getCathedralCost(r))(r) &&
                     self.at(r).%(_.canControlGate).any &&
                     !existingGlyphs.contains(r.glyph)
                }

                if (validRegions.any) {
                     options :+= BuildCathedralMainAction(self, validRegions.toList)
                }
            }

            if (self.has(CursedSlumber) && gates.%(_.glyph == Slumber).none && self.gates.nex.%(_.glyph.onMap).any)
                options :+= CursedSlumberSaveMainAction(self)

            if (self.has(CursedSlumber) && gates.%(_.glyph == Slumber).any)
                board.regions.nex.%(afford(1)).%!(gates.contains).some.foreach { options :+= CursedSlumberLoadMainAction(self, _) }

            ((self.pool.terrors ++ self.pool.monsters).sort)./(_.uclass).distinct.%(_.canBeSummoned(self)).reverse.foreach { uc =>
                board.regions.nex.%(afford(r => self.summonCost(uc, r))).%(self.canAccessGate).some.foreach { options :+= SummonMainAction(self, uc, _) }
            }

            if (self.has(Abhoth) && self.pool(Filth).any) {
                val affordableExists = board.regions.nex.%(afford(r => self.summonCost(Filth, r))).any
                if (affordableExists) {
                    options :+= FilthMainAction(self, board.regions)
                }
            }

            self.pool.goos.factionGOOs./(_.uclass).distinct.reverse.foreach { uc =>
                board.regions.nex.%(afford(r => self.awakenCost(uc, r).|(999))).some.foreach { options :+= AwakenMainAction(self, uc, _) }
            }

            loyaltyCards.%(_.doom == 0).%(_.power <= self.power).foreach { igoo =>
                self.gates.%(r => self.at(r, GOO).any || (self == AN && cathedrals.num == 4)).%(self.affords(igoo.power)).some.foreach { gates =>
                    options :+= IndependentGOOMainAction(self, igoo, gates)
                }
            }

            if (self.has(NightmareWeb) && self.pool(Nyogtha).any) {
                board.regions.%(self.affords(2)).%(self.present).some.foreach { l =>
                    options :+= NightmareWebMainAction(self, l)
                }
            }

            if (self.has(Dreams) && self.pool(Acolyte).any)
                board.regions.%(afford(2)).%(r => others.%(_.at(r, Acolyte).any).any).some.foreach { options :+= DreamsMainAction(self, _) }

            if (self.has(Submerge) && self.has(Cthulhu) && self.goo(Cthulhu).region.glyph == Ocean)
                options :+= SubmergeMainAction(self, self.goo(Cthulhu).region)

            if (self.at(GC.deep).any)
                board.regions.%(afford(0)).some.foreach { options :+= UnsubmergeMainAction(self, _) }

            if (self.has(Devolve) && self.all(Acolyte).any && self.pool(DeepOne).any)
                options :+= DevolveMainAction(self, MainAction(self))


            if (self.can(ThousandForms) && self.has(Nyarlathotep))
                options :+= ThousandFormsMainAction(self)

            if (self.needs(Pay4Power) && self.power >= 4)
                options :+= Pay4PowerMainAction(self)

            if (self.needs(Pay6Power) && self.power >= 6)
                options :+= Pay6PowerMainAction(self)

            if (self.needs(Pay4Power) && self.needs(Pay6Power) && self.power >= 10)
                options :+= Pay10PowerMainAction(self)


            if (self.needs(GiveWorstMonster))
                options :+= GiveWorstMonsterMainAction(self)

            if (self.needs(GiveBestMonster))
                options :+= GiveBestMonsterMainAction(self)


            if (self.has(Avatar) && self.has(ShubNiggurath)) {
                val r = self.goo(ShubNiggurath).region
                val t = self.taxIn(r)
                board.regions.but(r).%(afford(1 + t)).%(r => nfactions.%(_.at(r).vulnerable.any).any).some.foreach {
                    options :+= AvatarMainAction(self, r, _)
                }
            }

            if (self.has(Ghroth) && self.power >= 2)
                options :+= GhrothMainAction(self)

            if (self.has(DreadCurse)) {
                val n = self.all(Abomination).num + self.all(SpawnOW).num
                if (n > 0) {
                    val l = board.regions.%(afford(2)).%(r =>
                            others.exists(f =>
                                f.at(r).any
                            )
                        )
                    if (l.any)
                        options :+= DreadCurseMainAction(self, n, l)
                }
            }

            if (self.needs(Eliminate2Cultists) && self.all.cultists.num >= 2)
                options :+= Eliminate2CultistsMainAction(self)

            if (self.has(Desecrate) && self.has(KingInYellow) && desecrated.num <= 12) {
                val r = self.goo(KingInYellow).region
                if (!desecrated.contains(r)) {
                    val te = self.has(Hastur) && self.has(ThirdEye)
                    if (afford(te.?(1).|(2))(r))
                        options :+= DesecrateMainAction(self, r, te)
                }
            }

            if (self.can(HWINTBN) && !self.used(ScreamingDead) && self.has(Hastur)) {
                val o = self.goo(Hastur).region
                board.regions.%(afford(1)).but(o).%(r => factions.%(_.at(r, Cultist).any).any).some.foreach {
                    options :+= HWINTBNMainAction(self, o, _)
                }
            }

            if (self.can(ScreamingDead) && !self.used(HWINTBN) && self.has(KingInYellow)) {
                val o = self.goo(KingInYellow).region
                board.connected(o).%(afford(1)).some.foreach {
                    options :+= ScreamingDeadMainAction(self, o, _)
                }
            }

            if (self.has(Zingaya) && self.pool(Undead).any)
                board.regions.%(afford(1)).%(r => self.at(r, Undead).any).%(r => others.%(_.at(r, Acolyte).any).any).some.foreach {
                    options :+= ZingayaMainAction(self, _)
                }

            if (self.has(Shriek) && self.has(Byakhee))
                board.regions.%(afford(1)).%(r => self.all(Byakhee).%(_.region != r).any).some.foreach {
                    options :+= ShriekMainAction(self, _)
                }

            if (self.needs(Provide3Doom))
                options :+= Provide3DoomMainAction(self)


            if (self.has(AncientSorcery) && self.onMap(SerpentMan).nex.any && self.borrowed.num < factions.num - 1)
                options :+= AncientSorceryMainAction(self)

            if (self.needs(Pay3SomeoneGains3) && self.power >= 3)
                options :+= Pay3SomeoneGains3MainAction(self)

            if (self.needs(Pay3EverybodyLoses1) && self.power >= 3)
                options :+= Pay3EverybodyLoses1MainAction(self)

            if (self.needs(Pay3EverybodyGains1) && self.power >= 3)
                options :+= Pay3EverybodyGains1MainAction(self)


            if (self.needs(AnytimeGainElderSigns))
                options :+= AnytimeGainElderSignsMainAction(self)

            if (self.has(IceAge))
                board.regions.%(afford(1)).%(r => self.iceAge./(_ != r).|(true)).some.foreach {
                    options :+= IceAgeMainAction(self, _)
                }

            if (self.has(Undimensioned) && self.units.%(_.region.glyph.onMap)./(_.region).distinct.num > 1 && self.units.%(_.region.glyph.onMap)./(_.region).%(afford(2)).any)
                options :+= UndimensionedMainAction(self)

            if (self.has(Recriminations))
                options :+= RecriminationsMainAction(self)

            if (self.all(HighPriest).any)
                options :+= SacrificeHighPriestMainAction(self, MainAction(self))

            if (self.has(GodOfForgetfulness) && self.has(Byatis)) {
                val br = self.goo(Byatis).region

                val l = board.connected(br).filter { r =>
                    self.enemies.exists { f =>
                        f.at(r, Cultist).any
                    }
                }.distinct

                if (l.any && afford(1)(br)) {
                    options :+= GodOfForgetfulnessMainAction(self, br, l)
                }
            }

            if (self.es.num > 0)
                options :+= RevealESMainAction(self)

            options = options.% {
                case _ if self.active && self.acted.not && nexed.any => true
                case _ if self.active && self.acted.not && self.battled.none => true
                case _ if self.active && self.acted.not && self.hasAllSB => true
                case AttackMainAction(_, l, _) if self.active && (self.hasAllSB && !self.option(UnlimitedBattleOff) && (!self.option(UnlimitedBattleOnlyWithGOO) || l.%(r => self.at(r, GOO).any).any)) && (nexed.none || self.acted.not) => true
                case SummonMainAction(_, _, _) if self.acted && self.has(Fertility) && !self.option(UnlimitedSummonOff) && (!self.option(UnlimitedSummonEnemyGOO) || others./~(_.goos)./(_.region).%(self.canAccessGate).any) => true
                case FilthMainAction(_, _) if self.acted && self.has(Fertility) && !self.option(UnlimitedSummonOff) && (!self.option(UnlimitedSummonEnemyGOO) || others./~(_.goos)./(_.region).%(self.canAccessGate).any) && self.has(Abhoth) => true
                case RevealESMainAction(_) if self.doom + self.es./(_.value).sum >= 30 => true
                case DevolveMainAction(_, _) if self.active && ((self.acted.not && self.battled.none) || (!self.option(OutOfTurnDevolveOff) && !self.option(OutOfTurnDevolveAvoidCapture))) => true
                case AnytimeGainElderSignsMainAction(_) if self.doom + self.es./(_.value).sum + min(3, self.enemies.%(_.hasAllSB).num) * 3 >= 30 && (self.unfulfilled.num == 1 || others.%(_.hasAllSB).none) => true
                case DragonAscendingMainAction(_) if self.acted.not && self.battled.none => true
                case DragonAscendingMainAction(_) if (self.hasAllSB && !self.option(UnlimitedBattleOff)) && board.regions.nex.diff(self.battled).%(r => others.%(self.canAttack(r)).any).any => true
                case _ => false
            }

            if (self.active.not && nexed.none)
                options :+= MainNextPlayerAction(self).as("MainDoneCancelAction")
            else
            if (self.acted || self.battled.any || self.oncePerRound.contains(Fertility) || self.oncePerRound.contains(HWINTBN) || self.oncePerRound.contains(ScreamingDead) || nexed.any)
                options :+= MainDoneAction(self)
            else
                options :+= PassAction(self)

            if (options.num > 1) {
                if (self.hasAllSB)
                    options :+= ToggleUnlimitedBattleAction(self, $(UnlimitedBattleOn, UnlimitedBattleOff, UnlimitedBattleOnlyWithGOO).intersect(self.ignoreOptionsNew).single.|(UnlimitedBattleOn))

                if (self.has(Fertility))
                    options :+= ToggleUnlimitedSummonAction(self, $(UnlimitedSummonOn, UnlimitedSummonOff, UnlimitedSummonEnemyGOO).intersect(self.ignoreOptionsNew).single.|(UnlimitedSummonOn))

                if (self.has(Devolve))
                    options :+= ToggleOutOfTurnDevolveAction(self, $(OutOfTurnDevolveOn, OutOfTurnDevolveOff, OutOfTurnDevolveAvoidCapture).intersect(self.ignoreOptionsNew).single.|(OutOfTurnDevolveOn))

                if (self.all(HighPriest).any)
                    options :+= ToggleOutOfTurnSacrificeHighPriestAction(self, $(OutOfTurnSacrificeHighPriestOn, OutOfTurnSacrificeHighPriestOff, OutOfTurnSacrificeHighPriestAvoidCapture).intersect(self.ignoreOptionsNew).single.|(OutOfTurnSacrificeHighPriestOn))
            }

            Ask(self).list(options)

        case MainNextPlayerAction(f) =>
            if (queue.any)
                ProceedBattlesAction
            else {
                factions.foreach(_.acted = false)
                factions.foreach(_.battled = $)
                factions.foreach(_.oncePerRound = $)

                round += 1

                if (factions.%(_.doom >= 30).any)
                    CheckSpellbooksAction(GameOverPhaseAction)
                else {
                    if (factions.%(!_.hibernating).%(_.power > 0).none) {
                        CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingUpAction("power gather", PowerGatherAction(f))))
                    }
                    else {
                        val next = (order ++ order).dropWhile(_ != f).drop(1).first
                        CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingDownAction(next, "action", MainAction(next))))
                    }
                }
            }

        case MainDoneAction(self) =>
            self.acted = true
            Force(MainNextPlayerAction(self))

        case EndAction(self) =>
            self.acted = true
            AfterAction(self)

        case AfterAction(self) =>
            checkGatesOwnership(self)

            if (self.power == 0)
                self.log("ran out of power")

            if (self.power == -1)
                self.power = 0

            if (self.power < 0) {
                self.log("somehow ran into negative power")

                self.power = 0
            }

            factions.%(_.has(Passion)).%(_.oncePerAction.has(Passion)).foreach { f =>
                f.power += 1

                f.log("got", 1.power, "from", Passion.full)
            }

            factions.%(_.oncePerAction.has(LostAbhoth)).foreach { f =>
                NeutralAbhoth.units = self.units(Filth)./(u => new UnitFigure(NeutralAbhoth, u.uclass, u.index, (u.region == self.reserve).?(NeutralAbhoth.reserve).|(u.region), u.state, u.health))

                self.units = self.units.not(Filth)
            }

            factions.%(_.oncePerAction.has(NyogthaPrimed)).%!(_.oncePerAction.has(NyogthaMourning)).foreach { f =>
                if (f.upgrades.has(NightmareWeb).not) {
                    f.upgrades :+= NightmareWeb

                    f.log("gained", f.styled(NightmareWeb), "for", f.styled(Nyogtha))
                }
            }

            factions.foreach(_.oncePerAction = $)

            self.ignoreOptions = self.ignoreOptionsNew

            CheckSpellbooksAction(MainAction(self))

        // PASS
        case PassAction(self) =>
            val p = self.power

            self.power = -1

            self.log("passed and forfeited", p.power)

            EndAction(self)

        // MOVE
        case MoveMainAction(self) =>
            MoveContinueAction(self, false)

        case MoveContinueAction(self, moved) =>
            if (self.power == 0)
                MoveDoneAction(self)
            else {
                val units = self.units.nex.onMap.%!(_.has(Moved)).%(_.canMove).sort

                if (units.none)
                    MoveDoneAction(self)
                else
                if (moved)
                    Ask(self)
                        .group("Moved", self.units.%(_.has(Moved)).mkString(", "))
                        .done(MoveDoneAction(self))
                        .each(units)(u => MoveSelectAction(u.faction, u.uclass, u.region).as(u, "from", u.region)("Move", "another".hl, "unit"))
                else
                    Ask(self)
                        .group("Move unit")
                        .each(units)(u => MoveSelectAction(u.faction, u.uclass, u.region).as(u, "from", u.region))
                        .cancel
            }

        case MoveSelectAction(self, uc, region) =>
            var destinations = board.connected(region)

            if (self.has(Flight))
                destinations = destinations ++ destinations.flatMap(board.connected).distinct.but(region).diff(destinations)

            if (uc == Shantak)
                destinations = board.regions

            val free = uc == Nyogtha && self.all(Nyogtha).%(_.has(Moved)).any

            val arriving = self.units.%(_.region.glyph.onMap).%(_.has(Moved))./(_.region).distinct

            destinations = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            destinations = destinations.%(self.affords(1))

            val options = destinations./(d => MoveAction(self, uc, region, d).as(d, self.iced(d))("Move", self.styled(uc), free.??("with " + FromBelow), "from", region, "to"))

            if (uc == Nyogtha && self.all(Nyogtha).%(_.has(Moved)).any)
                Ask(self).list(options).skip(MoveContinueAction(self, true))
            else
            // if (self.units.onMap.%(_.has(Moved)).any)
            //     Ask(self).list(options).cancel
            // else
                Ask(self).list(options).cancel

        case MoveDoneAction(self) =>
            if (self.has(Burrow) && self.units.%(u => u.has(Moved))./(u => 1 - u.has(MovedForFree).??(1) + u.has(MovedForDouble).??(1)).sum > 1) {
                self.power += 1
                self.log("recovered", 1.power, "from", Burrow.full)
            }

            self.units.foreach(_.remove(Moved))
            self.units.foreach(_.remove(MovedForFree))
            self.units.foreach(_.remove(MovedForDouble))

            EndAction(self)

        case MoveAction(self, uc, o, r) =>
            val t = self.payTax(r)

            val free = uc == Nyogtha && self.all(Nyogtha).%(_.has(Moved)).any

            self.power -= free.not.??(1)

            val u = self.at(o, uc).%!(_.has(Moved)).first
            u.region = r
            u.add(Moved)

            if (t > 0)
                u.add(t.times(MovedForDouble))

            self.log("moved", u, "from", o, "to", r)

            if (u.uclass == Nyogtha) {
                self.all(Nyogtha).but(u).%!(_.has(Moved)).foreach { h =>
                     return Force(MoveSelectAction(self, h.uclass, h.region))
                }
            }

            if (u.uclass == Ithaqua && self.has(ArcticWind))
                Ask(self).each(self.at(o).%!(_.has(Moved)).%(_.canMove))(u => ArcticWindAction(self, o, u.uclass, r)).done(MoveContinueAction(self, true))
            else
            if (u.uclass == Shantak)
                Ask(self).each(self.at(o).%!(_.has(Moved)).cultists)(u => ShantakCarryCultistAction(self, o, u.uclass, r)).skip(MoveContinueAction(self, true))
            else
                MoveContinueAction(self, true)

        // ATTACK
        case AttackMainAction(f, l, effect) =>
            val ee = f.enemies ++ neutrals.keys
            val ll = l.sortBy(r => ee.%(_.present(r))./(e => f.strength(f.at(r), e)).maxOr(0)).reverse

            val variants = ll./~(r => ee.%(_.present(r)).%(f.canAttack(r)).sortBy(e => -e.strength(e.at(r), f))./(e => r -> e))

            Ask(f)
                .each(variants)((r, e) => AttackAction(f, r, e, effect))
                .group(" ")
                .cancelIf(effect.none)
                .cancelIf(effect.has(EnergyNexus))
                .skipIf(effect.has(FromBelow))(ProceedBattlesAction)

        case AttackAction(self, r, f, effect) =>
            self.power -= 1
            self.payTax(r)
            self.log("battled", f, "in", r, effect./("with " + _).|(""))

            if (effect.has(FromBelow))
                game.queue = game.queue.take(1) ++ $(new Battle(r, self, f, effect)) ++ game.queue.drop(1)
            else
                game.queue = game.queue.take(0) ++ $(new Battle(r, self, f, effect)) ++ game.queue.drop(0)

            if (effect.has(FromBelow).not && self.has(FromBelow) && self.at(r)(Nyogtha).any) {
                val l = self.all(Nyogtha)./(_.region).but(r).diff(self.battled)
                if (l.any)
                    return Force(AttackMainAction(self, l, |(FromBelow)))
            }

            ProceedBattlesAction

        case ProceedBattlesAction =>
            factions.%(f => f.has(EnergyNexus) && queue.exists(b => f.at(b.arena)(Wizard).any) && f.acted.not).foreach { f =>
                game.nexed = queue.%(_.attacker == queue.first.attacker)./(_.arena).%(f.at(_)(Wizard).any)
                f.log("interrupted battle", queue.exists(_.effect.has(EnergyNexus)).??("again"), "with", EnergyNexus.full)
                return Force(MainAction(f))
            }


            battle = queue.starting

            queue = queue.drop(1)

            if (game.nexed.any) {
                game.nexed = $

                battle.get.attacker.log("proceeded to battle", battle.get.defender, "in", battle.get.arena)
            }

            battle.get.proceed()

        // CAPTURE
        case CaptureMainAction(self, l, effect) =>
            val variants = l./~ { r =>
                self.enemies.%(self.canCapture(r))./ { f =>
                    val uc = if (f.at(r, HighPriest).any && f.at(r, Acolyte).none) HighPriest else Acolyte
                    CaptureAction(self, r, f, uc, effect)
                }
            }

            Ask(self)
                .list(variants)
                .group(" ")
                .cancelIf(effect.none)
                .skipIf(effect.has(FromBelow))(EndAction(self))

        case CaptureAction(self, r, f, uc, |(FromBelow)) =>
            if (self.all(Nyogtha)./(_.region).but(r).any)
                self.payTax(r)

            val c = f.at(r).one(uc)
            capture(self, c)

            self.log("captured", c, "in", r, "with", FromBelow)

            EndAction(self)

        case CaptureAction(self, r, f, uc, effect) =>
            self.power -= 1
            self.payTax(r)

            val c = f.at(r).one(uc)
            capture(self, c)

            self.log("captured", c, "in", r, effect./("with " + _).|(""))

            self.satisfy(CaptureCultist, "Capture Cultist")

            self.all(Nyogtha)./(_.region).diff($(r)).foreach { x =>
                return Force(CaptureMainAction(self, $(x), |(FromBelow)))
            }

            EndAction(self)

        // BUILD
        case BuildGateMainAction(self, locations) =>
            Ask(self).each(locations.sortBy(self.taxIn))(r => BuildGateAction(self, r)).cancel

        case BuildGateAction(self, r) =>
            self.power -= 3 - self.has(UmrAtTawil).??(1)
            self.payTax(r)
            gates :+= r
            self.gates :+= r
            self.log("built a gate in", r)
            EndAction(self)

        // RECRUIT
        case RecruitMainAction(self, uc, l) =>
            val (a, b) = l.partition(abandonedGates.contains)
            Ask(self).each(a ++ b)(r => RecruitAction(self, uc, r)).cancel

        case RecruitAction(self, uc, r) =>
            self.power -= self.recruitCost(uc, r)
            self.payTax(r)
            self.place(uc, r)
            self.log("recruited", self.styled(uc), "in", r)
            EndAction(self)

        // SUMMON
        case SummonMainAction(self, uc, l) =>
            Ask(self).each(l)(r => SummonAction(self, uc, r)).cancel

        case SummonAction(self, uc, r) =>
            self.power -= self.summonCost(uc, r)
            self.payTax(r)
            self.place(uc, r)
            self.log("summoned", self.styled(uc), "in", r)

            if (self.has(Fertility) && !self.ignored(Fertility))
                self.oncePerRound :+= Fertility

            if (uc == Ghast)
                ContinueFreeSummonAction(self, uc, $(r))
            else
            if (uc == UnMan && self.has(Festival))
                Ask(self).each(self.enemies)(f => FestivalUnManSummonAction(self, f))
            else
                EndAction(self)

        case ContinueFreeSummonAction(self, uc, l) =>
            if (self.pool(Ghast).any)
                Ask(self).each(self.summonRegions)(r => FreeSummonAction(self, uc, r, l))
            else
                EndAction(self)

        case FreeSummonAction(self, uc, r, l) =>
            if (l.has(r).not)
                self.payTax(r)

            self.place(uc, r)
            self.log("summoned", self.styled(uc), "in", r, "for free")

            ContinueFreeSummonAction(self, uc, l :+ r)

        case ToggleUnlimitedBattleAction(self, value) =>
            self.ignoreOptionsNew :-= value

            self.ignoreOptionsNew :+= (value match {
                case UnlimitedBattleOn => UnlimitedBattleOnlyWithGOO
                case UnlimitedBattleOnlyWithGOO => UnlimitedBattleOff
                case UnlimitedBattleOff => UnlimitedBattleOn
            })

            MainAction(self)

        case ToggleUnlimitedSummonAction(self, value) =>
            self.ignoreOptionsNew :-= value

            self.ignoreOptionsNew :+= (value match {
                case UnlimitedSummonOn => UnlimitedSummonEnemyGOO
                case UnlimitedSummonEnemyGOO => UnlimitedSummonOff
                case UnlimitedSummonOff => UnlimitedSummonOn
            })

            MainAction(self)

        case ToggleOutOfTurnDevolveAction(self, value) =>
            self.ignoreOptionsNew :-= value

            self.ignoreOptionsNew :+= (value match {
                case OutOfTurnDevolveOn => OutOfTurnDevolveAvoidCapture
                case OutOfTurnDevolveAvoidCapture => OutOfTurnDevolveOff
                case OutOfTurnDevolveOff => OutOfTurnDevolveOn
            })

            MainAction(self)

        case ToggleOutOfTurnSacrificeHighPriestAction(self, value) =>
            self.ignoreOptionsNew :-= value

            self.ignoreOptionsNew :+= (value match {
                case OutOfTurnSacrificeHighPriestOn => OutOfTurnSacrificeHighPriestAvoidCapture
                case OutOfTurnSacrificeHighPriestAvoidCapture => OutOfTurnSacrificeHighPriestOff
                case OutOfTurnSacrificeHighPriestOff => OutOfTurnSacrificeHighPriestOn
            })

            MainAction(self)

        // AWAKEN
        case AwakenMainAction(self, uc, locations) if uc == ShubNiggurath =>
            val cultists = board.regions./~(r => self.at(r, Cultist).take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).drop(1)./(b => (a.region, b.region))).distinct
            Ask(self).each(pairs)((a, b) => AwakenEliminate2CultistsAction(self, uc, locations, a, b)).cancel

        case AwakenEliminate2CultistsAction(self, uc, locations, a, b) =>
            val q = locations./~(r => self.awakenCost(uc, r)./(cost => AwakenAction(self, uc, r, cost)))
            $(a, b).foreach { r =>
                val c = self.at(r).one(Cultist)
                log(c, "in", c.region, "was sacrificed")
                eliminate(c)
            }
            Ask(self).list(q)

        case AwakenMainAction(self, uc, locations) =>
            Ask(self).some(locations)(r => self.awakenCost(uc, r)./(cost => AwakenAction(self, uc, r, cost))).cancel

        case AwakenAction(self, uc, r, cost) =>
            self.power -= cost

            self.payTax(r)
            self.place(uc, r)

            self.log("awakened", self.styled(uc), "in", r)

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal.full)
                self.takeES(1)
            }

            if (uc == Ithaqua) {
                if (gates.contains(r)) {
                    gates = gates.but(r)

                    factions.foreach { f =>
                        f.gates = f.gates.but(r)
                    }

                    self.log("destroyed gate in", r)
                }
                else {
                    val u = factions./~(_.unitGate).%(_.region == r).only

                    eliminate(u)

                    self.log("eliminated", u, "in", r)
                }
            }

            if (uc == YogSothoth) {
                val s = self.at(r).one(SpawnOW)

                eliminate(s)

                self.log("replaced", s, "in", r)

                self.unitGate = self.at(r, YogSothoth).single
            }

            uc match {
                case Cthulhu =>
                    self.satisfy(AwakenCthulhu, "Awaken Cthulhu")
                case Nyarlathotep =>
                    self.satisfy(AwakenNyarlathotep, "Awaken Nyarlathotep")
                case ShubNiggurath =>
                    self.satisfy(AwakenShubNiggurath, "Awaken Shub-Niggurath")
                case KingInYellow =>
                    self.satisfy(AwakenKing, "Awaken King in Yellow")
                case Hastur =>
                    self.satisfy(AwakenHastur, "Awaken Hastur")
                case Tsathoggua =>
                    self.satisfy(AwakenTsathoggua, "Awaken Tsathoggua")
                case RhanTegoth =>
                    self.satisfy(AwakenRhanTegoth, "Awaken Rhan Tegoth")
                case Ithaqua =>
                    self.satisfy(AwakenIthaqua, "Awaken Ithaqua")
                case YogSothoth =>
                    self.satisfy(AwakenYogSothoth, "Awaken Yog-Sothoth")
            }

            EndAction(self)

        // HIGH PRIESTS
        case SacrificeHighPriestDoomAction(self) =>
            Ask(self).each(board.regions./~(r => self.at(r, HighPriest)))(c => SacrificeHighPriestAction(self, c.region, DoomAction(self))).cancel

        case SacrificeHighPriestMainAction(self, then) =>
            if (self.all(HighPriest).any) {
                if (self.at(SL.slumber, HighPriest).any)
                    Ask(self, $(SL.slumber)./~(r => self.at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, then)) :+ then)
                else
                    Ask(self, board.regions./~(r => self.at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, then)) :+ then)
            }
            else
                Force(then)

        case SacrificeHighPriestAction(self, r, then) =>
            val c = self.at(r).one(HighPriest)

            eliminate(c)

            self.power += 2

            self.log("sacrificed", c, "in", r)

            checkPowerReached()
            checkGatesLost()

            if (then.isCancel)
                CheckSpellbooksAction(then)
            else
                CheckSpellbooksAction(SacrificeHighPriestMainAction(self, then))

        case SacrificeHighPriestDoneAction(self, then) =>
            self.ignoredSacrificeHighPriest = true
            Force(then)

        // OW DA defaults
        case DragonAscendingInstantAction(then) =>
            then

        case DragonAscendingUpAction(reason, then) =>
            then

        case DragonAscendingDownAction(f, reason, then) =>
            then

        // BATTLE
        case action if battle.any =>
            battle.get.perform(action)
    }

}
