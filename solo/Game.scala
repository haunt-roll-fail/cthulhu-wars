package cws

import hrf.colmat._

import html._


@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
trait Record extends Product with GoodMatch


sealed abstract class Glyph(val inPlay : Boolean, val onMap : Boolean)

trait AreaGlyph extends Glyph
case object Ocean extends Glyph(true, true) with AreaGlyph
case object GlyphAA extends Glyph(true, true) with AreaGlyph
case object GlyphOO extends Glyph(true, true) with AreaGlyph
case object GlyphWW extends Glyph(true, true) with AreaGlyph
case object NoGlyph extends Glyph(true, true) with AreaGlyph
case object Pool extends Glyph(false, false)
case object Prison extends Glyph(false, false)
case object Deep extends Glyph(true, false)
case object Slumber extends Glyph(true, false)
case object Sorcery extends Glyph(true, false)
case object Extinct extends Glyph(false, false)

trait Region extends GoodMatch {
    val id : String
    val name : String
    val glyph : Glyph
    def elem : String

    def +(ia : IceAges) = elem + ia.toString

    override def toString = elem
}

case class Area(name : String, glyph : AreaGlyph) extends Region {
    val id = name.split(" ").mkString("")
    def elem = glyph @@ {
        case Ocean => name.styled("sea")
        case _ => name.styled("region")
    }
}

trait FactionRegion extends Region {
    val faction : Faction
    def elem = name.styled(faction)
}

case class Pool(faction : Faction) extends FactionRegion {
    val glyph = Pool
    val id = "???"
    val name = faction.name + " Pool"
}

case class Prison(faction : Faction) extends FactionRegion {
    val glyph = Prison
    val id = "???"
    val name = faction.name + " Prison"
}

case class Deep(faction : Faction) extends FactionRegion {
    val glyph = Deep
    val id = "Deep"
    val name = "Ocean Deep"
}

case class Slumber(faction : Faction) extends FactionRegion {
    val glyph = Slumber
    val id = "Slumber"
    val name = "Cursed Slumber"
}

case class Sorcery(faction : Faction) extends FactionRegion {
    val glyph = Sorcery
    val id = "???"
    val name = "Ancient Sorcery"
}

case class Extinct(faction : Faction) extends FactionRegion {
    val glyph = Extinct
    val id = "???"
    val name = "Extinct"
}


trait Board {
    def id : String
    def name : String
    def regions : $[Region]
    def connected(region : Region) : $[Region]
    def starting(faction : Faction) : $[Region]
    def distance(a : Region, b : Region) : Int
    def gateXYO(r : Region) : (Int, Int)
    val width : Int = 1791
    val nonFactionRegions : $[Region]
    val west : $[Region]
    val east : $[Region]
}

case class ElderSign(value : Int) {
    def short = "[" + (value > 0).?(value.toString).|("?").styled("es") + "]"
}


trait LoyaltyCard {
    val icon : UnitClass
    val unit : UnitClass
    def name = unit.name
    val doom : Int = 0
    val power : Int = 0
    val cost : Int
    val quantity : Int
    val combat : Int

    def short = name.styled("nt")
}

abstract class IGOOLoyaltyCard(val icon : UnitClass, val unit : UnitClass, override val power : Int, val quantity : Int = 1, val combat : Int = 0) extends LoyaltyCard {
    override val doom = 0
    override val cost = power
}

abstract class NeutralMonsterLoyaltyCard(val icon : UnitClass, val unit : UnitClass, val cost : Int, val quantity : Int, val combat : Int) extends LoyaltyCard {
    override val doom = 2
    override val power = 0
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
    def styled(f : Faction) = name.styled(f)
    val priority = utype.priority * 1_00_00_00 + cost * 1_00 + this.is[NeutralMonster].??(1_00_00)

    def canMove(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = utype == Cultist
    def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = utype == Monster || utype == Terror
}


case object Acolyte extends UnitClass("Acolyte", Cultist, 1) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(r : UnitFigure)(implicit game : Game) : Boolean = true
}

case object HighPriest extends UnitClass("High Priest", Cultist, 3) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(u : UnitFigure)(implicit game : Game) = true
}

case object HighPriestIcon extends UnitClass(HighPriest.name + " Icon", Token, 0)
case object HighPriestCard extends LoyaltyCard {
    val icon = HighPriestIcon
    val unit = HighPriest
    val cost = 3
    val quantity = 1
    val combat = 0
}


abstract class FactionUnitClass(val faction : Faction, name : String, utype : UnitType, cost : Int) extends UnitClass(name, utype, cost) {
    def elem = name.styled(faction)
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
case object MovedForExtra extends UnitState("moved-for-extra")
case object Eliminated extends UnitState("eliminated")

class UnitFigure(val faction : Faction, val uclass : UnitClass, val index : Int, var region : Region, var onGate : Boolean = false, var state : $[UnitState] = $, var health : UnitHealth = Alive) {
    override def toString = short

    def dbg = faction.short + "/" + uclass.name + "/" + index

    def short = uclass.name.styled(faction)

    def full = uclass.name.styled(faction) + onGate.??(" (on the gate)") + state.some./(_.mkString(" (", "/", ")")).|("") + (health != Alive && health != DoubleHP(Alive, Alive)).??(" (" + health + ")")

    def tag(s : UnitState) = state.has(s)
    def add(s : UnitState) { state :+= s }
    def add(l : $[UnitState]) { state ++= l }
    def remove(s : UnitState) { state = state.but(s) }
    def count(s : UnitState) = state.count(_ == s)
    def ref = UnitRef(faction, uclass, index)
}

case class UnitRef(faction : Faction, uclass : UnitClass, index : Int) {
    def short = uclass.styled(faction)
    def full = UnitRefFull(this)
}

case class UnitRefShort(r : UnitRef)
case class UnitRefFull(r : UnitRef)

sealed abstract class Spellbook(val name : String) extends Record {
    def elem : String
    override def toString = elem
    def styled(f : Faction) = name.styled(f)
}

trait BattleSpellbook extends Spellbook

abstract class NeutralSpellbook(name : String) extends Spellbook(name) {
    override def elem = name.styled("nt")
}

abstract class FactionSpellbook(val faction : Faction, name : String) extends Spellbook(name) {
    override def elem = name.styled(faction)
}

abstract class Requirement(val text : String, val es : Int = 0)

trait Faction { f =>
    def name : String
    def short : String
    def style : String
    def abbr : String = style.toUpperCase.styled(style)
    val reserve = Pool(f)
    val prison = Prison(f)

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
    def canAwakenIGOO(r : Region)(implicit game : Game) : Boolean = f.gates.has(r) && f.at(r, GOO).any
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int
    def neutralStrength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) =
        units(Ghast).num * 0 +
        units(Gug).num * 3 +
        units(Shantak).num * 2 +
        units(StarVampire).num * 1 +
        units(Byatis).not(Zeroed).num * 4 +
        units(Abhoth).not(Zeroed).num * f.all(Filth).num +
        units(Daoloth).not(Zeroed).num * 0 +
        units(Nyogtha).not(Zeroed).num * game.battle./(_.attacker).has(f).?(4).|(1)
}

object NoFaction extends Faction {
    def name = "No Faction"
    def short = "NF"
    def style = "nt"

    def abilities = $
    def library = $
    def requirements(options : $[GameOption]) = $

    val allUnits = $

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) = 0
}

trait Action extends Record {
    def question(implicit game : Game) : String
    def safeQ(implicit game : Game) = question(game).splt("<br/>").first
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

    def isNoClear : Boolean = this match {
        case a : Wrapped => unwrap.isNoClear
        case a : NoClear => true
        case _ => false
    }

    def isRecorded = (isMore || isCancel || isSoft || isVoid).not

    def unary_+(implicit wrapper : AskWrapper) = wrapper.ask = wrapper.ask.add(this)
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
trait NoClear extends Action

trait Continue

object Info {
    def apply(l : Any*)(g : Any*) = InfoAction(g.$, l.$)
}

case class MultiAsk(asks : $[Ask]) extends Continue

case class Ask(faction : Faction, actions : $[Action] = $) extends Continue {
    def add(a : Action) = Ask(faction, actions :+ a)
    def when(c : Boolean)(a : Action) = c.?(add(a)).|(this)
    def list(l : $[Action]) = Ask(faction, actions ++ l)
    def prepend(a : Action) = Ask(faction, a +: actions)
    def each[T](l : IterableOnce[T])(a : T => Action) = list(l.iterator.map(a).$)
    def each[T, U](l : IterableOnce[(T, U)])(a : (T, U) => Action) = list(l.iterator.map { case (t, u) => a(t, u) }.$)
    def some[T](l : IterableOnce[T])(a : T => IterableOnce[Action]) = list(l.iterator.flatMap(a).$)
    def group(t : Any*) = add(GroupAction(t.$.but("").mkString(" ")))
    def done(a : ForcedAction) = add(a.as("Done")(" "))
    def doneIf(c : Boolean)(a : ForcedAction) = c.?(add(a.as("Done"))).|(this)
    def skip(a : ForcedAction) = add(a.as("Skip"))
    def skipIf(c : Boolean)(a : ForcedAction) = c.?(add(a.as("Skip"))).|(this)
    def cancel = add(CancelAction)
    def cancelIf(c : Boolean) = c.?(add(CancelAction)).|(this)
}

case object StartContinue extends Continue
case class Force(action : Action) extends Continue
case class Then(action : Action) extends Continue
case class DelayedContinue(delay : Int, continue : Continue) extends Continue
case class RollD6(question : Game => String, roll : Int => ForcedAction) extends Continue
case class RollBattle(question : Game => String, n : Int, roll : $[BattleRoll] => ForcedAction) extends Continue
case class DrawES(question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => ForcedAction) extends Continue
case class GameOver(winners : $[Faction]) extends Continue
case object UnknownContinue extends Continue
case object TryAgain extends Continue

class AskWrapper(var ask : Ask)

object Asking {
    def apply(f : Faction) = new AskWrapper(Ask(f))
}

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

case class InfoAction(g : $[Any], o : $[Any]) extends Action with Info {
    def question(implicit game : Game) = g./(game.desc).but("").mkString(" ")
    def option(implicit game : Game) = o./(game.desc).but("").mkString(" ")
}

trait HiddenAction extends Action with Info {
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = ""
}

case object NeedOk extends HiddenAction


case class WrappedForcedAction(action : ForcedAction, o : $[Any]) extends Action with Wrapped {
    override def unwrap = action.unwrap
    def apply(q : Any*) = new WrappedQForcedAction(action, q.$, o)
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = o./(game.desc).but("").mkString(" ")
}

case class WrappedQForcedAction(action : ForcedAction, q : $[Any], o : $[Any]) extends Action with Wrapped {
    override def unwrap = action.unwrap
    def question(implicit game : Game) = q./(game.desc).but("").mkString(" ")
    def option(implicit game : Game) = o./(game.desc).but("").mkString(" ")
}

trait Void { self : ForcedAction => }
trait OutOfTurn { self : Action => }

case object OutOfTurnReturn extends ForcedAction with Soft
case object OutOfTurnDone extends ForcedAction
case class OutOfTurnRepeat(faction : Faction, action : Action with Soft with OutOfTurn) extends ForcedAction with Soft



case object ReloadAction extends ForcedAction with Void
case object UpdateAction extends ForcedAction with Void
case class CommentAction(comment : String) extends ForcedAction with Void


trait Plan extends Record {
    val label : String
    val unselected : String = label
    val selected : String = label.hl
    val info : String = label.hh
    val group : String
    val requires : $[$[Plan]] = $($)
    def followers : $[Plan] = $
    def unfollowers : $[Plan] = $
}
trait DefaultPlan extends Plan
trait OnlyOnPlan extends Plan
trait OneOfPlan extends Plan with OnlyOnPlan


sealed abstract class GateDiplomacyPlan(val label : String) extends Plan {
    val group = "Gate Diplomacy".hh
}
case object GateDiplomacyPrompt extends GateDiplomacyPlan("Display all options") with DefaultPlan with OneOfPlan
case object GateDiplomacySkipAbandon extends GateDiplomacyPlan("Don't prompt abandoning gates") with OneOfPlan
case object GateDiplomacyCling extends GateDiplomacyPlan("Cling to the gates") with OneOfPlan


sealed abstract class HighPriestGatesPlan(val label : String) extends Plan {
    val group = "High Priests".hh
}
case object HighPriestGatesPrompt extends HighPriestGatesPlan("Prompt controlling a gate") with DefaultPlan with OneOfPlan
case object HighPriestGatesSkip extends HighPriestGatesPlan("Always prefer Acolytes on the gates") with OneOfPlan


sealed abstract class DevolvePlan(val label : String) extends Plan {
    val group = "Devolve".styled(GC)
}
case object DevolvePrompt extends DevolvePlan("Always prompt") with DefaultPlan with OneOfPlan
case object DevolveSkip extends DevolvePlan("Skip, unless Acolyte is under...") with OneOfPlan { override val followers = $(DevolveThreatOfCapture) }
trait DevolveThreat extends DevolvePlan { override val requires = $($(DevolveSkip)) }
case object DevolveThreatOfCapture extends DevolvePlan("...threat of capture") with DevolveThreat
case object DevolveThreatOfZingaya extends DevolvePlan("...threat of Zingaya") with DevolveThreat
case object DevolveThreatOfBeyondOne extends DevolvePlan("...threat of Beyond One") with DevolveThreat
case object DevolveThreatOfAttackOnGate extends DevolvePlan("...credible threat to the controlled gate") with DevolveThreat
case object DevolveThreatOfAttackOnGOO extends DevolvePlan("...credible threat of battle against GOO") with DevolveThreat


sealed abstract class DragonAscendingPlan(val label : String) extends Plan {
    val group = "Dragon Ascending".styled(OW)
}
case object DragonAscendingPrompt extends DragonAscendingPlan("Always prompt") with DefaultPlan with OneOfPlan
case object DragonAscendingSkip extends DragonAscendingPlan("Skip this action phase, unless...") with OneOfPlan { override val followers = $(DragonAscendingPowerPlus2) }
trait DragonAscendingPower extends DragonAscendingPlan with OnlyOnPlan {
    override val requires = $($(DragonAscendingSkip))
    override def unfollowers = $(DragonAscendingPowerPlus2, DragonAscendingPowerPlus3, DragonAscendingPowerPlus5, DragonAscendingPowerPlus7, DragonAscendingPowerPlus9).diff($(this))
    val power : Int
}
case object DragonAscendingPowerPlus2 extends DragonAscendingPlan("...to gain 2+ power") with DragonAscendingPower { val power = 2 }
case object DragonAscendingPowerPlus3 extends DragonAscendingPlan("...to gain 3+ power") with DragonAscendingPower { val power = 3 }
case object DragonAscendingPowerPlus5 extends DragonAscendingPlan("...to gain 5+ power") with DragonAscendingPower { val power = 5 }
case object DragonAscendingPowerPlus7 extends DragonAscendingPlan("...to gain 7+ power") with DragonAscendingPower { val power = 7 }
case object DragonAscendingPowerPlus9 extends DragonAscendingPlan("...to gain 9+ power") with DragonAscendingPower { val power = 9 }


sealed abstract class UnspeakableOathPlan(val label : String) extends Plan {
    val group = "Unspeakable Oath".hh
}
case object UnspeakableOathImmediately extends UnspeakableOathPlan("Queue Activation") with OneOfPlan {
    override val unselected = "§".styled("pain") + " " + label + " " + "§".styled("pain")
    override val selected = "§".styled("highlight") + " " + label.styled("kill") + " " + "§".styled("highlight")
    override val info = selected
}
case object UnspeakableOathPrompt extends UnspeakableOathPlan("Always prompt") with DefaultPlan with OneOfPlan
case object UnspeakableOathSkip extends UnspeakableOathPlan("Skip, unless...") with OneOfPlan { override val followers = $(UnspeakableOathThreatOfHPCapture, UnspeakableOathThreatOfAcolyteCapture, UnspeakableOathThreatOfAttackOnHighPriest, UnspeakableOathOpportunityEndOfPhase) }
trait UnspeakableThreat extends UnspeakableOathPlan { override val requires = $($(UnspeakableOathSkip)) }
case object UnspeakableOathThreatOfHPCapture extends UnspeakableOathPlan("...threat of High Priest capture") with UnspeakableThreat
case object UnspeakableOathThreatOfGhroth extends UnspeakableOathPlan("...threat of Ghroth eliminating High Priest") with UnspeakableThreat
case object UnspeakableOathThreatOfThousandForms extends UnspeakableOathPlan("...threat of unopposed Thousand Forms") with UnspeakableThreat
case object UnspeakableOathThreatOfDryEternal extends UnspeakableOathPlan("...threat of battle again Rhan-Tegoth with no power") with UnspeakableThreat
case object UnspeakableOathOpportunityOfDreadCurse extends UnspeakableOathPlan("...opportunity for Dread Curse") with UnspeakableThreat
case object UnspeakableOathOpportunityEndOfPhase extends UnspeakableOathPlan("...end of Action Phase") with UnspeakableThreat
case object UnspeakableOathOpportunityFirstPlayer extends UnspeakableOathPlan("...become eligible First Player") with UnspeakableThreat

case object UnspeakableOathThreatOfAcolyteCapture extends UnspeakableOathPlan("...threat of Acolyte capture") with UnspeakableThreat
case object UnspeakableOathThreatOfAttackOnHighPriest extends UnspeakableOathPlan("...credible threat of High Priest being killed") with UnspeakableThreat
case object UnspeakableOathThreatOfAttackOnGate extends UnspeakableOathPlan("...credible threat to the controlled gate") with UnspeakableThreat
case object UnspeakableOathThreatOfAttackOnGOO extends UnspeakableOathPlan("...credible threat of battle against GOO") with UnspeakableThreat


class Player(private val f : Faction)(implicit game : Game) {
    var gates : $[Region] = $
    var abandoned : $[Region] = $

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

    def clings = options.has(GateDiplomacy).not || commands.has(GateDiplomacyCling)

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

    var active = true

    var plans : $[Plan] = $
    var commands : $[Plan] = $

    def allGates = gates ++ unitGate./(_.region).$
    def needs(rq : Requirement) = unfulfilled.contains(rq)
    def has(sb : Spellbook) = f.abilities.contains(sb) || spellbooks.contains(sb) || upgrades.contains(sb) || borrowed.contains(sb)
    def used(sb : Spellbook) = oncePerGame.contains(sb) || oncePerTurn.contains(sb) || oncePerRound.contains(sb) || oncePerAction.contains(sb)
    def can(sb : Spellbook) = has(sb) && !used(sb)
    def ignored(sb : Spellbook) = ignorePerGame.contains(sb) || ignorePerTurn.contains(sb) || ignorePerInstant.contains(sb)
    def want(sb : Spellbook) = can(sb) && !ignored(sb)
    def hasAllSB = unfulfilled.none
    def unclaimedSB = f.library.num - spellbooks.num - unfulfilled.num
    def present(region : Region) = units.exists(_.region == region)
    def at(region : Region) = units.%(_.region == region)
    def at(region : Region, uclass : UnitClass) = units.%(_.region == region).%(_.uclass == uclass)
    def at(region : Region, utype : UnitType) = units.%(_.region == region).%(_.uclass.utype == utype)
    def at(region : Region, utype : UnitType, utype2 : UnitType) = units.%(_.region == region).%(u => u.uclass.utype == utype || u.uclass.utype == utype2)
    def pool = units.%(_.region == f.reserve)
    def onMap(uclass : UnitClass) = units.%(u => u.region.onMap && u.uclass == uclass)
    def onMap(utype : UnitType) = units.%(u => u.region.onMap && u.uclass.utype == utype)
    def all(uclass : UnitClass) = units.%(u => u.region.inPlay && u.uclass == uclass)
    def all(utype : UnitType) = units.%(u => u.region.inPlay && u.uclass.utype == utype)
    def allInPlay = units.%(_.region.inPlay)
    def goos = units.%(_.region.inPlay).%(_.uclass.utype == GOO)
    def cultists = units.%(_.region.inPlay).%(_.uclass.utype == Cultist)
    def acolytes = units.%(_.region.inPlay).%(_.uclass == Acolyte)

    def goo(uclass : UnitClass) = all(uclass).single.get
    def has(uclass : UnitClass) = all(uclass).any

    def satisfy(rq : Requirement, text : String, es : Int = 0) {
        if (f.needs(rq)) {
            f.unfulfilled = f.unfulfilled.but(rq)
            f.log("achieved", text.styled(f), ((es + rq.es) > 0).??("and gained " + (es + rq.es).es))
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
            val movedHere = f.at(r).tag(Moved).any
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
            f.log("lost", s.tax.power, "due to", s.list./(IceAge.styled).mkString(", "))
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
    def elem = list./(e => " (" + IceAge.styled(e) + ")").mkString("")
    override def toString = elem
}

object NoIceAges extends IceAges($)


sealed trait GameOption extends Record

case object QuickGame extends GameOption

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
        QuickGame,
        GateDiplomacy,
        AsyncActions,
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

    def triggers()(implicit game : Game) : Unit = {}

    def eliminate(u : UnitFigure)(implicit game : Game) : Unit = {}

    def afterAction()(implicit game : Game) : Unit = {}

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}


case object StartAction extends ForcedAction
case object SetupFactionsAction extends ForcedAction
case class CheckSpellbooksAction(then : ForcedAction) extends ForcedAction
case object AfterPowerGatherAction extends ForcedAction
case class BeforeFirstPlayerAction(l : $[Faction]) extends ForcedAction
case object FirstPlayerDeterminationAction extends ForcedAction
case object PlayOrderAction extends ForcedAction
case class PowerGatherAction(then : Faction) extends ForcedAction
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
    def question(implicit game : Game) = game.nexed.some./(n => "" + EnergyNexus + " in " + n.mkString(", ")).|("" + self + (self.acted || self.battled.any).??(" unlimited") + " action") + " (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
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

case class SpellbookAction(self : Faction, sb : Spellbook, then : ForcedAction) extends BaseFactionAction(implicit g => (self.unclaimedSB == 1).?("Receive spellbook").|("Receive " + self.unclaimedSB + " spellbooks"), {
    val p = s""""${self.short}", "${sb.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    val qm = Overlays.imageSource("question-mark")
    "<div class=sbdiv>" +
        sb +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
})

case class ElderSignAction(f : Faction, n : Int, value : Int, public : Boolean, then : ForcedAction) extends ForcedAction

case class DoomAction(faction : Faction) extends ForcedAction
case class DoomNextPlayerAction(faction : Faction) extends ForcedAction
case class DoomDoneAction(self : Faction) extends BaseFactionAction(" ", "Done".styled("power")) with PowerNeutral

case class PreMainAction(faction : Faction) extends ForcedAction
case class PreActionPromptsAction(faction : Faction, l : $[Faction]) extends ForcedAction
case class MainGatesAction(faction : Faction) extends ForcedAction
case class MainAction(faction : Faction) extends ForcedAction

case class EndPhasePromptsAction(last : Faction, l : $[Faction]) extends ForcedAction


case class OutOfTurnRefresh(action : Action) extends HiddenAction

case class EndAction(self : Faction) extends ForcedAction
case class AfterAction(self : Faction) extends ForcedAction

case object ProceedBattlesAction extends ForcedAction

case class EndTurnAction(self : Faction) extends BaseFactionAction(" ", "Done".styled("power")) with PowerNeutral
case class NextPlayerAction(faction : Faction) extends ForcedAction

case class AdjustGateControlAction(self : Faction, changed : Boolean, then : ForcedAction) extends OptionFactionAction("Control gates") with MainQuestion with Soft
case class ControlGateAction(self : Faction, r : Region, u : UnitRef, then : ForcedAction) extends ForcedAction with NoClear
case class AbandonGateAction(self : Faction, r : Region, then : ForcedAction) extends ForcedAction with NoClear

case class RitualAction(self : Faction, cost : Int, k : Int) extends OptionFactionAction(g => "Perform " + "Ritual of Annihilation".styled("doom") + " for " + cost.power) with DoomQuestion

case class RevealESMainAction(self : Faction, then : ForcedAction) extends BaseFactionAction("", "View " + "Elder Signs".styled("es")) with Soft with PowerNeutral
case class RevealESOutOfTurnAction(self : Faction) extends BaseFactionAction("Elder Signs", "View " + "Elder Signs".styled("es")) with Soft
case class InfoESOutOfTurnAction(self : Faction) extends BaseFactionAction("Elder Signs", "View " + "Elder Signs".styled("es")) with Soft
case class RevealESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(implicit g => "Elder Signs".styled("es") + " " + self.es./(_.short).mkString(" "), implicit g => (es.%(_.value > 0).num == 1).?("Reveal " + es(0).short).|("Reveal all for " + self.es./(_.value).sum.doom)) with OutOfTurn
case class InfoESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(implicit g => "Elder Signs".styled("es") + " " + self.es./(_.short).mkString(" "), implicit g => (es.%(_.value > 0).num == 1).?("Reveal " + es(0).short).|("Reveal all for " + self.es./(_.value).sum.doom)) with Info

case class PassAction(self : Faction) extends OptionFactionAction("Pass and lose remaining power") with MainQuestion

case class MoveMainAction(self : Faction) extends OptionFactionAction("Move") with MainQuestion with Soft
case class MoveContinueAction(self : Faction, moved : Boolean) extends ForcedAction with Soft
case class MoveSelectAction(self : Faction, u : UnitRef, r : Region, cost : Int) extends ForcedAction with Soft
case class MoveAction(self : Faction, u : UnitRef, from : Region, to : Region, cost : Int) extends ForcedAction
case class MovedAction(self : Faction, u : UnitRef, from : Region, to : Region) extends ForcedAction
case class MoveDoneAction(self : Faction) extends ForcedAction

case class AttackMainAction(self : Faction, l : $[Region], effect : |[Spellbook]) extends OptionFactionAction("Battle") with MainQuestion with Soft
case class AttackAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends BaseFactionAction(implicit g => "Battle in " + r + effect./(" with " + _).|("") + self.iced(r), f)

case class BuildGateMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Build Gate") with MainQuestion with Soft
case class BuildGateAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Build gate" + g.forNPowerWithTax(r, self, 3 - self.has(UmrAtTawil).??(1)) + " in", r)

case class CaptureMainAction(self : Faction, l : $[Region], effect : |[Spellbook]) extends OptionFactionAction("Capture") with MainQuestion with Soft
case class CaptureAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends ForcedAction
case class CaptureTargetAction(self : Faction, r : Region, f : Faction, ur : UnitRef, effect : |[Spellbook]) extends ForcedAction

case class RecruitMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Recruit " + uc.styled(self)) with MainQuestion with Soft
case class RecruitAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Recruit " + uc.styled(self) + g.forNPowerWithTax(r, self, self.recruitCost(uc, r)) + " in", implicit g => r + self.iced(r))

case class SummonMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Summon " + uc.styled(self)) with MainQuestion with Soft
case class SummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Summon " + uc.styled(self) + g.forNPowerWithTax(r, self, self.summonCost(uc, r)) + " in", implicit g => r + self.iced(r))
case class SummonedAction(self : Faction, uc : UnitClass, r : Region, l : $[Region]) extends ForcedAction

case class AwakenMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Awaken " + uc.styled(self)) with MainQuestion with Soft
case class AwakenAction(self : Faction, uc : UnitClass, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + uc.styled(self) + g.forNPowerWithTax(r, self, cost) + " in", implicit g => r + self.iced(r))
case class AwakenedAction(self : Faction, uc : UnitClass, r : Region, cost : Int) extends ForcedAction

case class Offer(f : Faction, n : Int)

case class SacrificeHighPriestDoomAction(self : Faction) extends OptionFactionAction("Sacrifice " + "High Priest".styled(self)) with DoomQuestion with Soft with PowerNeutral
case class SacrificeHighPriestMainAction(self : Faction) extends OptionFactionAction("Sacrifice " + "High Priest".styled(self)) with MainQuestion with Soft
case class SacrificeHighPriestPromptAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class SacrificeHighPriestAction(self : Faction, r : Region, then : ForcedAction) extends BaseFactionAction("Sacrifice to gain " + 2.power, HighPriest.styled(self) + " in " + r)

case object SacrificeHighPriestAllowedAction extends HiddenAction
case class SacrificeHighPriestOutOfTurnMainAction(self : Faction) extends BaseFactionAction("Unspeakable Oath".hl, "Sacrifice " + "High Priest".styled(self)) with Soft

case class CommandsMainAction(self : Faction) extends BaseFactionAction("  ", "Commands".styled(self)) with OutOfTurn with Soft
case class CommandsAddAction(self : Faction, plan : Plan) extends BaseFactionAction(plan.group, plan.unselected) with OutOfTurn with NoClear
case class CommandsRemoveAction(self : Faction, plan : Plan) extends BaseFactionAction(plan.group, plan.selected) with OutOfTurn with NoClear
case class CommandsInfoAction(self : Faction, plan : Plan) extends BaseFactionAction(plan.group, plan.unselected) with Info


class Game(val board : Board, val ritualTrack : $[Int], val setup : $[Faction], val logging : Boolean, val providedOptions : $[GameOption]) extends Expansion {
    private implicit val game : Game = this

    val options = providedOptions ++ $(PlayerCount(setup.num))

    var expansions : $[Expansion] =
        setup./~ {
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

    val players = setup./(f => f -> new Player(f)).toMap

    val noPlayer = new Player(NoFaction)

    var neutrals : Map[NeutralFaction, Player] = Map()
    def factionlike = setup ++ neutrals.keys

    var starting = Map[Faction, Region]()
    var turn = 1
    var round = 1
    var doomPhase = false
    var factions : $[Faction] = $
    var first : Faction = setup.first
    var gates : $[Region] = $
    def allGates = gates ++ factions./~(_.unitGate)./(_.region)
    var cathedrals : $[Region] = $
    var desecrated : $[Region] = $
    var ritualMarker = 0
    var battle : |[Battle] = None
    var nexed : $[Region] = $
    var queue : $[Battle] = $
    var anyIceAge : Boolean = false
    var lastDaolothRegion : |[Region] = None
    var neutralSpellbooks : $[Spellbook] = options.contains(NeutralSpellbooks).$(MaoCeremony, Recriminations, Shriveling, StarsAreRight, UmrAtTawil, Undimensioned)
    var loyaltyCards : $[LoyaltyCard] = options.of[LoyaltyCardGameOption]./(_.lc)

    // Solution for keeping track of use cases for dematerialization, for the AN bot.
    var demCaseMap : Map[Region, Int] = areas.map(r => r -> 0).toMap

    def forNPowerWithTax(r : Region, f : Faction, n : Int) : String = { val p = n + f.taxIn(r) ; " for " + p.power }
    def for1PowerWithTax(r : Region, f : Faction) : String = { val p = 1 + f.taxIn(r) ; if (p != 1) " for " + p.power else "" }

    def unit(ur : UnitRef) = ur.faction.units.%(u => u.uclass == ur.uclass && u.index == ur.index).only

    def desc(x : Any) : String = x @@ {
        case s : String => s
        case r : Region => r.toString
        case f : Faction => f.full
        case b : Spellbook => b.elem
        case |(n) => desc(n)
        case None => ""
        case u : UnitFigure => u.short
        case ur : UnitRef => unit(ur).short
        case ur : UnitRefShort => unit(ur.r).short
        case ur : UnitRefFull => unit(ur.r).full
        case x => x.toString
    }

    val undoubled = $(CthulhuWarsSolo.DottedLine, CthulhuWarsSolo.DoubleLine)
    var mlog : $[String] = $
    var last : |[String] = None
    var pendingLine : |[String] = None

    def appendLog(args : $[Any]) {
        if (logging) {
            val line = args./(desc).mkString(" ")

            if (undoubled.has(line).not && pendingLine.any)
                mlog = pendingLine.get +: mlog

            pendingLine = None

            if (last.has(line).not || undoubled.has(line).not)
                mlog = line +: mlog

            last = |(line)
        }
    }

    def ritualCost = min(10, ritualTrack(ritualMarker))

    def abandonedGates = gates.%(g => factions.exists(_.gates.has(g)).not)

    def eliminate(u : UnitFigure) {
        val f = u.faction

        u.add(Eliminated)

        expansions.foreach(_.eliminate(u))

        if (u.tag(Eliminated)) {
            u.region = u.faction.reserve
            u.onGate = false
            u.state = $
            u.health = Alive

            if (u.uclass == HighPriest && f.all(HighPriest).none) {
                f.plans = f.plans.notOf[UnspeakableOathPlan]
                // !! // f.commands = f.commands.notOf[UnspeakableOathPlan]

                f.plans = f.plans.notOf[HighPriestGatesPlan]
                // !! // f.commands = f.commands.notOf[HighPriestGatesPlan]
            }
        }
    }

    def compareUnitsActive(a : UnitFigure, b : UnitFigure) : Boolean = {
        if (a.uclass.priority != b.uclass.priority)
            a.uclass.priority > b.uclass.priority
        else
        if (a.onGate != b.onGate)
            a.onGate < b.onGate
        else
            areas.indexOf(a.region) < areas.indexOf(b.region)
    }

    def compareUnitsPassive(a : UnitFigure, b : UnitFigure) : Boolean = {
        if (a.uclass.priority != b.uclass.priority)
            a.uclass.priority < b.uclass.priority
        else
        if (a.onGate != b.onGate)
            a.onGate < b.onGate
        else
            areas.indexOf(a.region) < areas.indexOf(b.region)
    }

    def regionStatus(r : Region) : $[String] = {
        val gate = gates.contains(r)
        val cathedral = cathedrals.contains(r)
        val ds = desecrated.contains(r)
        val controler = factions.%(f => f.gates.contains(r)).single
        val keeper = controler./~(f => f.at(r).%(_.health == Alive).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && f.has(RedSign))).starting)
        val others = factions.%(f => !f.gates.contains(r)).%(_.at(r).num > 0).sortBy(f => f.strength(f.at(r), f))
        if (gate || !others.none || ds) {
            $("" + r + ":" + gate.??(" " + keeper./(u => ("[[[".styled(u.faction.style) + " " + u + " " + "]]]".styled(u.faction.style))).|("[[[ GATE ]]]".styled("power"))) + ds.??(" " + ")|(".styled(YS))) ++
            controler./(f => "    " + f.at(r).diff(keeper.$)./(u => u.short).mkString(", ")).$ ++
            others.sortBy(_.units.%(_.region == r).num)./ { f =>  "    " + f.at(r)./(u => u.short).mkString(", ") } ++ $("&nbsp;")
        }
        else
            $
    }

    def direction(from : Region, to : Region) = {
        val (ax, ay) = board.gateXYO(from)
        val (bx, by) = board.gateXYO(to)

        val shift = board.width * 13 / 11

        val (cx, cy) = ($(bx - shift, bx, bx + shift).minBy(x => (ax - x).abs), by)

        (((180 - math.atan2(cx - ax, 2*(cy - ay)) / math.Pi * 180) / 15).round * 15).toInt % 360
    }

    def showROAT() {
        def vv(v : Int) = (v == 999).?("Instant Death").|(v)
        log("Ritual of Annihilation".styled("doom"), "track", ritualTrack.zipWithIndex./{ case (v, n) => (n == ritualMarker).?(("[" + vv(v) + "]").styled("str")).|("[".styled("xxxhighlight") + vv(v) + "]".styled("xxxhighlight")) }.mkString("-".styled("highlight")))
    }

    def checkGatesLost() {
        factions.foreach { f =>
            f.gates.foreach { r =>
                if (f.at(r).%(_.onGate).none) {
                    f.gates :-= r
                    f.log("lost control of the gate in", r)
                }
            }
        }
    }

    def triggers() {
        expansions.foreach(_.triggers())
    }

    def checkGatesGained(self : Faction) {
        checkGatesLost()

        triggers()

        gates.nex.foreach { r =>
            if (self.abandoned.has(r).not) {
                if (factions.%(_.gates.has(r)).none) {
                    self.at(r).%(_.canControlGate).sortBy(_.uclass @@ {
                        case DarkYoung => 1
                        case Acolyte => 2
                        case HighPriest => 3
                    }).starting.foreach { u =>
                        self.gates :+= r
                        u.onGate = true

                        if (self.oncePerAction.has(UmrAtTawil).not)
                            self.log("gained control of the gate in", r)
                    }
                }
            }
        }

        triggers()
    }

    def extraActions(f : Faction, outOfTurn : Boolean, highPriests : Boolean) : $[Action] = {
        (f.can(DragonAscending) && f.power < f.enemies./(_.power).max && (outOfTurn.not || options.has(AsyncActions))).$(DragonAscendingOutOfTurnAction(f.sure[OW])) ++
        f.es.exists(_.value > 0).$((outOfTurn && options.has(AsyncActions).not).?(InfoESOutOfTurnAction(f)).|(RevealESOutOfTurnAction(f))) ++
        (options.has(AsyncActions) || outOfTurn.not).??(
            (highPriests && f.all(HighPriest).any).$(SacrificeHighPriestOutOfTurnMainAction(f)) ++
            f.plans.%(f.commands.has)./(p => Info(p.info)(p.group)) ++
            f.plans.any.$(CommandsMainAction(f))
        )
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

        if (action == OutOfTurnDone)
            return continue match {
                case MultiAsk(aa) if aa.exists(_.actions.of[OutOfTurnRefresh].any) => internalPerform(aa./~(_.actions.of[OutOfTurnRefresh]).distinct.only.action, NoVoid)
                case Ask(f, l) if l.of[OutOfTurnRefresh].any => internalPerform(l.of[OutOfTurnRefresh].only.action, NoVoid)
                case c => c
            }

        internalPerform(action, NoVoid) match {
            case Force(a) => internalPerform(a, NoVoid)
            case c =>
                if (action.isRecorded && action.is[OutOfTurn].not)
                    continue = c

                c
        }
    }

    def internalPerform(action : Action, soft : VoidGuard) : Continue = {
        if (action == OutOfTurnReturn)
            return continue match {
                case MultiAsk(aa) if aa.exists(_.actions.of[OutOfTurnRefresh].any) => internalPerform(aa./~(_.actions.of[OutOfTurnRefresh]).distinct.only.action, NoVoid)
                case Ask(f, l) if l.of[OutOfTurnRefresh].any => internalPerform(l.of[OutOfTurnRefresh].only.action, NoVoid)
                case c => c
            }

        expansions.foreach { e =>
            e.perform(action, soft) @@ {
                case UnknownContinue =>
                case Force(OutOfTurnReturn) => return Force(OutOfTurnReturn)
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

    def controls(f : Faction)(implicit w : AskWrapper) {
        if (gates.nex.exists(r => factions.%(_.gates.has(r)).none && f.at(r).exists(_.canControlGate))
            || f.gates.nex.exists(r => f.at(r).%(_.canControlGate)./(_.uclass).distinct.diff(f.commands.has(HighPriestGatesSkip).$(HighPriest)).num > 1)
            || (f.commands.has(GateDiplomacyPrompt) && f.gates.nex.any)
        )
            + AdjustGateControlAction(f, false, MainAction(f))
    }

    def battles(f : Faction)(implicit w : AskWrapper) {
        val enough = nexed.any.?(queue.%(_.attacker == f).%(_.effect.has(EnergyNexus))./(_.arena)).|(f.battled)

        areas.nex.%(f.affords(1)).diff(enough).%(r => factionlike.but(f).exists(f.canAttack(r))).some.foreach { r =>
            + AttackMainAction(f, r, nexed.any.?(EnergyNexus))
        }
    }

    def moves(f : Faction)(implicit w : AskWrapper) {
        if (f.units.nex.onMap.not(Moved).%(_.canMove).any && f.power > 0)
            + MoveMainAction(f)
    }

    def captures(f : Faction)(implicit w : AskWrapper) {
        areas.nex.%(f.affords(1)).%(r => factionlike.but(f).%(f.canCapture(r)).any).some.foreach { l =>
            + CaptureMainAction(f, l, None)
        }
    }

    def recruits(f : Faction)(implicit w : AskWrapper) {
        f.pool.cultists./(_.uclass).distinct.reverse.foreach { uc =>
            areas.%(f.present).some.|(areas).nex.%(r => f.affords(f.recruitCost(uc, r))(r)).some.foreach { l =>
                + RecruitMainAction(f, uc, l)
            }
        }
    }

    def builds(f : Faction)(implicit w : AskWrapper) {
        areas.nex.%(f.affords(3 - f.has(UmrAtTawil).??(1))).%!(gates.has).%(r => f.at(r).%(_.canControlGate).any).some.foreach { r =>
            + BuildGateMainAction(f, r)
        }
    }

    def summons(f : Faction)(implicit w : AskWrapper) {
        f.pool.monsterly.sortP./(_.uclass).distinct.%(_.canBeSummoned(f)).foreach { uc =>
            areas.nex.%(r => f.affords(f.summonCost(uc, r))(r)).%(f.canAccessGate).some.foreach { l =>
                + SummonMainAction(f, uc, l)
            }
        }

        if (f.has(Abhoth) && f.pool(Filth).any) {
            areas.nex.%(r => f.affords(f.summonCost(Filth, r))(r)).some.foreach { l =>
                + FilthMainAction(f, l)
            }
        }
    }

    def awakens(f : Faction)(implicit w : AskWrapper) {
        f.pool.goos.factionGOOs./(_.uclass).distinct.reverse.foreach { uc =>
            areas.nex.%(r => f.affords(f.awakenCost(uc, r).|(999))(r)).some.foreach { l =>
                + AwakenMainAction(f, uc, l)
            }
        }
    }

    def independents(f : Faction)(implicit w : AskWrapper) {
        loyaltyCards.of[IGOOLoyaltyCard].%(_.power <= f.power).foreach { igoo =>
            areas.nex.%(f.canAwakenIGOO).%(f.affords(igoo.power)).some.foreach { gates =>
                + IndependentGOOMainAction(f, igoo, gates)
            }
        }

        if (f.has(NightmareWeb) && f.pool(Nyogtha).any) {
            areas.nex.%(f.affords(2)).%(f.present).some.foreach { l =>
                + NightmareWebMainAction(f, l)
            }
        }

        if (f.has(GodOfForgetfulness) && f.has(Byatis)) {
            $(f.goo(Byatis).region).nex.%(f.affords(1)).foreach { br =>
                board.connected(br).%(r => factionlike.but(f).exists(_.at(r).cultists.any)).some.foreach { l =>
                    + GodOfForgetfulnessMainAction(f, br, l)
                }
            }
        }
    }

    def neutralSpellbooks(f : Faction)(implicit w : AskWrapper) {
        if (f.has(Undimensioned) && f.units.%(_.region.glyph.onMap)./(_.region).distinct.num > 1 && f.units.%(_.region.glyph.onMap)./(_.region).%(f.affords(2)).any)
            + UndimensionedMainAction(f)

        if (f.has(Recriminations))
            + RecriminationsMainAction(f)
    }

    def highPriests(f : Faction)(implicit w : AskWrapper) {
        if (f.all(HighPriest).any)
            if (doomPhase)
                + SacrificeHighPriestDoomAction(f)
            else
                + SacrificeHighPriestMainAction(f)
    }

    def reveals(f : Faction)(implicit w : AskWrapper) {
        if (f.es.exists(_.value > 0) && ((doomPhase && f.has(StarsAreRight)) || f.doom + f.es./(_.value).sum >= 30))
            + RevealESMainAction(f, doomPhase.?(DoomAction(f)).|(PreMainAction(f)))
    }

    def endTurn(f : Faction)(end : Boolean)(implicit w : AskWrapper) {
        if (end)
            + EndTurnAction(f)
        else
            + PassAction(f)

        + SacrificeHighPriestAllowedAction

        + OutOfTurnRefresh(PreMainAction(f))
    }

    def rituals(f : Faction)(implicit w : AskWrapper) {
        val cost = f.has(Herald).?(5).|(ritualCost)

        if (f.power >= cost && f.acted.not)
            + RitualAction(f, cost, 1)

        + SacrificeHighPriestAllowedAction

        + OutOfTurnRefresh(DoomAction(f))
    }

    def hires(f : Faction)(implicit w : AskWrapper) {
        if (f.hired.not && game.loyaltyCards.%(_.doom > 0).exists(c => f.doom >= c.doom && f.power >= c.power))
            + LoyaltyCardDoomAction(f)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {
        // INIT
        case StartAction =>
            log("Options", options./(_.toString.hh).mkString(" "))

            if (options.has(GateDiplomacy)) {
                setup.foreach { f =>
                    f.plans ++= $(
                        GateDiplomacyPrompt,
                        GateDiplomacySkipAbandon,
                        GateDiplomacyCling,
                    )

                    if (options.has(QuickGame))
                        f.commands :+= GateDiplomacyCling
                    else
                        f.commands :+= GateDiplomacyPrompt
                }
            }

            if (options.has(HighPriests)) {
                setup.foreach { f =>
                    f.loyaltyCards = f.loyaltyCards :+ HighPriestCard

                    f.units :+= new UnitFigure(f, HighPriest, 1, f.reserve)
                }
            }

            SetupFactionsAction

        case SetupFactionsAction if setup.forall(starting.contains) =>
            PlayOrderAction

        case SetupFactionsAction =>
            val f = setup.%!(starting.contains).minBy(board.starting(_).num)

            /*
            if (f == CC)
            areas.foreach { r =>
                var destinations = board.connected(r)

                destinations = destinations ++ destinations.flatMap(board.connected)

                destinations = destinations.distinct//.but(region).diff(destinations)

                log("From " + r, "cant fly to " + areas.diff(destinations).mkString(", "))
            }
            */

            Ask(f).each(board.starting(f).diff(starting.values.$))(r => StartingRegionAction(f, r).as(r)(f, "starts in"))

        case StartingRegionAction(f, r) =>
            starting += f -> r

            1.to(6).foreach(_ => f.place(Acolyte, r))

            f.at(r).one(Acolyte).onGate = true

            // Temp starting setup (for debug)
            // if (f.has(Immortal)) {
            //     f.place(Cthulhu, r)
            //     f.satisfy(FirstDoomPhase, "Debug")
            //     f.satisfy(KillDevour1, "Debug")
            //     f.satisfy(KillDevour2, "Debug")
            //     f.satisfy(AwakenCthulhu, "Debug")
            //     f.satisfy(OceanGates, "Debug")
            //     f.satisfy(FiveSpellbooks, "Debug")
            // }

            gates :+= r

            f.gates :+= r

            f.log("started in", r)

            SetupFactionsAction

        case PowerGatherAction(last) if factions.%!(_.hibernating).%(_.power > 0).any =>
            factions.foreach { f =>
                f.active = f.power > 0 && f.hibernating.not
            }

            PreMainAction(last)

        case PowerGatherAction(last) =>
            turn += 1

            factions.foreach { f =>
                f.oncePerTurn = $
                f.ignorePerTurn = $
            }

            log(CthulhuWarsSolo.DoubleLine)
            log("POWER GATHER")

            factions.foreach { f =>
                val hibernate = f.power
                val cultists = f.cultists.num
                val captured = factions./~(w => w.at(f.prison)).num
                val ownGates = f.gates.num + f.unitGate.any.??(1)
                val oceanGates = (f.has(YhaNthlei) && f.has(Cthulhu)).??(f.enemies./(f => f.allGates.%(_.glyph == Ocean).num).sum)
                val darkYoungs = f.has(RedSign).??(f.all(DarkYoung).num)
                val feast = f.has(Feast).??(desecrated.%(r => f.at(r).any).num)
                val abandoned = abandonedGates.num
                var worship = 0

                if (f.has(WorshipServices))
                    f.enemies.foreach { e =>
                        areas.%(cathedrals.contains).%(e.gates.has).some.foreach { l => worship += l.num }
                    }
                else
                if (factions.%(_.has(WorshipServices)).num > 0)
                    areas.%(cathedrals.contains).%(f.gates.has).some.foreach { l => worship += l.num }

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

                f.active = true
            }

            triggers()

            AfterPowerGatherAction // Then(...)

        case AfterPowerGatherAction =>
            factions.foreach { f =>
                if (f.want(MaoCeremony)) {
                    f.cultists.onMap.some.foreach { l =>
                        return Ask(f).each(l)(c => MaoCeremonyAction(f, c.region, c.uclass)).add(MaoCeremonyDoneAction(f))
                    }
                }
            }

            factions.foreach(_.ignorePerInstant = $)

            BeforeFirstPlayerAction(factions)

        case BeforeFirstPlayerAction(l) =>
            val max = factions./(_.power).max
            val asks = l./~{ f =>
                implicit val asking = Asking(f)

                + GroupAction("Before First Player determination")

                if (f.power + 2 >= max && f.power <= f.enemies./(_.power).max)
                    if (f.onMap(HighPriest).any)
                        if (f.commands.has(UnspeakableOathOpportunityFirstPlayer) || f.commands.has(UnspeakableOathPrompt))
                            + SacrificeHighPriestPromptAction(f, BeforeFirstPlayerAction(factions)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl)

                f << [OW] { f =>
                    if (f.can(DragonAscending) && f.power < max)
                        + DragonAscendingAction(f, None, None, max, BeforeFirstPlayerAction(factions))
                }

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./{ _
                    .add(NeedOk)
                    .add(OutOfTurnRefresh(BeforeFirstPlayerAction(l)))
                    .add(SacrificeHighPriestAllowedAction)
                    .group(" ")
                    .skip(BeforeFirstPlayerAction(l.but(f)))
                }
            }

            if (asks.any)
                MultiAsk(asks)
            else
                FirstPlayerDeterminationAction // Then(...)

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
                    if (factionlike.but(f).exists(_.present(r)).not) {
                        f.log("gained", 1.es, "from", Byatis.styled(f), "and", ToadOfBerkeley)
                        f.takeES(1)
                    }
                }
            }

            log(CthulhuWarsSolo.DottedLine)
            showROAT()

            CheckSpellbooksAction(DoomNextPlayerAction(game.first))

        case ActionPhaseAction =>
            if (factions.%(_.doom >= 30).any || ritualTrack(ritualMarker) == 999)
                return GameOverPhaseAction

            doomPhase = false

            log(CthulhuWarsSolo.DoubleLine)
            log("Turn", turn)
            log("ACTIONS")

            round = 0

            CheckSpellbooksAction(MainGatesAction(game.first))

        case GameOverPhaseAction =>
            factions.%(_.needs(AnytimeGainElderSigns)).foreach { f =>
                f.satisfy(AnytimeGainElderSigns, "Anytime Spellbook", f.enemies.%(_.hasAllSB).num.upTo(3))
                return CheckSpellbooksAction(GameOverPhaseAction)
            }

            factions.%(_.es.any).foreach { f =>
                val sum = f.es./(_.value).sum
                f.log("revealed", f.es./(_.short).mkString(" "), "for", sum.doom)
                f.doom += sum
                f.revealed ++= f.es
                f.es = $
            }

            val contenders = factions.%(_.hasAllSB)
            val winners = contenders.%(_.doom == contenders./(_.doom).max)

            if (winners.none)
                log("Humanity won")
            else
                log(winners.mkString(", "), "won")

            GameOver(winners)

        case FirstPlayerDeterminationAction =>
            val max = factions./(_.power).max
            val fs = factions.%(f => f.power == max)

            if (fs.num > 1) {
                Ask(game.first).each(fs)(f => FirstPlayerAction(game.first, f))
            }
            else {
                val old = game.first

                game.first = fs.only

                if (old != game.first)
                    game.first.log("became the first player")

                PlayOrderAction // Then(...)
            }

        case FirstPlayerAction(f, first) =>
            game.first = first

            if (f == first)
                f.log("decided to remain the first player")
            else
                f.log("chose", first, "as the first player")

            PlayOrderAction // Then(...)

        case PlayOrderAction =>
            game.first.satisfy(FirstPlayer, "Become First Player")

            val forward = setup.dropWhile(_ != game.first) ++ setup.takeWhile(_ != game.first)
            val backward = forward.take(1) ++ forward.drop(1).reverse

            Ask(game.first)
                .add(PlayDirectionAction(game.first, forward))
                .add(PlayDirectionAction(game.first, backward))

        case PlayDirectionAction(_, l) =>
            factions = l

            log("Play order", factions.mkString(", "))

            if (turn == 1)
                ActionPhaseAction // Then(...)
            else {
                log(CthulhuWarsSolo.DottedLine)
                log("DOOM PHASE")

                factions.foreach(f => f.satisfyIf(FirstDoomPhase, "The first Doom phase", turn == 2))
                factions.foreach(f => f.satisfyIf(FiveSpellbooks, "Have five spellbooks", f.unfulfilled.num == 1))

                CheckSpellbooksAction(DoomPhaseAction) // Then(...)
            }

        // SPELLBOOK
        case CheckSpellbooksAction(next) =>
            val fs = factions.%(f => f.unfulfilled.num + f.spellbooks.num < f.library.num)
            val fe = factions.%(f => f.es.%(_.value == 0).any)

            if (fs.any) {
                val f = fs(0)
                val bs = (f.library.%!(f.has) ++ neutralSpellbooks).diff(f.ignorePerInstant)
                Ask(f).each(bs)(b => SpellbookAction(f, b, next))
            }
            else
            if (fe.any) {
                val f = fe(0)
                val n = f.es.%(_.value == 0).num
                val es = factions./~(f => f.es ++ f.revealed)

                DrawES("" + f + " gets " + n.es, 18 - es.%(_.value == 1).num, 12 - es.%(_.value == 2).num, 6 - es.%(_.value == 3).num, (x, public) => ElderSignAction(f, n, x, public, next))
            }
            else {
                Then(next)
            }

        case SpellbookAction(f, sb, next) =>
            f.spellbooks = f.spellbooks :+ sb

            f.log("received", sb)

            neutralSpellbooks = neutralSpellbooks.but(sb)

            if (f.hasAllSB)
                factions.foreach(_.satisfy(AnotherFactionAllSpellbooks, "Another faction has all spellbooks"))

            f.ignorePerInstant = $

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
        case RevealESMainAction(f, then) =>
            Ask(f).each(f.es.%(_.value > 0).any.$($()) ++ f.es.%(_.value > 0)./(e => $(e)))(RevealESAction(f, _ , doomPhase && f.has(StarsAreRight), then)).cancel

        case RevealESOutOfTurnAction(f) =>
            Ask(f).each(f.es.%(_.value > 0).any.$($()) ++ f.es.%(_.value > 0)./(e => $(e)))(RevealESAction(f, _ , doomPhase && f.has(StarsAreRight), OutOfTurnReturn)).cancel

        case InfoESOutOfTurnAction(f) =>
            Ask(f).each(f.es.%(_.value > 0).any.$($()) ++ f.es.%(_.value > 0)./(e => $(e)))(InfoESAction(f, _ , doomPhase && f.has(StarsAreRight), OutOfTurnReturn)).add(NeedOk).cancel

        case RevealESAction(f, Nil, power, next) =>
            Force(RevealESAction(f, f.es.%(_.value > 0), power, next))

        case RevealESAction(f, es, power, next) =>
            val sum = es./(_.value).sum
            f.doom += sum

            f.revealed ++= es
            f.es = f.es.diff(es)

            if (power)
                f.power += sum

            f.log("revealed", es./(_.short).mkString(" "), "for", sum.doom, power.??("and " + sum.power))

            Force(next)

        // DOOM
        case DoomAction(f) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            + DoomDoneAction(f)

            asking

        case DoomDoneAction(f) =>
            f.acted = false

            f.hired = false

            factions = factions.drop(1) ++ factions.take(1)

            val next = factions.first

            if (next != game.first) {
                pendingLine = |(CthulhuWarsSolo.DottedLine)

                DoomNextPlayerAction(next)
            }
            else {
                factions.foreach(_.borrowed = $)
                CheckSpellbooksAction(ActionPhaseAction)
            }

        case DoomNextPlayerAction(f) =>
            DoomAction(f)

        // RITUAL
        case RitualAction(f, cost, k) =>
            f.power -= cost

            val brood = f.enemies.%(_.has(TheBrood))
            val gates = f.allGates
            val valid = gates.%!(r => brood.exists(_.at(r)(Filth).any))

            val doom = valid.num * k

            val es = f.goos.factionGOOs.num + f.has(Consecration).??($(0, 1, 1, 1, 2)(cathedrals.num))

            f.doom += doom

            log(CthulhuWarsSolo.DottedLine)
            f.log("performed the ritual", "for", cost.power, "and gained", doom.doom, (es > 0).??("and " + es.es))

            f.takeES(es)

            f.acted = true

            if (ritualTrack(ritualMarker) != 999)
                ritualMarker += 1

            showROAT()

            f.satisfy(PerformRitual, "Perform Ritual of Annihilation")

            CheckSpellbooksAction(DoomAction(f))

        // MAIN
        case PreMainAction(f) if factions.exists(f => f.unfulfilled.num + f.spellbooks.num < f.spellbooks.num) =>
            CheckSpellbooksAction(PreMainAction(f))

        case PreMainAction(f) if f.active.not && f.hibernating.not =>
            implicit val asking = Asking(f)

            + GroupAction("Before " + f + " turn")

            if (f.all(HighPriest).any) {
                var reasons = f.commands.has(UnspeakableOathPrompt).$("always prompted")

                f.enemies.%(e => e.active || e.all(HighPriest).any).foreach { e =>
                    val canAct = true // e.acted.not
                    val canBattle = true // canAct || e.hasAllSB
                    def canBattleIn(r : Region) = true // canBattle && e.battled.has(r).not

                    if (f.commands.has(UnspeakableOathThreatOfHPCapture) && canAct)
                        f.onMap(HighPriest)./(_.region).distinct.%(r => e.canCapture(r)(f) && f.at(r).acolytes.none).some./{ l =>
                            reasons :+= "" + e + " might capture " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnHighPriest) && canBattle)
                        f.onMap(HighPriest)./(_.region).distinct.%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num).some./{ l =>
                            reasons :+= "" + e + " might kill " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAcolyteCapture) && canAct)
                        f.onMap(Acolyte)./(_.region).distinct.%(r => e.canCapture(r)(f)).some./{ l =>
                            reasons :+= "" + e + " might capture " + Acolyte.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnGate) && canBattle)
                        f.gates.%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num).some./{ l =>
                            reasons :+= "" + e + " might take the gate" + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfGhroth))
                        if (e == BG && e.has(Ghroth) && e.power >= 2 && e.all(Fungi)./(_.region).distinct.num > f.acolytes.%!(_.onGate).num && f.acolytes.%(_.onGate).any)
                            reasons :+= "" + e + " might " + Ghroth + " an " + Acolyte.styled(f) + " on the Gate"

                    if (f.commands.has(UnspeakableOathOpportunityOfDreadCurse))
                        if (f == OW && f.has(DreadCurse) && (f.all(Abomination).any || f.all(SpawnOW).any))
                            if (e.onMap(GOO).exists(u => (f.all(Abomination).num + f.all(SpawnOW).num) / 2 + 1 > e.at(u.region).notGOOs.num))
                                reasons :+= "" + f + " might " + DreadCurse + " a GOO of " + e

                    if (f == GC && canAct)
                        if (e == SL && e.has(CursedSlumber) && e.gates.has(game.starting(f)))
                            reasons :+= "" + e + " might whisk away the gate " + ("from " + game.starting(f)).inline

                    if (f == GC && canAct)
                        if (e == OW && f.at(game.starting(f)).goos.none && e.at(game.starting(f)).%(_.uclass.cost >= 3).any)
                            reasons :+= "" + e + " might whisk away the gate " + ("from " + game.starting(f)).inline

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnGOO) && canBattle)
                        f.onMap(GOO)./(_.region).%(r => canBattleIn(r) && e.canAttack(r)(f) && (e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num || e.at(r).got(Hastur))).some./{ l =>
                            reasons :+= "GOO might be in danger from " + e + " " + ("in " + l.mkString(", ")).inline
                        }
                }

                if (reasons.any)
                    + SacrificeHighPriestPromptAction(f, PreMainAction(f)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl, reasons./("<br/>(" + _ + ")").mkString(""))
            }

            asking.ask.useIf(_.actions.exists(_.isInfo.not))(_.add(NeedOk).add(OutOfTurnRefresh(PreMainAction(f))).add(SacrificeHighPriestAllowedAction).group(" ")).skip(MainAction(f))

        case PreMainAction(f) if f.active && f.power > 0 =>
            PreActionPromptsAction(f, f.enemies)

        case PreActionPromptsAction(e, l) =>
            val canAct = e.acted.not
            val canBattle = canAct || e.hasAllSB
            def canBattleIn(r : Region) = canBattle && e.battled.has(r).not

            val asks = l./~{ f =>
                implicit val asking = Asking(f)

                + GroupAction("Before " + e + " action")

                if (f.onMap(HighPriest).any) {
                    var reasons = f.commands.has(UnspeakableOathPrompt).$("always prompted")

                    if (f.commands.has(UnspeakableOathThreatOfHPCapture) && canAct)
                        f.all(HighPriest)./(_.region).distinct.%(r => e.canCapture(r)(f) && f.at(r).acolytes.none).some./{ l =>
                            reasons :+= "" + e + " might capture " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfGhroth) && canAct)
                        if (e == BG && e.has(Ghroth) && e.power >= 2 && e.all(Fungi)./(_.region).distinct.num > f.acolytes.num)
                            reasons :+= "" + e + " might " + Ghroth + " " + HighPriest.styled(f)

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnHighPriest) && canBattle)
                        f.all(HighPriest)./(_.region).distinct.%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num).some./{ l =>
                            reasons :+= "" + e + " might kill " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfDryEternal) && canBattle)
                        if (f.has(RhanTegoth) && f.power == 0)
                            f.goos./(_.region).%(r => canBattleIn(r) && e.canAttack(r)(f) && (e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num || e.at(r).got(Hastur))).some./{ l =>
                                reasons :+= "" + RhanTegoth + " might not have power for " + Eternal
                            }

                    if (f.commands.has(UnspeakableOathThreatOfThousandForms) && canAct)
                        if (e == CC && e.has(Nyarlathotep) && e.can(ThousandForms) && f.power < 6)
                            reasons :+= "" + e + " might roll for " + ThousandForms + " unopposed"

                    if (reasons.any)
                        + SacrificeHighPriestPromptAction(f, PreMainAction(e)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl, reasons./("<br/>(" + _ + ")").mkString(""))
                }

                if (f.has(Devolve) && f.onMap(Acolyte).any && f.pool(DeepOne).any) {
                    var reasons = f.commands.has(DevolvePrompt).$("always prompted")
                    var areas = f.onMap(Acolyte)./(_.region).distinct

                    if (f.commands.has(DevolveThreatOfCapture) && canAct)
                        areas.%(r => e.canCapture(r)(f)).some./{ l =>
                            reasons :+= "" + e + " might capture " + Acolyte.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(DevolveThreatOfZingaya) && canAct)
                        if (e == YS && e.has(Zingaya) && e.pool(Undead).any)
                            areas.%(r => e.at(r).got(Undead)).some./{ l =>
                                reasons :+= "" + e + " might " + Zingaya + " " + ("in " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(DevolveThreatOfBeyondOne) && canAct)
                        if (e.can(BeyondOne))
                            f.gates.intersect(areas).%(r => f.at(r).goos.none && e.at(r).%(_.uclass.cost >= 3).any).some./{ l =>
                                reasons :+= "" + e + " might " + BeyondOne + " " + ("from " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(DevolveThreatOfAttackOnGate) && canBattle)
                        f.gates.intersect(areas).%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) * 3 / 4 + 1 >= f.at(r).num).some./{ l =>
                            reasons :+= "" + e + " might attack the gate" + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(DevolveThreatOfAttackOnGOO) && canBattle)
                        f.onMap(GOO)./(_.region).intersect(areas).%(r => canBattleIn(r) && e.canAttack(r)(f) && (e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num || e.at(r).got(Hastur))).some./{ l =>
                            reasons :+= "GOO might be in danger from " + e + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (reasons.any)
                        + DevolvePromptAction(f.sure[GC], PreMainAction(e)).as("Acolytes".styled(f), "to", "Deep Ones".styled(f))(Devolve, reasons./("<br/>(" + _ + ")").mkString(""))
                }

                if (f.can(DragonAscending) && f.enemies.exists(_.power > f.power))
                    if (factions./(_.power).max - f.power >= f.commands.of[DragonAscendingPower].single./(_.power).|(1))
                        + DragonAscendingPromptAction(f.sure[OW], e, PreMainAction(e)).as("Rise to", factions./(_.power).max.power)(DragonAscending)

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./(_.add(NeedOk).add(OutOfTurnRefresh(PreMainAction(e))).add(SacrificeHighPriestAllowedAction).group(" ").skip(PreActionPromptsAction(e, l.but(f))))
            }

            if (asks.any)
                MultiAsk(asks)
            else
                Then(MainGatesAction(e))

        case PreMainAction(f) if f.active.not =>
            MainAction(f)

        case PreMainAction(f) =>
            MainGatesAction(f)

        case MainGatesAction(f) =>
            checkGatesGained(f)

            MainAction(f)

        case MainAction(f) if f.active.not =>
            implicit val asking = Asking(f)

            game.reveals(f)

            + NextPlayerAction(f).as("Skip")

            asking

        case MainAction(f) if f.acted =>
            implicit val asking = Asking(f)

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            game.reveals(f)

            game.endTurn(f)(true)

            asking

        case EndAction(self) =>
            self.acted = true
            AfterAction(self)

        case AfterAction(self) =>
            checkGatesGained(self)

            if (self.power == 0)
                self.log("ran out of power")

            if (self.power == -1)
                self.power = 0

            if (self.power < 0) {
                self.log("somehow ran into negative power")

                self.power = 0
            }

            expansions.foreach(_.afterAction())

            factions.foreach(_.oncePerAction = $)

            CheckSpellbooksAction(PreMainAction(self))

        case EndTurnAction(f) =>
            f.acted = true

            NextPlayerAction(f)

        case NextPlayerAction(_) if queue.any =>
            ProceedBattlesAction

        case NextPlayerAction(_) if factions.%(_.doom >= 30).any =>
            CheckSpellbooksAction(GameOverPhaseAction)

        case NextPlayerAction(prev) =>
            factions.foreach(_.acted = false)
            factions.foreach(_.battled = $)
            factions.foreach(_.oncePerRound = $)

            round += 1

            factions.foreach { f =>
                f.active = f.power > 0 && f.hibernating.not
            }

            factions = factions.drop(1) ++ factions.take(1)

            val next = factions.first

            if (factions.exists(_.active)) {
                if (next.active)
                    log(CthulhuWarsSolo.DottedLine)

                CheckSpellbooksAction(PreMainAction(next))
            }
            else
                CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingUpAction("power gather", EndPhasePromptsAction(next, factions))))

        case EndPhasePromptsAction(next, Nil) =>
            Then(PowerGatherAction(next))

        case EndPhasePromptsAction(next, l) =>
            val asks = l./~{ f =>
                implicit val asking = Asking(f)

                + GroupAction("Before the end of Action Phase")

                if (f.onMap(HighPriest).any)
                    if (f.commands.has(UnspeakableOathOpportunityEndOfPhase) || f.commands.has(UnspeakableOathPrompt))
                        + SacrificeHighPriestPromptAction(f, PreMainAction(next)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl)

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./(_.add(NeedOk).add(OutOfTurnRefresh(EndPhasePromptsAction(next, l))).add(SacrificeHighPriestAllowedAction).group(" ").skip(EndPhasePromptsAction(next, l.but(f))))
            }

            if (asks.any)
                MultiAsk(asks)
            else
                Then(PowerGatherAction(next))


        // PASS
        case PassAction(self) =>
            val p = self.power

            self.power = -1

            self.log("passed and forfeited", p.power)

            EndAction(self)

        // CONTROL
        case AdjustGateControlAction(f, changed, then) =>
            Ask(f)
                .some(areas.%(r => f.gates.has(r) || abandonedGates.has(r))) { r =>
                    val l = f.at(r).%(_.canControlGate).sortBy(_.onGate.not).distinctBy(_.uclass)
                    val g = $[Any](f.gates.has(r).not.?("Abandoned gate").|("Gate"), "in", r)

                    l./(u =>
                        if (l.%(_.onGate).single./(_.uclass).has(u.uclass))
                            Info(u.full)(g : _*)
                        else
                        if (l.%(_.onGate).single./(_.uclass).has(Acolyte) && u.uclass == HighPriest && f.commands.has(HighPriestGatesSkip))
                            Info(u.full)(g : _*)
                        else
                            ControlGateAction(f, r, u, then).as(u.full)(g : _*)
                    ) ++
                    (f.gates.has(r) && f.clings.not && f.commands.has(GateDiplomacySkipAbandon).not).$(AbandonGateAction(f, r, then).as("Abandon")(""))
                }
                .add(OutOfTurnRefresh(AdjustGateControlAction(f, changed, then)))
                .group(" ")
                .doneIf(changed)(then)
                .cancelIf(changed.not)

        case ControlGateAction(f, r, u, then) if u.onGate =>
            Force(AdjustGateControlAction(f, true, then))

        case ControlGateAction(f, r, u, then) =>
            f.at(r).foreach(_.onGate = false)
            u.onGate = true

            if (f.gates.has(r).not) {
                f.gates :+= r
                f.abandoned :-= r

                f.log("took control of the gate in", r, "with", u)
            }
            else
                f.log("changed control of the gate in", r, "to", u)

            Force(AdjustGateControlAction(f, true, then))

        case AbandonGateAction(f, r, then) =>
            f.at(r).foreach(_.onGate = false)

            f.gates :-= r
            f.abandoned :+= r

            f.log("abandoned gate in", r)

            Force(AdjustGateControlAction(f, true, then))

        // MOVE
        case MoveMainAction(self) =>
            MoveContinueAction(self, false)

        case MoveContinueAction(self, moved) =>
            if (self.power == 0)
                Then(MoveDoneAction(self))
            else {
                val units = self.units.nex.onMap.not(Moved).%(_.canMove).sortA

                if (units.none)
                    Then(MoveDoneAction(self))
                else
                if (moved)
                    Ask(self)
                        .group("Moved", self.units.tag(Moved).mkString(", "))
                        .add(MoveDoneAction(self).as("Done"))
                        .each(units)(u => MoveSelectAction(u.faction, u, u.region, 1).as(u.full, "from", u.region)("Move", "another".hl, "unit"))
                else
                    Ask(self)
                        .group("Move unit")
                        .each(units)(u => MoveSelectAction(u.faction, u, u.region, 1).as(u.full, "from", u.region))
                        .cancel
            }

        case MoveSelectAction(self, u, from, cost) =>
            var destinations = board.connected(from)

            if (self.has(Flight))
                destinations = areas.but(from).intersect(destinations ++ destinations./~(board.connected))

            if (u.uclass == Shantak)
                destinations = areas.but(from)

            val arriving = self.units.%(_.region.glyph.onMap).tag(Moved)./(_.region).distinct

            val l1 = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            val l2 = destinations.sortBy(to => direction(from, to))

            Ask(self)
                .each(l2.%(self.affords(cost)))(to => MoveAction(self, u, from, to, cost).as
                    (to, self.iced(to), s"""<img class=direction src="${Overlays.imageSource("move-deg-" + direction(from, to))}" />""")
                    ("Move", u, (cost == 0).??("for free"), "from", from, "to")
                )
                .cancelIf(cost > 0)
                .skipIf(cost == 0)(MoveContinueAction(self, true))

        case MoveDoneAction(self) =>
            if (self.has(Burrow) && self.units.%(u => u.tag(Moved))./(u => 1 - u.count(MovedForFree) + u.count(MovedForExtra)).sum > 1) {
                self.power += 1
                self.log("recovered", 1.power, "from", Burrow)
            }

            self.units.foreach(_.remove(Moved))
            self.units.foreach(_.remove(MovedForFree))
            self.units.foreach(_.remove(MovedForExtra))

            EndAction(self)

        case MoveAction(self, u, o, r, cost) =>
            val t = self.payTax(r)

            if (cost > 0)
                self.power -= cost

            u.region = r
            u.add(Moved)
            u.onGate = false

            if (cost + t > 1)
                u.add((cost + t - 1).times(MovedForExtra))

            if (cost + t == 0)
                u.add(MovedForFree)

            self.log("moved", u, "from", o, "to", r)

            MovedAction(self, u, o, r)

        case MovedAction(self, u, o, r) =>
            MoveContinueAction(self, true)

        // ATTACK
        case AttackMainAction(f, l, effect) =>
            val ee = factionlike.but(f)
            val ll = l.sortBy(r => ee.%(_.present(r))./(e => f.strength(f.at(r), e)).maxOr(0)).reverse

            val variants = ll./~(r => ee.%(_.present(r)).%(f.canAttack(r)).sortBy(e => -e.strength(e.at(r), f))./(e => r -> e))

            Ask(f)
                .each(variants)((r, e) => AttackAction(f, r, e, effect))
                .group(" ")
                .cancelIf(effect.none)
                .cancelIf(effect.has(EnergyNexus))
                .skipIf(effect.has(FromBelow))(ProceedBattlesAction)

        case AttackAction(self, r, f, effect) =>
            if (effect.has(FromBelow).not)
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
            factions.%(f => game.nexed.none && f.has(EnergyNexus) && queue.exists(b => f.at(b.arena)(Wizard).any) && f.acted.not).foreach { f =>
                game.nexed = queue.%(_.attacker == queue.first.attacker)./(_.arena).%(r => f.at(r)(Wizard).any)
                f.log("interrupted battle", queue.exists(_.effect.has(EnergyNexus)).??("again"), "with", EnergyNexus)
                return Force(PreMainAction(f))
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
                    CaptureAction(self, r, f, effect).as(f.at(r).cultists./(_.uclass).distinct.single./(_.name).|(Cultist.name).styled(f))("Capture", for1PowerWithTax(r, self), "in", r, self.iced(r))
                }
            }

            Ask(self)
                .list(variants)
                .group(" ")
                .cancelIf(effect.none)
                .skipIf(effect.has(FromBelow))(EndAction(self))

        case CaptureAction(self, r, f, effect) =>
            if (effect.has(FromBelow).not)
                self.power -= 1

            if (effect.has(FromBelow).not || self.all(Nyogtha)./(_.region).but(r).any)
                self.payTax(r)

            val l = f.at(r).cultists.sortBy(u => u.uclass.cost * 10 + u.onGate.??(5))
            val ll = f.clings.?(l.take(1)).|(l)

            Ask(f).each(ll)(u => CaptureTargetAction(f, r, self, u, effect).as(u.full)(self, "captures in", r))

        case CaptureTargetAction(self, r, f, u, effect) =>
            eliminate(u)
            u.region = f.prison

            f.log("captured", u, "in", r, effect./("with " + _).|(""))

            f.satisfy(CaptureCultist, "Capture Cultist")

            if (effect.has(FromBelow).not)
                f.all(Nyogtha)./(_.region).diff($(r)).foreach { x =>
                    return Force(CaptureMainAction(f, $(x), |(FromBelow)))
                }

            EndAction(f)

        // BUILD
        case BuildGateMainAction(self, locations) =>
            Ask(self).each(locations.sortBy(self.taxIn))(r => BuildGateAction(self, r)).cancel

        case BuildGateAction(self, r) =>
            self.power -= 3 - self.has(UmrAtTawil).??(1)
            self.payTax(r)
            gates :+= r
            self.oncePerAction :+= UmrAtTawil
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
            self.log("recruited", uc.styled(self), "in", r)

            if (uc === HighPriest) {
                self.plans ++= $(
                    // UnspeakableOathImmediately,
                    UnspeakableOathPrompt,
                    UnspeakableOathSkip,
                    UnspeakableOathThreatOfHPCapture,
                    UnspeakableOathThreatOfAttackOnHighPriest,
                    UnspeakableOathThreatOfAcolyteCapture,
                    UnspeakableOathOpportunityEndOfPhase,
                    UnspeakableOathOpportunityFirstPlayer,
                ) ++
                (self == WW).$(UnspeakableOathThreatOfDryEternal) ++
                (self == OW).$(UnspeakableOathOpportunityOfDreadCurse) ++
                self.enemies.has(CC).$(UnspeakableOathThreatOfThousandForms) ++
                self.enemies.has(BG).$(UnspeakableOathThreatOfGhroth) ++
                (self != AN).$(UnspeakableOathThreatOfAttackOnGOO) ++
                $(UnspeakableOathThreatOfAttackOnGate)

                if (self.commands.of[UnspeakableOathPlan].none)
                    self.commands :+= UnspeakableOathPrompt

                self.plans ++= $(
                    HighPriestGatesPrompt,
                    HighPriestGatesSkip,
                )

                if (self.commands.of[HighPriestGatesPlan].none)
                    if (options.has(QuickGame))
                        self.commands :+= HighPriestGatesSkip
                    else
                        self.commands :+= HighPriestGatesPrompt
            }

            EndAction(self)

        // SUMMON
        case SummonMainAction(self, uc, l) =>
            Ask(self).each(l)(r => SummonAction(self, uc, r)).cancel

        case SummonAction(self, uc, r) =>
            self.power -= self.summonCost(uc, r)
            self.payTax(r)
            self.place(uc, r)
            self.log("summoned", uc.styled(self), "in", r)

            SummonedAction(self, uc, r, $)

        case SummonedAction(self, uc, r, l) =>
            EndAction(self)

        // AWAKEN
        case AwakenMainAction(self, uc, locations) =>
            Ask(self).some(locations)(r => self.awakenCost(uc, r)./(cost => AwakenAction(self, uc, r, cost))).cancel

        case AwakenAction(self, uc, r, cost) =>
            self.power -= cost

            self.payTax(r)
            self.place(uc, r)

            self.log("awakened", uc.styled(self), "in", r)

            AwakenedAction(self, uc, r, cost)

        // HIGH PRIESTS
        case SacrificeHighPriestDoomAction(self) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, DoomAction(self))).cancel

        case SacrificeHighPriestMainAction(self) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, PreMainAction(self))).cancel

        case SacrificeHighPriestPromptAction(self, then) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, then)).cancel

        case SacrificeHighPriestOutOfTurnMainAction(self) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, OutOfTurnReturn)).cancel

        case SacrificeHighPriestAction(self, r, then) =>
            val c = self.at(r).one(HighPriest)

            eliminate(c)

            self.oncePerAction :-= Passion

            self.power += 2

            log(CthulhuWarsSolo.DottedLine)

            self.log("sacrificed", c, "in", r)

            if (self.hibernating.not)
                self.active = true

            triggers()

            checkGatesLost()

            if (then == OutOfTurnReturn)
                then
            else
                CheckSpellbooksAction(then)

        // COMMANDS
        case CommandsMainAction(f) =>
            Ask(f)
                .each(f.plans) { p =>
                    if (f.commands.has(p))
                        CommandsRemoveAction(f, p)
                    else
                    if (p.requires.exists(_.forall(f.commands.has)))
                        CommandsAddAction(f, p)
                    else
                        CommandsInfoAction(f, p)
                }
                .group(" ")
                .cancel

        case CommandsAddAction(f, plan) =>
            plan.as[OneOfPlan]./(p => f.commands = f.commands.%!(_.as[OneOfPlan].?(_.group == p.group)))

            f.commands = f.commands.diff(plan.unfollowers)

            f.commands :+= plan

            def on() = plan.followers.intersect(f.plans).diff(f.commands).%(_.requires.exists(_.forall(f.commands.has)))

            while (on().any)
                f.commands ++= on()

            def off() = f.commands.%(_.requires.exists(_.forall(f.commands.has)).not)

            while (off().any)
                f.commands = f.commands.diff(off())

            // f.log("plan added", plan.selected)
            // if (plan.followers.any)
            //     f.log("plan added extra", plan.followers.mkString(" "))

            Then(OutOfTurnRepeat(f, CommandsMainAction(f)))

        case CommandsRemoveAction(f, plan) =>
            if (plan.is[OnlyOnPlan].not) {
                f.commands :-= plan

                def off() = f.commands.%(_.requires.exists(_.forall(f.commands.has)).not)

                while (off().any)
                    f.commands = f.commands.diff(off())
            }

            // f.log("plan removed", plan.unselected)

            Then(OutOfTurnRepeat(f, CommandsMainAction(f)))

        // OW Dragon Ascending defaults
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
