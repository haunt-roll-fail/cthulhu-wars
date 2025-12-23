package cws

import hrf.colmat._

import Html._

import scala.util._

import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

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

trait UnitUtils {
    implicit class UnitFigureCollectionOps(ufs : $[UnitFigure]) {
        def exceptGOO : $[UnitFigure] = ufs.filter(_.exceptGOO)
        def exceptTerror : $[UnitFigure] = ufs.filter(_.exceptTerror)

        def exceptReanimated : $[UnitFigure] = ufs.filterNot(_.uclass == Reanimated)
        def exceptGug : $[UnitFigure] = ufs.filterNot(_.uclass == Gug)
        def exceptByatis : $[UnitFigure] = ufs.filterNot(_.uclass == Byatis)

        def exceptIsolatedBrainless(p : Player, game : Game) : $[UnitFigure] = ufs.filterNot(game.isIsolatedBrainless(p, _))
    }

    implicit class UnitFigureOps(u : UnitFigure) {
        def exceptGOO : Boolean = u.uclass.utype != GOO
        def exceptTerror : Boolean = u.uclass.utype != Terror
    }

    implicit class GameOps(game : Game) {
        def isIsolatedBrainless(p : Player, u : UnitFigure) : Boolean = {
            if (!p.has(Brainless)) return false
            if (u.uclass != Reanimated) return false

            val r = u.region
            val monsters = p.at(r, Monster).but(u)

            !(p.at(r, Cultist).any || p.at(r, GOO).any || p.at(r, Terror).any || monsters.exceptReanimated.any)
        }
    }
}

trait GameImplicits {
    implicit def factionToState(f : Faction)(implicit game : Game) : Player = f match {
        case f : NeutralFaction => game.neutrals(f)
        case f : Faction => game.players(f)
    }

    def factions(implicit game : Game) = game.factions

    def log(m : Any*)(implicit game : Game) = game.appendLog(m.$)

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def neutral = f.is[NeutralFaction]
        def real = f.is[NeutralFaction].not
        def goos = f.all.goos
        def factionGOOs = f.all.factionGOOs
        def log(m : Any*) = game.appendLog(f +: m.$)
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

    implicit class UnitFigureGameEx(u : UnitFigure)(implicit game : Game) {
        def canMove = u.uclass.canMove(u)
        def canBeMoved = u.uclass.canBeMoved(u)
        def canCapture = u.uclass.canCapture(u)
    }

    implicit class UnitFigureListEx(l : $[UnitFigure]) {
        def apply(uc : UnitClass) = l.%(_.uclass == uc)
        def not(uc : UnitClass) = l.%(_.uclass != uc)
        def goos = l.%(_.uclass.utype == GOO)
        def factionGOOs = l.%(u => u.uclass.utype == GOO && u.uclass.is[IGOO].not)
        def independentGOOs = l.%(u => u.uclass.utype == GOO && u.uclass.is[IGOO])
        def monsters = l.%(_.uclass.utype == Monster)
        def monsterly = l.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror)
        def terrors = l.%(_.uclass.utype == Terror)
        def cultists = l.%(_.uclass.utype == Cultist)
        def notGoos = l.%(_.uclass.utype != GOO)
        def notMonsters = l.%(_.uclass.utype != Monster)
        def notTerrors = l.%(_.uclass.utype != Terror)
        def notCultists = l.%(_.uclass.utype != Cultist)
        def onMap = l.%(_.region.glyph.onMap)
        def inPlay = l.%(_.region.glyph.inPlay)
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
}

case class Region(name : String, glyph : Glyph) {
    override def toString = if (glyph == Ocean) name.styled("sea") else if (glyph == Deep) GC.styled(name) else name.styled("region")
    def +(ia : IceAges) = toString + ia.toString
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

object LoyaltyCardConfig {
  val loyaltyCardConditions = List(
    (UseGhast, GhastCard),
    (UseGug, GugCard),
    (UseShantak, ShantakCard),
    (UseStarVampire, StarVampireCard),
    (UseByatis, ByatisCard),
    (UseAbhoth, AbhothCard),
    (UseDaoloth, DaolothCard),
    (UseNyogtha, NyogthaCard),
    (HighPriests, HighPriestCard)
  )
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

@EnableReflectiveInstantiation
abstract class UnitClass(val name : String, val utype : UnitType, val cost : Int) {
    def plural = name + "s"
    val priority = utype.priority * 1_00_00_00 + cost * 1_00 + this.is[NeutralMonster].??(1_00_00)

    def canMove(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    def canBeRecruited(implicit game : Game) : Boolean = utype == Cultist
    def canBeSummoned(implicit game : Game) : Boolean = utype == Monster
}

case object Acolyte extends UnitClass("Acolyte", Cultist, 1) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(r : UnitFigure)(implicit game : Game) : Boolean = true
}

abstract class FactionUnitClass(val faction : Faction, name : String, utype : UnitType, cost : Int) extends UnitClass(name, utype, cost) {
    override def toString = faction.styled(name)
}

sealed class UnitState(val text : String) extends Ordered[UnitState]  {
    override def toString = text

    def compare(that : UnitState) = text.compare(that.text)
}

case object Moved extends UnitState("moved")
case object Retreated extends UnitState("retreated")
case object Absorbed extends UnitState("absorbed")
case object Harbinged extends UnitState("harbinged")
case object Invised extends UnitState("invised")
case object Hidden extends UnitState("hidden")
case object Zeroed extends UnitState("zeroed")
case object Primed extends UnitState("primed")
// case object Mourning extends UnitState("mourning")
case object Summoned extends UnitState("summoned")
case object MovedForFree extends UnitState("moved-for-free")
case object MovedForDouble extends UnitState("moved-for-double")

class UnitFigure(val faction : Faction, val uclass : UnitClass, val index : Int, var region : Region, var state : $[UnitState] = $, var health : UnitHealth = Alive) {
    override def toString = short

    def dbg = faction.short + "/" + uclass.name + "/" + index

    def short = faction.styled(uclass.name)

    def full = faction.styled(uclass.name) + (if (state.any) state.mkString(" (", "/", ")") else "") + (if (health == Alive || health == DoubleHP(Alive, Alive)) "" else (" (" + health + ")"))

    def has(s : UnitState) = state.contains(s)
    def add(s : UnitState) { state :+= s }
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

@EnableReflectiveInstantiation
sealed abstract class Spellbook(val name : String) {
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
    def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u.cost
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

@EnableReflectiveInstantiation
trait Action extends Product {
    def question(implicit game : Game) : String
    def safeQ(implicit game : Game) = question(game)
    def option(implicit game : Game) : String
}

trait FactionAction extends Action {
    def self : Faction
}

trait Soft extends Action
trait Cancel extends Action
trait More extends Soft
trait PowerNeutral extends Action

trait Continue

case class Ask(faction : Faction, list : $[Action] = $) extends Continue {
    def add(a : Action) = Ask(faction, list :+ a)
    def add(l : $[Action]) = Ask(faction, list ++ l)
    def prepend(a : Action) = Ask(faction, a +: list)
    def prepend(l : $[Action]) = Ask(faction, l ++ list)
    def each[T](l : IterableOnce[T])(a : T => Action) = Ask(faction, list ++ l.iterator.map(a))
    def cancel = add(MainCancelAction(faction))
    def doomCancel = add(DoomCancelAction(faction))
    def battleCancel = add(BattleCancelAction(faction))
}

case object StartContinue extends Continue
case class Force(action : Action) extends Continue
case class DelayedContinue(delay : Int, continue : Continue) extends Continue
case class RollD6(question : Game => String, roll : Int => ForcedAction) extends Continue
case class RollBattle(question : Game => String, n : Int, roll : $[BattleRoll] => ForcedAction) extends Continue
case class DrawES(question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => ForcedAction) extends Continue
case class GameOver(winners : $[Faction]) extends Continue

object Action {
    implicit def string2desc(s : String) : Game => String = (g : Game) => s
    implicit def region2desc(r : Region) : Game => String = (g : Game) => r.toString
    implicit def faction2desc(f : Faction) : Game => String = (g : Game) => f.full
    implicit def spellbook2desc(b : Spellbook) : Game => String = (g : Game) => b.full
    implicit def option2desc(n : |[String]) : Game => String = (g : Game) => n.|(null)
    implicit def unitrefshort2desc(ur : UnitRefShort) : Game => String = (g : Game) => g.unit(ur.r).short
    implicit def unitreffull2desc(ur : UnitRefFull) : Game => String = (g : Game) => g.unit(ur.r).toString

    implicit def action2force(fa : ForcedAction) : Continue = Force(fa)
    implicit def actions2ask(list : $[FactionAction]) : Continue = Ask(list./(_.self).distinct.single.get, list)
}

import Action._

object RollBattle {
    def apply(faction : Faction, side : String, n : Int, roll : $[BattleRoll] => ForcedAction) : RollBattle = RollBattle("" + faction + " rolls " + (n == 0).?(" no dice").|((n == 1).?(" one die").|("" + n + " dice")) + " for " + side, n, roll)
}

object QAsk {
    def apply(list : $[FactionAction]) = Ask(list./(_.self).distinct.single.get, list)
}


abstract class ForcedAction extends Action {
    def question(implicit game : Game) = "N/A"
    def option(implicit game : Game) = "N/A"
}


trait Void { self : ForcedAction => }
trait OutOfTurn { self : Action => }
trait OutOfTurnReturn extends Soft { self : Action => }


case object ReloadAction extends ForcedAction with Void
case object UpdateAction extends ForcedAction with Void
case class CommentAction(comment : String) extends ForcedAction with Void

case object StartAction extends ForcedAction
case class CheckSpellbooksAction(then : Action) extends ForcedAction
case object AfterPowerGatherAction extends ForcedAction
case object FirstPlayerDeterminationAction extends ForcedAction
case object PlayOrderAction extends ForcedAction
case class PowerGatherAction(next : Faction) extends ForcedAction
case object DoomPhaseAction extends ForcedAction
case object ActionPhaseAction extends ForcedAction
case object GameOverPhaseAction extends ForcedAction

abstract class BaseFactionAction(val qqq : Game => String, val ooo : Game => String) extends FactionAction {
    def question(implicit game : Game) = qqq(game)
    def option(implicit game : Game) = ooo(game)
}
abstract class OptionFactionAction(val ooo : Game => String) extends FactionAction {
    def option(implicit game : Game) = ooo(game)
}

case class StartingRegionAction(self : Faction, r : Region) extends BaseFactionAction("" + self + " starts in", r)
case class FirstPlayerAction(self : Faction, f : Faction) extends BaseFactionAction("First player", f)
case class PlayDirectionAction(self : Faction, order : $[Faction]) extends BaseFactionAction("Order of play", order.mkString(" > "))

trait MainQuestion extends FactionAction {
    def question(implicit game : Game) = game.nexus./(n => "" + EnergyNexus + " in " + n.region).|("" + self + " action") + " (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
    override def safeQ(implicit game : Game) = self @@ {
        case f if game.nexus.any => "" + EnergyNexus + " in " + game.nexus.get.region
        case f if game.acted => "Unlimited actions"
        case f => "Main action"
    }
}

trait DoomQuestion extends FactionAction {
    def question(implicit game : Game) = "" + self + " doom phase (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
    override def safeQ(implicit game : Game) = "" + self + " doom phase"
}

trait ExtraQuestion extends FactionAction {
    def question(implicit game : Game) = "<hr/>"
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
case class DoomCancelAction(self : Faction) extends BaseFactionAction("&nbsp;", "Cancel") with Cancel
case class DoomNextPlayerAction(faction : Faction) extends ForcedAction
case class DoomDoneAction(self : Faction) extends BaseFactionAction("&nbsp;", "Done".styled("power")) with PowerNeutral

case class MainAction(faction : Faction) extends ForcedAction
case class MainCancelAction(self : Faction) extends BaseFactionAction("&nbsp;", "Cancel") with Cancel
case class MainNextPlayerAction(faction : Faction) extends ForcedAction
case class MainDoneAction(self : Faction) extends BaseFactionAction("&nbsp;", "Done".styled("power")) with PowerNeutral
case class MainDoneCancelAction(self : Faction) extends BaseFactionAction("&nbsp;", "Cancel") with PowerNeutral

case class EndAction(self : Faction) extends ForcedAction
case class AfterAction(self : Faction) extends ForcedAction

case class RitualAction(self : Faction, cost : Int, k : Int) extends OptionFactionAction(g => "Perform " + "Ritual of Annihilation".styled("doom") + " for " + cost.power) with DoomQuestion

case class RevealESDoomAction(self : Faction) extends OptionFactionAction("View " + "Elder Signs".styled("es")) with DoomQuestion with Soft with PowerNeutral
case class RevealESMainAction(self : Faction) extends OptionFactionAction("View " + "Elder Signs".styled("es")) with MainQuestion with Soft with PowerNeutral
case class RevealESOutOfTurnAction(self : Faction) extends BaseFactionAction("Elder Signs", "View " + "Elder Signs".styled("es")) with Soft
case class RevealESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(implicit g => "Elder Signs".styled("es") + " " + self.es./(_.short).mkString(" "), (es.num == 1).?("Reveal " + es(0).short).|("Reveal all for " + es./(_.value).sum.doom)) with OutOfTurn


case class LoyaltyCardDoomAction(self : Faction) extends OptionFactionAction("Obtain " + "Loyalty Card".styled("nt")) with DoomQuestion with Soft with PowerNeutral
case class NeutralMonstersAction(self : Faction, lc : LoyaltyCard, next : Action) extends BaseFactionAction(g => "Obtain " + "Loyalty Card".styled("nt"), {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        lc.short +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with PowerNeutral

case class LoyaltyCardSummonAction(self : Faction, uc : UnitClass, r : Region, next : Action) extends BaseFactionAction(g => "Place " + self.styled(uc) + " in", g => r + g.ia(r, self))

case class IndependentGOOMainAction(self : Faction, lc : LoyaltyCard, l : $[Region]) extends OptionFactionAction(g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Awaken " + lc.name.styled("nt") +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with MainQuestion with Soft
case class IndependentGOOAction(self : Faction, lc : LoyaltyCard, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + self.styled(lc.unit) + g.forNPowerWithTax(r, self, cost) + " in", g => r + g.ia(r, self))


case class PassAction(self : Faction) extends OptionFactionAction("Pass and lose remaining power") with MainQuestion

case class MoveMainAction(self : Faction) extends OptionFactionAction("Move") with MainQuestion with Soft
case class MoveContinueAction(self : Faction, moved : Boolean) extends ForcedAction with Soft
case class MoveSelectAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => self.moved.any.?("<br/>" + "Move " + "another".styled("highlight") + " unit").|("Move unit"), self.styled(uc) + " from " + r) with Soft
case class MoveAction(self : Faction, uc : UnitClass, r : Region, dest : Region) extends BaseFactionAction(g => "Move " + self.styled(uc) + " from " + r + " to", g => dest + g.ia(dest, self))
case class MoveDoneAction(self : Faction) extends BaseFactionAction(implicit g => "Moved " + self.moved.mkString(", "), "Done")
case class MoveCancelAction(self : Faction) extends BaseFactionAction(None, "Cancel") with Cancel

case class AttackMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Battle") with MainQuestion with Soft
case class AttackAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(g => "Battle in " + r + g.ia(r, self), f)

case class BuildGateMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Build Gate") with MainQuestion with Soft
case class BuildGateAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Build gate" + g.forNPowerWithTax(r, self, 3 - self.has(UmrAtTawil).??(1)) + " in", r)

case class CaptureMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Capture") with MainQuestion with Soft
case class CaptureAction(self : Faction, r : Region, f : Faction, uc : UnitClass = Acolyte) extends BaseFactionAction(g => "Capture" + g.for1PowerWithTax(r, self) + " in " + r + g.ia(r, self), g => f.styled(uc))

case class RecruitMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Recruit " + self.styled(uc)) with MainQuestion with Soft
case class RecruitAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Recruit " + self.styled(uc) + g.forNPowerWithTax(r, self, self.recruitCost(uc, r)) + " in", g => r + g.ia(r, self))

case class SummonMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Summon " + self.styled(uc)) with MainQuestion with Soft
case class SummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Summon " + self.styled(uc) + g.forNPowerWithTax(r, self, self.summonCost(uc, r)) + " in", g => r + g.ia(r, self))
case class FreeSummonAction(self : Faction, uc : UnitClass, r : Region, next : Action) extends BaseFactionAction(g => "Summon " + self.styled(uc) + " for free in", implicit g => r + g.ia(r, self))

case class AwakenMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Awaken " + self.styled(uc)) with MainQuestion with Soft
case class AwakenAction(self : Faction, uc : UnitClass, r : Region, cost : Int = -1) extends BaseFactionAction(g => "Awaken " + self.styled(uc) + g.forNPowerWithTax(r, self, cost) + " in", g => r + g.ia(r, self))
case class AwakenEliminate2CultistsAction(self : Faction, uc : UnitClass, l : $[Region], a : Region, b : Region) extends BaseFactionAction("Eliminate two " + self.styled(Cultist.plural) + " to awaken " + self.styled(uc), (a == b).?("Two from " + a)|("From " + a + " and " + b))

case class Offer(f : Faction, n : Int)

case class SacrificeHighPriestDoomAction(self : Faction) extends OptionFactionAction("Sacrifice " + self.styled("High Priest")) with DoomQuestion with Soft with PowerNeutral
case class SacrificeHighPriestMainAction(self : Faction, then : Action) extends OptionFactionAction("Sacrifice " + self.styled("High Priest")) with MainQuestion with Soft
case class SacrificeHighPriestAction(self : Faction, r : Region, then : Action) extends BaseFactionAction("Sacrifice to gain " + 2.power, self.styled(HighPriest) + " in " + r)
case class SacrificeHighPriestDoneAction(self : Faction, then : Action) extends BaseFactionAction(None, "Done".styled("power"))

// GC
case class DevolveMainAction(self : Faction, then : Action) extends OptionFactionAction(self.styled(Devolve)) with MainQuestion with Soft
case class DevolveAction(self : Faction, r : Region, then : Action) extends BaseFactionAction(self.styled(Devolve), self.styled(Acolyte) + " in " + r)
case class DevolveDoneAction(self : Faction, then : Action) extends BaseFactionAction(None, "Done".styled("power"))

case class DreamsMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(self.styled(Dreams)) with MainQuestion with Soft
case class DreamsAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(self.styled(Dreams), g => f.styled(Acolyte) + " in " + r + g.ia(r, self))

case class SubmergeMainAction(self : Faction, r : Region) extends OptionFactionAction(self.styled(Submerge)) with MainQuestion
case class SubmergeAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(Submerge.full + " with " + self.styled(Cthulhu) + " from " + r, self.styled(uc))
case class SubmergeDoneAction(self : Faction, r : Region) extends BaseFactionAction(None, "Done".styled("power"))

case class UnsubmergeMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(self.styled("Unsubmerge")) with MainQuestion with Soft
case class UnsubmergeAction(self : Faction, r : Region) extends BaseFactionAction(self.styled("Unsubmerge"), g => r + g.ia(r, self))

// CC
case class ThousandFormsMainAction(self : Faction) extends OptionFactionAction(self.styled(ThousandForms)) with MainQuestion
case class ThousandFormsRollAction(f : Faction, x : Int) extends ForcedAction

case class ThousandFormsAction(f : Faction, x : Int) extends ForcedAction
case class ThousandFormsContinueAction(f : Faction, x : Int, offers : $[Offer], forum : $[Faction], time : Int) extends ForcedAction
case class ThousandFormsAskAction(f : Faction, x : Int, offers : $[Offer], forum : $[Faction], time : Int, self : Faction, n : Int) extends BaseFactionAction(
    g => f.styled(ThousandForms) + " demand " + x.power + "<br/>" + offers./(o => "" + o.f + " offers " + (o.n > 0).?(o.n.styled("power")).|("none")).mkString("<br/>") + "<hr/>" + self,
    (n < 0).?("Refuse to negotiate").|((x == n + offers./(_.n).sum).?("Offer".styled("highlight")).|("Offer") + " " + (n > 0).?(n.styled("power") + (x == n + offers./(_.n).sum).?(" Power".styled("highlight")).|(" Power")).|((x == n + offers./(_.n).sum).?("0 Power".styled("highlight")).|("0 Power")))
)

case class Pay4PowerMainAction(self : Faction) extends OptionFactionAction("Pay " + 4.power + " for a spellbook") with MainQuestion
case class Pay6PowerMainAction(self : Faction) extends OptionFactionAction("Pay " + 6.power + " for a spellbook") with MainQuestion
case class Pay10PowerMainAction(self : Faction) extends OptionFactionAction("Pay " + 10.power + " for two spellbooks") with MainQuestion

// BG
case class BloodSacrificeDoomAction(self : Faction) extends OptionFactionAction(BloodSacrifice) with DoomQuestion with Soft with PowerNeutral
case class BloodSacrificeAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(BloodSacrifice, self.styled(uc) + " in " + r)

case class Eliminate2CultistsMainAction(self : Faction) extends OptionFactionAction("Eliminate two " + self.styled(Cultist.plural) + " for a spellbook") with MainQuestion with Soft
case class Eliminate2CultistsAction(self : Faction, a : Region, b : Region) extends BaseFactionAction("Eliminate two " + self.styled(Cultist.plural) + " for a spellbook", (a == b).?("Two from " + a)|("From " + a + " and " + b))

case class AvatarMainAction(self : Faction, o : Region, l : $[Region]) extends OptionFactionAction(self.styled(Avatar)) with MainQuestion with Soft
case class AvatarAction(self : Faction, o : Region, r : Region, f : Faction) extends BaseFactionAction(g => self.styled(Avatar), g => f.toString + " in " + r + g.ia(r, self))
case class AvatarReplacementAction(self : Faction, f : Faction, r : Region, o : Region, uc : UnitClass) extends BaseFactionAction(Avatar.full + " replacement from " + r + " to " + o, self.styled(uc))

case class GhrothMainAction(self : Faction) extends OptionFactionAction(self.styled(Ghroth)) with MainQuestion
case class GhrothRollAction(f : Faction, x : Int) extends ForcedAction
case class GhrothAction(f : Faction, x : Int) extends ForcedAction
case class GhrothContinueAction(f : Faction, x : Int, offers : $[Offer], forum : $[Faction], time : Int) extends ForcedAction
case class GhrothAskAction(f : Faction, x : Int, offers : $[Offer], forum : $[Faction], time : Int, self : Faction, n : Int) extends BaseFactionAction(
    g => f.styled(Ghroth) + " demand " + x.styled("power") + " Cultists<br/>" + offers./(o => "" + o.f + " offers " + (o.n > 0).?(o.n.styled("power")).|("none")).mkString("<br/>") + "<hr/>" + self,
    (n < 0).?("Refuse to negotiate").|((x == n + offers./(_.n).sum).?("Offer".styled("highlight")).|("Offer") + " " + (n > 0).?(n.styled("power") + (x == n + offers./(_.n).sum).?((" Cultist" + (n > 1).??("s")).styled("highlight")).|((" Cultist" + (n > 1).??("s")))).|((x == n + offers./(_.n).sum).?("0 Cultists".styled("highlight")).|("0 Cultists")))
)
case class GhrothSplitAction(self : Faction, x : Int, factions : $[Faction]) extends BaseFactionAction((x > 1).?("Eliminate " + x.styled("hightlight") + " Cultists from").|("Eliminate a Cultist from"), factions.mkString(" and "))
case class GhrothSplitNumAction(self : Faction, x : Int, factions : $[Faction], full : $[Faction]) extends BaseFactionAction((x > 1).?("Eliminate " + x.styled("hightlight") + " Cultists from").|("Eliminate a Cultist from"), factions./(f => "" + f + (full.%(_ == f).num > 0).??(f.styled(" (" + full.%(_ == f).num + ")"))).mkString(", "))
case class GhrothEliminateAction(f : Faction, factions : $[Faction]) extends ForcedAction
case class GhrothUnitAction(self : Faction, uc : UnitClass, r : Region, f : Faction, factions : $[Faction]) extends BaseFactionAction((factions.%(_ == self).num > 1).?("Eliminate " + factions.%(_ == self).num.styled("hightlight") + " Cultists").|("Eliminate a Cultist"), self.styled(uc) + " in " + r)
case class GhrothFactionAction(self : Faction, f : Faction) extends BaseFactionAction("Place " + Acolyte.name, f.styled(Acolyte))
case class GhrothPlaceAction(self : Faction, f : Faction, r : Region) extends BaseFactionAction("Place " + f.styled(Acolyte) + " in", r)

// YS
case class Provide3DoomMainAction(self : Faction) extends OptionFactionAction("Get spellbook and another faction gets " + 3.doom) with MainQuestion with Soft
case class Provide3DoomAction(self : Faction, f : Faction) extends BaseFactionAction("Get spellbook", "" + f + " gets " + 3.doom)

case class DesecrateMainAction(self : Faction, r : Region, te : Boolean) extends OptionFactionAction(g => "" + Desecrate + " " + r + te.??(" (" + ThirdEye + ")") + g.ia(r, self)) with MainQuestion
case class DesecrateRollAction(f : Faction, r : Region, te : Boolean, x : Int) extends ForcedAction
case class DesecratePlaceAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Place in " + r, self.styled(uc))

case class HWINTBNMainAction(self : Faction, o : Region, l : $[Region]) extends OptionFactionAction(HWINTBN) with MainQuestion with Soft
case class HWINTBNAction(self : Faction, o : Region, r : Region) extends BaseFactionAction(HWINTBN, g => r + g.ia(r, self))

case class ScreamingDeadMainAction(self : Faction, o : Region, l : $[Region]) extends OptionFactionAction(ScreamingDead) with MainQuestion with Soft
case class ScreamingDeadAction(self : Faction, o : Region, r : Region) extends BaseFactionAction(ScreamingDead, g => r + g.ia(r, self))
case class ScreamingDeadFollowAction(self : Faction, o : Region, r : Region, uc : UnitClass) extends BaseFactionAction("Follow " + KingInYellow, self.styled(uc))
case class ScreamingDeadDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class ShriekMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(Shriek) with MainQuestion with Soft
case class ShriekAction(self : Faction, r : Region) extends BaseFactionAction(Shriek, g => r + g.ia(r, self))
case class ShriekFromAction(self : Faction, o : Region, r : Region) extends BaseFactionAction("" + Shriek.full + " to " + r, "" + Byakhee + " from " + o)
case class ShriekDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class ZingayaMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(self.styled(Zingaya)) with MainQuestion with Soft
case class ZingayaAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(self.styled(Zingaya), g => f.styled(Acolyte) + " in " + r + g.ia(r, self))

// SL
case class DeathFromBelowDoomAction(self : Faction) extends OptionFactionAction(DeathFromBelow) with DoomQuestion with Soft with PowerNeutral
case class DeathFromBelowSelectMonsterAction(self : Faction, uc : UnitClass) extends BaseFactionAction(DeathFromBelow, self.styled(uc))
case class DeathFromBelowAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(DeathFromBelow, self.styled(uc) + " in " + r)

case class LethargyMainAction(self : Faction) extends OptionFactionAction(Lethargy) with MainQuestion with PowerNeutral

case class Pay3SomeoneGains3MainAction(self : Faction) extends OptionFactionAction("Pay " + 3.power + " and another faction gains " + 3.power) with MainQuestion with Soft
case class Pay3SomeoneGains3Action(self : Faction, f : Faction) extends BaseFactionAction("Get spellbook for " + 3.power, "" + f + " gets " + 3.power)

case class Pay3EverybodyLoses1MainAction(self : Faction) extends OptionFactionAction("Pay " + 3.power + ", other factions lose " + 1.power + " each") with MainQuestion
case class Pay3EverybodyGains1MainAction(self : Faction) extends OptionFactionAction("Pay " + 3.power + ", other factions gain " + 1.power + " each") with MainQuestion

case class CaptureMonsterMainAction(self : Faction) extends OptionFactionAction(CaptureMonster) with MainQuestion with Soft
case class CaptureMonsterAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(CaptureMonster, "Capture " + f + " Monster in " + r)
case class CaptureMonsterUnitAction(f : Faction, r : Region, self : Faction, uc : UnitClass) extends BaseFactionAction(CaptureMonster.full + " in " + r, self.styled(uc))

case class AncientSorceryMainAction(self : Faction) extends OptionFactionAction(AncientSorcery) with MainQuestion with Soft
case class AncientSorceryAction(self : Faction, a : Spellbook) extends BaseFactionAction(AncientSorcery, a) with Soft
case class AncientSorceryUnitAction(self : Faction, a : Spellbook, r : Region, uc : UnitClass) extends BaseFactionAction("Access " + a.full + " with", self.styled(uc) + " from " + r)
case class AncientSorceryDoomAction(self : Faction) extends OptionFactionAction(AncientSorcery) with DoomQuestion with Soft
case class AncientSorceryPlaceAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Place " + uc + " in", r)

case class CursedSlumberSaveMainAction(self : Faction) extends OptionFactionAction(CursedSlumber) with MainQuestion with Soft
case class CursedSlumberSaveAction(self : Faction, r : Region) extends BaseFactionAction("Move gate to " + CursedSlumber.full + " from", r)
case class CursedSlumberLoadMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(CursedSlumber) with MainQuestion with Soft
case class CursedSlumberLoadAction(self : Faction, r : Region) extends BaseFactionAction("Move gate from " + CursedSlumber.full + " to", g => r + g.ia(r, self))

// WW
case class HibernateMainAction(self : Faction, n : Int) extends OptionFactionAction(Hibernate.full + " for extra " + n.power) with MainQuestion

case class IceAgeMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(IceAge) with MainQuestion with Soft
case class IceAgeAction(self : Faction, r : Region) extends BaseFactionAction(self.styled(IceAge) + " region", r)

case class ArcticWindAction(self : Faction, o : Region, uc : UnitClass, r : Region) extends BaseFactionAction(ArcticWind.full + " to " + r, self.styled(uc) + " from " + o)
case class ArcticWindDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class AnytimeGainElderSignsMainAction(self : Faction) extends OptionFactionAction(self.styled("Anytime Spellbook")) with MainQuestion with Soft with PowerNeutral
case class AnytimeGainElderSignsDoomAction(self : Faction) extends OptionFactionAction(self.styled("Anytime Spellbook")) with DoomQuestion with Soft with PowerNeutral
case class AnytimeGainElderSignsAction(self : Faction, n : Int, next : Action) extends BaseFactionAction(self.styled("Anytime Spellbook"), "Get spellbook and " + n.es)

// OW
case class BeyondOneMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(self.styled(BeyondOne)) with MainQuestion with Soft
case class BeyondOneUnitAction(self : Faction, o : Region, uc : UnitClass) extends BaseFactionAction(self.styled(BeyondOne), self.styled(uc) + " from " + o) with Soft
case class BeyondOneAction(self : Faction, o : Region, uc : UnitClass, r : Region) extends BaseFactionAction(self.styled(BeyondOne) + " from " + o + " with " + self.styled(uc) + " to", g => r + g.ia(r, self))

case class DreadCurseMainAction(self : Faction, n : Int, l : $[Region]) extends OptionFactionAction(self.styled(DreadCurse)) with MainQuestion with Soft
case class DreadCurseAction(self : Faction, n : Int, r : Region) extends BaseFactionAction(self.styled(DreadCurse), g => r + g.ia(r, self))
case class DreadCurseRollAction(f : Faction, r : Region, x : $[BattleRoll]) extends ForcedAction
case class DreadCurseSplitAction(self : Faction, r : Region, x : $[BattleRoll], e : $[Faction], k : $[Faction], p : $[Faction]) extends BaseFactionAction(self.styled(DreadCurse) + " in " + r + "<br/>" + x.any.?(x.mkString(" ")).|("None"), e.%(f => k.count(f) + p.count(f) > 0)./(f => "" + f + " - " + (k.count(f).times(Kill) ++ p.count(f).times(Pain)).mkString(" ")).mkString("<br/>"))
case class DreadCurseAssignAction(f : Faction, r : Region, e : $[Faction], k : $[Faction], p : $[Faction], self : Faction, s : BattleRoll, uc : UnitClass) extends BaseFactionAction("Assign " + s + " in " + r, self.styled(uc))
case class DreadCurseRetreatAction(self : Faction, r : Region, e : $[Faction], f : Faction, uc : UnitClass) extends BaseFactionAction("Retreat from " + r, self.styled(uc))
case class DreadCurseRetreatToAction(self : Faction, r : Region, e : $[Faction], f : Faction, uc : UnitClass, d : Region) extends BaseFactionAction("Retreat " + f.styled(uc) + " from " + r + " to", d)

case class DragonDescendingDoomAction(self : Faction, n : Int) extends OptionFactionAction("Ritual with " + DragonDescending.full) with DoomQuestion

case class DragonAscendingMainAction(self : Faction) extends OptionFactionAction(self.styled(DragonAscending)) with MainQuestion with Soft
case class DragonAscendingDoomAction(self : Faction) extends OptionFactionAction(self.styled(DragonAscending)) with DoomQuestion with Soft

case class DragonAscendingAction(self : Faction, f : Option[Faction], reason : String, n : Int, then : Action) extends BaseFactionAction(self.styled(DragonAscending) + " before " + f./("" + _ + " ").|("") + reason, "Rise to " + n.power)
case class DragonAscendingAskAction(self : Faction, f : Option[Faction], reason : String, then : Action) extends ForcedAction
case class DragonAscendingInstantAction(then : Action) extends ForcedAction
case class DragonAscendingUpAction(reason : String, then : Action) extends ForcedAction
case class DragonAscendingDownAction(f : Faction, reason : String, then : Action) extends ForcedAction
case class DragonAscendingCancelAction(self : Faction, then : Action) extends BaseFactionAction(None, "Cancel")
case class DragonAscendingNotThisTurnAction(self : Faction, then : Action) extends BaseFactionAction(None, "Not in this Action Phase")

// AN
case class GiveWorstMonsterMainAction(self : Faction) extends OptionFactionAction("Give enemies lowest cost monster") with MainQuestion
case class GiveWorstMonsterContinueAction(self : Faction, forum : $[Faction]) extends ForcedAction
case class GiveWorstMonsterSelectMonsterAction(self : Faction, f : Faction, uc : UnitClass, forum : $[Faction]) extends BaseFactionAction("Summon monster for free", self.styled(uc))
case class GiveWorstMonsterAskAction(self : Faction, f : Faction, uc : UnitClass, r : Region, forum : $[Faction]) extends BaseFactionAction("Summon a " + uc + " for free at", r)

case class GiveBestMonsterMainAction(self : Faction) extends OptionFactionAction("Give enemies highest cost monster") with MainQuestion
case class GiveBestMonsterContinueAction(self : Faction, forum : $[Faction]) extends ForcedAction
case class GiveBestMonsterSelectMonsterAction(self : Faction, f : Faction, uc : UnitClass, forum : $[Faction]) extends BaseFactionAction("Summon monster for free", self.styled(uc))
case class GiveBestMonsterAskAction(self : Faction, f : Faction, uc : UnitClass, r : Region, forum : $[Faction]) extends BaseFactionAction("Summon a " + uc + " for free at", r)

case class BuildCathedralMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Build " + AN.styled("Cathedral")) with MainQuestion with Soft
case class BuildCathedralAction(self : Faction, r : Region) extends BaseFactionAction(g => "Build cathedral" + g.forNPowerWithTax(r, self, g.getCathedralCost(r)) + " in", r)

case class FestivalUnManSummonAction(self : Faction, f : Faction) extends BaseFactionAction(AN.styled("UnMen") + " gave power to another faction", "" + f + " gets " + 1.power)

case class DematerializationDoomAction(self : Faction) extends OptionFactionAction(Dematerialization) with DoomQuestion with Soft with PowerNeutral
case class DematerializationFromRegionAction(self : Faction, o : Region) extends BaseFactionAction(self.styled(Dematerialization) + " from", o)
case class DematerializationToRegionAction(self : Faction, o : Region, r : Region) extends BaseFactionAction(self.styled(Dematerialization) + " from " + o + " to", r)
case class DematerializationMoveUnitAction(self : Faction, o : Region, r : Region, uc : UnitClass) extends BaseFactionAction(self.styled(Dematerialization) + " from " + o + " to " + r, self.styled(uc))
case class DematerializationDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

// Neutral Monsters
case class ShantakCarryCultistAction(self : Faction, o : Region, uc : UnitClass, r : Region) extends BaseFactionAction("Carry Cultist to " + r, self.styled(uc) + " from " + o)
case class ShantakCarryCultistCancelAction(self : Faction) extends BaseFactionAction(None, "Done")

// Independent Great Old Ones
case class GodOfForgetfulnessMainAction(self : Faction, d : Region, l : $[Region]) extends OptionFactionAction(self.styled("God of Forgetfulness")) with MainQuestion with Soft
case class GodOfForgetfulnessAction(self : Faction, d : Region, r : Region) extends BaseFactionAction(g => "Move all enemy Cultists to " + self.styled(Byatis) + " " + g.forNPowerWithTax(d, self, 1) + " from", r)

case class FilthMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Place " + self.styled(Filth)) with MainQuestion with Soft
case class FilthAction(self : Faction, r : Region) extends BaseFactionAction(g => "Place " + self.styled(Filth) + " " + g.forNPowerWithTax(r, self, 1) + " in", r)

case class FromBelowMoveSelectAction(self : Faction, uc : UnitClass, region : Region) extends BaseFactionAction("Move for free with " + self.styled(FromBelow), self.styled(uc) + " from " + region)
case class FromBelowMoveAction(self : Faction, uc : UnitClass, o : Region, r : Region) extends BaseFactionAction(g => "Move " + self.styled(uc) + " from " + o + " to", g => r + g.ia(r, self))
case class FromBelowMoveDoneAction(self : Faction) extends BaseFactionAction(None, "Done")
case class FromBelowAttackAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(g => { if (g.tax(r, self) == 0) "Battle for free with " + self.styled(FromBelow) + " in " + r else "Battle " + g.forNPowerWithTax(r, self, 0) + " with " + self.styled(FromBelow) + " in " + r + " " + g.ia(r, self) }, f)
case class FromBelowAttackDoneAction(self : Faction) extends BaseFactionAction(None, "Done")
case class FromBelowCaptureAction(self : Faction, r : Region, f : Faction, uc : UnitClass) extends BaseFactionAction(g => { if (g.tax(r, self) == 0) "Capture for free with " + self.styled(FromBelow) + " in " + r else "Capture " + g.forNPowerWithTax(r, self, 0) + " with " + self.styled(FromBelow) + " in " + r + " " + g.ia(r, self) }, g => f.styled(uc))
case class FromBelowCaptureDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class NightmareWebMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Awaken " + self.styled(Nyogtha) + " with " + self.styled(NightmareWeb)) with MainQuestion with Soft
case class NightmareWebAction(self : Faction, r : Region) extends BaseFactionAction(g => "Awaken " + self.styled(Nyogtha) + g.forNPowerWithTax(r, self, 2) + " in", g => r + g.ia(r, self))

// Neutral Spellbooks
case class MaoCeremonyAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(MaoCeremony, self.styled(uc) + " in " + r)
case class MaoCeremonyDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class RecriminationsMainAction(self : Faction) extends OptionFactionAction(Recriminations.full) with MainQuestion with Soft
case class RecriminationsAction(self : Faction, sb : Spellbook) extends BaseFactionAction("Discard spellbook", sb)

case class UndimensionedMainAction(self : Faction) extends OptionFactionAction(Undimensioned.full) with MainQuestion with Soft
case class UndimensionedContinueAction(self : Faction, destinations : $[Region], moved : Boolean) extends ForcedAction with Soft
case class UndimensionedSelectAction(self : Faction, destinations : $[Region], uc : UnitClass, r : Region) extends BaseFactionAction(g => Undimensioned.full + " move unit", self.styled(uc) + " from " + r) with Soft
case class UndimensionedAction(self : Faction, destinations : $[Region], uc : UnitClass, r : Region, dest : Region) extends BaseFactionAction(g => Undimensioned.full + " move " + self.styled(uc) + " from " + r + " to", g => dest + g.ia(dest, self))
case class UndimensionedDoneAction(self : Faction) extends BaseFactionAction(None, "Done")
case class UndimensionedCancelAction(self : Faction, destinations : $[Region]) extends BaseFactionAction(None, "Cancel") with Cancel

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

class Player(private val faction : Faction)(implicit game : Game) {
    var gates : $[Region] = $
    var cathedrals : $[Region] = $

    var spellbooks : $[Spellbook] = $
    var upgrades : $[Spellbook] = $
    var borrowed : $[Spellbook] = $

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

    var unfulfilled : $[Requirement] = faction.requirements(game.options)

    var power : Int = 8
    var doom : Int = 0
    var es : $[ElderSign] = $
    var revealed : $[ElderSign] = $
    var loyaltyCards : $[LoyaltyCard] = $
    var obtainedLoyaltyCard : Boolean = false

    var units : $[UnitFigure] = faction.allUnits.indexed./((uc, i) => new UnitFigure(faction, uc, faction.allUnits.take(i).count(uc) /* + 1 */, faction.reserve))

    var hibernating = false
    var iceAge : |[Region] = None
    var unitGate : |[UnitFigure] = None

    var ignoredSacrificeHighPriest = false

    def active = power > 0 && !hibernating
    def allGates = gates ++ unitGate./(_.region).toList
    def needs(rq : Requirement) = unfulfilled.contains(rq)
    def has(sb : Spellbook) = faction.abilities.contains(sb) || spellbooks.contains(sb) || upgrades.contains(sb) || borrowed.contains(sb)
    def used(sb : Spellbook) = oncePerGame.contains(sb) || oncePerTurn.contains(sb) || oncePerRound.contains(sb) || oncePerAction.contains(sb)
    def can(sb : Spellbook) = has(sb) && !used(sb)
    def ignored(sb : Spellbook) = ignorePerGame.contains(sb) || ignorePerTurn.contains(sb) || ignorePerInstant.contains(sb)
    def option(io : IgnoreOption) = ignoreOptions.contains(io)
    def want(sb : Spellbook) = can(sb) && !ignored(sb)
    def hasAllSB = unfulfilled.none
    def unclaimedSB = faction.library.num - spellbooks.num - unfulfilled.num
    def present(region : Region) = units.exists(_.region == region)
    def at(region : Region) = units.%(_.region == region)
    def at(region : Region, uclass : UnitClass) = units.%(_.region == region).%(_.uclass == uclass)
    def at(region : Region, utype : UnitType) = units.%(_.region == region).%(_.uclass.utype == utype)
    def at(region : Region, utype : UnitType, utype2 : UnitType) = units.%(_.region == region).%(u => u.uclass.utype == utype || u.uclass.utype == utype2)
    def inPool() = units.%(_.region == faction.reserve)
    def inPool(uclass : UnitClass) = units.%(_.region == faction.reserve).%(_.uclass == uclass)
    def inPool(utype : UnitType) = units.%(_.region == faction.reserve).%(_.uclass.utype == utype)
    def onMap(utype : UnitType) = units.%(_.region.glyph.onMap).%(_.uclass.utype == utype)
    def onMap(uclass : UnitClass) = units.%(_.region.glyph.onMap).%(_.uclass == uclass)
    def all = units.%(_.region.glyph.inPlay)
    def all(uclass : UnitClass) = units.%(_.region.glyph.inPlay).%(_.uclass == uclass)
    def all(utype : UnitType) = units.%(_.region.glyph.inPlay).%(_.uclass.utype == utype)
    def goo(uclass : UnitClass) = all(uclass).single.get
    def has(uclass : UnitClass) = all(uclass).any
    def moved = units.%(_.region.glyph.onMap).%(_.has(Moved))
}

object RitualTrack {
    val for3 = 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 999
    val for4 = 5 :: 6 :: 7 :: 7 :: 8 :: 8 :: 9 :: 10 :: 999
    val for5 = 5 :: 6 :: 6 :: 7 :: 7 :: 8 :: 8 :: 9 :: 9 :: 10 :: 999
}

case class Nexus(region : Region, attacker : Faction, defender : Faction, factions : $[Faction], acted : Boolean, battled : $[Region])

case class IceAges(list : $[Faction]) {
    def any = list.any
    def tax = list.num
    override def toString = list./(" (" + _.styled(IceAge) + ")").mkString("")
}

object NoIceAges extends IceAges($)

@EnableReflectiveInstantiation
sealed trait GameOption
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

sealed trait NeutralMonsterOption extends GameOption
case object UseGhast extends NeutralMonsterOption
case object UseGug extends NeutralMonsterOption
case object UseShantak extends NeutralMonsterOption
case object UseStarVampire extends NeutralMonsterOption

sealed trait IGOOOption extends GameOption
case object UseByatis extends IGOOOption
case object UseAbhoth extends IGOOOption
case object UseDaoloth extends IGOOOption
case object UseNyogtha extends IGOOOption

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

class Game(val board : Board, val ritualTrack : $[Int], val factions : $[Faction], val logging : Boolean, val providedOptions : $[GameOption]) {
    private implicit val game : Game = this

    val options = providedOptions ++ $(PlayerCount(factions.num))
    val players = factions./(f => f -> new Player(f)).toMap
    var neutrals : Map[NeutralFaction, Player] = Map()
    def nfactions = factions ++ neutrals.keys

    var starting = Map[Faction, Region]()
    var turn = 1
    var round = 1
    var doomPhase = false
    var order : $[Faction] = $
    var first : Faction = factions(0)
    var gates : $[Region] = $
    def unitGates = factions./~(_.unitGate)./(_.region)
    var cathedrals : $[Region] = $
    var desecrated : $[Region] = $
    var battled : $[Region] = $
    var acted : Boolean = false
    var reveal : Boolean = false
    var ritualMarker = 0
    var battle : |[Battle] = None
    var nexus : |[Nexus] = None
    var nexusExtra : |[Faction] = None
    var anyIceAge : Boolean = false
    var neutralSpellbooks : $[Spellbook] = options.contains(NeutralSpellbooks).$(MaoCeremony, Recriminations, Shriveling, StarsAreRight, UmrAtTawil, Undimensioned)
    var loyaltyCards : $[LoyaltyCard] = LoyaltyCardConfig.loyaltyCardConditions.collect {
        case (opt, card) if options.contains(opt) => card
    }
    var demCaseMap : Map[Region, Int] = board.regions.map(r => r -> 0).toMap // Solution for keeping track of use cases for dematerialization, for the AN bot.

    // When you declare a paid Battle in a Nyogtha region, remember the *other* Nyogtha region here.
    var nyogthaPendingFreeBattle  : Map[Faction, Region]      = Map.empty

    // These are used to keep track of Nyogthas spellbook requirement.
    var nyogthaEnemyGOOSeen       : Map[Faction, Boolean]     = Map.empty
    var nyogthaPairByFaction      : Map[Faction, Set[Region]] = Map.empty
    var nyogthaPairProgress       : Map[Faction, Int]         = Map.empty
    var nyogthaPairHadEnemyGOO    : Map[Faction, Boolean]     = Map.empty
    var nyogthaPairNyogthaDied    : Map[Faction, Boolean]     = Map.empty
    var lastBattleRegionByFaction : Map[Faction, Region]      = Map.empty

    // This is used to make sure Nyarlathotep does not get double rewards for paining/killing both Nyogtha units.
    var harbingerNyogthaAwarded : Set[Faction] = Set.empty

    def tryFromBelow(self : Faction, actedNyogtha : UnitFigure, xgame : Game, makeAction : Region => BaseFactionAction, doneAction : BaseFactionAction, canDo : Region => Boolean, allowSameRegion : Boolean) : Option[List[FactionAction]] = {
        if (!self.has(FromBelow) || self.oncePerAction.contains(FromBelow)) return None

        val nyogthas = self.all(Nyogtha)
        if (nyogthas.size != 2) return None

        val otherOpt = nyogthas.find(_ != actedNyogtha).filter(n => allowSameRegion || n.region != actedNyogtha.region)
        val other = otherOpt.getOrElse {
            return None
        }

        val otherRegion = other.region
        if (!canDo(otherRegion)) return None

        self.oncePerAction :+= FromBelow
        Some(List(makeAction(otherRegion), doneAction))
    }

    def ia(r : Region, f : Faction) : IceAges = {
        if (!anyIceAge)
            NoIceAges
        else {
            val movedHere = f.at(r).%(u => u.has(Moved)).any
            // Show Ice Age unless explicitly moved here this turn *and* it's a normal (paid) move.
            if (movedHere && !f.oncePerAction.contains(FromBelow))
                NoIceAges
            else
                IceAges(factions.%(_.iceAge./(_ == r).|(false)).but(f))
        }
    }

    def tax(r : Region, f : Faction) : Int = (!anyIceAge).?(0).|(ia(r, f).tax)
    def forNPowerWithTax(r : Region, f : Faction, n : Int) : String = { val p = n + tax(r, f); " for " + p.power }
    def for1PowerWithTax(r : Region, f : Faction) : String = { val p = 1 + tax(r, f); if (p != 1) " for " + p.power else "" }

    def unit(ur : UnitRef) = ur.faction.units.%(u => u.uclass == ur.uclass && u.index == ur.index).only

    def affordF(f : Faction, n : Int)(r : Region) = f.power >= tax(r, f) + n

    def payTax(self : Faction, r : Region) : Int = {
        val s = ia(r, self)
        if (s.any) {
            self.power -= s.tax
            self.log("lost", s.tax.power, "due to", s.list./(_.styled(IceAge)).mkString(", "))
            s.tax
        }
        else
            0
    }

    def hasMoved(f : Faction) = f.units.%(_.region.glyph.onMap).%(_.has(Moved)).any // ??


    def nx(r : Region) = nexus./(_.region == r).|(true)
    def nx(u : UnitFigure) = nexus./(_.region == u.region).|(true)

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
            p.obtainedLoyaltyCard = o.obtainedLoyaltyCard
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

    def satisfy(f : Faction, rq : Requirement, text : String, e : Int = 0) =
        if (f.needs(rq)) {
            f.unfulfilled = f.unfulfilled.but(rq)
            f.log("achieved", f.styled(text), ((e + rq.es) > 0).??("and gained " + (e + rq.es).es))
            giveES(f, e + rq.es)
        }

    def satisfyIf(f : Faction, rq : Requirement, text : String, c : => Boolean, p : Int = 0) =
        if (f.needs(rq)) {
            if (c) {
                satisfy(f, rq, text, 0)
                if (p != 0) {
                    f.power += p
                    f.log("got", p.power)
                }
            }
        }

    def giveES(f : Faction, n : Int = 1) {
        val total = factions./(f => f.es.num + f.revealed.num).sum
        var count = n

        if (total + count > 36) {
            f.log("got", (total + count - 36).doom, "instead of", (total + count - 36).es)
            f.doom += (total + count - 36)
            count = 36 - total
        }

        f.es ++= count.times(ElderSign(0))
    }

    def capture(f : Faction, u : UnitFigure) {
        eliminate(u)
        u.region = f.prison
    }

    def eliminate(u : UnitFigure) {
        val f = u.faction

        if (u.uclass.utype == Cultist && f.has(Passion) && u.region.glyph.onMap)
            f.oncePerAction :+= Passion

        if (u.uclass.utype == GOO) {
            val isTrueNyogthaDeath =
                if (u.uclass == Nyogtha) {
                    val nyogthas = f.all(Nyogtha)
                    val othersAlive = nyogthas.but(u).exists(_.health != Killed)
                    !othersAlive
                } else true

            if (isTrueNyogthaDeath) {
                factions.foreach { f =>
                    if (f.has(Daoloth) && f.upgrades.has(Interdimensional).not) {
                        f.upgrades :+= Interdimensional
                        f.log("gained", f.styled(Interdimensional), "for", f.styled(Daoloth))
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

    def place(f : Faction, uc : UnitClass, r : Region) {
        val u = f.inPool(uc).head
        u.region = r
    }

    def summon(f : Faction, uc : UnitClass, r : Region) {
        val u = f.inPool(uc).head
        u.region = r
        u.add(Summoned)
    }

    def move(u : UnitFigure, r : Region) {
        u.region = r
    }

    def moveableUnits(p : Faction) : $[UnitFigure] = {
        p.units.%(nx).%(_.region.glyph.onMap).%(!_.has(Moved)).%(_.canMove)
    }

    def canCapture(faction : Faction, victim : Faction, r : Region) : Boolean =
        if (faction == victim)
            false
        else
        if (victim.at(r, Cultist).none)
            false
        else
        if (victim.at(r, GOO).any)
            false
        else
        if (faction.at(r, GOO).any)
            true
        else
        if (victim.at(r, Terror).any)
            false
        else
        if (victim.at(r, Monster).any)
            victim.at(r, Monster).none
        else
        if (victim.has(Ferox) && victim.has(Ithaqua))
            false
        else
        if (faction.at(r, Terror).any)
            true
        else {
            faction.at(r, Monster).%(u => u.uclass.canCapture(u)).any
    }

    def canAttack(f : Faction, v : Faction, r : Region) : Boolean =
        v.at(r).any &&
        f.strength(f.at(r), v) > 0 &&
        f.at(r).exceptIsolatedBrainless(f, this).any

    def canAccessGate(f : Faction, r : Region) = f.gates.contains(r) || f.unitGate.?(_.region == r) || (f.has(TheyBreakThrough) && gates.contains(r))

    def getControlledGatesRegions(f : Faction) : $[Region] = {
        val gates = f.gates.%(nx).%(_.glyph.onMap)

        val yogRegion = if (f == OW && f.has(YogSothoth)) List(f.goo(YogSothoth).region) else List()

        (gates ++ yogRegion).distinct
    }

    def getGOOControlledGateRegions(f : Faction) : $[Region] = {
        val gates = f.gates.%(nx).%(_.glyph.onMap)

        val gooGateRegions = gates.filter(r => f.at(r, GOO).any)

        val yogRegion =
            if (f == OW && f.has(YogSothoth)) List(f.goo(YogSothoth).region)
            else List()

        (gooGateRegions ++ yogRegion).distinct
    }

    def getSummonRegions(f : Faction) : List[Region] = {
        val gates = (f.gates.%(nx).%(_.glyph.onMap).any).??(f.gates.%(nx).%(_.glyph.onMap))

        val yogRegion = (f == OW && f.has(YogSothoth)).$(f.goo(YogSothoth).region)

        val breakThroughRegions = (f == OW && f.has(TheyBreakThrough)).??(factions.but(f).flatMap(_.gates) ++ abandonedGates)

        (gates ++ yogRegion ++ breakThroughRegions).distinct
    }

    def isFactionGOO(u : UnitFigure) : Boolean = u.uclass.utype == GOO && !u.uclass.isInstanceOf[IGOO]

    def sortAllUnits(p : Faction)(a : UnitFigure, b : UnitFigure) : Boolean = {
        val igoosPriority : Map[String, Int] = Map(
            Abhoth.name -> 1,
            Byatis.name -> 2,
            Daoloth.name -> 3,
            Nyogtha.name -> 4
        ).withDefaultValue(1000)

        val neutralsPriority : Map[String, Int] = Map(
            Ghast.name -> 10,
            Gug.name -> 11,
            Shantak.name -> 12,
            StarVampire.name -> 13,
            Filth.name -> 14
        ).withDefaultValue(0)

        val ap = igoosPriority(a.uclass.name)
        val bp = igoosPriority(b.uclass.name)

        if (ap != bp)
            ap < bp
        else {
            val an = neutralsPriority(a.uclass.name)
            val bn = neutralsPriority(b.uclass.name)

            if (an != bn)
                an < bn
            else if (a.uclass == b.uclass)
                board.regions.indexOf(a.region) < board.regions.indexOf(b.region)
            else if (a.uclass.utype != b.uclass.utype)
                a.uclass.utype.priority > b.uclass.utype.priority
            else
                p.units.indexOf(a) < p.units.indexOf(b)
        }
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
        val keeper = controler./~(f => f.at(r).%(_.health == Alive).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && f.has(RedSign))).headOption)
        val others = factions.%(f => !f.gates.contains(r)).%(_.at(r).num > 0).sortBy(f => f.strength(f.at(r), f))
        if (gate || !others.none || ds) {
            $("" + r + ":" + gate.??(" " + keeper./(u => ("[[[".styled(u.faction.style) + " " + u + " " + "]]]".styled(u.faction.style))).|("[[[ GATE ]]]".styled("power"))) + ds.??(" " + YS.styled(")|("))) ++
            controler./(f => "    " + f.at(r).diff(keeper.$)./(u => u.toString).mkString(", ")).$ ++
            others.sortBy(_.units.%(_.region == r).num)./ { f =>  "    " + f.at(r)./(u => u.toString).mkString(", ") } ++ $("&nbsp;")
        }
        else
            $
    }

    def powerLeader(f : Faction) = f.power > factions.but(f)./(_.power).max

    def targetDragonAscending(f : Faction) = (f.power > factions.but(f)./(_.power).max).?(factions.but(f).%(_.want(DragonAscending))).|(Nil)

    def showROAT() {
        def vv(v : Int) = (v == 999).?("Instant Death").|(v)
        log("Ritual of Annihilation".styled("doom"), "track", ritualTrack.zipWithIndex./{ case (v, n) => (n == ritualMarker).?(("[" + vv(v) + "]").styled("str")).|("[".styled("xxxhighlight") + vv(v) + "]".styled("xxxhighlight")) }.mkString("-".styled("highlight")))
    }

    def hasCultistOrRSDY(f : Faction, r : Region) : Boolean = f.at(r, Cultist).any || (f.at(r, DarkYoung).any && f.has(RedSign))

    def checkGatesLost() {
        factions.foreach { f =>
            f.gates.foreach { g =>
                if (!hasCultistOrRSDY(f, g)) {
                    f.gates = f.gates.but(g)
                    f.log("lost control of the gate in", g)
                }
            }
        }
    }

    def checkPowerReached() {
        factions.foreach { f =>
            satisfyIf(f, Gates3Power12, "Have 12 Power", f.power >= 12)
            satisfyIf(f, Gates4Power15, "Have 15 Power", f.power >= 15)
        }
    }

    def checkAbhothSpellbook() {
        factions.foreach { f =>
            if (f.has(Abhoth) && f.upgrades.has(TheBrood).not) {
                val monsters = f.units.monsters.onMap
                val classes = monsters./(_.uclass).distinct.num
                val total = monsters.num + f.onMap(Filth).num

                if (classes >= 4 || total >= 8) {
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

                if (r != GC.deep && !gates.contains(r)) {
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

        gates.foreach { g =>
            if (factions.%(f => f.gates.contains(g)).none && hasCultistOrRSDY(self, g)) {
                self.gates :+= g
                self.log("gained control of the gate in", g)
            }
        }

        factions.foreach { f =>
            satisfyIf(f, OceanGates, "Control three Gates in Ocean areas", f.gates.%(_.glyph == Ocean).num >= 3)
            satisfyIf(f, OceanGates, "Four Gates exist in Ocean areas", (gates ++ unitGates).%(_.glyph == Ocean).num >= 4)

            satisfyIf(f, Gates3Power12, "Control three Gates", f.gates.num >= 3)
            satisfyIf(f, Gates4Power15, "Control four Gates", f.gates.num >= 4)

            satisfyIf(f, Spread4, "Have Units in four Areas", board.regions.%(r => f.at(r).any).num >= 4)
            satisfyIf(f, Spread6, "Have Units in six Areas", board.regions.%(r => f.at(r).any).num >= 6)
            satisfyIf(f, Spread8, "Have Units in eight Areas", board.regions.%(r => f.at(r).any).num >= 8)
            satisfyIf(f, SpreadSocial, "Share Areas with all enemies", factions.but(f).forall(e => board.regions.exists(r => f.at(r).any && e.at(r).any)), factions.but(f).num)

            if (board.starting(f).num == 2) {
                val o = board.starting(f).but(starting(f)).head
                satisfyIf(f, OppositeGate, "Gate exists in " + o.name, (gates ++ unitGates).contains(o))
            }

            satisfyIf(f, EightGates, "Eight Gates on the map", (gates ++ unitGates).%(_.glyph.onMap).num >= 8)
            satisfyIf(f, TenGates, "Ten Gates on the map", (gates ++ unitGates).%(_.glyph.onMap).num >= 10)
            satisfyIf(f, TwelveGates, "Twelve Gates on the map", (gates ++ unitGates).%(_.glyph.onMap).num >= 12)

            satisfyIf(f, GooMeetsGoo, "GOO shares Area with another GOO", board.regions.%(r => f.at(r, GOO).any && factions.but(f).%(e => e.at(r, GOO).any).any).any)
            satisfyIf(f, UnitsAtEnemyGates, "Units at two enemy Gates", board.regions.%(r => f.at(r).any && factions.but(f).%(e => e.gates.contains(r)).any).num >= 2)
        }
    }

    def getCathedralCost(r : Region) : Int = 1 + board.connected(r).intersect(cathedrals).any.??(2)

    def outOfTurn(f : Faction) : $[Action] = {
        $ ++
        f.es.some./(l => RevealESOutOfTurnAction(f))
    }

    def perform(action : Action) : ($[String], Continue) = {
        val c = performX(action)

        val l = mlog.reverse

        mlog = $

        (l, c)
    }

    def performX(action : Action) : Continue = {
        val c = performY(action)

        c match {
            case Force(a : OutOfTurnReturn) => c
            case Force(a) => performX(a)
            case _ => c
        }
    }

    def performY(action : Action) : Continue = action match {
        // INIT
        case StartAction =>
            val pending = factions.%!(starting.contains)

            if (pending.any) {
                val f = pending.minBy(board.starting(_).num)

                /*
                if (f == CC)
                board.regions.foreach { r =>
                    var destinations = board.connected(r)

                    destinations = destinations ++ destinations.flatMap(board.connected)

                    destinations = destinations.distinct//.but(region).diff(destinations)

                    log("From " + r, "cant fly to " + board.regions.diff(destinations).mkString(", "))
                }
                */

                board.starting(f).diff(starting.values.$)./(StartingRegionAction(f, _))
            }
            else
                PlayOrderAction

        case StartingRegionAction(self, r) =>
            starting += self -> r

            1.to(6).foreach(_ => place(self, Acolyte, r))

            // Temp starting setup (for debug)
            // if (self.has(Immortal)) {
            //     place(self, Cthulhu, r)
            //     satisfy(self, FirstDoomPhase, "Debug")
            //     satisfy(self, KillDevour1, "Debug")
            //     satisfy(self, KillDevour2, "Debug")
            //     satisfy(self, AwakenCthulhu, "Debug")
            //     satisfy(self, OceanGates, "Debug")
            //     satisfy(self, FiveSpellbooks, "Debug")
            // }

            if (options.has(HighPriests)) {
                // Add High Priest to pool.
                1.times(HighPriest).foreach { u =>
                    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num, self.reserve)
                }

                // Add High Priest Loyalty Card to the faction.
                self.loyaltyCards = self.loyaltyCards :+ HighPriestCard
            }

            gates :+= r

            self.gates :+= r

            self.log("started in", r)

            StartAction

        case PowerGatherAction(last) =>
            if (factions.%(f => !f.hibernating && f.power > 0).any)
                return MainNextPlayerAction(last)

            turn += 1

            // Probably don't need to clear flags here, but to be safe.
            nyogthaEnemyGOOSeen       = Map.empty
            nyogthaPairByFaction      = Map.empty
            nyogthaPairProgress       = Map.empty
            nyogthaPairHadEnemyGOO    = Map.empty
            nyogthaPairNyogthaDied    = Map.empty
            lastBattleRegionByFaction = Map.empty

            factions.foreach { f =>
                f.oncePerTurn = Nil
                f.ignorePerTurn = Nil
            }

            log("POWER GATHER")

            factions.foreach { f =>
                val hibernate = f.power
                val cultists = f.all(Cultist).num
                val captured = factions./~(w => w.at(f.prison)).num
                val ownGates = f.gates.num + f.unitGate.any.??(1)
                val oceanGates = (f.has(YhaNthlei) && f.has(Cthulhu)).?(factions.but(f)./(f => f.allGates.%(_.glyph == Ocean).num).sum).|(0)
                val darkYoungs = (f.has(RedSign)).?(f.all(DarkYoung).num).|(0)
                val feast = (f.has(Feast)).?(desecrated.%(r => f.at(r).any).num).|(0)
                val abandoned = abandonedGates.num
                var cathedralGates = 0

                if (f.has(WorshipServices)) {
                    factions.but(f).foreach { fx =>
                        board.regions.%(nx).%(cathedrals.contains).%(r => fx.gates.contains(r)).some.foreach { l => cathedralGates += l.num }
                    }
                }
                else if (factions.%(_.has(WorshipServices)).num > 0) {
                    board.regions.%(nx).%(cathedrals.contains).%(r => f.gates.contains(r)).some.foreach { l => cathedralGates += l.num }
                }

                f.power = hibernate + ownGates * 2 + abandoned + cultists + captured + oceanGates + darkYoungs + feast + cathedralGates
                f.hibernating = false

                val fromHibernate = if (hibernate > 0) Some(hibernate.styled("region") + " hibernate") else None
                val fromGates = if (ownGates > 0) Some((("2 x " + ownGates).styled("region") + " gate" + (if (ownGates == 1) "" else "s"))) else None
                val fromAbandoned = if (abandoned > 0) Some(abandoned.styled("region") + " abandoned") else None
                val fromCultist = if (cultists > 0) Some(cultists.styled("region") + " cultist" + (if (cultists == 1) "" else "s")) else None
                val fromCaptured = if (captured > 0) Some(captured.styled("region") + " captured") else None
                val fromYhaNthlei = if (oceanGates > 0) Some(oceanGates.styled("region") + " enemy controlled ocean gate" + (if (oceanGates == 1) "" else "s")) else None
                val fromDarkYoungs = if (darkYoungs > 0) Some(darkYoungs.styled("region") + " Dark Young" + (if (darkYoungs == 1) "" else "s")) else None
                val fromFeast = if (feast > 0) Some(feast.styled("region") + " desecrated") else None
                val fromCathedralGates = if (cathedralGates > 0) Some(cathedralGates.styled("region") + " cathedral" + (if (cathedralGates == 1) "" else "s")) else None

                f.log("got", f.power.power, "(" + $(fromHibernate, fromGates, fromAbandoned, fromCultist, fromCaptured, fromYhaNthlei, fromDarkYoungs, fromFeast, fromCathedralGates).flatten.mkString(" + ") + ")")
            }

            factions.foreach { f =>
                val captured = factions.flatMap(w => w.at(f.prison))

                if (captured.any) {
                    captured.foreach(eliminate)

                    f.log("released", captured.mkString(", "))
                }
            }


            val max = factions./(_.power).max
            val min = (max + 1) / 2

            if (min == 0) {
                log("Humanity won")
                return GameOver(Nil)
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
                    val cs = f.onMap(Cultist)
                    if (cs.any)
                        return cs./(c => MaoCeremonyAction(f, c.region, c.uclass)) :+ MaoCeremonyDoneAction(f)
                }
            }

            factions.foreach(_.ignorePerInstant = $)

            DragonAscendingInstantAction(DragonAscendingUpAction("first player determination", FirstPlayerDeterminationAction))

        case DoomPhaseAction =>
            doomPhase = true

            factions.foreach { f =>
                val validGates = f.gates.filter { r =>
                    val filthHere = factions.exists { other =>
                            other != f &&
                            other.has(TheBrood) &&
                            other.at(r).exists(_.uclass == Filth)
                        }

                    !filthHere
                }

                val g = validGates.num + f.unitGate.any.??(1)
                f.doom += g
                f.log("got", g.doom)

                if (f.has(Byatis)) {
                    val r = f.goo(Byatis).region
                    val enemiesPresent = factions.but(f).exists(other => other.at(r).any)
                    if (!enemiesPresent) {
                        f.log("gained", 1.es, "from", f.styled(Byatis), "and", "Toad of Berkeley".styled("nt"))
                        giveES(f, 1)
                    }
                }
            }

            log(CthulhuWarsSolo.DottedLine)
            showROAT()

            CheckSpellbooksAction(DoomNextPlayerAction(first))

        case ActionPhaseAction =>
            if (factions.%(_.doom >= 30).any || ritualTrack(ritualMarker) == 999)
                return GameOverPhaseAction

            doomPhase = false

            // Remove the High Priest Loyalty card, if there is one (it's only ever assigned at start).
            loyaltyCards = loyaltyCards.but(HighPriestCard)

            log("=======================================================================================================================")
            log("Turn", turn)
            log("ACTIONS")

            round = 0

            CheckSpellbooksAction(MainAction(first))

        case GameOverPhaseAction =>
            factions.%(_.needs(AnytimeGainElderSigns)).foreach { f =>
                satisfy(f, AnytimeGainElderSigns, "Anytime Spellbook", min(3, factions.but(f).%(_.hasAllSB).num))
                return CheckSpellbooksAction(GameOverPhaseAction)
            }

            factions.%(_.es.any).foreach { f =>
                f.log("revealed", f.es.num.es, "for", f.es./(_.value).sum.doom)
                f.doom += f.es./(_.value).sum
                f.revealed ++= f.es
                f.es = Nil
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
                fs./(FirstPlayerAction(first, _))
            }
            else {
                val old = first
                first = fs.head

                if (old != first)
                    first.log("became the first player")

                PlayOrderAction
            }

        case FirstPlayerAction(self, f) =>
            first = f

            if (self == f)
                self.log("decided to remain the first player")
            else
                self.log("chose", f, "as the first player")

            PlayOrderAction

        case PlayOrderAction =>
            satisfy(first, FirstPlayer, "Become Starting Player")

            val forward = factions.dropWhile(_ != first) ++ factions.takeWhile(_ != first)
            val backward = forward.take(1) ++ forward.drop(1).reverse

            PlayDirectionAction(first, forward) :: PlayDirectionAction(first, backward)

        case PlayDirectionAction(self, fs) =>
            order = fs

            log("Play order", order.mkString(", "))

            if (turn == 1)
                ActionPhaseAction
            else {
                log(CthulhuWarsSolo.DottedLine)
                log("DOOM PHASE")

                factions.foreach { f => satisfyIf(f, FirstDoomPhase, "The first Doom phase", turn == 2) }
                factions.foreach { f => satisfyIf(f, FiveSpellbooks, "Have five spellbooks", f.unfulfilled.num == 1) }

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
                if (!end && next.isInstanceOf[MainAction])
                    log(CthulhuWarsSolo.DottedLine)

                Force(next)
            }

        case SpellbookAction(self, sb, next) =>
            self.spellbooks = self.spellbooks :+ sb

            self.log("received", sb.full)

            neutralSpellbooks = neutralSpellbooks.but(sb)

            if (self.hasAllSB)
                factions.foreach { f => satisfy(f, AnotherFactionAllSpellbooks, "Another faction has all spellbooks") }

            self.ignorePerInstant = $

            CheckSpellbooksAction(next)

        case ElderSignAction(f, _, v, public, next) =>
            if (v == 0) {
                val n = f.es.%(_.value == 0).num
                f.doom += n
                f.es = f.es.%(_.value > 0)
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
            (self.es +: self.es.sortBy(_.value)./(e => $(e))).distinct./(RevealESAction(self, _ , false, MainAction(self))) :+ MainCancelAction(self)

        case RevealESDoomAction(self) =>
            (self.es +: self.es.sortBy(_.value)./(e => $(e))).distinct./(RevealESAction(self, _ , self.has(StarsAreRight), DoomAction(self))) :+ DoomCancelAction(self)

        case RevealESOutOfTurnAction(self) =>
            (self.es +: self.es.sortBy(_.value)./(e => $(e))).distinct./(RevealESAction(self, _ , doomPhase && self.has(StarsAreRight), OutOfTurnDoneAction)) :+ OutOfTurnCancelAction(self)

        case RevealESAction(self, es, power, next) =>
            val sum = es./(_.value).sum
            self.doom += sum

            self.revealed ++= es
            self.es = self.es.diff(es)

            if (power)
                self.power += sum

            self.log("revealed", es./(_.short).mkString(" "), "for", sum.doom, power.??("and " + sum.power))

            Force(next)


        // LOYALTY CARDS
        case IndependentGOOMainAction(self, lc, l) =>
            Ask(self).each(l)(r => IndependentGOOAction(self, lc, r, lc.power)).cancel

        case IndependentGOOAction(self, lc, r, _) =>
            self.loyaltyCards :+= lc
            loyaltyCards :-= lc

            self.power -= lc.power

            self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", lc.power.power)

            self.units :+= new UnitFigure(self, lc.unit, 1, r)

            lc.unit match {
                case Abhoth =>
                    if (neutrals.contains(NeutralAbhoth).not)
                        neutrals += NeutralAbhoth -> new Player(NeutralAbhoth)

                    self.units ++= NeutralAbhoth.units./(u => new UnitFigure(self, u.uclass, u.index, (u.region == NeutralAbhoth.reserve).?(self.reserve).|(u.region), u.state, u.health))

                    NeutralAbhoth.units = $

                case Daoloth =>
                    self.upgrades :+= CosmicUnity

                case Nyogtha =>
                    self.units :+= new UnitFigure(self, lc.unit, 2, r)

                    self.upgrades :+= FromBelow

                case _ =>
            }

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal.full)

                giveES(self, 1)
            }

            Force(MainAction(self))

        case LoyaltyCardDoomAction(self) =>
            val hasGOOAtGate = self.gates.exists(r => self.at(r, GOO).any)
            val isAncientsAndHaveFourCathedrals = self == AN && cathedrals.num == 4

            val availableCards = loyaltyCards
                .filter { c =>
                    val doomOK  = self.doom  >= c.doom
                    val powerOK = self.power >= c.power
                    doomOK && powerOK && (c.doom > 0 || (hasGOOAtGate || isAncientsAndHaveFourCathedrals))
                }
                .distinct

            val cardActions = availableCards./(c => NeutralMonstersAction(self, c, DoomAction(self)))

            cardActions.:+(DoomCancelAction(self))

        case NeutralMonstersAction(self, lc, next) =>
            self.loyaltyCards :+= lc
            loyaltyCards :-= lc

            self.obtainedLoyaltyCard = true

            self.doom -= lc.doom
            self.power -= lc.power

            if (lc.power == 0) {
                self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", lc.doom.doom)
            }
            else if (lc.doom == 0) {
                self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", lc.power.power)
            }
            else {
                self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", lc.doom.doom, "and", lc.power.power)
            }

            lc.quantity.times(lc.unit).foreach { u =>
                self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num, self.reserve)
            }

            // Place unit(s).
            val regions = getControlledGatesRegions(self)

            if (regions.any) {
                Ask(self).each(regions)(r => LoyaltyCardSummonAction(self, lc.unit, r, next))
            } else {
                self.log("had nowhere to place", self.styled(lc.unit))
                Force(next)
            }

        case LoyaltyCardSummonAction(self, uc, r, next) =>
            place(self, uc, r)
            self.log("placed", self.styled(uc), "in", r)

            CheckSpellbooksAction(DoomAction(self))

            if (uc == Ghast && self.inPool(Ghast).any) {
                // Ghast: Repeat until none left in pool.
                QAsk(getControlledGatesRegions(self)./(r => LoyaltyCardSummonAction(self, uc, r, next)))
            }
            else {
                // Standard behavior is summoning one unit.
                Force(next)
            }


        // DOOM
        case DoomAction(self) =>
            checkGatesLost()

            var options : $[FactionAction] = $

            val cost = self.has(Herald).?(5).|(ritualCost)

            if (self.want(DragonAscending) && factions.%(_.power > self.power).any)
                options :+= DragonAscendingDoomAction(self)

            if (self.power >= cost && !acted) {
                options :+= RitualAction(self, cost, 1)

                if (self.can(DragonDescending))
                    options :+= DragonDescendingDoomAction(self, cost)
            }

            if (self.can(BloodSacrifice) && self.has(ShubNiggurath) && self.all(Cultist).any)
                options :+= BloodSacrificeDoomAction(self)

            if (self.can(DeathFromBelow) && self.inPool(Monster).any)
                options :+= DeathFromBelowDoomAction(self)

            if (self.can(Dematerialization))
                options :+= DematerializationDoomAction(self)

            if (self.es.num > 0)
                options :+= RevealESDoomAction(self)

            if (self.all(HighPriest).any)
                options :+= SacrificeHighPriestDoomAction(self)

            // If you can afford any of the available loyaltyCards.
            // In the case of 0-doom (= IGOO) cards also: if you have a GOO at a controlled gate.
            // Unless you are AN, in which case four cathedrals are required for IGOO:s instead.
            if (!self.obtainedLoyaltyCard && loyaltyCards.exists { c =>
                val doomOK  = self.doom  >= c.doom
                val powerOK = self.power >= c.power
                doomOK && powerOK && c.doom > 0
            })
                options :+= LoyaltyCardDoomAction(self)


            if (self.needs(AnytimeGainElderSigns))
                options :+= AnytimeGainElderSignsDoomAction(self)

            if (self.has(AncientSorcery) && self.at(SL.slumber, SerpentMan).any)
                options :+= AncientSorceryDoomAction(self)
            else
                options :+= DoomDoneAction(self)

            options

        case DoomCancelAction(self) =>
            DoomAction(self)

        case DoomNextPlayerAction(self) =>
            CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingDownAction(self, "doom action", DoomAction(self))))

        case DoomDoneAction(self) =>
            acted = false

            self.obtainedLoyaltyCard = false

            val next = (order ++ order).dropWhile(_ != self).drop(1).head

            if (next != first)
                DoomNextPlayerAction(next)
            else {
                factions.foreach(_.borrowed = $)
                CheckSpellbooksAction(ActionPhaseAction)
            }

        // RITUAL
        case RitualAction(self, cost, k) =>
            self.power -= cost

            val validGates = self.gates.filter { r =>
                val filthHere = factions.exists { other =>
                    other != self &&
                    other.has(TheBrood) &&
                    other.at(r).exists(_.uclass == Filth)
                }
                !filthHere
            }

            val doom = (validGates.num + self.unitGate.any.??(1)) * k

            var es = self.all(GOO).count(isFactionGOO)

            if (self.has(Consecration)) {
                if (cathedrals.num == 4) {
                    es = 2
                }
                else if (cathedrals.num > 0) {
                    es = 1
                }
            }

            self.doom += doom
            self.log("performed the ritual", "for", cost.power, "and gained", doom.doom, (es > 0).??("and " + es.es))
            giveES(self, es)

            acted = true

            if (ritualTrack(ritualMarker) != 999)
                ritualMarker += 1

            showROAT()
            satisfy(self, PerformRitual, "Perform Ritual of Annihilation")
            CheckSpellbooksAction(DoomAction(self))

        // MAIN
        case MainAction(self) =>
            if (factions.%(f => f.unfulfilled.num + f.spellbooks.num < f.spellbooks.num).any)
                return CheckSpellbooksAction(MainAction(self))

            val others = nfactions.but(self)

            if (self.active) {
                others.%(_.can(Devolve)).%(_.inPool(DeepOne).any).foreach { f =>
                    board.regions.%(f.at(_, Acolyte).any).foreach { r =>
                        if (!acted)
                            if (canCapture(self, f, r))
                                if (!f.option(OutOfTurnDevolveOff))
                                    return Force(DevolveMainAction(f, DevolveDoneAction(f, MainAction(self))))

                        if (!f.ignored(Devolve))
                            if (!acted || (self.hasAllSB && !battled.contains(r)))
                                if (canAttack(self, f, r))
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
                            if (!acted)
                                if (canCapture(self, f, r))
                                    if (!f.option(OutOfTurnSacrificeHighPriestOff))
                                        return Force(SacrificeHighPriestMainAction(f, SacrificeHighPriestDoneAction(f, MainAction(self))))

                            if (!acted || (self.hasAllSB && !battled.contains(r)))
                                if (canAttack(self, f, r))
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
                def apply(n : Int)(r : Region) = self.power >= tax(r, self) + n
                def apply(c : Region => Int)(r : Region) = self.power >= tax(r, self) + c(r)
            }

            var options : $[FactionAction] = $

            if (self.has(Lethargy) && self.has(Tsathoggua) && nexus.none && others.%(f => f.power > 0 && !f.hibernating).any)
                if (!this.options.has(IceAgeAffectsLethargy) || afford(0)(self.goo(Tsathoggua).region))
                    options :+= LethargyMainAction(self)

            if (self.has(Hibernate)) {
                val enemyGOOs =
                    others.flatMap(f => f.all(GOO))
                        .groupBy(_.uclass match {
                            case Nyogtha => "Nyogtha"
                            case other   => other.name
                        })
                        .map(_._2.head)

                val n = min(self.power, enemyGOOs.size)
                options :+= HibernateMainAction(self, n)
            }

            if (self.want(DragonAscending) && self.power < others./(_.power).max)
                options :+= DragonAscendingMainAction(self)

            if (moveableUnits(self).any)
                options :+= MoveMainAction(self)

            if (self.has(BeyondOne) && gates.num < board.regions.num && board.regions.diff(gates).%(afford(1)).any)
                gates.%(r => others.%(_.at(r, GOO).any).none).%(r => self.at(r).%(_.uclass.cost >= 3).exceptByatis.any).some.foreach {
                    options :+= BeyondOneMainAction(self, _)
                }

            board.regions.%(nx).%(afford(1)).%(r => others.%(f => canCapture(self, f, r)).any).some.foreach { options :+= CaptureMainAction(self, _) }

            if (
                self.has(CaptureMonster) &&
                board.regions.%(nx).%(afford(1)).%(r =>
                    self.at(r, Tsathoggua).any && (others.exists(f => f.at(r, GOO).none && f.at(r, Monster).any))
                ).any
            )
                options :+= CaptureMonsterMainAction(self)

            val cs = self.inPool(Cultist)./(_.uclass).distinct.reverse

            cs.foreach { uc =>
                board.regions.%(self.at(_).any).some.|(board.regions).%(nx).%(afford(r => self.recruitCost(uc, r))).some.foreach {
                    options :+= RecruitMainAction(self, uc, _)
                }
            }

            if (nexusExtra.none)
                board.regions.%(nx).%(afford(1)).diff(battled).%(r =>
                    others.exists(f => canAttack(self, f, r))
                )
                .some.foreach {
                    options :+= AttackMainAction(self, _)
                }

            board.regions.%(nx).%(afford(3 - self.has(UmrAtTawil).??(1))).%!(gates.contains).%(r => hasCultistOrRSDY(self, r)).some.foreach {
                options :+= BuildGateMainAction(self, _)
            }

            if (self == AN && cathedrals.num < 4) {
                 val existingGlyphs = cathedrals.map(_.glyph).toSet

                 val validRegions = board.regions.filter { r =>
                     !cathedrals.contains(r) &&
                     afford(getCathedralCost(r))(r) &&
                     hasCultistOrRSDY(self, r) &&
                     !existingGlyphs.contains(r.glyph)
                }

                if (validRegions.any) {
                     options :+= BuildCathedralMainAction(self, validRegions.toList)
                }
            }

            if (self.has(CursedSlumber) && gates.%(_.glyph == Slumber).none && self.gates.%(nx).%(_.glyph.onMap).any)
                options :+= CursedSlumberSaveMainAction(self)

            if (self.has(CursedSlumber) && gates.%(_.glyph == Slumber).any)
                board.regions.%(nx).%(afford(1)).%!(gates.contains).some.foreach { options :+= CursedSlumberLoadMainAction(self, _) }

            ((self.inPool(Terror) ++ self.inPool(Monster)).sort)./(_.uclass).distinct.reverse.foreach { uc =>
                board.regions.%(nx).%(afford(r => self.summonCost(uc, r))).%(r => canAccessGate(self, r)).some.foreach { options :+= SummonMainAction(self, uc, _) }
            }

            if (self.has(Abhoth) && self.inPool(Filth).any) {
                val affordableExists = board.regions.%(nx).%(afford(r => self.summonCost(Filth, r))).any
                if (affordableExists) {
                    options :+= FilthMainAction(self, board.regions)
                }
            }

            self.inPool(GOO).filter(isFactionGOO)./(_.uclass).distinct.reverse.foreach { uc =>
                board.regions.%(nx).%(afford(r => self.awakenCost(uc, r))).some.foreach { options :+= AwakenMainAction(self, uc, _) }
            }

            println()
            println()
            println()
            println(loyaltyCards)
            println(loyaltyCards.%(_.doom == 0))
            println(loyaltyCards.%(_.doom == 0).%(_.power >= self.power))
            println(loyaltyCards)
            println(loyaltyCards.%(_.doom == 0))
            println(self.gates)
            println(self.gates.%(r => self.at(r, GOO).any))
            println(self.gates.%(r => self.at(r, GOO).any || (self == AN && cathedrals.num == 4)))
            println(self.gates.%(r => self.at(r, GOO).any || (self == AN && cathedrals.num == 4)).%(affordF(self, 2)))
            println(self.gates.%(r => self.at(r, GOO).any || (self == AN && cathedrals.num == 4)).%(affordF(self, 4)))
            println(self.gates.%(r => self.at(r, GOO).any || (self == AN && cathedrals.num == 4)).%(affordF(self, 6)))

            loyaltyCards.%(_.doom == 0).%(_.power <= self.power).foreach { igoo =>
                self.gates.%(r => self.at(r, GOO).any || (self == AN && cathedrals.num == 4)).%(affordF(self, igoo.power)).some.foreach { gates =>
                    options :+= IndependentGOOMainAction(self, igoo, gates)
                }
            }

            if (self.has(NightmareWeb) && self.inPool(Nyogtha).any) {
                board.regions.%(affordF(self, 2)).some.foreach { l =>
                    options :+= NightmareWebMainAction(self, l)
                }
            }

            if (self.has(Dreams) && self.inPool(Acolyte).any)
                board.regions.%(afford(2)).%(r => others.%(_.at(r, Acolyte).any).any).some.foreach { options :+= DreamsMainAction(self, _) }

            if (self.has(Submerge) && self.has(Cthulhu) && self.goo(Cthulhu).region.glyph == Ocean)
                options :+= SubmergeMainAction(self, self.goo(Cthulhu).region)

            if (self.at(GC.deep).any)
                board.regions.%(afford(0)).some.foreach { options :+= UnsubmergeMainAction(self, _) }

            if (self.has(Devolve) && self.all(Acolyte).any && self.inPool(DeepOne).any)
                options :+= DevolveMainAction(self, MainCancelAction(self))


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
                val t = tax(r, self)
                board.regions.but(r).%(afford(1 + t)).%(r => nfactions.%(_.at(r, Cultist, Monster).any).any).some.foreach {
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

            if (self.needs(Eliminate2Cultists) && self.all(Cultist).num >= 2)
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

            if (self.has(Zingaya) && self.inPool(Undead).any)
                board.regions.%(afford(1)).%(r => self.at(r, Undead).any).%(r => others.%(_.at(r, Acolyte).any).any).some.foreach {
                    options :+= ZingayaMainAction(self, _)
                }

            if (self.has(Shriek) && self.has(Byakhee))
                board.regions.%(afford(1)).%(r => self.all(Byakhee).%(_.region != r).any).some.foreach {
                    options :+= ShriekMainAction(self, _)
                }

            if (self.needs(Provide3Doom))
                options :+= Provide3DoomMainAction(self)


            if (self.has(AncientSorcery) && self.onMap(SerpentMan).%(nx).any && self.borrowed.num < factions.num - 1)
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
                options :+= SacrificeHighPriestMainAction(self, MainCancelAction(self))

            if (self.has(GodOfForgetfulness) && self.has(Byatis)) {
                val br = self.goo(Byatis).region

                val l = board.connected(br).filter { r =>
                    factions.but(self).exists { f =>
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
                case AttackMainAction(_, _) if self.has(FromBelow) && self.oncePerAction.contains(FromBelow) && !self.hasAllSB => false
                case _ if self.active && !acted && battled.none => true
                case _ if self.active && !acted && self.hasAllSB => true
                case AttackMainAction(_, l) if self.active && (self.hasAllSB && !self.option(UnlimitedBattleOff) && (!self.option(UnlimitedBattleOnlyWithGOO) || l.%(r => self.at(r, GOO).any).any)) && nexusExtra.none && (nexus.none || !acted) => true
                case SummonMainAction(_, _, _) if acted && self.has(Fertility) && !self.option(UnlimitedSummonOff) && (!self.option(UnlimitedSummonEnemyGOO) || others./~(_.all(GOO))./(_.region).%(r => canAccessGate(self, r)).any) => true
                case FilthMainAction(_, _) if acted && self.has(Fertility) && !self.option(UnlimitedSummonOff) && (!self.option(UnlimitedSummonEnemyGOO) || others./~(_.all(GOO))./(_.region).%(r => canAccessGate(self, r)).any) && self.has(Abhoth) => true
                case RevealESMainAction(_) if self.doom + self.es./(_.value).sum >= 30 => true
                case DevolveMainAction(_, _) if self.active && ((!acted && battled.none) || (!self.option(OutOfTurnDevolveOff) && !self.option(OutOfTurnDevolveAvoidCapture))) => true
                case AnytimeGainElderSignsMainAction(_) if self.doom + self.es./(_.value).sum + min(3, factions.but(self).%(_.hasAllSB).num) * 3 >= 30 && (self.unfulfilled.num == 1 || others.%(_.hasAllSB).none) => true
                case DragonAscendingMainAction(_) if !acted && battled.none => true
                case DragonAscendingMainAction(_) if (self.hasAllSB && !self.option(UnlimitedBattleOff)) && board.regions.%(nx).diff(battled).%(r => others.%(f => canAttack(self, f, r)).any).any => true
                case _ => false
            }

            if (!self.active)
                options :+= MainDoneCancelAction(self)
            else
            if (acted || battled.any || self.oncePerRound.contains(Fertility) || self.oncePerRound.contains(HWINTBN) || self.oncePerRound.contains(ScreamingDead) || nexus.any)
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

            options

        case MainCancelAction(self) =>
            Force(MainAction(self))

        case MainNextPlayerAction(f) =>
            if (nexusExtra.any) {
                battle = |(new Battle(nexus.get.region, f, nexusExtra.get))

                nexusExtra = None

                battle.get.attacker.log("proceeded to battle", battle.get.defender, "in", battle.get.region)

                battle.get.proceed()
            }
            else
            if (nexus.any) {
                acted = nexus.get.acted
                battled = nexus.get.battled

                battle = |(new Battle(nexus.get.region, nexus.get.attacker, nexus.get.defender))

                nexus = None

                battle.get.attacker.log("proceeded to battle", battle.get.defender, "in", battle.get.region)

                battle.get.proceed()
            }
            else {
                acted = false
                reveal = false
                battled = Nil
                factions.foreach(_.oncePerRound = Nil)
                round += 1

                if (factions.%(_.doom >= 30).any)
                    CheckSpellbooksAction(GameOverPhaseAction)
                else {
                    if (factions.%(!_.hibernating).%(_.power > 0).none) {
                        CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingUpAction("power gather", PowerGatherAction(f))))
                    }
                    else {
                        val next = (order ++ order).dropWhile(_ != f).drop(1).head
                        CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingDownAction(next, "action", MainAction(next))))
                    }
                }
            }

        case MainDoneAction(self) =>
            Force(MainNextPlayerAction(self))

        case MainDoneCancelAction(self) =>
            Force(MainNextPlayerAction(self))

        case EndAction(self) =>
            acted = true
            self.units.foreach(_.remove(Summoned))
            AfterAction(self)

        case AfterAction(self) =>
            checkGatesOwnership(self)

            if (self.has(FromBelow) && !self.oncePerAction.contains(FromBelow)) {
                nyogthaPendingFreeBattle.get(self).foreach { otherRegion =>
                    val attackableFactions = nfactions.but(self).filter(e => canAttack(self, e, otherRegion))

                    if ((attackableFactions.any) && affordF(self, 0)(otherRegion)) {
                        self.oncePerAction :+= FromBelow
                        nyogthaPendingFreeBattle -= self

                        val actions = attackableFactions.map(g => FromBelowAttackAction(self, otherRegion, g))

                        if (actions.any)
                            return QAsk(actions :+ FromBelowAttackDoneAction(self))
                    } else {
                        nyogthaPendingFreeBattle -= self
                    }
                }
            }

            if (self.power == 0)
                self.log("ran out of power")

            if (self.power == -1)
                self.power = 0

            if (self.power < 0) {
                self.log("somehow ran into negative power")

                self.power = 0
            }

            factions.%(_.has(Passion)).%(_.oncePerAction.contains(Passion)).foreach { f =>
                f.power += 1

                f.log("got", 1.power, "from", Passion.full)
            }

            factions.%(_.oncePerAction.contains(LostAbhoth)).foreach { f =>
                NeutralAbhoth.units = self.units(Filth)./(u => new UnitFigure(NeutralAbhoth, u.uclass, u.index, (u.region == self.reserve).?(NeutralAbhoth.reserve).|(u.region), u.state, u.health))

                // TODO: Destroy submerged Filth? Or leave it?

                self.units = self.units.not(Filth)
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
                Force(MoveDoneAction(self))
            else {
                val units = moveableUnits(self).sortWith(sortAllUnits(self))
                if (units.none)
                    Force(MoveDoneAction(self))
                else
                if (moved)
                    MoveDoneAction(self) +: units./(u => MoveSelectAction(u.faction, u.uclass, u.region))
                else
                    units./(u => MoveSelectAction(u.faction, u.uclass, u.region)) :+ MainCancelAction(self)
            }

        case MoveSelectAction(self, uc, region) =>
            var destinations = board.connected(region)

            if (self.has(Flight))
                destinations = destinations ++ destinations.flatMap(board.connected).distinct.but(region).diff(destinations)

            if (uc == Shantak)
                destinations = board.regions

            val arriving = self.units.%(_.region.glyph.onMap).%(_.has(Moved))./(_.region).distinct

            destinations = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            destinations = destinations.%(affordF(self, 1))

            val options = destinations./(d => MoveAction(self, uc, region, d))

            if (hasMoved(self))
                Ask(self).add(options).add(MoveCancelAction(self))
            else
                Ask(self).add(options).cancel

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
            val t = payTax(self, r)

            self.power -= 1

            val u = self.at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)

            if (t > 0)
                t.timesDo { () =>
                    u.add(MovedForDouble)
                }

            self.log("moved", self.styled(uc), "from", o, "to", r)

            if (u.uclass == Nyogtha) {
                if (self.all(Nyogtha).num < 2)
                return MoveContinueAction(self, true)

                val result = tryFromBelow(
                    self, u, this,
                    makeAction = otherRegion => FromBelowMoveSelectAction(self, Nyogtha, otherRegion),
                    doneAction = FromBelowMoveDoneAction(self),
                    canDo = _ => true,
                    allowSameRegion = true)

                result match {
                    case Some(actions) => return QAsk(actions)
                    case None          => return MoveContinueAction(self, true)
                }
            }

            if (u.uclass == Ithaqua && self.has(ArcticWind))
                QAsk(self.at(o).%(!_.has(Moved)).exceptByatis./(u => ArcticWindAction(self, o, u.uclass, r)) :+ ArcticWindDoneAction(self))
            else if (u.uclass == Shantak)
                QAsk(self.at(o).%(!_.has(Moved)).%(_.uclass.utype == Cultist)./(u => ShantakCarryCultistAction(self, o, u.uclass, r)) :+ ShantakCarryCultistCancelAction(self))
            else
                MoveContinueAction(self, true)

        case MoveCancelAction(self) =>
            MoveContinueAction(self, true)

        // ATTACK
        case AttackMainAction(self, l) =>
            val factionVariants = l./~ { r =>
                (factions.but(self) ++ neutrals.keys)
                .%(_.at(r).any)
                .%(f => self.strength(self.at(r), f) > 0)
                ./(f => AttackAction(self, r, f))
            }.sortBy { ao =>
                self.strength(self.at(ao.r), ao.f) * 10000 +
                ao.f.strength(ao.f.at(ao.r), self) * 100 +
                ao.f.at(ao.r).num
            }.reverse

            Ask(self).add(factionVariants).cancel

        case AttackAction(self, r, f) =>
            self.power -= 1
            payTax(self, r)
            self.log("battled", f, "in", r)
            var sl = factions.%(f => f.has(EnergyNexus) && f.at(r, Wizard).any)

            if (self.has(FromBelow) && !self.oncePerAction.contains(FromBelow)) {
                val nyRegions = self.all(Nyogtha).map(_.region).distinct
                if (nyRegions.size == 2 && nyRegions.contains(r)) {
                    val other = nyRegions.find(_ != r)
                    other.foreach { o => nyogthaPendingFreeBattle += self -> o }
                }
            }

            if (nexus.any) {
                sl.head.log("interrupted battle again with", EnergyNexus.full)
                nexusExtra = |(f)
                Force(MainAction(sl.head))
            }
            else
            if (sl.any) {
                sl.head.log("interrupted battle with", EnergyNexus.full)
                nexus = |(Nexus(r, self, f, factions, acted, battled))
                acted = false
                battled = Nil
                Force(MainAction(sl.head))
            }
            else {
                lastBattleRegionByFaction += self -> r
                battle = |(new Battle(r, self, f))
                battle.get.proceed()
            }

        // CAPTURE
        case CaptureMainAction(self, l) =>
            val variants = l./~ { r =>
                factions.but(self).% { f =>
                    canCapture(self, f, r)
                }./ { f =>
                    val uc = if (f.at(r, HighPriest).any && f.at(r, Acolyte).none) HighPriest else Acolyte
                    CaptureAction(self, r, f, uc)
                }
            }

            QAsk(variants :+ MainCancelAction(self))

        case CaptureAction(self, r, f, _) =>
            self.power -= 1
            payTax(self, r)
            val c = f.at(r, Cultist).minBy(_.uclass.cost)
            capture(self, c)
            self.log("captured", c, "in", r)
            satisfy(self, CaptureCultist, "Capture Cultist")

            val nyogthasHere = self.at(r, Nyogtha)
            if (self.has(FromBelow) && !self.oncePerAction.contains(FromBelow) && nyogthasHere.any) {
                val actedNyogtha = nyogthasHere.head

                val result = tryFromBelow(
                    self,
                    actedNyogtha,
                    this,
                    makeAction = otherRegion => {
                        val possibleTargets = factions.but(self).filter(ff => canCapture(self, ff, otherRegion))
                        val targetFaction = possibleTargets.head
                        val uc =
                            if (targetFaction.at(otherRegion, HighPriest).any &&
                                targetFaction.at(otherRegion, Acolyte).none)
                                HighPriest else Acolyte
                        FromBelowCaptureAction(self, otherRegion, targetFaction, uc)
                    },
                    doneAction = FromBelowCaptureDoneAction(self),
                    canDo = otherRegion =>
                        factions.but(self).exists(ff => canCapture(self, ff, otherRegion)) &&
                        affordF(self, 0)(otherRegion),
                    allowSameRegion = true
                )

                result match {
                    case Some(actions) => return QAsk(actions)
                    case None          => // no valid follow-up
                }
            }

            EndAction(self)

        // BUILD
        case BuildGateMainAction(self, locations) =>
            Ask(self, locations.sortBy(tax(_, self))./(r => BuildGateAction(self, r)) :+ MainCancelAction(self))

        case BuildGateAction(self, r) =>
            self.power -= 3 - self.has(UmrAtTawil).??(1)
            payTax(self, r)
            gates :+= r
            self.gates :+= r
            self.log("built a gate in", r)
            EndAction(self)

        // RECRUIT
        case RecruitMainAction(self, uc, l) =>
            val (a, b) = l.partition(abandonedGates.contains)
            QAsk((a ++ b)./(r => RecruitAction(self, uc, r)) :+ MainCancelAction(self))

        case RecruitAction(self, uc, r) =>
            self.power -= self.recruitCost(uc, r)
            payTax(self, r)
            place(self, uc, r)
            self.log("recruited", self.styled(uc), "in", r)
            EndAction(self)

        // SUMMON
        case SummonMainAction(self, uc, l) =>
            QAsk(l./(r => SummonAction(self, uc, r)) :+ MainCancelAction(self))

        case SummonAction(self, uc, r) =>
            self.power -= self.summonCost(uc, r)
            payTax(self, r)
            summon(self, uc, r)
            self.log("summoned", self.styled(uc), "in", r)
            if (uc == Ghast && self.inPool(Ghast).any) {
                QAsk(getSummonRegions(self)./(r => FreeSummonAction(self, uc, r, EndAction(self))))
            }
            else if (self.has(Fertility) && !self.ignored(Fertility)) {
                self.oncePerRound :+= Fertility
                checkGatesOwnership(self)
                CheckSpellbooksAction(MainAction(self))
            }
            else if (self.has(Festival) && uc == UnMan) {
                QAsk(factions.but(self)./(f => FestivalUnManSummonAction(self, f)))
            }
            else
                EndAction(self)

        case FreeSummonAction(self, uc, r, next) =>
            place(self, uc, r)
            self.log("summoned", self.styled(uc), "in", r, "for free")
            CheckSpellbooksAction(MainAction(self))

            if (uc == Ghast && self.inPool(Ghast).any) {
                QAsk(getSummonRegions(self)./(r => FreeSummonAction(self, uc, r, next)))
            }
            else {
                Force(next)
            }

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
            QAsk(pairs./((a, b) => AwakenEliminate2CultistsAction(self, uc, locations, a, b)) :+ MainCancelAction(self))

        case AwakenEliminate2CultistsAction(self, uc, locations, a, b) =>
            val q = locations./(r => AwakenAction(self, uc, r, self.awakenCost(uc, r)))
            $(a, b).foreach { r =>
                val c = self.at(r, Cultist).head
                log(c, "in", c.region, "was sacrificed")
                eliminate(c)
            }
            QAsk(q)

        case AwakenMainAction(self, uc, locations) =>
            QAsk(locations./(r => AwakenAction(self, uc, r, self.awakenCost(uc, r))) :+ MainCancelAction(self))

        case AwakenAction(self, uc, r, cost) =>
            self.power -= (if (cost < 0) self.awakenCost(uc, r) else cost)

            payTax(self, r)
            place(self, uc, r)

            self.log("awakened", self.styled(uc), "in", r)

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal.full)
                giveES(self, 1)
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
                    val u = factions./~(_.unitGate).%(_.region == r).head

                    eliminate(u)

                    self.log("eliminated", u, "in", r)
                }
            }

            if (uc == YogSothoth) {
                val s = self.at(r, SpawnOW).head

                eliminate(s)

                self.log("replaced", s, "in", r)

                self.unitGate = self.at(r, YogSothoth).single
            }

            uc match {
                case Cthulhu =>
                    satisfy(self, AwakenCthulhu, "Awaken Cthulhu")
                case Nyarlathotep =>
                    satisfy(self, AwakenNyarlathotep, "Awaken Nyarlathotep")
                case ShubNiggurath =>
                    satisfy(self, AwakenShubNiggurath, "Awaken Shub-Niggurath")
                case KingInYellow =>
                    satisfy(self, AwakenKing, "Awaken King in Yellow")
                case Hastur =>
                    satisfy(self, AwakenHastur, "Awaken Hastur")
                case Tsathoggua =>
                    satisfy(self, AwakenTsathoggua, "Awaken Tsathoggua")
                case RhanTegoth =>
                    satisfy(self, AwakenRhanTegoth, "Awaken Rhan Tegoth")
                case Ithaqua =>
                    satisfy(self, AwakenIthaqua, "Awaken Ithaqua")
                case YogSothoth =>
                    satisfy(self, AwakenYogSothoth, "Awaken Yog-Sothoth")
            }

            EndAction(self)

        // HIGH PRIESTS
        case SacrificeHighPriestDoomAction(self) =>
            Ask(self, board.regions./~(r => self.at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, DoomCancelAction(self))) :+ DoomCancelAction(self))

        case SacrificeHighPriestMainAction(self, then) =>
            if (self.all(HighPriest).any) {
                if (self.at(SL.slumber, HighPriest).any) {
                    Ask(self, List(SL.slumber)./~(r => self.at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, then)) :+ then)
                }
                else {
                    Ask(self, board.regions./~(r => self.at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, then)) :+ then)
                }
            }
            else
                Force(then)

        case SacrificeHighPriestAction(self, r, then) =>
            val c = self.at(r, HighPriest).head
            eliminate(c)

            self.power += 2

            self.log("sacrificed", c, "in", r)

            checkGatesLost()

            if (Explode.isCancel(then))
                Force(then)
            else
                Force(SacrificeHighPriestMainAction(self, then))

        case SacrificeHighPriestDoneAction(self, then) =>
            self.ignoredSacrificeHighPriest = true
            Force(then)

        // GC -- GREAT CTHULHU

        // DEVOLVE
        case DevolveMainAction(self, then) =>
            if (self.inPool(DeepOne).any)
                Ask(self, board.regions./~(r => self.at(r, Acolyte))./(c => DevolveAction(self, c.region, then)) :+ then)
            else
                Force(then)

        case DevolveAction(self, r, then) =>
            if (self.at(r, Monster, GOO).none)
                factions.but(self).%(_.at(r, Monster, GOO).none).%(_.at(r, Cultist).any).foreach { f => f.oncePerAction = f.oncePerAction.but(Devolve) }

            val c = self.at(r, Acolyte).head
            eliminate(c)
            place(self, DeepOne, r)

            log(c, "in", r, "devolved into", DeepOne)

            checkGatesLost()

            if (Explode.isCancel(then))
                Force(then)
            else
                Force(DevolveMainAction(self, then))

        case DevolveDoneAction(self, then) =>
            self.oncePerAction :+= Devolve
            Force(then)

        // DREAMS
        case DreamsMainAction(self, l) =>
            QAsk(l./~(r => factions.but(self)./~(_.at(r, Acolyte).take(1)))./(c => DreamsAction(self, c.region, c.faction)) :+ MainCancelAction(self))

        case DreamsAction(self, r, f) =>
            val c = f.at(r, Acolyte).head
            self.power -= 2
            payTax(self, r)
            self.log("sent dreams to", c, "in", c.region, "and replaced it with", self.styled(Acolyte))
            place(self, Acolyte, c.region)
            eliminate(c)
            EndAction(self)

        // SUBMERGE
        case SubmergeMainAction(self, r) =>
            self.power -= 1
            Force(SubmergeAction(self, r, Cthulhu))

        case SubmergeAction(self, r, uc) =>
            move(self.at(r, uc).head, GC.deep)
            QAsk(self.at(r).exceptByatis./(u => SubmergeAction(self, r, u.uclass)) :+ SubmergeDoneAction(self, r))

        case SubmergeDoneAction(self, r) =>
            val cthulu = self.at(GC.deep, Cthulhu).head
            val court = self.at(GC.deep).but(cthulu)
            log(cthulu, "submerged in", r, court.any.??("with " + court.mkString(", ")))
            EndAction(self)

        case UnsubmergeMainAction(self, l) =>
            QAsk(l./(r => UnsubmergeAction(self, r)) :+ MainCancelAction(self))

        case UnsubmergeAction(self, r) =>
            payTax(self, r)
            val cthulu = self.at(GC.deep, Cthulhu).head
            val court = self.at(GC.deep).but(cthulu)
            self.at(GC.deep).foreach(move(_, r))
            log(cthulu, "unsubmerged in", r, court.any.??("with " + court.mkString(", ")))
            EndAction(self)

        // CC -- CRAWLING CHAOS

        // PAYXPOWER
        case Pay4PowerMainAction(self) =>
            self.power -= 4
            self.log("paid", 4.power)
            satisfy(self, Pay4Power, "Pay four Power")
            EndAction(self)

        case Pay6PowerMainAction(self) =>
            self.power -= 6
            self.log("paid", 6.power)
            satisfy(self, Pay6Power, "Pay six Power")
            EndAction(self)

        case Pay10PowerMainAction(self) =>
            self.power -= 10
            self.log("paid", 10.power)
            satisfy(self, Pay4Power, "Pay four Power")
            satisfy(self, Pay6Power, "Pay six Power")
            EndAction(self)

        // 1000F
        case ThousandFormsMainAction(self) =>
            self.oncePerTurn +:= ThousandForms
            RollD6("Roll for " + ThousandForms.full, x => ThousandFormsRollAction(self, x))

        case ThousandFormsRollAction(f, x) =>
            f.log("used", ThousandForms.full, "and rolled", ("[" + x.styled("power") + "]"))
            log("Other factions were to lose", x.power)
            Force(ThousandFormsAction(f, x))

        case ThousandFormsAction(f, x) =>
            val mp = factions./(_.power).max
            val sm = factions.but(f)./(_.power).sum

            if (sm < x) {
                log("Not enough power among other factions")
                f.log("got", x.power)
                f.power += x
                EndAction(f)
            }
            else {
                factions.but(f).%(_.power == 0).foreach(f => f.log("had no power"))

                val forum = factions.but(f).%(_.power > 0)
                Force(ThousandFormsContinueAction(f, x, $, forum, forum.num * 3))
            }

        case ThousandFormsContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropRight(1)

            if (offers./(_.n).sum == x) {
                offers.%(_.n > 0).reverse.foreach { o =>
                    o.f.power -= o.n

                    log(o.f, "lost", o.n.power)
                }
                EndAction(f)
            }
            else
            if (time <= 0 || xforum./(_.power).sum < x) {
                if (factions.but(f).%(_.power > 0).num > 1)
                    log("Negotiations failed")

                f.power += x

                f.log("got", x.power)

                EndAction(f)
            }
            else {
                if (xforum.num == 1)
                    time = 0

                if (time == xforum.num) {
                    time -= 1

                    log("Negotiations time was running out")
                }

                val next = xforum.head
                val forum = xforum.drop(1) :+ next

                offers = offers.%(_.f != next)
                val offered = offers./(_.n).sum
                val maxp = min(next.power, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(_.power).sum
                val minp = max(1, x - maxother)

                QAsk((-1 +: 0 +: minp.to(maxp).$)./(n => ThousandFormsAskAction(f, x, offers, forum, time - (random() * 1.0).round.toInt, next, n)))
            }


        case ThousandFormsAskAction(f, x, offers, forum, time, self, n) =>
            if (n < 0) {
                self.log("refused to negotiate")

                Force(ThousandFormsContinueAction(f, x, offers, forum.but(self), time))
            }
            else {
                if (n == 0)
                    self.log("offered no power")
                else
                    self.log("offered to lose", n.styled("highlight"))

                Force(ThousandFormsContinueAction(f, x, Offer(self, n) +: offers, forum, time))
            }

        // BG -- BLACK GOAT

        // BLOOD SACRIFICE
        case BloodSacrificeDoomAction(self) =>
            QAsk(self.all(Cultist)./(c => BloodSacrificeAction(self, c.region, c.uclass)) :+ DoomCancelAction(self))

        case BloodSacrificeAction(self, r, uc) =>
            val c = self.at(r, uc).head
            eliminate(c)
            self.oncePerTurn :+= BloodSacrifice
            self.log("sacrificed", c, "in", r, "for", 1.es)
            giveES(self, 1)
            checkGatesLost()
            CheckSpellbooksAction(DoomAction(self))

        // ELIMINATE CULTISTS
        case Eliminate2CultistsMainAction(self) =>
            val cultists = board.regions./~(r => self.at(r, Cultist).take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).drop(1)./(b => (a.region, b.region))).distinct
            QAsk(pairs./((a, b) => Eliminate2CultistsAction(self, a, b)) :+ MainCancelAction(self))

        case Eliminate2CultistsAction(self, a, b) =>
            $(a, b).foreach { r =>
                val c = self.at(r, Cultist).head
                log(c, "in", c.region, "was sacrificed")
                eliminate(c)
            }
            satisfy(self, Eliminate2Cultists, "Eliminate two Cultists")
            EndAction(self)

        // AVATAR
        case AvatarMainAction(self, o, l) =>
            val variants = l.flatMap { r =>
                nfactions.filter(_.at(r, Cultist, Monster).any).map(f =>
                    AvatarAction(self, o, r, f)
                )
            }

            QAsk(variants :+ MainCancelAction(self))

        case AvatarAction(self, o, r, f) =>
            self.power -= 1
            payTax(self, r)
            val sn = self.goo(ShubNiggurath)
            move(sn, r)
            log(sn, "avatared to", r)
            payTax(self, o)
            val units = f.at(r, Cultist, Monster)
            val l = units.useIf(units./(_.uclass).distinct.num == 1)(_.take(1))

            Ask(f.real.?(f).|(self)).each(l)(u => AvatarReplacementAction(f, self, r, o, u.uclass))

        case AvatarReplacementAction(self, f, r, o, uc) =>
            val u = self.at(r, uc).head
            log(u, "was sent back to", o)
            move(u, o)
            EndAction(f)

        // GHROTH
        case GhrothMainAction(self) =>
            self.power -= 2
            RollD6("Roll for " + self.styled(Ghroth), x => GhrothRollAction(self, x))

        case GhrothRollAction(f, x) =>
            var b = f.all(Fungi)./(_.region).distinct.num

            if (x > b) {
                f.log("failed", Ghroth.full, "with roll of", ("[" + x.styled("power") + "]"))

                val fs = factions.%(_.inPool(Acolyte).any)
                if (fs.any)
                    QAsk(fs./(GhrothFactionAction(f, _)))
                else {
                    log("No Cultists were available to place")
                    EndAction(f)
                }
            }
            else {
                f.log("used", Ghroth.full, "and rolled", ("[" + x.styled("power") + "]"))

                val n = factions.but(f)./~(_.onMap(Cultist)).num
                if (n <= x) {
                    if (n < x)
                        log("Not enough Cultists among other factions")

                    factions.but(f)./~(_.onMap(Cultist)).foreach { c =>
                        log(c, "was eliminated in", c.region)
                        eliminate(c)
                    }

                    EndAction(f)
                }
                else {
                    Force(GhrothAction(f, x))
                }
            }

        case GhrothAction(f, x) =>
            factions.but(f).%(_.onMap(Cultist).none).foreach(f => f.log("had no Cultists"))

            val forum = factions.but(f).%(_.onMap(Cultist).any)
            Force(GhrothContinueAction(f, x, Nil, forum, forum.num * 3))

        case GhrothContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropRight(1)

            if (offers./(_.n).sum == x && offers.num == xforum.num) {
                Force(GhrothEliminateAction(f, offers./~(o => o.n.times(o.f))))
            }
            else
            if (time < 0 || xforum./(_.onMap(Cultist).num).sum < x) {
                f.log("eliminated", x, "Cultist" + (x > 1).??("s"))

                val affected = factions.but(f).%(_.onMap(Cultist).any)
                val split = 1.to(x)./~(n => affected.combinations(n))
                val valid = split.%(_./(_.onMap(Cultist).num).sum >= x)

                QAsk(valid./(GhrothSplitAction(f, x, _)))
            }
            else {
                if (xforum.num == 1)
                    time = 0

                if (time == xforum.num) {
                    time -= 1

                    log("Negotiations time was running out")
                }

                val next = xforum.head
                val forum = xforum.drop(1) :+ next

                offers = offers.%(_.f != next)
                val offered = offers./(_.n).sum
                val maxp = min(next.onMap(Cultist).num, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(_.onMap(Cultist).num).sum
                val minp = max(1, x - maxother)

                QAsk((-1 +: 0 +: minp.to(maxp).$)./(n => GhrothAskAction(f, x, offers, forum, time - (random() * 1.0).round.toInt, next, n)))
            }


        case GhrothAskAction(f, x, offers, forum, time, self, n) =>
            if (n < 0) {
                self.log("refused to negotiate")

                Force(GhrothContinueAction(f, x, offers, forum.but(self), time))
            }
            else {
                if (n == 0)
                    self.log("offered no Cultists")
                else
                    self.log("offered to lose", n.styled("highlight"))

                Force(GhrothContinueAction(f, x, Offer(self, n) +: offers, forum, time))
            }

        case GhrothSplitAction(self, x, ff) =>
            val split = ff./~(f => (x - ff.num).times(f)).combinations(x - ff.num)./(_ ++ ff)./(l => ff./~(f => l.count(f).times(f)))
            val valid = split.%(s => ff.%(f => f.onMap(Cultist).num < s.%(_ == f).num).none)
            QAsk(valid./(l => GhrothSplitNumAction(self, x, ff, l)))

        case GhrothSplitNumAction(self, x, ff, full) =>
            Force(GhrothEliminateAction(self, full))

        case GhrothEliminateAction(f, full) =>
            if (full.none)
                EndAction(f)
            else {
                val next = full.head
                val cultists = board.regions./~(r => next.at(r, Cultist))
                // If we want to allow SL to eliminate a cultist in Cursed Slumber (which you should be able to do, according to the FAQ).
                // val cultists = {
                //     val base = board.regions./~(r => next.at(r, Cultist))
                //     val slumberCultists = next.at(SL.slumber, Cultist)
                //     if (slumberCultists.any) {
                //         base ++ slumberCultists
                //     } else base
                // }
                QAsk(cultists./(c => GhrothUnitAction(next, c.uclass, c.region, f, full.drop(1))))
            }

        case GhrothUnitAction(self, uc, r, f, full) =>
            val c = self.at(r, uc).head
            log(c, "was eliminated in", c.region)
            eliminate(c)
            Force(GhrothEliminateAction(f, full))

        case GhrothFactionAction(self, f) =>
            QAsk(board.regions./(r => GhrothPlaceAction(self, f, r)))

        case GhrothPlaceAction(self, f, r) =>
            log(f.styled(Acolyte), "was placed in", r)
            place(f, Acolyte, r)
            EndAction(self)

        // YS -- YELLOW SIGN

        // PROVIDE 3 DOOM
        case Provide3DoomMainAction(self) =>
            QAsk(factions.but(self)./(f => Provide3DoomAction(self, f)) :+ MainCancelAction(self))

        case Provide3DoomAction(self, f) =>
            f.doom += 3
            self.log("supplied", f, "with", 3.doom)
            satisfy(self, Provide3Doom, "Provide 3 Doom")
            EndAction(self)

        // DESECRATE
        case DesecrateMainAction(self, r, te) =>
            self.power -= te.?(1).|(2)
            payTax(self, r)
            RollD6("Roll for " + self.styled(Desecrate) + " in " + r, x => DesecrateRollAction(self, r, te, x))

        case DesecrateRollAction(self, r, te, x) =>
            if (self.at(r).num >= x) {
                log(self.styled(KingInYellow), "desecrated", r, "with roll [" + x.styled("power") + "]")
                if (te) {
                    self.log("gained", 1.es, "using", ThirdEye.full)
                    giveES(self, 1)
                }
                r.glyph match {
                    case GlyphAA => satisfy(self, DesecrateAA, "Desecrated /^\\ ".trim)
                    case GlyphOO => satisfy(self, DesecrateOO, "Desecrated (*)")
                    case GlyphWW => satisfy(self, DesecrateWW, "Desecrated |||")
                    case _ =>
                }
                desecrated :+= r
            }
            else
                log(self.styled(KingInYellow), "failed", r, "desecration with roll [" + x.styled("power") + "]")

            val us = (self.inPool(Cultist) ++ self.inPool(Monster))./( _.uclass ).filter( _.cost <= 2 ).distinct

            if (us.any)
                QAsk(us./(DesecratePlaceAction(self, r, _)))
            else
                EndAction(self)

        case DesecratePlaceAction(self, r, uc) =>
            place(self, uc, r)
            log(self.styled(uc), "appeared in", r)
            EndAction(self)

        // HWINTBN
        case HWINTBNMainAction(self, o, l) =>
            QAsk(l./(HWINTBNAction(self, o, _)) :+ MainCancelAction(self))

        case HWINTBNAction(self, o, r) =>
            self.power -= 1
            payTax(self, r)
            move(self.at(o, Hastur).head, r)
            self.oncePerRound :+= HWINTBN

            log(Hastur, "heard his name in", r)

            AfterAction(self)

        // SCREAMING DEAD
        case ScreamingDeadMainAction(self, o, l) =>
            QAsk(l./(ScreamingDeadAction(self, o, _)) :+ MainCancelAction(self))

        case ScreamingDeadAction(self, o, r) =>
            self.power -= 1
            payTax(self, r)
            Force(ScreamingDeadFollowAction(self, o, r, KingInYellow))

        case ScreamingDeadFollowAction(self, o, r, uc) =>
            val u = self.at(o, uc).head
            move(u, r)
            if (uc == KingInYellow)
                log(KingInYellow, "screamed from", o, "to", r)
            else
                log(u, "followed along")
            QAsk(self.at(o, Undead)./(_.uclass)./(ScreamingDeadFollowAction(self, o, r, _)) :+ ScreamingDeadDoneAction(self))

        case ScreamingDeadDoneAction(self) =>
            self.oncePerRound :+= ScreamingDead

            AfterAction(self)

        // SHRIEK
        case ShriekMainAction(self, l) =>
            QAsk(l./(ShriekAction(self, _)) :+ MainCancelAction(self))

        case ShriekAction(self, r) =>
            val b = self.all(Byakhee)./(_.region).but(r)
            if (b.none)
                EndAction(self)
            else
                QAsk(b./(ShriekFromAction(self, _, r)) :+ self.oncePerAction.contains(Shriek).?(ShriekDoneAction(self)).|(MainCancelAction(self)))

        case ShriekFromAction(self, o, r) =>
            val u = self.at(o, Byakhee).head
            if (!self.oncePerAction.contains(Shriek)) {
                self.power -= 1
                payTax(self, r)
                self.oncePerAction :+= Shriek
            }
            move(u, r)
            log(u, "flew to", r, "from", o)
            Force(ShriekAction(self, r))

        case ShriekDoneAction(self) =>
            EndAction(self)

        // ZINGAYA
        case ZingayaMainAction(self, l) =>
            QAsk(l./~(r => factions.but(self)./~(f => f.at(r, Acolyte).take(1)))./(u => ZingayaAction(self, u.region, u.faction)) :+ MainCancelAction(self))

        case ZingayaAction(self, r, f) =>
            val c = f.at(r, Acolyte).head
            self.power -= 1
            payTax(self, r)
            eliminate(c)
            place(self, Undead, r)
            self.log("replaced", c, "in", r, "with", Undead)
            EndAction(self)

        // SL - SLEEPER

        // DEATH FROM BELOW
        case DeathFromBelowDoomAction(self) =>
            val unitClasses = self.inPool(Monster)./(_.uclass)

            val minCost = unitClasses.map(_.cost).min
            val ucs = unitClasses.filter(_.cost == minCost).distinct

            if (ucs.num == 1) {
                QAsk(board.regions.%(r => self.at(r).any).some.|(board.regions)./(r => DeathFromBelowAction(self, r, ucs.head)) :+ DoomCancelAction(self))
            }
            else {
                QAsk(ucs./(uc => DeathFromBelowSelectMonsterAction(self, uc)) :+ DoomCancelAction(self))
            }

        case DeathFromBelowSelectMonsterAction(self, uc) =>
            QAsk(board.regions.%(r => self.at(r).any).some.|(board.regions)./(r => DeathFromBelowAction(self, r, uc)) :+ DoomCancelAction(self))

        case DeathFromBelowAction(self, r, uc) =>
            place(self, uc, r)
            self.log("placed", uc, "in", r, "with", DeathFromBelow.full)
            self.oncePerTurn :+= DeathFromBelow
            CheckSpellbooksAction(DoomAction(self))

        // LETHARGY
        case LethargyMainAction(self) =>
            if (options.has(IceAgeAffectsLethargy))
                payTax(self, self.goo(Tsathoggua).region)

            self.log("was sleeping")
            battled = board.regions
            EndAction(self)

        // PAY 3 POWER
        case Pay3SomeoneGains3MainAction(self) =>
            QAsk(factions.but(self)./(Pay3SomeoneGains3Action(self, _)) :+ MainCancelAction(self))

        case Pay3SomeoneGains3Action(self, f) =>
            self.power -= 3
            f.power += 3
            self.log("spent", 3.power, "and", f, "gained", 3.power)
            satisfy(self, Pay3SomeoneGains3, "Provide 3 Power")
            EndAction(self)

        case Pay3EverybodyLoses1MainAction(self) =>
            self.power -= 3
            factions.but(self).%(f => f.power > 0).foreach(f => f.power -= 1)
            self.log("spent", 3.power, "and each other faction lost", 1.power)
            satisfy(self, Pay3EverybodyLoses1, "Everybody loses 1 power")
            EndAction(self)

        case Pay3EverybodyGains1MainAction(self) =>
            self.power -= 3
            factions.but(self).foreach(f => f.power += 1)
            self.log("spent", 3.power, "and each other faction gained", 1.power)
            satisfy(self, Pay3EverybodyGains1, "Everybody gains 1 power")
            EndAction(self)

        // CAPTURE MONSTER
        case CaptureMonsterMainAction(self) =>
            val variants = board.regions./~ { r =>
                self.at(r, Tsathoggua).any.?? {
                    nfactions.but(self).%(f => f.at(r, Monster).any && f.at(r, GOO).none)
                        ./(f => CaptureMonsterAction(self, r, f))
                }
            }

            Ask(self).add(variants).cancel

        case CaptureMonsterAction(self, r, f) =>
            self.power -= 1

            Ask(f).each(f.at(r, Monster).sortBy(_.uclass.cost))(u => CaptureMonsterUnitAction(self, r, u.faction, u.uclass))

        case CaptureMonsterUnitAction(self, r, f, uc) =>
            val m = f.at(r, uc).head
            capture(self, m)
            self.log("captured", m, "in", r)
            EndAction(self)

        // ANCIENT SORCERY
        case AncientSorceryMainAction(self) =>
            Ask(self).each(factions.but(self)./(_.abilities.head).diff(self.borrowed))(a => AncientSorceryAction(self, a)).cancel

        case AncientSorceryAction(self, a) =>
            Ask(self).each(self.onMap(SerpentMan).%(nx))(u => AncientSorceryUnitAction(self, a, u.region, u.uclass)).cancel

        case AncientSorceryUnitAction(self, a, r, uc) =>
            self.power -= 1
            move(self.at(r, uc).head, SL.slumber)
            self.borrowed :+= a
            self.log("sent", uc, "from", r, "to access", a.full)
            EndAction(self)

        case AncientSorceryDoomAction(self) =>
            QAsk(board.regions./(r => AncientSorceryPlaceAction(self, r, SerpentMan)) :+ DoomCancelAction(self))

        case AncientSorceryPlaceAction(self, r, uc) =>
            move(self.at(SL.slumber, uc).head, r)
            self.power += 1
            self.log("placed", uc, "in", r, "with", AncientSorcery.full, "and gained", 1.power)
            CheckSpellbooksAction(DoomAction(self))

        // CURSED SLUMBER
        case CursedSlumberSaveMainAction(self) =>
            QAsk(self.gates.%(nx)./(CursedSlumberSaveAction(self, _)) :+ MainCancelAction(self))

        case CursedSlumberSaveAction(self, r) =>
            self.power -= 1
            self.gates = self.gates.but(r) :+ SL.slumber
            gates = gates.but(r) :+ SL.slumber
            move(self.at(r, Cultist).head, SL.slumber)
            self.log("moved gate from", r, "to", CursedSlumber.full)
            EndAction(self)

        case CursedSlumberLoadMainAction(self, l) =>
            QAsk(l./(CursedSlumberLoadAction(self, _)) :+ MainCancelAction(self))

        case CursedSlumberLoadAction(self, r) =>
            self.power -= 1
            payTax(self, r)
            self.gates = self.gates.but(SL.slumber) :+ r
            gates = gates.but(SL.slumber) :+ r

            if (self.at(SL.slumber, Cultist).any)
                move(self.at(SL.slumber, Cultist).head, r)

            self.log("moved gate from", CursedSlumber.full, "to", r)
            EndAction(self)

        // WW - WINDWALKER

        // HIBERNATE
        case HibernateMainAction(self, n) =>
            self.power += n
            self.hibernating = true
            self.log("hibernated", (n != 0).??("for extra " + n.power))
            battled = board.regions
            EndAction(self)

        // ICE AGE
        case IceAgeMainAction(self, l) =>
            QAsk(l./(r => IceAgeAction(self, r)) :+ MainCancelAction(self))

        case IceAgeAction(self, r) =>
            self.power -= 1
            self.iceAge = |(r)
            anyIceAge = true
            self.log("started", self.styled(IceAge), "in", r)
            EndAction(self)

        // ARCTIC WIND
        case ArcticWindAction(self, o, uc, r) =>
            val u = self.at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)
            log(u, "followed with", ArcticWind.full)
            QAsk(self.at(o).%(!_.has(Moved)).exceptByatis./(u => ArcticWindAction(self, o, u.uclass, r)) :+ ArcticWindDoneAction(self))

        case ArcticWindDoneAction(self) =>
            MoveContinueAction(self, true)

        // ANYTIME
        case AnytimeGainElderSignsMainAction(self) =>
            QAsk(AnytimeGainElderSignsAction(self, min(3, factions.but(self).%(_.hasAllSB).num), MainAction(self)) :: MainCancelAction(self))

        case AnytimeGainElderSignsDoomAction(self) =>
            QAsk(AnytimeGainElderSignsAction(self, min(3, factions.but(self).%(_.hasAllSB).num), DoomAction(self)) :: DoomCancelAction(self))

        case AnytimeGainElderSignsAction(self, n, next) =>
            satisfy(self, AnytimeGainElderSigns, "Anytime Spellbook", n)
            CheckSpellbooksAction(next)

        // OW - OPENER OF THE WAY

        // BEYOND ONE
        case BeyondOneMainAction(self, l) =>
            QAsk(l./~(r => self.at(r).%(_.uclass.cost >= 3)).exceptByatis./(u => BeyondOneUnitAction(self, u.region, u.uclass)) :+ MainCancelAction(self))

        case BeyondOneUnitAction(self, o, uc) =>
            QAsk(board.regions.diff(gates).%(affordF(self, 1))./(BeyondOneAction(self, o, uc, _)) :+ MainCancelAction(self))

        case BeyondOneAction(self, o, uc, r) =>
            self.power -= 1
            payTax(self, r)
            gates = gates.but(o) :+ r
            factions.%(_.gates.contains(o)).foreach { f =>
                f.gates = f.gates.but(o) :+ r
                move(f.at(o).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && f.has(RedSign))).head, r)
            }
            move(self.at(o, uc).head, r)
            self.log("moved gate with", self.styled(uc), "from", o, "to", r)
            EndAction(self)

        // DREAD CURSE
        case DreadCurseMainAction(self, n, l) =>
            QAsk(l./(DreadCurseAction(self, n, _)) :+ MainCancelAction(self))

        case DreadCurseAction(self, n, r) =>
            self.power -= 2
            payTax(self, r)
            self.log("sent", self.styled(DreadCurse), "to", r)
            RollBattle(self, self.styled(DreadCurse), n, x => DreadCurseRollAction(self, r, x))

        case DreadCurseRollAction(self, r, x) =>
            self.log("rolled", x.mkString(" "))
            var k = x.count(Kill)
            var p = x.count(Pain)
            if (k + p == 0)
                EndAction(self)
            else {
                val e = factions.but(self).%(f => f.at(r).any).sortBy(-_.at(r).sortBy(_.uclass.cost).take(k)./(_.uclass.cost).sum)

                val kva = e./~(f => k.times(f)).combinations(k).$.sortBy(_.distinct.num)
                val pva = e./~(f => p.times(f)).combinations(p).$.sortBy(_.distinct.num)
                val kpva = kva./~(kk => pva./(pp => (kk, pp))).sortBy(v => 100 * v._1.distinct.num + 10 * v._2.distinct.num + (v._1 ++ v._2).distinct.num)

                val n = e./(_.at(r).num).sum

                while (n < k + p && p > 0)
                    p -= 1
                while (n < k && k > 0)
                    k -= 1

                val kvb = e./~(f => min(k, f.at(r).num).times(f)).combinations(k).$
                val pvb = e./~(f => min(p, f.at(r).num).times(f)).combinations(p).$
                val kpvb = kvb./~(kk => pvb./(pp => (kk, pp))).%((a, b) => e.%(f => f.at(r).num < (a ++ b).count(f)).none)

                QAsk(kpvb./((a, b) => DreadCurseSplitAction(self, r, x, e.%(f => a.contains(f) || b.contains(f)), a, b)))
            }

        case DreadCurseSplitAction(self, r, x, e, k, p) =>
            if (x.any && e.num > 1) {
                e.foreach { f =>
                    f.log("recieved", (k.count(f).times(Kill) ++ p.count(f).times(Pain)).mkString(" "))
                }
            }

            val ee = e.%(f => f.at(r).%(_.health == Killed).num < k.count(f) || f.at(r).%(_.health == Pained).num < p.count(f))

            val killall = ee.%(f => f.at(r).num == k.count(f))

            killall.foreach(f => f.at(r).foreach(_.health = Killed))

            val painall = ee.%(f => f.at(r).num == p.count(f))

            painall.foreach(f => f.at(r).foreach(_.health = Pained))

            val aa = ee.diff(killall).diff(painall)

            if (aa.any) {
                val f = aa(0)
                val rs = (k.count(f) - f.at(r).%(_.health == Killed).num).times(Kill) ++ (p.count(f) - f.at(r).%(_.health == Pained).num).times(Pain)
                val us = f.at(r).%(_.health == Alive)./(_.uclass).sortBy(_.cost)
                val uu = (us.num > 1).?(us).|(us.take(1))
                QAsk(uu./(u => DreadCurseAssignAction(self, r, e, k, p, f, rs.head, u)))
            }
            else {
                e.foreach { f =>
                    f.at(r).%(_.health == Killed).foreach { u =>
                        log(u, "was", "killed".styled("kill"))
                        eliminate(u)
                    }
                }

                var m = e./~(f => f.at(r).%(_.health == Pained))

                m = m.take(1)

                if (m.any)
                    QAsk(m./(u => DreadCurseRetreatAction(self, r, e, u.faction, u.uclass)))
                else
                    EndAction(self)
            }

        case DreadCurseAssignAction(f, r, e, k, p, self, s, uc) =>
            val u = self.at(r, uc).%(_.health == Alive).head
            u.health = (s == Kill).?(Killed).|(Pained)
            QAsk(List(DreadCurseSplitAction(f, r, $, e, k, p)))

        case DreadCurseRetreatAction(self, r, e, f, uc) =>
            QAsk(board.connected(r)./(d => DreadCurseRetreatToAction(self, r, e, f, uc, d)))

        case DreadCurseRetreatToAction(self, r, e, f, uc, d) =>
            val u = f.at(r, uc).%(_.health == Pained).head
            move(u, d)
            u.health = Alive
            log(u, "was", "pained".styled("pain"), "to", d)

            var m = e./~(f => f.at(r).%(_.health == Pained))

            m = m.take(1)

            if (m.any)
                Ask(self).each(m)(u => DreadCurseRetreatAction(self, r, e, u.faction, u.uclass))
            else
                EndAction(self)

        // DRAGON DESCENDING
        case DragonDescendingDoomAction(self, cost) =>
            self.oncePerGame :+= DragonDescending
            self.log("used", DragonDescending.full)
            Force(RitualAction(self, cost, 2))

        // DRAGON ASCENDING
        case DragonAscendingMainAction(self) =>
            DragonAscendingAction(self, Some(self), "own action", factions./(_.power).max, MainAction(self)) :: MainCancelAction(self)

        case DragonAscendingDoomAction(self) =>
            DragonAscendingAction(self, Some(self), "own " + "Doom".styled("doom") + " action", factions./(_.power).max, DoomAction(self)) :: DoomCancelAction(self)

        case DragonAscendingAskAction(self, f, reason, then) =>
            DragonAscendingAction(self, f, reason, factions./(_.power).max, then) :: DragonAscendingCancelAction(self, then) :: DragonAscendingNotThisTurnAction(self, then)

        case DragonAscendingAction(self, _, _, p, then) =>
            self.power = p
            self.oncePerGame :+= DragonAscending

            factions.foreach(_.ignorePerInstant = $)

            self.log("used", DragonAscending.full, "and rose to", p.power)

            Force(then)

        case DragonAscendingCancelAction(self, then) =>
            self.ignorePerInstant :+= DragonAscending
            Force(then)

        case DragonAscendingNotThisTurnAction(self, then) =>
            self.ignorePerTurn :+= DragonAscending
            Force(then)

        case DragonAscendingInstantAction(then) =>
            factions.foreach(f => f.ignorePerInstant = f.ignorePerInstant.but(DragonAscending))
            Force(then)

        case DragonAscendingUpAction(reason, then) =>
            val daf = factions.%(_.power < factions./(_.power).max).%(_.want(DragonAscending))

            if (daf.none) {
                Force(then)
            }
            else {
                val self = daf(0)
                DragonAscendingAskAction(self, None, reason, DragonAscendingUpAction(reason, then))
            }

        case DragonAscendingDownAction(f, reason, then) =>
            val daf = targetDragonAscending(f)

            if (daf.none || (f.hibernating && then == MainAction(f))) {
                Force(then)
            }
            else {
                val self = daf(0)
                DragonAscendingAskAction(self, Some(f), reason, DragonAscendingDownAction(f, reason, then))
            }

        // AN - THE ANCIENTS

        // BUILD CATHEDRAL
        case BuildCathedralMainAction(self, locations) =>
            Ask(self, locations.sortBy(tax(_, self))./(r => BuildCathedralAction(self, r)) :+ MainCancelAction(self))

        case BuildCathedralAction(self, r) =>
            self.power -= getCathedralCost(r)
            payTax(self, r)
            cathedrals :+= r
            self.cathedrals :+= r
            self.log("built a cathedral in", r)
            r.glyph match {
                    case GlyphAA => satisfy(self, CathedralAA, "Cathedral in /^\\ ".trim)
                    case GlyphOO => satisfy(self, CathedralOO, "Cathedral in (*)")
                    case GlyphWW => satisfy(self, CathedralWW, "Cathedral in |||")
                    case _ => satisfy(self, CathedralNG, "Cathedral in no-glyph Area")
                }
            EndAction(self)


        // GIVE WORST MONSTER
        case GiveWorstMonsterMainAction(self) =>
            satisfy(self, GiveWorstMonster, "Enemies got lowest cost monster")
            self.log("allowed enemy factions to summon their lowest cost monster for free")
            val forum = factions.but(self)
            Force(GiveWorstMonsterContinueAction(self, forum))

        case GiveWorstMonsterContinueAction(self, xforum) => {
            if (xforum.num == 0) {
                EndAction(self)
            }
            else {
                val f = xforum.head
                val forum = xforum.drop(1)
                val validPool = f.inPool(Monster)

                if (!validPool.any) {
                    f.log("didn't have any monsters in the pool")
                    Force(GiveWorstMonsterContinueAction(f, forum))
                }
                else if (!getControlledGatesRegions(f).any) {
                    f.log("had no way of summoning monsters")
                    Force(GiveWorstMonsterContinueAction(f, forum))
                }
                else {
                    val unitClasses = validPool./(_.uclass)
                    val minCost = unitClasses.map(_.cost).min
                    val ucs = unitClasses.filter(_.cost == minCost).distinct

                    if (ucs.num == 1) {
                        QAsk(getControlledGatesRegions(f)./(r => GiveWorstMonsterAskAction(f, self, ucs.head, r, forum)))
                    }
                    else {
                        QAsk(ucs./(uc => GiveWorstMonsterSelectMonsterAction(f, self, uc, forum)))
                    }
                }
            }
        }

        case GiveWorstMonsterSelectMonsterAction(self, f, uc, forum) =>
            QAsk(getControlledGatesRegions(self)./(r => GiveWorstMonsterAskAction(self, f, uc, r, forum)))

        case GiveWorstMonsterAskAction(self, f, uc, r, forum) =>
            place(self, uc, r)
            // payTax(self, r) // Not sure if Ice Age affects this // probably doesn't HRF
            self.log("summoned", uc, "in", r, "for free")
            Force(GiveWorstMonsterContinueAction(f, forum))


        // GIVE BEST MONSTER
        case GiveBestMonsterMainAction(self) =>
            satisfy(self, GiveBestMonster, "Enemies got highest cost monster")
            self.log("allowed enemy factions to summon their highest cost monster for free")
            val forum = factions.but(self)
            Force(GiveBestMonsterContinueAction(self, forum))

        case GiveBestMonsterContinueAction(self, xforum) => {
            if (xforum.num == 0) {
                EndAction(self)
            }
            else {
                val f = xforum.head
                val forum = xforum.drop(1)
                val validPool = f.inPool(Monster)

                if (!validPool.any) {
                    f.log("didn't have any monsters in the pool")
                    Force(GiveBestMonsterContinueAction(f, forum))
                }
                else if (!getControlledGatesRegions(f).any) {
                    f.log("had no way of summoning monsters")
                    Force(GiveBestMonsterContinueAction(f, forum))
                }
                else {
                    val unitClasses = validPool./(_.uclass)
                    val maxCost = unitClasses.map(_.cost).max
                    val ucs = unitClasses.filter(_.cost == maxCost).distinct

                    if (ucs.num == 1) {
                        QAsk(getControlledGatesRegions(f)./(r => GiveBestMonsterAskAction(f, self, ucs.head, r, forum)))
                    }
                    else {
                        QAsk(ucs./(uc => GiveBestMonsterSelectMonsterAction(f, self, uc, forum)))
                    }
                }
            }
        }

        case GiveBestMonsterSelectMonsterAction(self, f, uc, forum) =>
            QAsk(getControlledGatesRegions(self)./(r => GiveBestMonsterAskAction(self, f, uc, r, forum)))

        case GiveBestMonsterAskAction(self, f, uc, r, forum) =>
            place(self, uc, r)
            // payTax(self, r) // Not sure if Ice Age affects this // probably doesn't HRF
            self.log("summoned", uc, "in", r, "for free")
            Force(GiveBestMonsterContinueAction(f, forum))


        // SUMMONING UN-MAN WITH FESTIVAL
        case FestivalUnManSummonAction(self, f) =>
            f.power += 1
            f.log("got", 1.power, "from", self.styled(Festival))
            EndAction(self)


        // DEMATERIALIZATION
        case DematerializationDoomAction(self) =>
            QAsk(board.regions.%(r => self.at(r).any)./(r => DematerializationFromRegionAction(self, r)):+ DoomCancelAction(self))

        case DematerializationFromRegionAction(self, o) =>
            QAsk(board.regions.but(o)./(r => DematerializationToRegionAction(self, o, r)) :+ DoomCancelAction(self))

        case DematerializationToRegionAction(self, o, d) =>
            QAsk(self.at(o).exceptByatis./(u => DematerializationMoveUnitAction(self, o, d, u.uclass)) :+ DematerializationDoneAction(self))

        case DematerializationMoveUnitAction(self, o, d, uc) =>
            val u = self.at(o, uc).head
            move(u, d)
            self.log("sent", self.styled(uc), "from", o, "to", d, "with", Dematerialization.full)
            QAsk(self.at(o).exceptByatis./(u => DematerializationMoveUnitAction(self, o, d, u.uclass)) :+ DematerializationDoneAction(self))

        case DematerializationDoneAction(self) =>
            self.oncePerTurn :+= Dematerialization
            demCaseMap = demCaseMap.keys.map(key => key -> 0).toMap
            CheckSpellbooksAction(DoomAction(self))


        // NEUTRAL MONSTERS

        case ShantakCarryCultistAction(self, o, uc, r) =>
            val u = self.at(o, uc).%(!_.has(Moved)).head
            move(u, r)

            u.add(Moved)
            u.add(MovedForFree)

            log(self.styled(Shantak), "carried", u, "to", r)
            MoveContinueAction(self, true)

        case ShantakCarryCultistCancelAction(self) =>
            MoveContinueAction(self, true)

        // INDEPENDENT GREAT OLD ONES

        case GodOfForgetfulnessMainAction(self, d, l) =>
            Ask(self, l./(r => GodOfForgetfulnessAction(self, d, r)) :+ MainCancelAction(self))

        case GodOfForgetfulnessAction(self, d, r) =>
            self.power -= 1
            payTax(self, d)

            factions.but(self).foreach { f =>
                f.at(r, Cultist).foreach { u =>
                    move(u, d)
                }
            }
            log(self.styled(Byatis), "used", GodOfForgetfulness.name.styled("nt"), "to move all enemy cultist from", r, "to", d)
            EndAction(self)

        case FilthMainAction(self, l) =>
            Ask(self, l./(r => FilthAction(self, r)) :+ MainCancelAction(self))

        case FilthAction(self, r) =>
            self.power -= 1
            payTax(self, r)

            place(self, Filth, r)
            log(self.styled(Abhoth), "placed", self.styled(Filth), "in", r)

            if (self.has(Fertility) && !self.ignored(Fertility)) {
                self.oncePerRound :+= Fertility
                checkGatesOwnership(self)
                CheckSpellbooksAction(MainAction(self))
            }
            else
                EndAction(self)

        case FromBelowMoveSelectAction(self, uc, region) =>
            var destinations = board.connected(region)
            if (self.has(Flight))
                destinations = destinations ++ destinations.flatMap(board.connected).distinct.but(region).diff(destinations)

            val arriving = self.units.%(_.region.glyph.onMap).%(_.has(Moved))./(_.region).distinct
            destinations = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            destinations = destinations.%(affordF(self, 0))

            val options = destinations./(d => FromBelowMoveAction(self, uc, region, d))
            QAsk(options :+ FromBelowMoveDoneAction(self))

        case FromBelowMoveAction(self, uc, o, r) =>
            payTax(self, r)
            val u = self.at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)
            u.add(MovedForFree)
            self.log("moved", self.styled(uc), "from", o, "to", r, "with", FromBelow.full)
            Force(MoveContinueAction(self, true))

        case FromBelowMoveDoneAction(self) =>
            Force(MoveContinueAction(self, true))

        case FromBelowAttackAction(self, r, f) =>
            payTax(self, r)
            self.log("battled", f, "in", r)
            if (!nyogthaPairByFaction.contains(self)) {
                val firstRegion = lastBattleRegionByFaction.getOrElse(self, r)
                nyogthaPairByFaction   += self -> Set(firstRegion, r)
                nyogthaPairProgress    += self -> 0
                nyogthaPairHadEnemyGOO += self -> false
                nyogthaPairNyogthaDied += self -> false
            }
            lastBattleRegionByFaction += self -> r
            battle = |(new Battle(r, self, f))
            battle.get.proceed()

        case FromBelowAttackDoneAction(self) =>
            Force(EndAction(self))

        case FromBelowCaptureAction(self, r, f, uc) =>
            payTax(self, r)
            val c = f.at(r, Cultist).minBy(_.uclass.cost)
            capture(self, c)
            self.log("captured", c, "in", r)
            satisfy(self, CaptureCultist, "Capture Cultist")
            Force(EndAction(self))

        case FromBelowCaptureDoneAction(self) =>
            Force(EndAction(self))

        case NightmareWebMainAction(self, regions) =>
            Ask(self).each(regions)(r => NightmareWebAction(self, r)).cancel

        case NightmareWebAction(self, r) =>
            self.power -= 2
            payTax(self, r)

            val ny = self.inPool(Nyogtha).head

            ny.region = r

            self.log("awakened", self.styled(Nyogtha), "in", r, "with", self.styled(NightmareWeb))

            EndAction(self)


        // NEUTRAL SPELLBOOKS

        // MAO CEREMONY
        case MaoCeremonyAction(self, r, uc) =>
            val c = self.at(r, uc).head
            eliminate(c)
            self.power += 1
            self.log("sacrificed", c, "in", r, "for", 1.power)

            checkPowerReached()

            checkGatesLost()

            AfterPowerGatherAction

        case MaoCeremonyDoneAction(self) =>
            self.ignorePerInstant :+= MaoCeremony

            AfterPowerGatherAction

        // RECRIMINATIONS
        case RecriminationsMainAction(self) =>
            Ask(self).each(self.spellbooks)(RecriminationsAction(self, _))

        case RecriminationsAction(self, sb) =>
            self.power -= 1
            self.spellbooks = self.spellbooks.but(sb)

            if (sb.isInstanceOf[NeutralSpellbook])
                neutralSpellbooks :+= sb

            self.log("discarded", sb.full)

            self.ignorePerInstant :+= sb

            EndAction(self)

        // UNDIMENSIONED
        case UndimensionedMainAction(self) =>
            UndimensionedContinueAction(self, self.units.onMap./(_.region).distinct, false)

        case UndimensionedContinueAction(self, destinations, moved) =>
            val units = self.units.%(nx).onMap.%!(_.has(Moved)).%(u => destinations.but(u.region).%(affordF(self, hasMoved(self).not.??(2))).any).sortWith(sortAllUnits(self))
            if (units.none)
                Force(UndimensionedDoneAction(self))
            else
            if (moved)
                UndimensionedDoneAction(self) +: units./(u => UndimensionedSelectAction(u.faction, destinations, u.uclass, u.region))
            else
                units./(u => UndimensionedSelectAction(u.faction, destinations, u.uclass, u.region)) :+ MainCancelAction(self)

        case UndimensionedSelectAction(self, destinations, uc, r) =>
            val options = destinations.but(r).%(affordF(self, hasMoved(self).not.??(2)))./(d => UndimensionedAction(self, destinations, uc, r, d))

            if (hasMoved(self))
                QAsk(options :+ UndimensionedCancelAction(self, destinations))
            else
                QAsk(options :+ MainCancelAction(self))

        case UndimensionedDoneAction(self) =>
            self.units.foreach(_.remove(Moved))
            EndAction(self)

        case UndimensionedAction(self, destinations, uc, o, r) =>
            if (hasMoved(self).not) {
                self.log("units are", Undimensioned.full)
                self.power -= 2
            }

            payTax(self, r)

            val u = self.at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)

            log(self.styled(uc), "from", o, "is now in", r)

            UndimensionedContinueAction(self, destinations, true)

        case UndimensionedCancelAction(self, destinations) =>
            UndimensionedContinueAction(self, destinations, true)

        case a if battle.any =>
            battle.get.perform(action)
    }

}
