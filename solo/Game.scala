package cws

import hrf.colmat._

import Html._

import scala.util._

import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

import cws.SpellbookUtils._
import cws.UnitUtils._

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

object UnitUtils {
    implicit class UnitFigureCollectionOps(ufs : $[UnitFigure]) {
        def exceptGOO : $[UnitFigure] = ufs.filter(_.exceptGOO)
        def exceptTerror : $[UnitFigure] = ufs.filter(_.exceptTerror)

        def exceptReanimated : $[UnitFigure] = ufs.filterNot(_.uclass == Reanimated)
        def exceptGug : $[UnitFigure] = ufs.filterNot(_.uclass == Gug)
        def exceptFilth : $[UnitFigure] = ufs.filterNot(_.uclass == Filth)
        def exceptByatis : $[UnitFigure] = ufs.filterNot(_.uclass == Byatis)

        def exceptIsolatedBrainless(p : Player, game : Game) : $[UnitFigure] = ufs.filterNot(game.isIsolatedBrainless(p, _))

        def exceptUncontrolledFilth(game : Game) : $[UnitFigure] = ufs.filterNot(_.isUncontrolledFilth(game))
        def uncontrolledFilthOnly(game : Game) : $[UnitFigure] = ufs.filter(_.isUncontrolledFilth(game))
    }

    implicit class UnitFigureCollectionPredicateOps(ufs : $[UnitFigure]) {
        def isUncontrolledFilthOnly(game : Game) : Boolean = ufs.exceptUncontrolledFilth(game).isEmpty
    }

    implicit class UnitFigureOps(u : UnitFigure) {
        def exceptGOO : Boolean = u.uclass.utype != GOO
        def exceptTerror : Boolean = u.uclass.utype != Terror

        def isUncontrolledFilth(game : Game) : Boolean = u.uclass == Filth && !game.factions.exists(f => game.of(f).has(Abhoth))
    }

    implicit class UnitClassCollectionOps(ucs : $[UnitClass]) {
        def exceptUncontrolledFilth(game : Game) : $[UnitClass] = if (game.isAbhothAbsent) ucs.filter(_ != Filth) else ucs
        def uncontrolledFilthOnly(game : Game) : $[UnitClass] = if (game.isAbhothAbsent) ucs.filter(_ == Filth) else Nil
    }

    implicit class FactionOps(f : Faction) {
        def allUnits(game : Game) : List[UnitFigure] = game.board.regions.flatMap(r => game.of(f).at(r))
    }

    implicit class GameOps(game : Game) {
        def isAbhothPresent : Boolean = game.factions.exists(f => game.of(f).has(Abhoth))
        def isAbhothAbsent : Boolean = !isAbhothPresent

        def isIsolatedBrainless(p : Player, u : UnitFigure) : Boolean = {
            if (!p.has(Brainless)) return false
            if (u.uclass != Reanimated) return false

            val r = u.region
            val monsters = p.at(r, Monster).but(u)

            !(p.at(r, Cultist).any || p.at(r, GOO).any || p.at(r, Terror).any || monsters.exceptReanimated.exceptUncontrolledFilth(game).nonEmpty)
        }
    }
}

object SpellbookUtils {
    def nonIGOO(spellbooks : hrf.colmat.$[Spellbook]) : hrf.colmat.$[Spellbook] =
        spellbooks.filterNot(_.isInstanceOf[IGOOSpellbook])
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

abstract class LoyaltyCard(n : String, dc : Int, pc : Int, q : Int, c : Int, cb : Int) {
    def short = n.styled("nt")
    def name = n
    def doomCost = dc
    def powerCost = pc
    def quantity = q
    def cost = c
    def combat = cb
    var hasSpellbook : Boolean = false
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
}

case object Acolyte extends UnitClass("Acolyte", Cultist, 1)

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
case object Summoned extends UnitState("summoned")
case object MovedForFree extends UnitState("moved-for-free")
case object MovedForDouble extends UnitState("moved-for-double")

class UnitFigure(val faction : Faction, val uclass : UnitClass, val index : Int, var region : Region, var state : List[UnitState] = Nil, var health : UnitHealth = Alive, val getFactionUnits : Faction => Seq[UnitFigure] = _ => Nil) {
    override def toString = short

    def short = {
        if (uclass == Filth && !hasAbhoth)
            uclass.name.styled("nt")
        else
            faction.styled(uclass.name)
    }

    def full = {
        if (uclass == Filth && !hasAbhoth)
            uclass.name.styled("nt") + (if (state.any) state.mkString(" (", "/", ")") else "") + (if (health == Alive || health == DoubleHP(Alive, Alive)) "" else (" (" + health + ")"))
        else
            faction.styled(uclass.name) + (if (state.any) state.mkString(" (", "/", ")") else "") + (if (health == Alive || health == DoubleHP(Alive, Alive)) "" else (" (" + health + ")"))
    }

    def has(s : UnitState) = state.contains(s)
    def add(s : UnitState) { state :+= s }
    def remove(s : UnitState) { state = state.but(s) }
    def count(s : UnitState) = state.count(_ == s)
    def ref = UnitRef(faction, uclass, index, getFactionUnits)
    private def hasAbhoth : Boolean = getFactionUnits(faction).exists(_.uclass == Abhoth)
}

case class UnitRef(faction : Faction, uclass : UnitClass, index : Int, val getFactionUnits : Faction => Seq[UnitFigure] = _ => Nil) {
    def short = {
        if (uclass == Filth && !hasAbhoth) {
            uclass.name.styled("nt")
        }
        else
            faction.styled(uclass)
    }
    def full = UnitRefFull(this)
    private def hasAbhoth : Boolean = getFactionUnits(faction).exists(_.uclass == Abhoth)
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

abstract class IGOOSpellbook(name : String) extends Spellbook(name) {
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
    def poolR : Region
    def prison : Region

    def full = name.styled(style, "inline-block")
    def ss = short.styled(style)

    override def toString = full

    def allUnits : $[UnitClass]
    def abilities : $[Spellbook]
    def spellbooks : $[Spellbook]
    def requirements(options : $[GameOption]) : $[Requirement]
    def recruitCost(g : Game, u : UnitClass, r : Region) = u.cost
    def summonCost(g : Game, u : UnitClass, r : Region) = u.cost
    def awakenCost(g : Game, u : UnitClass, r : Region) = u.cost
    def awakenDesc(g : Game, u : UnitClass) : Option[String] = None
    def strength(g : Game, units : $[UnitFigure], opponent : Faction) : Int
    def neutralStrength(g : Game, units : $[UnitFigure], opponent : Faction) =
        units.count(_.uclass == Ghast) * 0 +
        units.count(_.uclass == Gug) * 3 +
        units.count(_.uclass == Shantak) * 2 +
        units.count(_.uclass == StarVampire) * 1 +
        units.count(_.uclass == Byatis) * 4 +
        units.count(_.uclass == Abhoth) * g.of(this).all(Filth).num +
        units.count(_.uclass == Daoloth) * 0 +
        units.count(_.uclass == Nyogtha) * (
            if (g.battle != null && g.battle.attacker == this)
                4
            else
                1
        )

    var ignoredSacrificeHighPriest : Boolean
}

@EnableReflectiveInstantiation
trait Action extends Product {
    def question : Game => String
    def option : Game => String
}

trait FactionAction extends Action {
    def self : Faction
}

trait Soft extends Action
trait Cancel extends Action
trait More extends Soft
trait PowerNeutral extends Action

trait Continue
case class Ask(faction : Faction, list : $[Action]) extends Continue
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
    implicit def option2desc(n : Option[String]) : Game => String = (g : Game) => n.|(null)
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
    def question = (g : Game) => null
    def option = (g : Game) => null
}


trait VoidAction { self : ForcedAction => }

case object ReloadAction extends ForcedAction with VoidAction
case object UpdateAction extends ForcedAction with VoidAction
case class CommentAction(comment : String) extends ForcedAction with VoidAction

case object StartAction extends ForcedAction
case class CheckSpellbooksAction(then : Action) extends ForcedAction
case object AfterPowerGatherAction extends ForcedAction
case object FirstPlayerDeterminationAction extends ForcedAction
case object PlayOrderAction extends ForcedAction
case class PowerGatherAction(next : Faction) extends ForcedAction
case object DoomPhaseAction extends ForcedAction
case object ActionPhaseAction extends ForcedAction
case object GameOverPhaseAction extends ForcedAction

abstract class BaseFactionAction(val question : Game => String, val option : Game => String) extends FactionAction
abstract class OptionFactionAction(val option : Game => String) extends FactionAction

case class StartingRegionAction(self : Faction, r : Region) extends BaseFactionAction("" + self + " starts in", r)
case class FirstPlayerAction(self : Faction, f : Faction) extends BaseFactionAction("First player", f)
case class PlayDirectionAction(self : Faction, order : $[Faction]) extends BaseFactionAction("Order of play", order.mkString(" > "))

trait MainQuestion extends FactionAction {
    def question = (g : Game) => g.nexus./(n => "" + EnergyNexus + " in " + n.region).|("" + self + " action") + " (" + (g.of(self).power > 0).?(g.of(self).power.power).|("0 power") + ")"
}

trait DoomQuestion extends FactionAction {
    def question = (g : Game) => "" + self + " doom phase (" + (g.of(self).power > 0).?(g.of(self).power.power).|("0 power") + ")"
}

trait ExtraQuestion extends FactionAction {
    def question = (g : Game) => "<hr/>"
}

case class SpellbookAction(self : Faction, sb : Spellbook, next : Action) extends BaseFactionAction(g => (g.of(self).unclaimedSB == 1).?("Receive spellbook").|("Receive " + g.of(self).unclaimedSB + " spellbooks"), {
    val p = s""""${self.short}", "${sb.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;") // "
    val qm = Overlays.imageSource("question-mark")
    "<div class=sbdiv>" +
        sb.full +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
})

case class ElderSignAction(f : Faction, n : Int, value : Int, public : Boolean, next : Action) extends ForcedAction


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
case class RevealESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(g => "Elder Signs".styled("es") + " " + g.of(self).es./(_.short).mkString(" "), (es.num == 1).?("Reveal " + es(0).short).|("Reveal all for " + es./(_.value).sum.doom))

case class LoyaltyCardDoomAction(self : Faction) extends OptionFactionAction("Obtain " + "Loyalty Card".styled("nt")) with DoomQuestion with Soft with PowerNeutral
case class LoyaltyCardAction(self : Faction, lcs : $[LoyaltyCard], next : Action) extends BaseFactionAction(g => "Obtain " + "Loyalty Card".styled("nt"), {
    val qm = Overlays.imageSource("question-mark")
    lcs.map { lc =>
        val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
        "<div class=sbdiv>" +
        lc.short +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
        "</div>"
    }.mkString("\n")
}) with PowerNeutral
case class AddNeutralUnits(self : Faction, lc : LoyaltyCard, next : Action) extends ForcedAction
case class LoyaltyCardSummonAction(self : Faction, uc : UnitClass, r : Region, next : Action) extends BaseFactionAction(g => "Place " + self.styled(uc) + " in", g => r + g.ia(r, self))

case class PassAction(self : Faction) extends OptionFactionAction("Pass and lose remaining power") with MainQuestion

case class MoveMainAction(self : Faction) extends OptionFactionAction("Move") with MainQuestion with Soft
case class MoveContinueAction(self : Faction, moved : Boolean) extends ForcedAction with Soft
case class MoveSelectAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(g => g.hasMoved(self).?("Move " + "another".styled("highlight") + " unit").|("Move unit"), self.styled(uc) + " from " + r) with Soft
case class MoveAction(self : Faction, uc : UnitClass, r : Region, dest : Region) extends BaseFactionAction(g => "Move " + self.styled(uc) + " from " + r + " to", g => dest + g.ia(dest, self))
case class MoveDoneAction(self : Faction) extends BaseFactionAction(None, "Done")
case class MoveCancelAction(self : Faction) extends BaseFactionAction(None, "Cancel") with Cancel

case class AttackMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Battle") with MainQuestion with Soft
case class AttackAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(g => "Battle in " + r + g.ia(r, self), f)
case class AttackUncontrolledFilthAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(g => "Battle in " + r + g.ia(r, self), "Uncontrolled".styled("nt") + " " + Filth.name.styled("nt"))

case class BuildGateMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Build Gate") with MainQuestion with Soft
case class BuildGateAction(self : Faction, r : Region) extends BaseFactionAction(g => "Build gate" + g.forNPowerWithTax(r, self, 3 - g.of(self).has(UmrAtTawil).??(1)) + " in", r)

case class CaptureMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Capture") with MainQuestion with Soft
case class CaptureAction(self : Faction, r : Region, f : Faction, uc : UnitClass = Acolyte) extends BaseFactionAction(g => "Capture" + g.for1PowerWithTax(r, self) + " in " + r + g.ia(r, self), g => f.styled(uc))

case class RecruitMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Recruit " + self.styled(uc)) with MainQuestion with Soft
case class RecruitAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(g => "Recruit " + self.styled(uc) + g.forNPowerWithTax(r, self, self.recruitCost(g, uc, r)) + " in", g => r + g.ia(r, self))

case class SummonMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Summon " + self.styled(uc)) with MainQuestion with Soft
case class SummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(g => "Summon " + self.styled(uc) + g.forNPowerWithTax(r, self, self.summonCost(g, uc, r)) + " in", g => r + g.ia(r, self))
case class FreeSummonAction(self : Faction, uc : UnitClass, r : Region, next : Action) extends BaseFactionAction(g => "Summon " + self.styled(uc) + " for free in", g => r + g.ia(r, self))

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
case class AvatarUncontrolledFilthAction(self : Faction, o : Region, r : Region, owner : Faction) extends BaseFactionAction(g => self.styled(Avatar), g => "Uncontrolled".styled("nt") + " " + Filth.name.styled("nt") + " in " + r + g.ia(r, self)) with Soft
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
case class CaptureUncontrolledFilthAction(self : Faction, r : Region, owner : Faction) extends BaseFactionAction(CaptureMonster, "Capture " + "Uncontrolled".styled("nt") + " " + Filth.name.styled("nt") + " in " + r) with Soft
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
case class FromBelowAttackUncontrolledFilthAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction( g => { if (g.tax(r, self) == 0) "Battle for free with " + self.styled(FromBelow) + " in " + r else "Battle " + g.forNPowerWithTax(r, self, 0) + " with " + self.styled(FromBelow) + " in " + r + " " + g.ia(r, self) }, "Uncontrolled".styled("nt") + " " + Filth.name.styled("nt"))
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

class Player(val faction : Faction)(options : $[GameOption]) {
    var gates : $[Region] = $
    var cathedrals : $[Region] = $

    var spellbooks : $[Spellbook] = $
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

    var requirements : $[Requirement] = faction.requirements(options)

    var power : Int = 8
    var doom : Int = 0
    var es : $[ElderSign] = $
    var revealed : $[ElderSign] = $
    var loyaltyCards : $[LoyaltyCard] = $
    var obtainedLoyaltyCard : Boolean = false
    var units : $[UnitFigure] = $
    var hibernating : Boolean = false
    var iceage : Option[Region] = None
    var ugate : Option[UnitFigure] = None

    def active = power > 0 && !hibernating
    def allGates = gates ++ ugate./(_.region).toList
    def needs(rq : Requirement) = requirements.contains(rq)
    def has(sb : Spellbook) = faction.abilities.contains(sb) || spellbooks.contains(sb) || borrowed.contains(sb)
    def used(sb : Spellbook) = oncePerGame.contains(sb) || oncePerTurn.contains(sb) || oncePerRound.contains(sb) || oncePerAction.contains(sb)
    def can(sb : Spellbook) = has(sb) && !used(sb)
    def ignored(sb : Spellbook) = ignorePerGame.contains(sb) || ignorePerTurn.contains(sb) || ignorePerInstant.contains(sb)
    def option(io : IgnoreOption) = ignoreOptions.contains(io)
    def want(sb : Spellbook) = can(sb) && !ignored(sb)
    def hasAllSB = requirements.none
    def unclaimedSB = nonIGOO(faction.spellbooks).num - nonIGOO(spellbooks).num - requirements.num
    def at(region : Region) = units.%(_.region == region)
    def at(region : Region, uclass : UnitClass) = units.%(_.region == region).%(_.uclass == uclass)
    def at(region : Region, utype : UnitType) = units.%(_.region == region).%(_.uclass.utype == utype)
    def at(region : Region, utype : UnitType, utype2 : UnitType) = units.%(_.region == region).%(u => u.uclass.utype == utype || u.uclass.utype == utype2)
    def inPool() = units.%(_.region == faction.poolR)
    def inPool(uclass : UnitClass) = units.%(_.region == faction.poolR).%(_.uclass == uclass)
    def inPool(utype : UnitType) = units.%(_.region == faction.poolR).%(_.uclass.utype == utype)
    def onMap(utype : UnitType) = units.%(_.region.glyph.onMap).%(_.uclass.utype == utype)
    def onMap(uclass : UnitClass) = units.%(_.region.glyph.onMap).%(_.uclass == uclass)
    def all() = units.%(_.region.glyph.inPlay)
    def all(uclass : UnitClass) = units.%(_.region.glyph.inPlay).%(_.uclass == uclass)
    def all(utype : UnitType) = units.%(_.region.glyph.inPlay).%(_.uclass.utype == utype)
    def goo(uclass : UnitClass) = all(uclass).single.get
    def has(uclass : UnitClass) = all(uclass).any
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
    val options = providedOptions ++ $(PlayerCount(factions.num))
    val players = factions./(f => f -> new Player(f)(options)).toMap

    var starting = Map[Faction, Region]()
    var turn = 1
    var round = 1
    var order : $[Faction] = $
    var first : Faction = factions(0)
    var gates : $[Region] = Nil
    def ugates = factions./~(of(_).ugate)./(_.region)
    var cathedrals : $[Region] = Nil
    var desecrated : $[Region] = Nil
    var battled : $[Region] = Nil
    var acted : Boolean = false
    var reveal : Boolean = false
    var ritualMarker = 0
    var battle : Battle = null
    var nexus : Option[Nexus] = None
    var nexusExtra : Option[Faction] = None
    var anyia : Boolean = false
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

    def tryFromBelow(self : Faction, actedNyogtha : UnitFigure, game : Game, makeAction : Region => BaseFactionAction, doneAction : BaseFactionAction, canDo : Region => Boolean, allowSameRegion : Boolean) : Option[List[FactionAction]] = {
        if (!game.of(self).has(FromBelow) || game.of(self).oncePerAction.contains(FromBelow)) return None

        val nyogthas = game.of(self).all(Nyogtha)
        if (nyogthas.size != 2) return None

        val otherOpt = nyogthas.find(_ != actedNyogtha).filter(n => allowSameRegion || n.region != actedNyogtha.region)
        val other = otherOpt.getOrElse {
            return None
        }

        val otherRegion = other.region
        if (!canDo(otherRegion)) return None

        game.of(self).oncePerAction :+= FromBelow
        Some(List(makeAction(otherRegion), doneAction))
    }

    def ia(r : Region, f : Faction) : IceAges = {
        if (!anyia)
            NoIceAges
        else {
            val movedHere = of(f).at(r).%(u => u.has(Moved)).any
            // Show Ice Age unless explicitly moved here this turn *and* it's a normal (paid) move.
            if (movedHere && !of(f).oncePerAction.contains(FromBelow))
                NoIceAges
            else
                IceAges(factions.%(of(_).iceage./(_ == r).|(false)).but(f))
        }
    }
    def tax(r : Region, f : Faction) : Int = (!anyia).?(0).|(ia(r, f).tax)
    def forNPowerWithTax(r : Region, f : Faction, n : Int) : String = { val p = n + tax(r, f); " for " + p.power }
    def for1PowerWithTax(r : Region, f : Faction) : String = { val p = 1 + tax(r, f); if (p != 1) " for " + p.power else "" }

    def unit(u : UnitRef) = of(u.faction).units.%(_.uclass == u.uclass)(u.index)

    def affordF(f : Faction, n : Int)(r : Region) = of(f).power >= tax(r, f) + n

    def payTax(self : Faction, r : Region) : Int = {
        val s = ia(r, self)
        if (s.any) {
            of(self).power -= s.tax
            log("" + self + " lost " + s.tax.power + " due to " + s.list./(_.styled(IceAge)).mkString(", "))
            s.tax
        }
        else
            0
    }

    def hasMoved(f : Faction) = of(f).units.%(_.region.glyph.onMap).%(_.has(Moved)).any

    def nx(r : Region) = nexus./(_.region == r).|(true)
    def nx(u : UnitFigure) = nexus./(_.region == u.region).|(true)

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
        g.battle = battle
        g.nexus = nexus
        g.nexusExtra = nexusExtra
        g.anyia = anyia
        g.neutralSpellbooks = neutralSpellbooks
        g.loyaltyCards = loyaltyCards

        factions.foreach { f =>
            val p = g.of(f)
            val o = of(f)
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
            p.requirements = o.requirements
            p.power = o.power
            p.doom = o.doom
            p.es = o.es
            p.revealed = o.revealed
            p.units = o.units.map(u => new UnitFigure(u.faction, u.uclass, u.index, u.region, u.state, u.health))
            p.hibernating = o.hibernating
            p.iceage = o.iceage
            p.ugate = o.ugate
            p.loyaltyCards = o.loyaltyCards
            p.obtainedLoyaltyCard = o.obtainedLoyaltyCard
        }

        g
    }

    var mlog : $[String] = $

    def log(s : => String) {
        if (logging)
            mlog = s +: mlog
    }

    def ritualCost = min(10, ritualTrack(ritualMarker))

    def of(f : Faction) = players(f)

    def noGate(r : Region) = !gates.contains(r)

    def abandonedGates = gates.%(g => factions.%(f => of(f).gates.contains(g)).none)

    def satisfy(f : Faction, rq : Requirement, text : String, e : Int = 0) =
        if (of(f).needs(rq)) {
            of(f).requirements = of(f).requirements.but(rq)
            log("" + f + " achieved " + f.styled(text) + ((e + rq.es) > 0).??(" and gained " + (e + rq.es).es))
            giveES(f, e + rq.es)
        }

    def satisfyIf(f : Faction, rq : Requirement, text : String, c : => Boolean, p : Int = 0) =
        if (of(f).needs(rq)) {
            if (c) {
                satisfy(f, rq, text, 0)
                if (p != 0) {
                    of(f).power += p
                    log("" + f + " got " + p.power)
                }
            }
        }

    def giveES(f : Faction, n : Int = 1) {
        val total = factions./(f => of(f).es.num + of(f).revealed.num).sum
        var count = n

        if (total + count > 36) {
            log("" + f + " got " + (total + count - 36).doom + "  of " + (total + count - 36).es)
            of(f).doom += (total + count - 36)
            count = 36 - total
        }

        of(f).es ++= List.fill(count)(ElderSign(0))
    }

    def capture(f : Faction, u : UnitFigure) {
        eliminate(u)
        u.region = f.prison
    }

    def eliminate(u : UnitFigure) {
        val p = of(u.faction)

        if (u.uclass.utype == Cultist && p.has(Passion) && u.region.glyph.onMap)
            p.oncePerAction :+= Passion

        if (u.uclass.utype == GOO) {
            val isTrueNyogthaDeath =
                if (u.uclass == Nyogtha) {
                    val nyogthas = of(u.faction).all(Nyogtha)
                    val othersAlive = nyogthas.but(u).exists(_.health != Killed)
                    !othersAlive
                } else true

            if (isTrueNyogthaDeath) {
                factions.foreach { f =>
                    if (of(f).has(Daoloth) && !DaolothCard.hasSpellbook) {
                        of(f).spellbooks :+= Interdimensional
                        DaolothCard.hasSpellbook = true
                        log("" + f + " gained " + f.styled(Interdimensional) + " for " + f.styled(Daoloth))
                    }
                }
            }
        }
        
        if (u.uclass.isInstanceOf[IGOO]) {
            if (u.uclass == Byatis) {
                ByatisCard.hasSpellbook = false
                p.spellbooks = p.spellbooks.but(GodOfForgetfulness)
                p.loyaltyCards = p.loyaltyCards.but(ByatisCard)
                loyaltyCards :+= ByatisCard
                p.units = p.units.but(u)
            }
            else if (u.uclass == Abhoth) {
                AbhothCard.hasSpellbook = false
                p.spellbooks = p.spellbooks.but(TheBrood)
                p.loyaltyCards = p.loyaltyCards.but(AbhothCard)
                loyaltyCards :+= AbhothCard
                p.units = p.units.but(u)
            }
            else if (u.uclass == Daoloth) {
                DaolothCard.hasSpellbook = false
                p.spellbooks = p.spellbooks.but(CosmicUnity)
                p.spellbooks = p.spellbooks.but(Interdimensional)
                p.loyaltyCards = p.loyaltyCards.but(DaolothCard)
                loyaltyCards :+= DaolothCard
                p.units = p.units.but(u)
            }
            else if (u.uclass == Nyogtha) {
                val nyogthas = of(u.faction).all(Nyogtha)
                val others = nyogthas.but(u)

                if (others.isEmpty) {
                    NyogthaCard.hasSpellbook = false
                    p.spellbooks = p.spellbooks.but(FromBelow)
                    p.spellbooks = p.spellbooks.but(NightmareWeb)
                    p.loyaltyCards = p.loyaltyCards.but(NyogthaCard)
                    loyaltyCards :+= NyogthaCard
                    p.units = p.units.filterNot(_.uclass == Nyogtha)
                } else {
                    u.region = u.faction.poolR
                }
            }

            u.state = Nil
            u.health = Alive
        }
        else if (u.uclass == Yothan && p.has(Extinction)) {
            u.region = AN.extinct
            u.state = Nil
            u.health = Alive
            log(u.faction.styled(Yothan) + " was removed from the game permanently, due to " + u.faction.styled(Extinction))
        }
        else {
            u.region = u.faction.poolR
            u.state = Nil
            u.health = Alive
        }

        if (p.ugate == Some(u))
            p.ugate = None
    }

    def place(f : Faction, uc : UnitClass, r : Region) {
        val u = of(f).inPool(uc).head
        u.region = r
    }

    def summon(f : Faction, uc : UnitClass, r : Region) {
        val u = of(f).inPool(uc).head
        u.region = r
        u.add(Summoned)
    }

    def move(u : UnitFigure, r : Region) {
        u.region = r
    }

    def moveableUnits(p : Player) : $[UnitFigure] = {
        p.units.%(nx).%(_.region.glyph.onMap).%(!_.has(Moved)).exceptIsolatedBrainless(p, this).exceptByatis.exceptFilth
    }

    def canCapture(faction : Faction, victim : Faction, r : Region) : Boolean =
        if (faction == victim)
            false
        else
        if (of(victim).at(r, Cultist).none)
            false
        else
        if (of(victim).at(r, GOO).any)
            false
        else
        if (of(faction).at(r, GOO).any)
            true
        else
        if (of(victim).at(r, Terror).any)
            false
        else
        if (of(victim).at(r, Monster).any)
            of(victim).at(r, Monster).exceptUncontrolledFilth(this).isEmpty
        else
        if (of(victim).has(Ferox) && of(victim).has(Ithaqua))
            false
        else
        if (of(faction).at(r, Terror).any)
            true
        else {
            val monstersThatAreAbleToCapture = of(faction).at(r, Monster).exceptGug.exceptFilth.exceptIsolatedBrainless(of(faction), this)
            monstersThatAreAbleToCapture.nonEmpty
    }

    def canAttack(f : Faction, v : Faction, r : Region) : Boolean =
        of(v).at(r).any &&
        f.strength(this, of(f).at(r), v) > 0 &&
        of(f).at(r).exceptIsolatedBrainless(of(f), this).nonEmpty

    def canAccessGate(f : Faction, r : Region) = of(f).gates.contains(r) || of(f).ugate./(_.region == r).|(false) || (of(f).has(TheyBreakThrough) && gates.contains(r))

    def getControlledGatesRegions(f : Faction) : $[Region] = {
        val gates = of(f).gates.%(nx).%(_.glyph.onMap)

        val yogRegion = if (f == OW && of(f).has(YogSothoth)) List(of(f).goo(YogSothoth).region) else List()

        (gates ++ yogRegion).distinct
    }

    def getGOOControlledGateRegions(f : Faction) : $[Region] = {
        val gates = of(f).gates.%(nx).%(_.glyph.onMap)

        val gooGateRegions = gates.filter(r => of(f).at(r, GOO).any)

        val yogRegion =
            if (f == OW && of(f).has(YogSothoth)) List(of(f).goo(YogSothoth).region)
            else List()

        (gooGateRegions ++ yogRegion).distinct
    }

    def getSummonRegions(f : Faction) : List[Region] = {
        val gates = (of(f).gates.%(nx).%(_.glyph.onMap).nonEmpty).??(of(f).gates.%(nx).%(_.glyph.onMap))

        val yogRegion = (f == OW && of(f).has(YogSothoth)).$(of(f).goo(YogSothoth).region)

        val breakThroughRegions = (f == OW && of(f).has(TheyBreakThrough)).??(factions.but(f).flatMap(of(_).gates) ++ abandonedGates)

        (gates ++ yogRegion ++ breakThroughRegions).distinct
    }

    def isFactionGOO(u : UnitFigure) : Boolean = u.uclass.utype == GOO && !u.uclass.isInstanceOf[IGOO]

    def sortAllUnits(p : Player)(a : UnitFigure, b : UnitFigure) : Boolean = {
        val igoosPriority : Map[String, Int] = Map(
            "Abhoth" -> 1,
            "Byatis" -> 2,
            "Daoloth" -> 3,
            "Nyogtha" -> 4
        ).withDefaultValue(1000)

        val neutralsPriority : Map[String, Int] = Map(
            "Ghast" -> 10,
            "Gug" -> 11,
            "Shantak" -> 12,
            "Star Vampire" -> 13,
            "Filth" -> 14
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

    def regionStatus(r : Region) : List[String] = {
        val gate = gates.contains(r)
        val cathedral = cathedrals.contains(r)
        val ds = desecrated.contains(r)
        val controler = factions.%(f => of(f).gates.contains(r)).single
        val keeper = controler.flatMap(f => of(f).at(r).%(_.health == Alive).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && of(f).has(RedSign))).headOption)
        val others = factions.%(f => !of(f).gates.contains(r)).%(of(_).at(r).num > 0).sortBy(f => f.strength(this, of(f).at(r), f))
        if (gate || !others.isEmpty || ds) {
            List("" + r + ":" + gate.??(" " + keeper./(u => ("[[[".styled(u.faction.style) + " " + u + " " + "]]]".styled(u.faction.style))).|("[[[ GATE ]]]".styled("power"))) + ds.??(" " + YS.styled(")|("))) ++
            controler./(f => "    " + of(f).at(r).diff(keeper.toList)./(u => u.toString).mkString(", ")).toList ++
            others.sortBy(of(_).units.%(_.region == r).num)./ { f =>  "    " + of(f).at(r)./(u => u.toString).mkString(", ") } ++ List("&nbsp;")
        }
        else
            Nil
    }

    def powerLeader(f : Faction) = of(f).power > factions.but(f)./(of(_).power).max

    def targetDragonAscending(f : Faction) = (of(f).power > factions.but(f)./(of(_).power).max).?(factions.but(f).%(of(_).want(DragonAscending))).|(Nil)

    def showROAT() {
        def vv(v : Int) = (v == 999).?("Instant Death").|(v)
        log("Ritual of Annihilation".styled("doom") + " track " + ritualTrack.zipWithIndex./{ case (v, n) => (n == ritualMarker).?(("[" + vv(v) + "]").styled("str")).|("[".styled("xxxhighlight") + vv(v) + "]".styled("xxxhighlight")) }.mkString("-".styled("highlight")))
    }

    def hasCultistOrRSDY(f : Faction, r : Region) : Boolean = of(f).at(r, Cultist).any || (of(f).at(r, DarkYoung).any && of(f).has(RedSign))

    def checkGatesLost() {
        factions.foreach { f =>
            of(f).gates.foreach { g =>
                if (!hasCultistOrRSDY(f, g)) {
                    of(f).gates = of(f).gates.but(g)
                    log("" + f + " lost control of the gate in " + g)
                }
            }
        }
    }

    def checkPowerReached() {
        factions.foreach { f =>
            satisfyIf(f, Gates3Power12, "Have 12 Power", of(f).power >= 12)
            satisfyIf(f, Gates4Power15, "Have 15 Power", of(f).power >= 15)
        }
    }

    def checkAbhothSpellbook() {
        factions.foreach { f =>
            if (of(f).has(Abhoth) && !AbhothCard.hasSpellbook) {
                val monsters = of(f).units.%(_.uclass.utype == Monster).%(u => u.region.glyph.onMap)
                val monsterTypes = monsters./(_.uclass).distinct.num
                val totalMonsters = monsters.num + of(f).onMap(Filth).num

                if ((monsterTypes >= 4 || totalMonsters >= 8)) {
                    of(f).spellbooks :+= TheBrood
                    AbhothCard.hasSpellbook = true
                    log("" + f + " gained " + f.styled(TheBrood) + " for " + f.styled(Abhoth))
                }
            }
        }
    }

    def checkInterdimensional() {
        factions.foreach { f =>
            if (of(f).has(Interdimensional)) {
                val r = of(f).goo(Daoloth).region
                println("gates.contains(" + r + "):" + gates.contains(r))
                if (r != GC.deep && !gates.contains(r)) {
                    gates :+= r
                    log("" + f.styled(Daoloth) + " placed a Gate in " + r + " with " + f.styled(Interdimensional))
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
            if (factions.%(f => of(f).gates.contains(g)).none && hasCultistOrRSDY(self, g)) {
                of(self).gates :+= g
                log("" + self + " gained control of the gate in " + g)
            }
        }

        factions.foreach { f =>
            satisfyIf(f, OceanGates, "Control three Gates in Ocean areas", of(f).gates.%(_.glyph == Ocean).num >= 3)
            satisfyIf(f, OceanGates, "Four Gates exist in Ocean areas", (gates ++ ugates).%(_.glyph == Ocean).num >= 4)

            satisfyIf(f, Gates3Power12, "Control three Gates", of(f).gates.num >= 3)
            satisfyIf(f, Gates4Power15, "Control four Gates", of(f).gates.num >= 4)

            satisfyIf(f, Spread4, "Have Units in four Areas", board.regions.%(r => of(f).at(r).exceptUncontrolledFilth(this).any).num >= 4)
            satisfyIf(f, Spread6, "Have Units in six Areas", board.regions.%(r => of(f).at(r).exceptUncontrolledFilth(this).any).num >= 6)
            satisfyIf(f, Spread8, "Have Units in eight Areas", board.regions.%(r => of(f).at(r).exceptUncontrolledFilth(this).any).num >= 8)
            satisfyIf(f, SpreadSocial, "Share Areas with all enemies", factions.but(f).forall(e => board.regions.exists(r => of(f).at(r).exceptUncontrolledFilth(this).any && of(e).at(r).exceptUncontrolledFilth(this).any)), factions.but(f).num)

            if (board.starting(f).num == 2) {
                val o = board.starting(f).but(starting(f)).head
                satisfyIf(f, OppositeGate, "Gate exists in " + o.name, (gates ++ ugates).contains(o))
            }

            satisfyIf(f, EightGates, "Eight Gates on the map", (gates ++ ugates).%(_.glyph.onMap).num >= 8)
            satisfyIf(f, TenGates, "Ten Gates on the map", (gates ++ ugates).%(_.glyph.onMap).num >= 10)
            satisfyIf(f, TwelveGates, "Twelve Gates on the map", (gates ++ ugates).%(_.glyph.onMap).num >= 12)

            satisfyIf(f, GooMeetsGoo, "GOO shares Area with another GOO", board.regions.%(r => of(f).at(r, GOO).any && factions.but(f).%(e => of(e).at(r, GOO).any).any).any)
            satisfyIf(f, UnitsAtEnemyGates, "Units at two enemy Gates", board.regions.%(r => of(f).at(r).exceptUncontrolledFilth(this).any && factions.but(f).%(e => of(e).gates.contains(r)).any).num >= 2)
        }
    }

    def getCathedralCost(r : Region) : Int = 1 + board.connected(r).intersect(cathedrals).any.??(2)

    def perform(action : Action) : ($[String], Continue) = {
        val c = performX(action)

        val l = mlog.reverse

        mlog = $

        (l, c)
    }

    def performX(action : Action) : Continue = {
        val c = performY(action)

        c match {
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

                    log("From " + r + " cant fly to " + board.regions.diff(destinations).mkString(", "))
                }
                */

                board.starting(f).diff(starting.values.toList)./(StartingRegionAction(f, _))
            }
            else
                PlayOrderAction

        case StartingRegionAction(self, r) =>
            starting += self -> r

            self.allUnits.foreach { uc => of(self).units :+= new UnitFigure(self, uc, of(self).units.%(_.uclass == uc).num, self.poolR) }

            1.to(6).foreach(_ => place(self, Acolyte, r))

            // Temp starting setup (for debug)
            // if (of(self).has(Immortal)) {
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
                    of(self).units :+= new UnitFigure(self, u, of(self).units.%(_.uclass == u).num, self.poolR)
                }

                // Add High Priest Loyalty Card to the faction.
                of(self).loyaltyCards = of(self).loyaltyCards :+ HighPriestCard
            }

            gates :+= r

            of(self).gates :+= r

            log("" + self + " started in " + r)

            StartAction

        case PowerGatherAction(last) =>
            if (factions.%(f => !of(f).hibernating && of(f).power > 0).any)
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
                of(f).oncePerTurn = Nil
                of(f).ignorePerTurn = Nil
            }

            log("POWER GATHER")

            factions.foreach { f =>
                val hibernate = of(f).power
                val cultists = of(f).all(Cultist).num
                val captured = factions./~(w => of(w).at(f.prison)).num
                val ownGates = of(f).gates.num + of(f).ugate.any.??(1)
                val oceanGates = (of(f).has(YhaNthlei) && of(f).has(Cthulhu)).?(factions.but(f)./(f => of(f).allGates.%(_.glyph == Ocean).num).sum).|(0)
                val darkYoungs = (of(f).has(RedSign)).?(of(f).all(DarkYoung).num).|(0)
                val feast = (of(f).has(Feast)).?(desecrated.%(r => of(f).at(r).any).num).|(0)
                val abandoned = abandonedGates.num
                var cathedralGates = 0

                if (of(f).has(WorshipServices)) {
                    factions.but(f).foreach { fx =>
                        board.regions.%(nx).%(cathedrals.contains).%(r => of(fx).gates.contains(r)).some.foreach { l => cathedralGates += l.num }
                    }
                }
                else if (factions.%(of(_).has(WorshipServices)).num > 0) {
                    board.regions.%(nx).%(cathedrals.contains).%(r => of(f).gates.contains(r)).some.foreach { l => cathedralGates += l.num }
                }

                of(f).power = hibernate + ownGates * 2 + abandoned + cultists + captured + oceanGates + darkYoungs + feast + cathedralGates
                of(f).hibernating = false

                val fromHibernate = if (hibernate > 0) Some(hibernate.styled("region") + " hibernate") else None
                val fromGates = if (ownGates > 0) Some((("2 x " + ownGates).styled("region") + " gate" + (if (ownGates == 1) "" else "s"))) else None
                val fromAbandoned = if (abandoned > 0) Some(abandoned.styled("region") + " abandoned") else None
                val fromCultist = if (cultists > 0) Some(cultists.styled("region") + " cultist" + (if (cultists == 1) "" else "s")) else None
                val fromCaptured = if (captured > 0) Some(captured.styled("region") + " captured") else None
                val fromYhaNthlei = if (oceanGates > 0) Some(oceanGates.styled("region") + " enemy controlled ocean gate" + (if (oceanGates == 1) "" else "s")) else None
                val fromDarkYoungs = if (darkYoungs > 0) Some(darkYoungs.styled("region") + " Dark Young" + (if (darkYoungs == 1) "" else "s")) else None
                val fromFeast = if (feast > 0) Some(feast.styled("region") + " desecrated") else None
                val fromCathedralGates = if (cathedralGates > 0) Some(cathedralGates.styled("region") + " cathedral" + (if (cathedralGates == 1) "" else "s")) else None

                log("" + f + " got " + of(f).power.power + " (" + List(fromHibernate, fromGates, fromAbandoned, fromCultist, fromCaptured, fromYhaNthlei, fromDarkYoungs, fromFeast, fromCathedralGates)./~(x => x).mkString(" + ") + ")")
            }

            factions.foreach { f =>
                val captured = factions.flatMap(w => of(w).at(f.prison))

                if (captured.any) {
                    captured.foreach(eliminate)

                    log("" + f + " released " + captured.mkString(", "))
                }
            }


            val max = factions./(of(_).power).max
            val min = (max + 1) / 2

            if (min == 0) {
                log("Humanity won")
                return GameOver(Nil)
            }

            factions.foreach { f =>
                if (of(f).power < min) {
                   log("" + f + " power increased to " + min.power)
                   of(f).power = min
                }
            }

            checkPowerReached()

            AfterPowerGatherAction

        case AfterPowerGatherAction =>
            factions.foreach { f =>
                if (of(f).want(MaoCeremony)) {
                    val cs = of(f).onMap(Cultist)
                    if (cs.any)
                        return cs./(c => MaoCeremonyAction(f, c.region, c.uclass)) :+ MaoCeremonyDoneAction(f)
                }
            }

            factions.foreach(of(_).ignorePerInstant = Nil)

            DragonAscendingInstantAction(DragonAscendingUpAction("first player determination", FirstPlayerDeterminationAction))

        case DoomPhaseAction =>
            factions.foreach { f =>
                val validGates = of(f).gates.filter { r =>
                    val filthHere = factions.exists { other =>
                            other != f &&
                            of(other).has(TheBrood) &&
                            of(other).at(r).exists(_.uclass == Filth)
                        }

                    !filthHere
                }

                val g = validGates.num + of(f).ugate.any.??(1)
                of(f).doom += g
                log("" + f + " got " + g.doom)

                if (of(f).has(Byatis)) {
                    val r = of(f).goo(Byatis).region
                    val enemiesPresent = factions.but(f).exists(other => of(other).at(r).any)
                    if (!enemiesPresent) {
                        log("" + f + " gained " + 1.es + " from " + f.styled(Byatis) + " and " + "Toad of Berkeley".styled("nt"))
                        giveES(f, 1)
                    }
                }
            }

            log(CthulhuWarsSolo.DottedLine)
            showROAT()

            CheckSpellbooksAction(DoomNextPlayerAction(first))

        case ActionPhaseAction =>
            if (factions.%(of(_).doom >= 30).any || ritualTrack(ritualMarker) == 999)
                return GameOverPhaseAction

            // Remove the High Priest Loyalty card, if there is one (it's only ever assigned at start).
            loyaltyCards = loyaltyCards.but(HighPriestCard)

            log("=======================================================================================================================")
            log("Turn " + turn)
            log("ACTIONS")

            round = 0

            CheckSpellbooksAction(MainAction(first))

        case GameOverPhaseAction =>
            factions.%(of(_).needs(AnytimeGainElderSigns)).foreach { f =>
                satisfy(f, AnytimeGainElderSigns, "Anytime Spellbook", min(3, factions.but(f).%(of(_).hasAllSB).num))
                return CheckSpellbooksAction(GameOverPhaseAction)
            }

            factions.%(of(_).es.any).foreach { f =>
                log("" + f + " revealed " + of(f).es.num.es + " for " + of(f).es./(_.value).sum.doom)
                of(f).doom += of(f).es./(_.value).sum
                of(f).revealed ++= of(f).es
                of(f).es = Nil
            }

            val contenders = factions.%(of(_).hasAllSB)
            val winners = contenders.%(of(_).doom == contenders./(of(_).doom).max)

            if (winners.none)
                log("Humanity won")
            else {
                log(winners.mkString(", ") + " won")
            }

            GameOver(winners)

        case FirstPlayerDeterminationAction =>
            val max = factions./(of(_).power).max
            val fs = factions.%(f => of(f).power == max)

            if (fs.num > 1) {
                fs./(FirstPlayerAction(first, _))
            }
            else {
                val old = first
                first = fs.head

                if (old != first)
                    log("" + first + " became the first player")

                PlayOrderAction
            }

        case FirstPlayerAction(self, f) =>
            first = f

            if (self == f)
                log("" + self + " decided to remain the first player")
            else
                log("" + self + " chose " + f + " as the first player")

            PlayOrderAction

        case PlayOrderAction =>
            satisfy(first, FirstPlayer, "Become Starting Player")

            val forward = factions.dropWhile(_ != first) ++ factions.takeWhile(_ != first)
            val backward = forward.take(1) ++ forward.drop(1).reverse

            PlayDirectionAction(first, forward) :: PlayDirectionAction(first, backward)

        case PlayDirectionAction(self, fs) =>
            order = fs

            log("Play order " + order.mkString(", "))

            if (turn == 1)
                ActionPhaseAction
            else {
                log(CthulhuWarsSolo.DottedLine)
                log("DOOM PHASE")

                factions.foreach { f => satisfyIf(f, FirstDoomPhase, "The first Doom phase", turn == 2) }
                factions.foreach { f => satisfyIf(f, FiveSpellbooks, "Have five spellbooks", of(f).requirements.num == 1) }

                CheckSpellbooksAction(DoomPhaseAction)
            }

        // SPELLBOOK
        case CheckSpellbooksAction(next) =>
            val fs = factions.%(f => of(f).requirements.num + nonIGOO(of(f).spellbooks).num < nonIGOO(f.spellbooks).num)
            val fe = factions.%(f => of(f).es.%(_.value == 0).any)
            if (fs.any) {
                val f = fs(0)
                val bs = (nonIGOO(f.spellbooks).%!(of(f).has) ++ neutralSpellbooks).diff(of(f).ignorePerInstant)
                bs./(SpellbookAction(f, _, next))
            }
            else
            if (fe.any) {
                val f = fe(0)
                val n = of(f).es.%(_.value == 0).num
                val es = factions./~(f => of(f).es ++ of(f).revealed)

                DrawES("" + f + " gets " + n.es, 18 - es.%(_.value == 1).num, 12 - es.%(_.value == 2).num, 6 - es.%(_.value == 3).num, (x, public) => ElderSignAction(f, n, x, public, next))
            }
            else {
                val end = factions.%(of(_).doom >= 30).any || ritualTrack(ritualMarker) == 999
                if (!end && next.isInstanceOf[MainAction])
                    log(CthulhuWarsSolo.DottedLine)

                Force(next)
            }

        case SpellbookAction(self, sb, next) =>
            of(self).spellbooks = of(self).spellbooks :+ sb

            log("" + self + " received " + sb.full)

            neutralSpellbooks = neutralSpellbooks.but(sb)

            if (of(self).hasAllSB)
                factions.foreach { f => satisfy(f, AnotherFactionAllSpellbooks, "Another faction has all spellbooks") }

            of(self).ignorePerInstant = Nil

            CheckSpellbooksAction(next)

        case ElderSignAction(f, _, v, public, next) =>
            if (v == 0) {
                val n = of(f).es.%(_.value == 0).num
                of(f).doom += n
                of(f).es = of(f).es.%(_.value > 0)
                log("No more " + "Elder Signs".styled("es") + ", " + f + " got " + n.doom + " instead")
            }
            else {
                of(f).es = of(f).es.%(_.value > 0) ++ of(f).es.%(_.value == 0).drop(1) :+ ElderSign(v)
                if (public)
                    log("" + f + " got " + 1.es + " worth " + v.doom)
            }
            CheckSpellbooksAction(next)


        // REVEAL
        case RevealESMainAction(self) =>
            (of(self).es +: of(self).es.sortBy(_.value)./(e => List(e))).distinct./(RevealESAction(self, _ , false, MainAction(self))) :+ MainCancelAction(self)

        case RevealESDoomAction(self) =>
            (of(self).es +: of(self).es.sortBy(_.value)./(e => List(e))).distinct./(RevealESAction(self, _ , of(self).has(StarsAreRight), DoomAction(self))) :+ DoomCancelAction(self)

        case RevealESAction(self, es, power, next) =>
            val sum = es./(_.value).sum
            of(self).doom += sum

            of(self).revealed ++= es
            of(self).es = of(self).es.diff(es)

            if (power) {
                of(self).power += sum
                log("" + self + " revealed " + es.num.es + " for " + sum.doom + " and " + sum.power)
            }
            else
                log("" + self + " revealed " + es.num.es + " for " + sum.doom)

            Force(next)


        // LOYALTY CARDS
        case LoyaltyCardDoomAction(self) =>
            val hasGOOAtGate = of(self).gates.exists(r => of(self).at(r, GOO).any)
            val isAncientsAndHaveFourCathedrals = self == AN && cathedrals.num == 4

            val availableCards = loyaltyCards
                .filter { c =>
                    val doomOK  = of(self).doom  >= c.doomCost
                    val powerOK = of(self).power >= c.powerCost
                    doomOK && powerOK && (c.doomCost > 0 || (hasGOOAtGate || isAncientsAndHaveFourCathedrals))
                }
                .distinct

            val cardActions = availableCards./(c => LoyaltyCardAction(self, List(c), DoomAction(self)))

            cardActions.:+(DoomCancelAction(self))

        case LoyaltyCardAction(self, lcs, next) =>
            var lc = lcs(0)
            of(self).loyaltyCards = of(self).loyaltyCards :+ lc
            of(self).obtainedLoyaltyCard = true
            loyaltyCards = loyaltyCards.but(lc)

            of(self).doom -= lc.doomCost
            of(self).power -= lc.powerCost

            if (lc.powerCost == 0) {
                log("" + self + " obtained the " + lc.short + " Loyalty Card".styled("nt") + " for " + lc.doomCost.styled("doom") + " " + "doom".styled("doom"))
            }
            else if (lc.doomCost == 0) {
                log("" + self + " obtained the " + lc.short + " Loyalty Card".styled("nt") + " for " + lc.powerCost.styled("power") + " " + "power".styled("power"))
            }
            else {
                log("" + self + " obtained the " + lc.short + " Loyalty Card".styled("nt") + " for " + lc.doomCost.styled("doom") + " doom and " + lc.powerCost.styled("power") + " " + "power".styled("power"))
            }

            AddNeutralUnits(self, lc, next)

        case AddNeutralUnits(self, lc, next) =>
            val unitClassByName : Map[String, UnitClass] = Map(
                "Ghast" -> Ghast,
                "Gug" -> Gug,
                "Shantak" -> Shantak,
                "Star Vampire" -> StarVampire,
                "Byatis" -> Byatis,
                "Abhoth" -> Abhoth,
                "Daoloth" -> Daoloth,
                "Nyogtha" -> Nyogtha
            )

            unitClassByName.get(lc.name) match {
                case Some(uc) =>
                    val isIGOO = uc.isInstanceOf[IGOO]

                    // Add units to pool.
                    lc.quantity.times(uc).foreach { u =>
                        of(self).units :+= new UnitFigure(self, u, of(self).units.%(_.uclass == u).num, self.poolR)
                    }

                    // Abhoth: Also add Filth to the pool.
                    if (uc == Abhoth) {
                        // Remove all Filth from other players' pools (should be max one player).
                        factions.but(self)./~(of(_).inPool(Filth)).foreach { u =>
                            of(u.faction).units = of(u.faction).units.but(u)
                        }

                        // Get all Filth that currently exist in the game (on the map, submerged, captured etc).
                        val filthInGame = factions.but(self)./~(of(_).units.filter(_.uclass == Filth))

                        filthInGame.foreach { u =>
                            // Remove Filth from the old owner.
                            of(u.faction).units = of(u.faction).units.but(u)
                            // Delete submerged Filth if the new owner isn't GC.
                            if (self != GC && u.region == GC.deep) {
                                log("Submerged " + self.styled(Filth) + " destroyed as " + self.styled(Abhoth) + " changes hands.")
                            } else {
                                // Recreate Filth for the new owner in the same region.
                                of(self).units :+= new UnitFigure(self, u.uclass, of(self).units.count(_.uclass == u.uclass), u.region, getFactionUnits = f => of(f).units)
                            }
                        }

                        val numberOfFilthInGame = factions.flatMap(f => of(f).units.filter(_.uclass == Filth)).num

                        // Add remaining Filth to pool, up to 12 total.
                        (12 - numberOfFilthInGame).times(Filth).foreach { uc =>
                            of(self).units :+= new UnitFigure(self, uc, of(self).units.count(_.uclass == uc), self.poolR, getFactionUnits = f => of(f).units)
                        }
                    }
                    else if (uc == Daoloth) {
                        of(self).spellbooks :+= CosmicUnity
                    }
                    else if (uc == Nyogtha) {
                        of(self).spellbooks :+= FromBelow
                    }

                    if (isIGOO && of(self).has(Immortal)) {
                        log("" + self + " gained " + 1.es + " as " + Immortal.full)
                        giveES(self, 1)
                    }

                    // Place unit(s).
                    val regions =
                        if (isIGOO && self != AN)
                            getGOOControlledGateRegions(self)
                        else
                            getControlledGatesRegions(self)

                    if (regions.any) {
                        QAsk(regions./(r => LoyaltyCardSummonAction(self, uc, r, next)))
                    } else {
                        log("" + self + " had nowhere to place " + self.styled(uc))
                        Force(next)
                    }

                case None =>
                    println("Undefined loyalty card name \"" + lc.name + "\"")
                    Force(next)
            }

        case LoyaltyCardSummonAction(self, uc, r, next) =>
            place(self, uc, r)
            log("" + self + " placed " + self.styled(uc) + " in " + r)

            CheckSpellbooksAction(DoomAction(self))

            if (uc == Ghast && of(self).inPool(Ghast).any) {
                // Ghast: Repeat until none left in pool.
                QAsk(getControlledGatesRegions(self)./(r => LoyaltyCardSummonAction(self, uc, r, next)))
            }
            else if (uc == Nyogtha && of(self).inPool(Nyogtha).any) {
                // Nyogtha: Place both in same region
                Force(LoyaltyCardSummonAction(self, uc, r, next))
            }
            else {
                // Standard behavior is summoning one unit.
                Force(next)
            }


        // DOOM
        case DoomAction(self) =>
            checkGatesLost()

            var player = of(self)

            var options : List[FactionAction] = Nil

            val cost = player.has(Herald).?(5).|(ritualCost)

            if (player.want(DragonAscending) && factions.%(of(_).power > player.power).any)
                options :+= DragonAscendingDoomAction(self)

            if (player.power >= cost && !acted) {
                options :+= RitualAction(self, cost, 1)

                if (player.can(DragonDescending))
                    options :+= DragonDescendingDoomAction(self, cost)
            }

            if (player.can(BloodSacrifice) && player.has(ShubNiggurath) && player.all(Cultist).any)
                options :+= BloodSacrificeDoomAction(self)

            if (player.can(DeathFromBelow) && player.inPool(Monster).any)
                options :+= DeathFromBelowDoomAction(self)

            if (player.can(Dematerialization) && !player.units.isUncontrolledFilthOnly(this))
                options :+= DematerializationDoomAction(self)

            if (player.es.num > 0)
                options :+= RevealESDoomAction(self)

            if (player.all(HighPriest).any)
                options :+= SacrificeHighPriestDoomAction(self)

            // If you can afford any of the available loyaltyCards.
            // In the case of 0-doom (= IGOO) cards also: if you have a GOO at a controlled gate.
            // Unless you are AN, in which case four cathedrals are required for IGOO:s instead.
            if (!of(self).obtainedLoyaltyCard && loyaltyCards.exists { c =>
                val hasGOOAtGate = player.gates.exists(r => player.at(r, GOO).any)
                val doomOK  = player.doom  >= c.doomCost
                val powerOK = player.power >= c.powerCost
                val isAncientsAndHaveFourCathedrals = self == AN && cathedrals.num == 4
                doomOK && powerOK && (c.doomCost > 0 || (hasGOOAtGate || isAncientsAndHaveFourCathedrals))
            })
                options :+= LoyaltyCardDoomAction(self)

            if (player.needs(AnytimeGainElderSigns))
                options :+= AnytimeGainElderSignsDoomAction(self)

            if (player.has(AncientSorcery) && player.at(SL.slumber, SerpentMan).any)
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

            of(self).obtainedLoyaltyCard = false

            val next = (order ++ order).dropWhile(_ != self).drop(1).head

            if (next != first)
                DoomNextPlayerAction(next)
            else {
                factions.foreach(of(_).borrowed = Nil)
                CheckSpellbooksAction(ActionPhaseAction)
            }

        // RITUAL
        case RitualAction(self, cost, k) =>
            of(self).power -= cost

            val validGates = of(self).gates.filter { r =>
                val filthHere = factions.exists { other =>
                    other != self &&
                    of(other).has(TheBrood) &&
                    of(other).at(r).exists(_.uclass == Filth)
                }
                !filthHere
            }

            val doom = (validGates.num + of(self).ugate.any.??(1)) * k

            var es = of(self).all(GOO).count(isFactionGOO)

            if (of(self).has(Consecration)) {
                if (cathedrals.num == 4) {
                    es = 2
                }
                else if (cathedrals.num > 0) {
                    es = 1
                }
            }

            of(self).doom += doom
            log("" + self + " performed the ritual" + " for " + cost.power + " and gained " + doom.doom + (es > 0).??(" and " + es.es))
            giveES(self, es)

            acted = true

            if (ritualTrack(ritualMarker) != 999)
                ritualMarker += 1

            showROAT()
            satisfy(self, PerformRitual, "Perform Ritual of Annihilation")
            CheckSpellbooksAction(DoomAction(self))

        // MAIN
        case MainAction(self) =>
            if (factions.%(f => of(f).requirements.num + nonIGOO(of(f).spellbooks).num < nonIGOO(f.spellbooks).num).any)
                return CheckSpellbooksAction(MainAction(self))

            val player = of(self)
            val others = factions.but(self)

            if (player.active) {
                others.%(of(_).can(Devolve)).%(of(_).inPool(DeepOne).any).foreach { f =>
                    board.regions.%(of(f).at(_, Acolyte).any).foreach { r =>
                        if (!acted)
                            if (canCapture(self, f, r))
                                if (!of(f).option(OutOfTurnDevolveOff))
                                    return Force(DevolveMainAction(f, DevolveDoneAction(f, MainAction(self))))

                        if (!of(f).ignored(Devolve))
                            if (!acted || (player.hasAllSB && !battled.contains(r)))
                                if (canAttack(self, f, r))
                                    if (!of(f).option(OutOfTurnDevolveOff) && !of(f).option(OutOfTurnDevolveAvoidCapture))
                                        return Force(DevolveMainAction(f, DevolveDoneAction(f, MainAction(self))))
                    }
                }

                checkGatesOwnership(self)
            }

            if (player.active) {
                others.%(of(_).all(HighPriest).any).foreach { f =>
                    board.regions.%(of(f).at(_, HighPriest).any).foreach { r =>
                        if (!f.ignoredSacrificeHighPriest) {
                            if (!acted)
                                if (canCapture(self, f, r))
                                    if (!of(f).option(OutOfTurnSacrificeHighPriestOff))
                                        return Force(SacrificeHighPriestMainAction(f, SacrificeHighPriestDoneAction(f, MainAction(self))))

                            if (!acted || (player.hasAllSB && !battled.contains(r)))
                                if (canAttack(self, f, r))
                                    if (!of(f).option(OutOfTurnSacrificeHighPriestOff) && !of(f).option(OutOfTurnSacrificeHighPriestAvoidCapture))
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
                def apply(n : Int)(r : Region) = of(self).power >= tax(r, self) + n
                def apply(c : Region => Int)(r : Region) = of(self).power >= tax(r, self) + c(r)
            }

            var options : List[FactionAction] = Nil

            if (player.has(Lethargy) && player.has(Tsathoggua) && nexus.none && others.%(f => of(f).power > 0 && !of(f).hibernating).any)
                if (!this.options.has(IceAgeAffectsLethargy) || afford(0)(player.goo(Tsathoggua).region))
                    options :+= LethargyMainAction(self)

            if (player.has(Hibernate)) {
                val enemyGOOs =
                    others.flatMap(f => of(f).all(GOO))
                        .groupBy(_.uclass match {
                            case Nyogtha => "Nyogtha"
                            case other   => other.name
                        })
                        .map(_._2.head)

                val n = min(player.power, enemyGOOs.size)
                options :+= HibernateMainAction(self, n)
            }

            if (player.want(DragonAscending) && player.power < others./(of(_).power).max)
                options :+= DragonAscendingMainAction(self)

            if (moveableUnits(player).any)
                options :+= MoveMainAction(self)

            if (player.has(BeyondOne) && gates.num < board.regions.num && board.regions.diff(gates).%(afford(1)).any)
                gates.%(r => others.%(of(_).at(r, GOO).any).none).%(r => player.at(r).%(_.uclass.cost >= 3).exceptByatis.any).some.foreach {
                    options :+= BeyondOneMainAction(self, _)
                }

            board.regions.%(nx).%(afford(1)).%(r => others.%(f => canCapture(self, f, r)).any).some.foreach { options :+= CaptureMainAction(self, _) }

            if (
                player.has(CaptureMonster) &&
                board.regions.%(nx).%(afford(1)).%(r =>
                    player.at(r, Tsathoggua).any && (
                        // Monsters from other factions
                        others.exists(f => of(f).at(r, GOO).none && of(f).at(r, Monster).any)
                        // Or uncontrolled Filth
                        || factions.exists(f => of(f).at(r).uncontrolledFilthOnly(this).nonEmpty)
                    )
                ).any
            )
                options :+= CaptureMonsterMainAction(self)

            val cs = player.inPool(Cultist)./(_.uclass).distinct.reverse

            cs.foreach { uc =>
                board.regions.%(player.at(_).any).some.|(board.regions).%(nx).%(afford(r => self.recruitCost(this, uc, r))).some.foreach {
                    options :+= RecruitMainAction(self, uc, _)
                }
            }

            if (nexusExtra.none)
                board.regions.%(nx).%(afford(1)).diff(battled).%(r =>
                    // Either: you can attack another faction...
                    others.exists(f => canAttack(self, f, r)) ||
                    // Or: you can attack your own uncontrolled Filth.
                    {
                        val ownUnits = of(self).at(r)
                        val hasAttackers = ownUnits.exceptUncontrolledFilth(this).nonEmpty
                        val hasTargets = ownUnits.uncontrolledFilthOnly(this).nonEmpty
                        hasAttackers && hasTargets
                    }
                )
                .some.foreach {
                    options :+= AttackMainAction(self, _)
                }

            board.regions.%(nx).%(afford(3 - player.has(UmrAtTawil).??(1))).%!(gates.contains).%(r => hasCultistOrRSDY(self, r)).some.foreach {
                options :+= BuildGateMainAction(self, _)
            }

            if (player.faction == AN && cathedrals.num < 4) {
                 val existingGlyphs = cathedrals.map(_.glyph).toSet

                 val validRegions = board.regions.filter { r =>
                     !cathedrals.contains(r) &&
                     afford(getCathedralCost(r))(r) &&
                     hasCultistOrRSDY(self, r) &&
                     !existingGlyphs.contains(r.glyph)
                }

                if (validRegions.nonEmpty) {
                     options :+= BuildCathedralMainAction(self, validRegions.toList)
                }
            }

            if (player.has(CursedSlumber) && gates.%(_.glyph == Slumber).none && player.gates.%(nx).%(_.glyph.onMap).any)
                options :+= CursedSlumberSaveMainAction(self)

            if (player.has(CursedSlumber) && gates.%(_.glyph == Slumber).any)
                board.regions.%(nx).%(afford(1)).%!(gates.contains).some.foreach { options :+= CursedSlumberLoadMainAction(self, _) }

            ((player.inPool(Terror) ++ player.inPool(Monster)).exceptFilth.sortWith(sortAllUnits(player)))./(_.uclass).distinct.reverse.foreach { uc =>
                board.regions.%(nx).%(afford(r => self.summonCost(this, uc, r))).%(r => canAccessGate(self, r)).some.foreach { options :+= SummonMainAction(self, uc, _) }
            }

            if (player.has(Abhoth) && player.inPool(Filth).nonEmpty) {
                val affordableExists = board.regions.%(nx).%(afford(r => self.summonCost(this, Filth, r))).nonEmpty
                if (affordableExists) {
                    options :+= FilthMainAction(self, board.regions)
                }
            }

            player.inPool(GOO).filter(isFactionGOO)./(_.uclass).distinct.reverse.foreach { uc =>
                board.regions.%(nx).%(afford(r => self.awakenCost(this, uc, r))).some.foreach { options :+= AwakenMainAction(self, uc, _) }
            }

            if (player.has(NightmareWeb)) {
                val nyogthaInPool = player.inPool(Nyogtha).nonEmpty
                if (nyogthaInPool && board.regions.exists(affordF(self, 2))) {
                    val validRegions = board.regions.filter(r => of(self).at(r).nonEmpty && affordF(self, 2)(r))
                    if (validRegions.nonEmpty)
                        options :+= NightmareWebMainAction(self, validRegions)
                }
            }

            if (player.has(Dreams) && player.inPool(Acolyte).any)
                board.regions.%(afford(2)).%(r => others.%(of(_).at(r, Acolyte).any).any).some.foreach { options :+= DreamsMainAction(self, _) }

            if (player.has(Submerge) && player.has(Cthulhu) && player.goo(Cthulhu).region.glyph == Ocean)
                options :+= SubmergeMainAction(self, player.goo(Cthulhu).region)

            if (player.at(GC.deep).any)
                board.regions.%(afford(0)).some.foreach { options :+= UnsubmergeMainAction(self, _) }

            if (player.has(Devolve) && player.all(Acolyte).any && player.inPool(DeepOne).any)
                options :+= DevolveMainAction(self, MainCancelAction(self))


            if (player.can(ThousandForms) && player.has(Nyarlathotep))
                options :+= ThousandFormsMainAction(self)

            if (player.needs(Pay4Power) && player.power >= 4)
                options :+= Pay4PowerMainAction(self)

            if (player.needs(Pay6Power) && player.power >= 6)
                options :+= Pay6PowerMainAction(self)

            if (player.needs(Pay4Power) && player.needs(Pay6Power) && player.power >= 10)
                options :+= Pay10PowerMainAction(self)


            if (player.needs(GiveWorstMonster))
                options :+= GiveWorstMonsterMainAction(self)

            if (player.needs(GiveBestMonster))
                options :+= GiveBestMonsterMainAction(self)


            if (player.has(Avatar) && player.has(ShubNiggurath)) {
                val r = player.goo(ShubNiggurath).region
                val t = tax(r, self)
                board.regions.but(r).%(afford(1 + t)).%(r => factions.%(of(_).at(r, Cultist, Monster).any).any).some.foreach {
                    options :+= AvatarMainAction(self, r, _)
                }
            }

            if (player.has(Ghroth) && player.power >= 2)
                options :+= GhrothMainAction(self)

            if (player.has(DreadCurse)) {
                val n = player.all(Abomination).num + player.all(SpawnOW).num
                if (n > 0) {
                    val l = board.regions.%(afford(2)).%(r =>
                            others.exists(f =>
                                of(f).at(r).exceptUncontrolledFilth(this).nonEmpty
                            )
                        )
                    if (l.any)
                        options :+= DreadCurseMainAction(self, n, l)
                }
            }

            if (player.needs(Eliminate2Cultists) && player.all(Cultist).num >= 2)
                options :+= Eliminate2CultistsMainAction(self)

            if (player.has(Desecrate) && player.has(KingInYellow) && desecrated.num <= 12) {
                val r = player.goo(KingInYellow).region
                if (!desecrated.contains(r)) {
                    val te = player.has(Hastur) && player.has(ThirdEye)
                    if (afford(te.?(1).|(2))(r))
                        options :+= DesecrateMainAction(self, r, te)
                }
            }

            if (player.can(HWINTBN) && !player.used(ScreamingDead) && player.has(Hastur)) {
                val o = player.goo(Hastur).region
                board.regions.%(afford(1)).but(o).%(r => factions.%(of(_).at(r, Cultist).any).any).some.foreach {
                    options :+= HWINTBNMainAction(self, o, _)
                }
            }

            if (player.can(ScreamingDead) && !player.used(HWINTBN) && player.has(KingInYellow)) {
                val o = player.goo(KingInYellow).region
                board.connected(o).%(afford(1)).some.foreach {
                    options :+= ScreamingDeadMainAction(self, o, _)
                }
            }

            if (player.has(Zingaya) && player.inPool(Undead).any)
                board.regions.%(afford(1)).%(r => player.at(r, Undead).any).%(r => others.%(of(_).at(r, Acolyte).any).any).some.foreach {
                    options :+= ZingayaMainAction(self, _)
                }

            if (player.has(Shriek) && player.has(Byakhee))
                board.regions.%(afford(1)).%(r => of(self).all(Byakhee).%(_.region != r).any).some.foreach {
                    options :+= ShriekMainAction(self, _)
                }

            if (player.needs(Provide3Doom))
                options :+= Provide3DoomMainAction(self)


            if (player.has(AncientSorcery) && player.onMap(SerpentMan).%(nx).any && player.borrowed.num < factions.num - 1)
                options :+= AncientSorceryMainAction(self)

            if (player.needs(Pay3SomeoneGains3) && player.power >= 3)
                options :+= Pay3SomeoneGains3MainAction(self)

            if (player.needs(Pay3EverybodyLoses1) && player.power >= 3)
                options :+= Pay3EverybodyLoses1MainAction(self)

            if (player.needs(Pay3EverybodyGains1) && player.power >= 3)
                options :+= Pay3EverybodyGains1MainAction(self)


            if (player.needs(AnytimeGainElderSigns))
                options :+= AnytimeGainElderSignsMainAction(self)

            if (player.has(IceAge))
                board.regions.%(afford(1)).%(r => of(self).iceage./(_ != r).|(true)).some.foreach {
                    options :+= IceAgeMainAction(self, _)
                }

            if (player.has(Undimensioned) && player.units.%(_.region.glyph.onMap)./(_.region).distinct.num > 1 && player.units.%(_.region.glyph.onMap)./(_.region).%(afford(2)).any)
                options :+= UndimensionedMainAction(self)

            if (player.has(Recriminations))
                options :+= RecriminationsMainAction(self)

            if (player.all(HighPriest).any)
                options :+= SacrificeHighPriestMainAction(self, MainCancelAction(self))

            if (player.has(GodOfForgetfulness) && player.has(Byatis)) {
                val br = of(self).goo(Byatis).region

                val l = board.connected(br).filter { r =>
                    factions.but(self).exists { f =>
                        of(f).at(r, Cultist).any
                    }
                }.distinct

                if (l.nonEmpty && afford(1)(br)) {
                    options :+= GodOfForgetfulnessMainAction(self, br, l)
                }
            }

            if (player.es.num > 0)
                options :+= RevealESMainAction(self)

            options = options.% {
                case AttackMainAction(_, _) if player.has(FromBelow) && player.oncePerAction.contains(FromBelow) && !player.hasAllSB => false
                case _ if player.active && !acted && battled.none => true
                case _ if player.active && !acted && player.hasAllSB => true
                case AttackMainAction(_, l) if player.active && (player.hasAllSB && !player.option(UnlimitedBattleOff) && (!player.option(UnlimitedBattleOnlyWithGOO) || l.%(r => player.at(r, GOO).any).any)) && nexusExtra.none && (nexus.none || !acted) => true
                case SummonMainAction(_, _, _) if acted && player.has(Fertility) && !player.option(UnlimitedSummonOff) && (!player.option(UnlimitedSummonEnemyGOO) || others./~(of(_).all(GOO))./(_.region).%(r => canAccessGate(self, r)).any) => true
                case FilthMainAction(_, _) if acted && player.has(Fertility) && !player.option(UnlimitedSummonOff) && (!player.option(UnlimitedSummonEnemyGOO) || others./~(of(_).all(GOO))./(_.region).%(r => canAccessGate(self, r)).any) && player.has(Abhoth) => true
                case RevealESMainAction(_) if player.doom + player.es./(_.value).sum >= 30 => true
                case DevolveMainAction(_, _) if player.active && ((!acted && battled.none) || (!player.option(OutOfTurnDevolveOff) && !player.option(OutOfTurnDevolveAvoidCapture))) => true
                case AnytimeGainElderSignsMainAction(_) if player.doom + player.es./(_.value).sum + min(3, factions.but(self).%(of(_).hasAllSB).num) * 3 >= 30 && (player.requirements.num == 1 || others.%(of(_).hasAllSB).none) => true
                case DragonAscendingMainAction(_) if !acted && battled.none => true
                case DragonAscendingMainAction(_) if (player.hasAllSB && !player.option(UnlimitedBattleOff)) && board.regions.%(nx).diff(battled).%(r => others.%(f => canAttack(self, f, r)).any).any => true
                case _ => false
            }

            if (!player.active)
                options :+= MainDoneCancelAction(self)
            else
            if (acted || battled.any || player.oncePerRound.contains(Fertility) || player.oncePerRound.contains(HWINTBN) || player.oncePerRound.contains(ScreamingDead) || nexus.any)
                options :+= MainDoneAction(self)
            else
                options :+= PassAction(self)

            if (options.num > 1) {
                if (player.hasAllSB)
                    options :+= ToggleUnlimitedBattleAction(self, List(UnlimitedBattleOn, UnlimitedBattleOff, UnlimitedBattleOnlyWithGOO).intersect(player.ignoreOptionsNew).single.|(UnlimitedBattleOn))

                if (player.has(Fertility))
                    options :+= ToggleUnlimitedSummonAction(self, List(UnlimitedSummonOn, UnlimitedSummonOff, UnlimitedSummonEnemyGOO).intersect(player.ignoreOptionsNew).single.|(UnlimitedSummonOn))

                if (player.has(Devolve))
                    options :+= ToggleOutOfTurnDevolveAction(self, List(OutOfTurnDevolveOn, OutOfTurnDevolveOff, OutOfTurnDevolveAvoidCapture).intersect(player.ignoreOptionsNew).single.|(OutOfTurnDevolveOn))

                if (player.all(HighPriest).any)
                    options :+= ToggleOutOfTurnSacrificeHighPriestAction(self, List(OutOfTurnSacrificeHighPriestOn, OutOfTurnSacrificeHighPriestOff, OutOfTurnSacrificeHighPriestAvoidCapture).intersect(player.ignoreOptionsNew).single.|(OutOfTurnSacrificeHighPriestOn))
            }

            options

        case MainCancelAction(self) =>
            Force(MainAction(self))

        case MainNextPlayerAction(f) =>
            if (nexusExtra.any) {
                battle = new Battle(this, nexus.get.region, f, nexusExtra.get, log)

                nexusExtra = None

                log("" + battle.attacker + " proceeded to battle " + battle.defender + " in " + battle.region)

                battle.proceed()
            }
            else
            if (nexus.any) {
                acted = nexus.get.acted
                battled = nexus.get.battled

                battle = new Battle(this, nexus.get.region, nexus.get.attacker, nexus.get.defender, log)

                nexus = None

                log("" + battle.attacker + " proceeded to battle " + battle.defender + " in " + battle.region)

                battle.proceed()
            }
            else {
                acted = false
                reveal = false
                battled = Nil
                factions.foreach(of(_).oncePerRound = Nil)
                round += 1

                if (factions.%(of(_).doom >= 30).any)
                    CheckSpellbooksAction(GameOverPhaseAction)
                else {
                    if (factions.%(!of(_).hibernating).%(of(_).power > 0).none) {
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
            of(self).units.foreach(_.remove(Summoned))
            AfterAction(self)

        case AfterAction(self) =>
            checkGatesOwnership(self)

            if (of(self).has(FromBelow) && !of(self).oncePerAction.contains(FromBelow)) {
                nyogthaPendingFreeBattle.get(self).foreach { otherRegion =>
                    val attackableFactions = factions.but(self).filter(g => canAttack(self, g, otherRegion))
                    val uncontrolledFilth = {
                        val ownUnits = of(self).at(otherRegion)
                        ownUnits.exceptUncontrolledFilth(this).nonEmpty &&
                        ownUnits.uncontrolledFilthOnly(this).nonEmpty
                    }

                    if ((attackableFactions.nonEmpty || uncontrolledFilth) && affordF(self, 0)(otherRegion)) {
                        of(self).oncePerAction :+= FromBelow
                        nyogthaPendingFreeBattle -= self

                        val actions =
                            attackableFactions.map(g => FromBelowAttackAction(self, otherRegion, g)) ++
                            (if (uncontrolledFilth) List(FromBelowAttackUncontrolledFilthAction(self, otherRegion, self)) else Nil)

                        if (actions.nonEmpty)
                            return QAsk(actions :+ FromBelowAttackDoneAction(self))
                    } else {
                        nyogthaPendingFreeBattle -= self
                    }
                }
            }

            if (of(self).power == 0) log("" + self + " ran out of power")
            if (of(self).power == -1) of(self).power = 0

            factions.%(of(_).has(Passion)).%(of(_).oncePerAction.contains(Passion)).foreach { f =>
                of(f).power += 1
                log("" + f + " got " + 1.power + " from " + Passion.full)
            }

            factions.foreach(of(_).oncePerAction = Nil)

            of(self).ignoreOptions = of(self).ignoreOptionsNew

            CheckSpellbooksAction(MainAction(self))

        // PASS
        case PassAction(self) =>
            val p = of(self).power

            of(self).power = -1

            log("" + self + " passed and forfeited " + p.power)

            EndAction(self)

        // MOVE
        case MoveMainAction(self) =>
            MoveContinueAction(self, false)

        case MoveContinueAction(self, moved) =>
            if (of(self).power == 0)
                Force(MoveDoneAction(self))
            else {
                val units = moveableUnits(of(self)).sortWith(sortAllUnits(of(self)))
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

            if (of(self).has(Flight))
                destinations = destinations ++ destinations.flatMap(board.connected).distinct.but(region).diff(destinations)

            if (uc == Shantak)
                destinations = board.regions

            val arriving = of(self).units.%(_.region.glyph.onMap).%(_.has(Moved))./(_.region).distinct

            destinations = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            destinations = destinations.%(affordF(self, 1))

            val options = destinations./(d => MoveAction(self, uc, region, d))

            if (hasMoved(self))
                QAsk(options :+ MoveCancelAction(self))
            else
                QAsk(options :+ MainCancelAction(self))

        case MoveDoneAction(self) =>
            if (of(self).has(Burrow) && of(self).units.%(u => u.has(Moved))./(u => 1 - u.has(MovedForFree).??(1) + u.has(MovedForDouble).??(1)).sum > 1) {
                of(self).power += 1
                log("" + self + " recovered " + 1.power + " from " + Burrow.full)
            }

            of(self).units.foreach(_.remove(Moved))
            of(self).units.foreach(_.remove(MovedForFree))
            of(self).units.foreach(_.remove(MovedForDouble))

            EndAction(self)

        case MoveAction(self, uc, o, r) =>
            val t = payTax(self, r)

            of(self).power -= 1

            val u = of(self).at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)

            if (t > 0)
                t.timesDo { () =>
                    u.add(MovedForDouble)
                }

            log("" + self + " moved " + self.styled(uc) + " from " + o + " to " + r)

            if (u.uclass == Nyogtha) {
                if (of(self).all(Nyogtha).num < 2)
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

            if (u.uclass == Ithaqua && of(self).has(ArcticWind))
                QAsk(of(self).at(o).%(!_.has(Moved)).exceptByatis.exceptUncontrolledFilth(this)./(u => ArcticWindAction(self, o, u.uclass, r)) :+ ArcticWindDoneAction(self))
            else if (u.uclass == Shantak)
                QAsk(of(self).at(o).%(!_.has(Moved)).%(_.uclass.utype == Cultist)./(u => ShantakCarryCultistAction(self, o, u.uclass, r)) :+ ShantakCarryCultistCancelAction(self))
            else
                MoveContinueAction(self, true)

        case MoveCancelAction(self) =>
            MoveContinueAction(self, true)
        
        // ATTACK
        case AttackMainAction(self, l) =>
            // Faction attack variants.
            val factionVariants = l./~ { r =>
                factions.but(self)
                .%(f => of(f).at(r).nonEmpty)
                .%(f => of(f).at(r).exceptUncontrolledFilth(this).nonEmpty) // Exclude factions with only uncontrolled Filth.
                .%(f => self.strength(this, of(self).at(r), f) > 0)
                ./(f => AttackAction(self, r, f))
            }.sortBy { ao =>
                self.strength(this, of(self).at(ao.r), ao.f) * 10000 +
                ao.f.strength(this, of(ao.f).at(ao.r), self) * 100 +
                of(ao.f).at(ao.r).num
            }.reverse

            // Uncontrolled Filth attack variants.
            val filthVariants = l./~ { r =>
                // Other factions' uncontrolled Filth.
                val others = factions.but(self)./~ { f =>
                    val attackers = of(self).at(r).exceptUncontrolledFilth(this)
                    val targets   = of(f).at(r).uncontrolledFilthOnly(this)
                    val str       = self.strength(this, attackers, f)

                    if (targets.nonEmpty && str > 0)
                        Some(AttackUncontrolledFilthAction(self, r, f))
                    else
                        None
                }

                // Own uncontrolled Filth.
                val ownUnits   = of(self).at(r)
                val attackers  = ownUnits.exceptUncontrolledFilth(this)
                val ownTargets = ownUnits.uncontrolledFilthOnly(this)
                val str        = self.strength(this, attackers, self)

                val ownFilth =
                    if (attackers.nonEmpty && ownTargets.nonEmpty && str > 0)
                        Some(AttackUncontrolledFilthAction(self, r, self))
                    else
                        None

                others ++ ownFilth.toList
            }

            val variants = (factionVariants ++ filthVariants).sortBy {
                case ao : AttackAction =>
                    self.strength(this, of(self).at(ao.r), ao.f) * 10000 +
                    ao.f.strength(this, of(ao.f).at(ao.r), self) * 100 +
                    of(ao.f).at(ao.r).num
                case _ : AttackUncontrolledFilthAction => 0
            }.reverse

            QAsk(variants :+ MainCancelAction(self))

        case AttackAction(self, r, f) =>
            of(self).power -= 1
            payTax(self, r)
            log("" + self + " battled " + f + " in " + r)
            var sl = factions.%(f => of(f).has(EnergyNexus) && of(f).at(r, Wizard).any)

            if (of(self).has(FromBelow) && !of(self).oncePerAction.contains(FromBelow)) {
                val nyRegions = of(self).all(Nyogtha).map(_.region).distinct
                if (nyRegions.size == 2 && nyRegions.contains(r)) {
                    val other = nyRegions.find(_ != r)
                    other.foreach { o => nyogthaPendingFreeBattle += self -> o }
                }
            }
            
            if (nexus.any) {
                log("" + sl.head + " interrupted battle again with " + EnergyNexus.full)
                nexusExtra = Some(f)
                Force(MainAction(sl.head))
            }
            else
            if (sl.any) {
                log("" + sl.head + " interrupted battle with " + EnergyNexus.full)
                nexus = Some(Nexus(r, self, f, factions, acted, battled))
                acted = false
                battled = Nil
                Force(MainAction(sl.head))
            }
            else {
                lastBattleRegionByFaction += self -> r
                battle = new Battle(this, r, self, f, log)
                battle.proceed()
            }

        case AttackUncontrolledFilthAction(self, r, f) =>
            of(self).power -= 1
            payTax(self, r)
            log("" + self + " battled " + "Uncontrolled".styled("nt") + " " + Filth.name.styled("nt") + " in " + r)
            val uncontrolledDefender = Filth.name
            battle = new Battle(this, r, self, f, log, uncontrolledDefender)
            battle.proceed()

        // CAPTURE
        case CaptureMainAction(self, l) =>
            val variants = l./~ { r =>
                factions.but(self).% { f =>
                    canCapture(self, f, r)
                }./ { f =>
                    val uc = if (of(f).at(r, HighPriest).nonEmpty && of(f).at(r, Acolyte).isEmpty) HighPriest else Acolyte
                    CaptureAction(self, r, f, uc)
                }
            }

            QAsk(variants :+ MainCancelAction(self))

        case CaptureAction(self, r, f, _) =>
            of(self).power -= 1
            payTax(self, r)
            val c = of(f).at(r, Cultist).minBy(_.uclass.cost)
            capture(self, c)
            log("" + self + " captured " + c + " in " + r)
            satisfy(self, CaptureCultist, "Capture Cultist")

            val nyogthasHere = of(self).at(r, Nyogtha)
            if (of(self).has(FromBelow) && !of(self).oncePerAction.contains(FromBelow) && nyogthasHere.nonEmpty) {
                val actedNyogtha = nyogthasHere.head

                val result = tryFromBelow(
                    self,
                    actedNyogtha,
                    this,
                    makeAction = otherRegion => {
                        val possibleTargets = factions.but(self).filter(ff => canCapture(self, ff, otherRegion))
                        val targetFaction = possibleTargets.head
                        val uc =
                            if (of(targetFaction).at(otherRegion, HighPriest).nonEmpty &&
                                of(targetFaction).at(otherRegion, Acolyte).isEmpty)
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
            of(self).power -= 3 - of(self).has(UmrAtTawil).??(1)
            payTax(self, r)
            gates :+= r
            of(self).gates :+= r
            log("" + self + " built a gate in " + r)
            EndAction(self)

        // RECRUIT
        case RecruitMainAction(self, uc, l) =>
            val (a, b) = l.partition(abandonedGates.contains)
            QAsk((a ++ b)./(r => RecruitAction(self, uc, r)) :+ MainCancelAction(self))

        case RecruitAction(self, uc, r) =>
            of(self).power -= self.recruitCost(this, uc, r)
            payTax(self, r)
            place(self, uc, r)
            log("" + self + " recruited " + self.styled(uc) + " in " + r)
            EndAction(self)

        // SUMMON
        case SummonMainAction(self, uc, l) =>
            QAsk(l./(r => SummonAction(self, uc, r)) :+ MainCancelAction(self))

        case SummonAction(self, uc, r) =>
            of(self).power -= self.summonCost(this, uc, r)
            payTax(self, r)
            summon(self, uc, r)
            log("" + self + " summoned " + self.styled(uc) + " in " + r)
            if (uc == Ghast && of(self).inPool(Ghast).any) {
                QAsk(getSummonRegions(self)./(r => FreeSummonAction(self, uc, r, EndAction(self))))
            }
            else if (of(self).has(Fertility) && !of(self).ignored(Fertility)) {
                of(self).oncePerRound :+= Fertility
                checkGatesOwnership(self)
                CheckSpellbooksAction(MainAction(self))
            }
            else if (of(self).has(Festival) && uc == UnMan) {
                QAsk(factions.but(self)./(f => FestivalUnManSummonAction(self, f)))
            }
            else
                EndAction(self)

        case FreeSummonAction(self, uc, r, next) =>
            place(self, uc, r)
            log("" + self + " summoned " + self.styled(uc) + " in " + r + " for free")
            CheckSpellbooksAction(MainAction(self))

            if (uc == Ghast && of(self).inPool(Ghast).any) {
                QAsk(getSummonRegions(self)./(r => FreeSummonAction(self, uc, r, next)))
            }
            else {
                Force(next)
            }

        case ToggleUnlimitedBattleAction(self, value) =>
            of(self).ignoreOptionsNew :-= value

            of(self).ignoreOptionsNew :+= (value match {
                case UnlimitedBattleOn => UnlimitedBattleOnlyWithGOO
                case UnlimitedBattleOnlyWithGOO => UnlimitedBattleOff
                case UnlimitedBattleOff => UnlimitedBattleOn
            })

            MainAction(self)

        case ToggleUnlimitedSummonAction(self, value) =>
            of(self).ignoreOptionsNew :-= value

            of(self).ignoreOptionsNew :+= (value match {
                case UnlimitedSummonOn => UnlimitedSummonEnemyGOO
                case UnlimitedSummonEnemyGOO => UnlimitedSummonOff
                case UnlimitedSummonOff => UnlimitedSummonOn
            })

            MainAction(self)

        case ToggleOutOfTurnDevolveAction(self, value) =>
            of(self).ignoreOptionsNew :-= value

            of(self).ignoreOptionsNew :+= (value match {
                case OutOfTurnDevolveOn => OutOfTurnDevolveAvoidCapture
                case OutOfTurnDevolveAvoidCapture => OutOfTurnDevolveOff
                case OutOfTurnDevolveOff => OutOfTurnDevolveOn
            })

            MainAction(self)

        case ToggleOutOfTurnSacrificeHighPriestAction(self, value) =>
            of(self).ignoreOptionsNew :-= value

            of(self).ignoreOptionsNew :+= (value match {
                case OutOfTurnSacrificeHighPriestOn => OutOfTurnSacrificeHighPriestAvoidCapture
                case OutOfTurnSacrificeHighPriestAvoidCapture => OutOfTurnSacrificeHighPriestOff
                case OutOfTurnSacrificeHighPriestOff => OutOfTurnSacrificeHighPriestOn
            })

            MainAction(self)

        // AWAKEN
        case AwakenMainAction(self, uc, locations) if uc == ShubNiggurath =>
            val cultists = board.regions./~(r => of(self).at(r, Cultist).take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).drop(1)./(b => (a.region, b.region))).distinct
            QAsk(pairs./((a, b) => AwakenEliminate2CultistsAction(self, uc, locations, a, b)) :+ MainCancelAction(self))

        case AwakenEliminate2CultistsAction(self, uc, locations, a, b) =>
            val q = locations./(r => AwakenAction(self, uc, r, self.awakenCost(this, uc, r)))
            List(a, b).foreach { r =>
                val c = of(self).at(r, Cultist).head
                log("" + c + " in " + c.region + " was sacrificed")
                eliminate(c)
            }
            QAsk(q)

        case AwakenMainAction(self, uc, locations) =>
            QAsk(locations./(r => AwakenAction(self, uc, r, self.awakenCost(this, uc, r))) :+ MainCancelAction(self))

        case AwakenAction(self, uc, r, cost) =>
            of(self).power -= (if (cost < 0) self.awakenCost(this, uc, r) else cost)

            payTax(self, r)
            place(self, uc, r)

            log("" + self + " awakened " + self.styled(uc) + " in " + r)

            if (of(self).has(Immortal)) {
                log("" + self + " gained " + 1.es + " as " + Immortal.full)
                giveES(self, 1)
            }

            if (uc == Ithaqua) {
                if (gates.contains(r)) {
                    gates = gates.but(r)

                    factions.foreach { f =>
                        of(f).gates = of(f).gates.but(r)
                    }

                    log("" + self + " destroyed gate in " + r)
                }
                else {
                    val u = factions./~(of(_).ugate).%(_.region == r).head

                    eliminate(u)

                    log("" + self + " eliminated " + u + " in " + r)
                }
            }

            if (uc == YogSothoth) {
                val s = of(self).at(r, SpawnOW).head

                eliminate(s)

                log("" + self + " replaced " + s + " in " + r)

                of(self).ugate = of(self).at(r, YogSothoth).single
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
            Ask(self, board.regions./~(r => of(self).at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, DoomCancelAction(self))) :+ DoomCancelAction(self))

        case SacrificeHighPriestMainAction(self, then) =>
            if (of(self).all(HighPriest).any) {
                if (of(self).at(SL.slumber, HighPriest).any) {
                    Ask(self, List(SL.slumber)./~(r => of(self).at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, then)) :+ then)
                }
                else {
                    Ask(self, board.regions./~(r => of(self).at(r, HighPriest))./(c => SacrificeHighPriestAction(self, c.region, then)) :+ then)
                }
            }
            else
                Force(then)

        case SacrificeHighPriestAction(self, r, then) =>
            val c = of(self).at(r, HighPriest).head
            eliminate(c)

            of(self).power += 2

            log("" + self + " sacrificed " + c + " in " + r)

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
            if (of(self).inPool(DeepOne).any)
                Ask(self, board.regions./~(r => of(self).at(r, Acolyte))./(c => DevolveAction(self, c.region, then)) :+ then)
            else
                Force(then)

        case DevolveAction(self, r, then) =>
            if (of(self).at(r, Monster, GOO).none)
                factions.but(self).%(of(_).at(r, Monster, GOO).none).%(of(_).at(r, Cultist).any).foreach { f => of(f).oncePerAction = of(f).oncePerAction.but(Devolve) }

            val c = of(self).at(r, Acolyte).head
            eliminate(c)
            place(self, DeepOne, r)

            log("" + c + " in " + r + " devolved into " + DeepOne)

            checkGatesLost()

            if (Explode.isCancel(then))
                Force(then)
            else
                Force(DevolveMainAction(self, then))

        case DevolveDoneAction(self, then) =>
            of(self).oncePerAction :+= Devolve
            Force(then)

        // DREAMS
        case DreamsMainAction(self, l) =>
            QAsk(l./~(r => factions.but(self)./~(of(_).at(r, Acolyte).take(1)))./(c => DreamsAction(self, c.region, c.faction)) :+ MainCancelAction(self))

        case DreamsAction(self, r, f) =>
            val c = of(f).at(r, Acolyte).head
            of(self).power -= 2
            payTax(self, r)
            log("" + self + " sent dreams to " + c + " in " + c.region + " and replaced it with " + self.styled(Acolyte))
            place(self, Acolyte, c.region)
            eliminate(c)
            EndAction(self)

        // SUBMERGE
        case SubmergeMainAction(self, r) =>
            of(self).power -= 1
            Force(SubmergeAction(self, r, Cthulhu))

        case SubmergeAction(self, r, uc) =>
            move(of(self).at(r, uc).head, GC.deep)
            QAsk(of(self).at(r).exceptByatis.exceptUncontrolledFilth(this)./(u => SubmergeAction(self, r, u.uclass)) :+ SubmergeDoneAction(self, r))

        case SubmergeDoneAction(self, r) =>
            val cthulu = of(self).at(GC.deep, Cthulhu).head
            val court = of(self).at(GC.deep).but(cthulu)
            log("" + cthulu + " submerged in " + r + court.any.??(" with " + court.mkString(", ")))
            EndAction(self)

        case UnsubmergeMainAction(self, l) =>
            QAsk(l./(r => UnsubmergeAction(self, r)) :+ MainCancelAction(self))

        case UnsubmergeAction(self, r) =>
            payTax(self, r)
            val cthulu = of(self).at(GC.deep, Cthulhu).head
            val court = of(self).at(GC.deep).but(cthulu)
            of(self).at(GC.deep).foreach(move(_, r))
            log("" + cthulu + " unsubmerged in " + r + court.any.??(" with " + court.mkString(", ")))
            EndAction(self)

        // CC -- CRAWLING CHAOS

        // PAYXPOWER
        case Pay4PowerMainAction(self) =>
            of(self).power -= 4
            log("" + self + " payed " + 4.power)
            satisfy(self, Pay4Power, "Pay four Power")
            EndAction(self)

        case Pay6PowerMainAction(self) =>
            of(self).power -= 6
            log("" + self + " payed " + 6.power)
            satisfy(self, Pay6Power, "Pay six Power")
            EndAction(self)

        case Pay10PowerMainAction(self) =>
            of(self).power -= 10
            log("" + self + " payed " + 10.power)
            satisfy(self, Pay4Power, "Pay four Power")
            satisfy(self, Pay6Power, "Pay six Power")
            EndAction(self)

        // 1000F
        case ThousandFormsMainAction(self) =>
            of(self).oncePerTurn +:= ThousandForms
            RollD6("Roll for " + ThousandForms.full, x => ThousandFormsRollAction(self, x))

        case ThousandFormsRollAction(f, x) =>
            log("" + f + " used " + ThousandForms.full + " and rolled " + ("[" + x.styled("power") + "]"))
            log("Other factions were to lose " + x.power)
            Force(ThousandFormsAction(f, x))

        case ThousandFormsAction(f, x) =>
            val mp = factions./(of(_).power).max
            val sm = factions.but(f)./(of(_).power).sum

            if (sm < x) {
                log("Not enough power among other factions")
                log("" + f + " got " + x.power)
                of(f).power += x
                EndAction(f)
            }
            else {
                factions.but(f).%(of(_).power == 0).foreach(f => log("" + f + " had no power"))

                val forum = factions.but(f).%(of(_).power > 0)
                Force(ThousandFormsContinueAction(f, x, Nil, forum, forum.num * 3))
            }

        case ThousandFormsContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropRight(1)

            if (offers./(_.n).sum == x) {
                offers.%(_.n > 0).reverse.foreach { o =>
                    of(o.f).power -= o.n

                    log("" + o.f + " lost " + o.n.power)
                }
                EndAction(f)
            }
            else
            if (time <= 0 || xforum./(of(_).power).sum < x) {
                if (factions.but(f).%(of(_).power > 0).num > 1)
                    log("Negotiations failed")

                of(f).power += x

                log("" + f + " got " + x.power)

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
                val maxp = min(of(next).power, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(of(_).power).sum
                val minp = max(1, x - maxother)

                QAsk((-1 +: 0 +: minp.to(maxp).toList)./(n => ThousandFormsAskAction(f, x, offers, forum, time - (random() * 1.0).round.toInt, next, n)))
            }


        case ThousandFormsAskAction(f, x, offers, forum, time, self, n) =>
            if (n < 0) {
                log("" + self + " refused to negotiate")

                Force(ThousandFormsContinueAction(f, x, offers, forum.but(self), time))
            }
            else {
                if (n == 0)
                    log("" + self + " offered no power")
                else
                    log("" + self + " offered to lose " + n.styled("highlight"))

                Force(ThousandFormsContinueAction(f, x, Offer(self, n) +: offers, forum, time))
            }

        // BG -- BLACK GOAT

        // BLOOD SACRIFICE
        case BloodSacrificeDoomAction(self) =>
            QAsk(of(self).all(Cultist)./(c => BloodSacrificeAction(self, c.region, c.uclass)) :+ DoomCancelAction(self))

        case BloodSacrificeAction(self, r, uc) =>
            val c = of(self).at(r, uc).head
            eliminate(c)
            of(self).oncePerTurn :+= BloodSacrifice
            log("" + self + " sacrificed " + c + " in " + r + " for " + 1.es)
            giveES(self, 1)
            checkGatesLost()
            CheckSpellbooksAction(DoomAction(self))

        // ELIMINATE CULTISTS
        case Eliminate2CultistsMainAction(self) =>
            val cultists = board.regions./~(r => of(self).at(r, Cultist).take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).drop(1)./(b => (a.region, b.region))).distinct
            QAsk(pairs./((a, b) => Eliminate2CultistsAction(self, a, b)) :+ MainCancelAction(self))

        case Eliminate2CultistsAction(self, a, b) =>
            List(a, b).foreach { r =>
                val c = of(self).at(r, Cultist).head
                log("" + c + " in " + c.region + " was sacrificed")
                eliminate(c)
            }
            satisfy(self, Eliminate2Cultists, "Eliminate two Cultists")
            EndAction(self)

        // AVATAR
        case AvatarMainAction(self, o, l) =>
            val variants = l.flatMap { r =>
                val factionTargets = factions.filter(f => of(f).at(r, Cultist, Monster).nonEmpty).map(f =>
                    AvatarAction(self, o, r, f)
                )

                val filthTargets = factions.flatMap { f =>
                    if (of(f).at(r).uncontrolledFilthOnly(this).nonEmpty)
                        Some(AvatarUncontrolledFilthAction(self, o, r, f))
                    else None
                }

                factionTargets ++ filthTargets
            }

            QAsk(variants :+ MainCancelAction(self))

        case AvatarAction(self, o, r, f) =>
            of(self).power -= 1
            payTax(self, r)
            val sn = of(self).goo(ShubNiggurath)
            move(sn, r)
            log("" + sn + " avatared to " + r)
            payTax(self, o)
            val units = of(f).at(r, Cultist, Monster).exceptUncontrolledFilth(this)
            QAsk(units.map(_.uclass).map(AvatarReplacementAction(f, self, r, o, _)))

        case AvatarUncontrolledFilthAction(self, o, r, owner) =>
            of(self).power -= 1
            payTax(self, r)
            val sn = of(self).goo(ShubNiggurath)
            move(sn, r)
            log("" + sn + " avatared to " + r)

            of(owner).at(r).uncontrolledFilthOnly(this).headOption match {
                case Some(u) =>
                    log("" + u + " was sent back to " + o)
                    move(u, o)
                    EndAction(self)
                case None =>
                    EndAction(self)
            }

        case AvatarReplacementAction(self, f, r, o, uc) =>
            val u = of(self).at(r, uc).head
            log("" + u + " was sent back to " + o)
            move(u, o)
            EndAction(f)

        // GHROTH
        case GhrothMainAction(self) =>
            of(self).power -= 2
            RollD6("Roll for " + self.styled(Ghroth), x => GhrothRollAction(self, x))

        case GhrothRollAction(f, x) =>
            var b = of(f).all(Fungi)./(_.region).distinct.num

            if (x > b) {
                log("" + f + " failed " + Ghroth.full + " with roll of " + ("[" + x.styled("power") + "]"))

                val fs = factions.%(of(_).inPool(Acolyte).any)
                if (fs.any)
                    QAsk(fs./(GhrothFactionAction(f, _)))
                else {
                    log("No Cultists were available to place")
                    EndAction(f)
                }
            }
            else {
                log("" + f + " used " + Ghroth.full + " and rolled " + ("[" + x.styled("power") + "]"))

                val n = factions.but(f)./~(of(_).onMap(Cultist)).num
                if (n <= x) {
                    if (n < x)
                        log("Not enough Cultists among other factions")

                    factions.but(f)./~(of(_).onMap(Cultist)).foreach { c =>
                        log("" + c + " was eliminated in " + c.region)
                        eliminate(c)
                    }

                    EndAction(f)
                }
                else {
                    Force(GhrothAction(f, x))
                }
            }

        case GhrothAction(f, x) =>
            factions.but(f).%(of(_).onMap(Cultist).none).foreach(f => log("" + f + " had no Cultists"))

            val forum = factions.but(f).%(of(_).onMap(Cultist).any)
            Force(GhrothContinueAction(f, x, Nil, forum, forum.num * 3))

        case GhrothContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropRight(1)

            if (offers./(_.n).sum == x && offers.num == xforum.num) {
                Force(GhrothEliminateAction(f, offers./~(o => List.fill(o.n)(o.f))))
            }
            else
            if (time < 0 || xforum./(of(_).onMap(Cultist).num).sum < x) {
                log("" + f + " eliminated " + x + " Cultist" + (x > 1).??("s"))

                val affected = factions.but(f).%(of(_).onMap(Cultist).any)
                val split = 1.to(x).toList./~(n => affected.combinations(n))
                val valid = split.%(_./(of(_).onMap(Cultist).num).sum >= x)

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
                val maxp = min(of(next).onMap(Cultist).num, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(of(_).onMap(Cultist).num).sum
                val minp = max(1, x - maxother)

                QAsk((-1 +: 0 +: minp.to(maxp).toList)./(n => GhrothAskAction(f, x, offers, forum, time - (random() * 1.0).round.toInt, next, n)))
            }


        case GhrothAskAction(f, x, offers, forum, time, self, n) =>
            if (n < 0) {
                log("" + self + " refused to negotiate")

                Force(GhrothContinueAction(f, x, offers, forum.but(self), time))
            }
            else {
                if (n == 0)
                    log("" + self + " offered no Cultists")
                else
                    log("" + self + " offered to lose " + n.styled("highlight"))

                Force(GhrothContinueAction(f, x, Offer(self, n) +: offers, forum, time))
            }

        case GhrothSplitAction(self, x, ff) =>
            val split = ff./~(f => (x - ff.num).times(f)).combinations(x - ff.num).toList./(_ ++ ff)./(l => ff./~(f => l.count(f).times(f)))
            val valid = split.%(s => ff.%(f => of(f).onMap(Cultist).num < s.%(_ == f).num).none)
            QAsk(valid./(l => GhrothSplitNumAction(self, x, ff, l)))

        case GhrothSplitNumAction(self, x, ff, full) =>
            Force(GhrothEliminateAction(self, full))

        case GhrothEliminateAction(f, full) =>
            if (full.none)
                EndAction(f)
            else {
                val next = full.head
                val cultists = board.regions./~(r => of(next).at(r, Cultist))
                // If we want to allow SL to eliminate a cultist in Cursed Slumber (which you should be able to do, according to the FAQ).
                // val cultists = {
                //     val base = board.regions./~(r => of(next).at(r, Cultist))
                //     val slumberCultists = of(next).at(SL.slumber, Cultist)
                //     if (slumberCultists.nonEmpty) {
                //         base ++ slumberCultists
                //     } else base
                // }
                QAsk(cultists./(c => GhrothUnitAction(next, c.uclass, c.region, f, full.drop(1))))
            }

        case GhrothUnitAction(self, uc, r, f, full) =>
            val c = of(self).at(r, uc).head
            log("" + c + " was eliminated in " + c.region)
            eliminate(c)
            Force(GhrothEliminateAction(f, full))

        case GhrothFactionAction(self, f) =>
            QAsk(board.regions./(r => GhrothPlaceAction(self, f, r)))

        case GhrothPlaceAction(self, f, r) =>
            log(f.styled(Acolyte) + " was placed in " + r)
            place(f, Acolyte, r)
            EndAction(self)

        // YS -- YELLOW SIGN

        // PROVIDE 3 DOOM
        case Provide3DoomMainAction(self) =>
            QAsk(factions.but(self)./(f => Provide3DoomAction(self, f)) :+ MainCancelAction(self))

        case Provide3DoomAction(self, f) =>
            of(f).doom += 3
            log("" + self + " supplied " + f + " with " + 3.doom)
            satisfy(self, Provide3Doom, "Provide 3 Doom")
            EndAction(self)

        // DESECRATE
        case DesecrateMainAction(self, r, te) =>
            of(self).power -= te.?(1).|(2)
            payTax(self, r)
            RollD6("Roll for " + self.styled(Desecrate) + " in " + r, x => DesecrateRollAction(self, r, te, x))

        case DesecrateRollAction(self, r, te, x) =>
            if (of(self).at(r).exceptUncontrolledFilth(this).num >= x) {
                log(self.styled(KingInYellow) + " desecrated " + r + " with roll [" + x.styled("power") + "]")
                if (te) {
                    log("" + self + " gained " + 1.es + " using " + ThirdEye.full)
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
                log(self.styled(KingInYellow) + " failed " + r + " desecration with roll [" + x.styled("power") + "]")

            val us = (of(self).inPool(Cultist) ++ of(self).inPool(Monster).exceptUncontrolledFilth(this))./( _.uclass ).filter( _.cost <= 2 ).distinct

            if (us.any)
                QAsk(us./(DesecratePlaceAction(self, r, _)))
            else
                EndAction(self)

        case DesecratePlaceAction(self, r, uc) =>
            place(self, uc, r)
            log(self.styled(uc) + " appeared in " + r)
            EndAction(self)

        // HWINTBN
        case HWINTBNMainAction(self, o, l) =>
            QAsk(l./(HWINTBNAction(self, o, _)) :+ MainCancelAction(self))

        case HWINTBNAction(self, o, r) =>
            of(self).power -= 1
            payTax(self, r)
            move(of(self).at(o, Hastur).head, r)
            of(self).oncePerRound :+= HWINTBN

            log("" + Hastur + " heard his name in " + r)

            AfterAction(self)

        // SCREAMING DEAD
        case ScreamingDeadMainAction(self, o, l) =>
            QAsk(l./(ScreamingDeadAction(self, o, _)) :+ MainCancelAction(self))

        case ScreamingDeadAction(self, o, r) =>
            of(self).power -= 1
            payTax(self, r)
            Force(ScreamingDeadFollowAction(self, o, r, KingInYellow))

        case ScreamingDeadFollowAction(self, o, r, uc) =>
            val u = of(self).at(o, uc).head
            move(u, r)
            if (uc == KingInYellow)
                log("" + KingInYellow + " screamed from " + o + " to " + r)
            else
                log("" + u + " followed along")
            QAsk(of(self).at(o, Undead)./(_.uclass)./(ScreamingDeadFollowAction(self, o, r, _)) :+ ScreamingDeadDoneAction(self))

        case ScreamingDeadDoneAction(self) =>
            of(self).oncePerRound :+= ScreamingDead

            AfterAction(self)

        // SHRIEK
        case ShriekMainAction(self, l) =>
            QAsk(l./(ShriekAction(self, _)) :+ MainCancelAction(self))

        case ShriekAction(self, r) =>
            val b = of(self).all(Byakhee)./(_.region).but(r)
            if (b.none)
                EndAction(self)
            else
                QAsk(b./(ShriekFromAction(self, _, r)) :+ of(self).oncePerAction.contains(Shriek).?(ShriekDoneAction(self)).|(MainCancelAction(self)))

        case ShriekFromAction(self, o, r) =>
            val u = of(self).at(o, Byakhee).head
            if (!of(self).oncePerAction.contains(Shriek)) {
                of(self).power -= 1
                payTax(self, r)
                of(self).oncePerAction :+= Shriek
            }
            move(u, r)
            log("" + u + " flew to " + r + " from " + o)
            Force(ShriekAction(self, r))

        case ShriekDoneAction(self) =>
            EndAction(self)

        // ZINGAYA
        case ZingayaMainAction(self, l) =>
            QAsk(l./~(r => factions.but(self)./~(f => of(f).at(r, Acolyte).take(1)))./(u => ZingayaAction(self, u.region, u.faction)) :+ MainCancelAction(self))

        case ZingayaAction(self, r, f) =>
            val c = of(f).at(r, Acolyte).head
            of(self).power -= 1
            payTax(self, r)
            eliminate(c)
            place(self, Undead, r)
            log("" + self + " replaced " + c + " in " + r + " with " + Undead)
            EndAction(self)

        // SL - SLEEPER

        // DEATH FROM BELOW
        case DeathFromBelowDoomAction(self) =>
            val unitClasses = of(self).inPool(Monster)./(_.uclass).exceptUncontrolledFilth(this)

            val minCost = unitClasses.map(_.cost).min
            val ucs = unitClasses.filter(_.cost == minCost).distinct

            if (ucs.num == 1) {
                QAsk(board.regions.%(r => of(self).at(r).any).some.|(board.regions)./(r => DeathFromBelowAction(self, r, ucs.head)) :+ DoomCancelAction(self))
            }
            else {
                QAsk(ucs./(uc => DeathFromBelowSelectMonsterAction(self, uc)) :+ DoomCancelAction(self))
            }

        case DeathFromBelowSelectMonsterAction(self, uc) =>
            QAsk(board.regions.%(r => of(self).at(r).any).some.|(board.regions)./(r => DeathFromBelowAction(self, r, uc)) :+ DoomCancelAction(self))

        case DeathFromBelowAction(self, r, uc) =>
            place(self, uc, r)
            log("" + self + " placed " + uc + " in " + r + " with " + DeathFromBelow.full)
            of(self).oncePerTurn :+= DeathFromBelow
            CheckSpellbooksAction(DoomAction(self))

        // LETHARGY
        case LethargyMainAction(self) =>
            if (options.has(IceAgeAffectsLethargy))
                payTax(self, of(self).goo(Tsathoggua).region)

            log("" + self + " was sleeping")
            battled = board.regions
            EndAction(self)

        // PAY 3 POWER
        case Pay3SomeoneGains3MainAction(self) =>
            QAsk(factions.but(self)./(Pay3SomeoneGains3Action(self, _)) :+ MainCancelAction(self))

        case Pay3SomeoneGains3Action(self, f) =>
            of(self).power -= 3
            of(f).power += 3
            log("" + self + " spent " + 3.power + " and " + f + " gained " + 3.power)
            satisfy(self, Pay3SomeoneGains3, "Provide 3 Power")
            EndAction(self)

        case Pay3EverybodyLoses1MainAction(self) =>
            of(self).power -= 3
            factions.but(self).%(f => of(f).power > 0).foreach(f => of(f).power -= 1)
            log("" + self + " spent " + 3.power + " and each other faction lost " + 1.power)
            satisfy(self, Pay3EverybodyLoses1, "Everybody loses 1 power")
            EndAction(self)

        case Pay3EverybodyGains1MainAction(self) =>
            of(self).power -= 3
            factions.but(self).foreach(f => of(f).power += 1)
            log("" + self + " spent " + 3.power + " and each other faction gained " + 1.power)
            satisfy(self, Pay3EverybodyGains1, "Everybody gains 1 power")
            EndAction(self)

        // CAPTURE MONSTER
        case CaptureMonsterMainAction(self) =>
            val variants = board.regions./~ { r =>
                val hasTsathoggua = of(self).at(r, Tsathoggua).nonEmpty
                if (!hasTsathoggua) Nil
                else {
                    // Normal monster targets.
                    val normalTargets = factions
                        .filter(f => f != self)
                        .filter(f => of(f).at(r, Monster).exceptUncontrolledFilth(this).nonEmpty && of(f).at(r, GOO).isEmpty)
                        .map(f => CaptureMonsterAction(self, r, f))

                    // Uncontrolled Filth.
                    val filthTargets = factions.flatMap { f =>
                        if (of(f).at(r).uncontrolledFilthOnly(this).nonEmpty)
                            Some(CaptureUncontrolledFilthAction(self, r, f))
                        else None
                    }

                    normalTargets ++ filthTargets
                }
            }

            QAsk(variants :+ MainCancelAction(self))

        case CaptureMonsterAction(self, r, f) =>
            of(self).power -= 1
            val monsters = of(f).at(r, Monster).exceptUncontrolledFilth(this)
            Ask(f, monsters.sortBy(_.uclass.cost).map(u => CaptureMonsterUnitAction(self, r, u.faction, u.uclass)))

        case CaptureUncontrolledFilthAction(self, r, owner) =>
            of(self).power -= 1
            val filth = of(owner).at(r).uncontrolledFilthOnly(this)
            filth.headOption match {
                case Some(u) =>
                    capture(self, u)
                    log("" + self + " captured " + u + " in " + r)
                    EndAction(self)
                case None =>
                    EndAction(self)
            }

        case CaptureMonsterUnitAction(self, r, f, uc) =>
            val m = of(f).at(r, uc).head
            capture(self, m)
            log("" + self + " captured " + m + " in " + r)
            EndAction(self)

        // ANCIENT SORCERY
        case AncientSorceryMainAction(self) =>
            QAsk(factions.but(self)./(_.abilities.head).diff(of(self).borrowed)./(AncientSorceryAction(self, _)) :+ MainCancelAction(self))

        case AncientSorceryAction(self, a) =>
            QAsk(of(self).onMap(SerpentMan).%(nx)./(u => AncientSorceryUnitAction(self, a, u.region, u.uclass)) :+ MainCancelAction(self))

        case AncientSorceryUnitAction(self, a, r, uc) =>
            of(self).power -= 1
            move(of(self).at(r, uc).head, SL.slumber)
            of(self).borrowed :+= a
            log("" + self + " sent " + uc + " from " + r + " to access " + a.full)
            EndAction(self)

        case AncientSorceryDoomAction(self) =>
            QAsk(board.regions./(r => AncientSorceryPlaceAction(self, r, SerpentMan)) :+ DoomCancelAction(self))

        case AncientSorceryPlaceAction(self, r, uc) =>
            move(of(self).at(SL.slumber, uc).head, r)
            of(self).power += 1
            log("" + self + " placed " + uc + " in " + r + " with " + AncientSorcery.full + " and gained " + 1.power)
            CheckSpellbooksAction(DoomAction(self))

        // CURSED SLUMBER
        case CursedSlumberSaveMainAction(self) =>
            QAsk(of(self).gates.%(nx)./(CursedSlumberSaveAction(self, _)) :+ MainCancelAction(self))

        case CursedSlumberSaveAction(self, r) =>
            of(self).power -= 1
            of(self).gates = of(self).gates.but(r) :+ SL.slumber
            gates = gates.but(r) :+ SL.slumber
            move(of(self).at(r, Cultist).head, SL.slumber)
            log("" + self + " moved gate from " + r + " to " + CursedSlumber.full)
            EndAction(self)

        case CursedSlumberLoadMainAction(self, l) =>
            QAsk(l./(CursedSlumberLoadAction(self, _)) :+ MainCancelAction(self))

        case CursedSlumberLoadAction(self, r) =>
            of(self).power -= 1
            payTax(self, r)
            of(self).gates = of(self).gates.but(SL.slumber) :+ r
            gates = gates.but(SL.slumber) :+ r

            if (of(self).at(SL.slumber, Cultist).any)
                move(of(self).at(SL.slumber, Cultist).head, r)

            log("" + self + " moved gate from " + CursedSlumber.full + " to " + r)
            EndAction(self)

        // WW - WINDWALKER

        // HIBERNATE
        case HibernateMainAction(self, n) =>
            of(self).power += n
            of(self).hibernating = true
            log("" + self + " hibernated" + (n != 0).??(" for extra " + n.power))
            battled = board.regions
            EndAction(self)

        // ICE AGE
        case IceAgeMainAction(self, l) =>
            QAsk(l./(r => IceAgeAction(self, r)) :+ MainCancelAction(self))

        case IceAgeAction(self, r) =>
            of(self).power -= 1
            of(self).iceage = Some(r)
            anyia = true
            log("" + self + " started " + self.styled(IceAge) + " in " + r)
            EndAction(self)

        // ARCTIC WIND
        case ArcticWindAction(self, o, uc, r) =>
            val u = of(self).at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)
            log("" + u + " followed with " + ArcticWind.full)
            QAsk(of(self).at(o).%(!_.has(Moved)).exceptByatis.exceptUncontrolledFilth(this)./(u => ArcticWindAction(self, o, u.uclass, r)) :+ ArcticWindDoneAction(self))

        case ArcticWindDoneAction(self) =>
            MoveContinueAction(self, true)

        // ANYTIME
        case AnytimeGainElderSignsMainAction(self) =>
            QAsk(AnytimeGainElderSignsAction(self, min(3, factions.but(self).%(of(_).hasAllSB).num), MainAction(self)) :: MainCancelAction(self))

        case AnytimeGainElderSignsDoomAction(self) =>
            QAsk(AnytimeGainElderSignsAction(self, min(3, factions.but(self).%(of(_).hasAllSB).num), DoomAction(self)) :: DoomCancelAction(self))

        case AnytimeGainElderSignsAction(self, n, next) =>
            satisfy(self, AnytimeGainElderSigns, "Anytime Spellbook", n)
            CheckSpellbooksAction(next)

        // OW - OPENER OF THE WAY

        // BEYOND ONE
        case BeyondOneMainAction(self, l) =>
            QAsk(l./~(r => of(self).at(r).%(_.uclass.cost >= 3)).exceptByatis./(u => BeyondOneUnitAction(self, u.region, u.uclass)) :+ MainCancelAction(self))

        case BeyondOneUnitAction(self, o, uc) =>
            QAsk(board.regions.diff(gates).%(affordF(self, 1))./(BeyondOneAction(self, o, uc, _)) :+ MainCancelAction(self))

        case BeyondOneAction(self, o, uc, r) =>
            of(self).power -= 1
            payTax(self, r)
            gates = gates.but(o) :+ r
            factions.%(of(_).gates.contains(o)).foreach { f =>
                of(f).gates = of(f).gates.but(o) :+ r
                move(of(f).at(o).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && of(f).has(RedSign))).head, r)
            }
            move(of(self).at(o, uc).head, r)
            log("" + self + " moved gate with " + self.styled(uc) + " from " + o + " to " + r)
            EndAction(self)

        // DREAD CURSE
        case DreadCurseMainAction(self, n, l) =>
            QAsk(l./(DreadCurseAction(self, n, _)) :+ MainCancelAction(self))

        case DreadCurseAction(self, n, r) =>
            of(self).power -= 2
            payTax(self, r)
            log("" + self + " sent " + self.styled(DreadCurse) + " to " + r)
            RollBattle(self, self.styled(DreadCurse), n, x => DreadCurseRollAction(self, r, x))

        case DreadCurseRollAction(self, r, x) =>
            log("" + self + " rolled " + x.mkString(" "))
            var k = x.count(Kill)
            var p = x.count(Pain)
            if (k + p == 0)
                EndAction(self)
            else {
                val e = factions.but(self).%(f => of(f).at(r).exceptUncontrolledFilth(this).nonEmpty).sortBy(-of(_).at(r).sortBy(_.uclass.cost).take(k)./(_.uclass.cost).sum)

                val kva = e./~(f => List.fill(k)(f)).combinations(k).toList.sortBy(_.distinct.num)
                val pva = e./~(f => List.fill(p)(f)).combinations(p).toList.sortBy(_.distinct.num)
                val kpva = kva./~(kk => pva./(pp => (kk, pp))).sortBy(v => 100 * v._1.distinct.num + 10 * v._2.distinct.num + (v._1 ++ v._2).distinct.num)

                val n = e./(of(_).at(r).num).sum

                while (n < k + p && p > 0)
                    p -= 1
                while (n < k && k > 0)
                    k -= 1

                val kvb = e./~(f => List.fill(min(k, of(f).at(r).num))(f)).combinations(k).toList
                val pvb = e./~(f => List.fill(min(p, of(f).at(r).num))(f)).combinations(p).toList
                val kpvb = kvb./~(kk => pvb./(pp => (kk, pp))).%((a, b) => e.%(f => of(f).at(r).num < (a ++ b).count(f)).none)

                QAsk(kpvb./((a, b) => DreadCurseSplitAction(self, r, x, e.%(f => a.contains(f) || b.contains(f)), a, b)))
            }

        case DreadCurseSplitAction(self, r, x, e, k, p) =>
            if (x.any && e.num > 1) {
                e.foreach { f =>
                    log("" + f + " recieved " + (List.fill(k.count(f))(Kill) ++ List.fill(p.count(f))(Pain)).mkString(" "))
                }
            }

            val ee = e.%(f => of(f).at(r).%(_.health == Killed).num < k.count(f) || of(f).at(r).%(_.health == Pained).num < p.count(f))

            val killall = ee.%(f => of(f).at(r).num == k.count(f))

            killall.foreach(f => of(f).at(r).foreach(_.health = Killed))

            val painall = ee.%(f => of(f).at(r).num == p.count(f))

            painall.foreach(f => of(f).at(r).foreach(_.health = Pained))

            val aa = ee.diff(killall).diff(painall)

            if (aa.any) {
                val f = aa(0)
                val rs = List.fill(k.count(f) - of(f).at(r).%(_.health == Killed).num)(Kill) ++ List.fill(p.count(f) - of(f).at(r).%(_.health == Pained).num)(Pain)
                val us = of(f).at(r).exceptUncontrolledFilth(this).%(_.health == Alive)./(_.uclass).sortBy(_.cost)
                val uu = (us.num > 1).?(us).|(us.take(1))
                QAsk(uu./(u => DreadCurseAssignAction(self, r, e, k, p, f, rs.head, u)))
            }
            else {
                e.foreach { f =>
                    of(f).at(r).%(_.health == Killed).foreach { u =>
                        log("" + u + " was " + "killed".styled("kill"))
                        eliminate(u)
                    }
                }

                var m = e./~(f => of(f).at(r).%(_.health == Pained))

                m = m.take(1)

                if (m.any)
                    QAsk(m./(u => DreadCurseRetreatAction(self, r, e, u.faction, u.uclass)))
                else
                    EndAction(self)
            }

        case DreadCurseAssignAction(f, r, e, k, p, self, s, uc) =>
            val u = of(self).at(r, uc).exceptUncontrolledFilth(this).%(_.health == Alive).head
            u.health = if (s == Kill) Killed else Pained
            QAsk(List(DreadCurseSplitAction(f, r, Nil, e, k, p)))

        case DreadCurseRetreatAction(self, r, e, f, uc) =>
            QAsk(board.connected(r)./(d => DreadCurseRetreatToAction(self, r, e, f, uc, d)))

        case DreadCurseRetreatToAction(self, r, e, f, uc, d) =>
            val u = of(f).at(r, uc).%(_.health == Pained).head
            move(u, d)
            u.health = Alive
            log("" + u + " was " + "pained".styled("pain") + " to " + d)

            var m = e./~(f => of(f).at(r).%(_.health == Pained))

            m = m.take(1)

            if (m.any)
                QAsk(m./(u => DreadCurseRetreatAction(self, r, e, u.faction, u.uclass)))
            else
                EndAction(self)

        // DRAGON DESCENDING
        case DragonDescendingDoomAction(self, cost) =>
            of(self).oncePerGame :+= DragonDescending
            log("" + self + " used " + DragonDescending.full)
            Force(RitualAction(self, cost, 2))

        // DRAGON ASCENDING
        case DragonAscendingMainAction(self) =>
            DragonAscendingAction(self, Some(self), "own action", factions./(of(_).power).max, MainAction(self)) :: MainCancelAction(self)

        case DragonAscendingDoomAction(self) =>
            DragonAscendingAction(self, Some(self), "own " + "Doom".styled("doom") + " action", factions./(of(_).power).max, DoomAction(self)) :: DoomCancelAction(self)

        case DragonAscendingAskAction(self, f, reason, then) =>
            DragonAscendingAction(self, f, reason, factions./(of(_).power).max, then) :: DragonAscendingCancelAction(self, then) :: DragonAscendingNotThisTurnAction(self, then)

        case DragonAscendingAction(self, _, _, p, then) =>
            of(self).power = p
            of(self).oncePerGame :+= DragonAscending

            factions.foreach(of(_).ignorePerInstant = Nil)

            log("" + self + " used " + DragonAscending.full + " and rose to " + p.power)

            Force(then)

        case DragonAscendingCancelAction(self, then) =>
            of(self).ignorePerInstant :+= DragonAscending
            Force(then)

        case DragonAscendingNotThisTurnAction(self, then) =>
            of(self).ignorePerTurn :+= DragonAscending
            Force(then)

        case DragonAscendingInstantAction(then) =>
            factions.foreach(f => of(f).ignorePerInstant = of(f).ignorePerInstant.but(DragonAscending))
            Force(then)

        case DragonAscendingUpAction(reason, then) =>
            val daf = factions.%(of(_).power < factions./(of(_).power).max).%(of(_).want(DragonAscending))

            if (daf.none) {
                Force(then)
            }
            else {
                val self = daf(0)
                DragonAscendingAskAction(self, None, reason, DragonAscendingUpAction(reason, then))
            }

        case DragonAscendingDownAction(f, reason, then) =>
            val daf = targetDragonAscending(f)

            if (daf.none || (of(f).hibernating && then == MainAction(f))) {
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
            of(self).power -= getCathedralCost(r)
            payTax(self, r)
            cathedrals :+= r
            of(self).cathedrals :+= r
            log("" + self + " built a cathedral in " + r)
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
            log("" + self + " allowed enemy factions to summon their lowest cost monster for free")
            val forum = factions.but(self)
            Force(GiveWorstMonsterContinueAction(self, forum))

        case GiveWorstMonsterContinueAction(self, xforum) => {
            if (xforum.num == 0) {
                EndAction(self)
            }
            else {
                val f = xforum.head
                val forum = xforum.drop(1)
                val validPool = of(f).inPool(Monster).exceptUncontrolledFilth(this)

                if (!validPool.any) {
                    log("" + f + " didn't have any monsters in the pool")
                    Force(GiveWorstMonsterContinueAction(f, forum))
                }
                else if (!getControlledGatesRegions(f).any) {
                    log("" + f + " had no way of summoning monsters")
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
            //payTax(self, r) // Not sure if Ice Age affects this
            log("" + self + " summoned " + uc + " in " + r + " for free")
            Force(GiveWorstMonsterContinueAction(f, forum))


        // GIVE BEST MONSTER
        case GiveBestMonsterMainAction(self) =>
            satisfy(self, GiveBestMonster, "Enemies got highest cost monster")
            log("" + self + " allowed enemy factions to summon their highest cost monster for free")
            val forum = factions.but(self)
            Force(GiveBestMonsterContinueAction(self, forum))

        case GiveBestMonsterContinueAction(self, xforum) => {
            if (xforum.num == 0) {
                EndAction(self)
            }
            else {
                val f = xforum.head
                val forum = xforum.drop(1)
                val validPool = of(f).inPool(Monster).exceptUncontrolledFilth(this)

                if (!validPool.any) {
                    log("" + f + " didn't have any monsters in the pool")
                    Force(GiveBestMonsterContinueAction(f, forum))
                }
                else if (!getControlledGatesRegions(f).any) {
                    log("" + f + " had no way of summoning monsters")
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
            //payTax(self, r) // Not sure if Ice Age affects this
            log("" + self + " summoned " + uc + " in " + r + " for free")
            Force(GiveBestMonsterContinueAction(f, forum))


        // SUMMONING UN-MAN WITH FESTIVAL
        case FestivalUnManSummonAction(self, f) =>
            of(f).power += 1
            log("" + f + " got " + 1.power + " from " + self.styled(Festival))
            EndAction(self)


        // DEMATERIALIZATION
        case DematerializationDoomAction(self) =>
            QAsk(board.regions.%(r => of(self).at(r).exceptUncontrolledFilth(this).any)./(r => DematerializationFromRegionAction(self, r)):+ DoomCancelAction(self))

        case DematerializationFromRegionAction(self, o) =>
            QAsk(board.regions.but(o)./(r => DematerializationToRegionAction(self, o, r)) :+ DoomCancelAction(self))

        case DematerializationToRegionAction(self, o, d) =>
            QAsk(of(self).at(o).exceptByatis.exceptUncontrolledFilth(this)./(u => DematerializationMoveUnitAction(self, o, d, u.uclass)) :+ DematerializationDoneAction(self))

        case DematerializationMoveUnitAction(self, o, d, uc) =>
            val u = of(self).at(o, uc).head
            move(u, d)
            log("" + self + " sent " + self.styled(uc) + " from " + o + " to " + d + " with " + Dematerialization.full)
            QAsk(of(self).at(o).exceptByatis.exceptUncontrolledFilth(this)./(u => DematerializationMoveUnitAction(self, o, d, u.uclass)) :+ DematerializationDoneAction(self))

        case DematerializationDoneAction(self) =>
            of(self).oncePerTurn :+= Dematerialization
            demCaseMap = demCaseMap.keys.map(key => key -> 0).toMap
            CheckSpellbooksAction(DoomAction(self))


        // NEUTRAL MONSTERS

        case ShantakCarryCultistAction(self, o, uc, r) =>
            val u = of(self).at(o, uc).%(!_.has(Moved)).head
            move(u, r)

            u.add(Moved)
            u.add(MovedForFree)

            log("" + self.styled(Shantak) + " carried " + u + " to " + r)
            MoveContinueAction(self, true)

        case ShantakCarryCultistCancelAction(self) =>
            MoveContinueAction(self, true)

        // INDEPENDENT GREAT OLD ONES

        case GodOfForgetfulnessMainAction(self, d, l) =>
            Ask(self, l./(r => GodOfForgetfulnessAction(self, d, r)) :+ MainCancelAction(self))

        case GodOfForgetfulnessAction(self, d, r) =>
            of(self).power -= 1
            payTax(self, d)

            factions.but(self).foreach { f =>
                of(f).at(r, Cultist).foreach { u =>
                    move(u, d)
                }
            }
            log("" + self.styled(Byatis) + " used " + GodOfForgetfulness.name.styled("nt") + " to move all enemy cultist from " + r + " to " + d)
            EndAction(self)

        case FilthMainAction(self, l) =>
            Ask(self, l./(r => FilthAction(self, r)) :+ MainCancelAction(self))

        case FilthAction(self, r) =>
            of(self).power -= 1
            payTax(self, r)

            place(self, Filth, r)
            log("" + self.styled(Abhoth) + " placed " + self.styled(Filth) + " in " + r)

            if (of(self).has(Fertility) && !of(self).ignored(Fertility)) {
                of(self).oncePerRound :+= Fertility
                checkGatesOwnership(self)
                CheckSpellbooksAction(MainAction(self))
            }
            else
                EndAction(self)

        case FromBelowMoveSelectAction(self, uc, region) =>
            var destinations = board.connected(region)
            if (of(self).has(Flight))
                destinations = destinations ++ destinations.flatMap(board.connected).distinct.but(region).diff(destinations)

            val arriving = of(self).units.%(_.region.glyph.onMap).%(_.has(Moved))./(_.region).distinct
            destinations = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            destinations = destinations.%(affordF(self, 0))

            val options = destinations./(d => FromBelowMoveAction(self, uc, region, d))
            QAsk(options :+ FromBelowMoveDoneAction(self))

        case FromBelowMoveAction(self, uc, o, r) =>
            payTax(self, r)
            val u = of(self).at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)
            u.add(MovedForFree)
            log("" + self + " moved " + self.styled(uc) + " from " + o + " to " + r + " with " + FromBelow.full)
            Force(MoveContinueAction(self, true))

        case FromBelowMoveDoneAction(self) =>
            Force(MoveContinueAction(self, true))

        case FromBelowAttackAction(self, r, f) =>
            payTax(self, r)
            log("" + self + " battled " + f + " in " + r)
            if (!nyogthaPairByFaction.contains(self)) {
                val firstRegion = lastBattleRegionByFaction.getOrElse(self, r)
                nyogthaPairByFaction   += self -> Set(firstRegion, r)
                nyogthaPairProgress    += self -> 0
                nyogthaPairHadEnemyGOO += self -> false
                nyogthaPairNyogthaDied += self -> false
            }
            lastBattleRegionByFaction += self -> r
            battle = new Battle(this, r, self, f, log)
            battle.proceed()

        case FromBelowAttackUncontrolledFilthAction(self, r, f) =>
            payTax(self, r)
            log("" + self + " battled " + "Uncontrolled".styled("nt") + " " + Filth.name.styled("nt") + " in " + r)

            if (!nyogthaPairByFaction.contains(self)) {
                val firstRegion = lastBattleRegionByFaction.getOrElse(self, r)
                nyogthaPairByFaction   += self -> Set(firstRegion, r)
                nyogthaPairProgress    += self -> 0
                nyogthaPairHadEnemyGOO += self -> false
                nyogthaPairNyogthaDied += self -> false
            }
            lastBattleRegionByFaction += self -> r

            battle = new Battle(this, r, self, f, log, uncontrolledDefender = Filth.name)
            battle.proceed()

        case FromBelowAttackDoneAction(self) =>
            Force(EndAction(self))

        case FromBelowCaptureAction(self, r, f, uc) =>
            payTax(self, r)
            val c = of(f).at(r, Cultist).minBy(_.uclass.cost)
            capture(self, c)
            log("" + self + " captured " + c + " in " + r)
            satisfy(self, CaptureCultist, "Capture Cultist")
            Force(EndAction(self))

        case FromBelowCaptureDoneAction(self) =>
            Force(EndAction(self))

        case NightmareWebMainAction(self, regions) =>
            QAsk(regions.map(r => NightmareWebAction(self, r)) :+ MainCancelAction(self))

        case NightmareWebAction(self, r) =>
            of(self).power -= 2
            payTax(self, r)

            val ny = of(self).inPool(Nyogtha).head
            ny.region = r
            log("" + self + " awakened " + self.styled(Nyogtha) + " in " + r + " with " + self.styled(NightmareWeb))

            EndAction(self)


        // NEUTRAL SPELLBOOKS

        // MAO CEREMONY
        case MaoCeremonyAction(self, r, uc) =>
            val c = of(self).at(r, uc).head
            eliminate(c)
            of(self).power += 1
            log("" + self + " sacrificed " + c + " in " + r + " for " + 1.power)

            checkPowerReached()

            checkGatesLost()

            AfterPowerGatherAction

        case MaoCeremonyDoneAction(self) =>
            of(self).ignorePerInstant :+= MaoCeremony

            AfterPowerGatherAction

        // RECRIMINATIONS
        case RecriminationsMainAction(self) =>
            nonIGOO(of(self).spellbooks)./(RecriminationsAction(self, _))

        case RecriminationsAction(self, sb) =>
            of(self).power -= 1
            of(self).spellbooks = of(self).spellbooks.but(sb)

            if (sb.isInstanceOf[NeutralSpellbook])
                neutralSpellbooks :+= sb

            log("" + self + " discarded " + sb.full)

            of(self).ignorePerInstant :+= sb

            EndAction(self)

        // UNDIMENSIONED
        case UndimensionedMainAction(self) =>
            UndimensionedContinueAction(self, of(self).units.exceptUncontrolledFilth(this).%(_.region.glyph.onMap)./( _.region ).distinct, false)

        case UndimensionedContinueAction(self, destinations, moved) =>
            val units = of(self).units.exceptUncontrolledFilth(this).%(nx).%(_.region.glyph.onMap).%(!_.has(Moved)).%(u => destinations.but(u.region).%(affordF(self, hasMoved(self).not.??(2))).any).sortWith(sortAllUnits(of(self)))
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
            of(self).units.foreach(_.remove(Moved))
            EndAction(self)

        case UndimensionedAction(self, destinations, uc, o, r) =>
            if (hasMoved(self).not) {
                log("" + self + " units are " + Undimensioned.full)
                of(self).power -= 2
            }

            payTax(self, r)

            val u = of(self).at(o, uc).%(!_.has(Moved)).head
            move(u, r)
            u.add(Moved)

            log(self.styled(uc) + " from " + o + " is now in " + r)

            UndimensionedContinueAction(self, destinations, true)

        case UndimensionedCancelAction(self, destinations) =>
            UndimensionedContinueAction(self, destinations, true)

        case a if battle != null =>
            battle.perform(action)
    }

}
