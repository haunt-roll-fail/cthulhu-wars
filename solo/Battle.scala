package cws

import hrf.colmat._

import Html._

import cws.UnitUtils._

@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
sealed trait BattleRoll
case object Miss extends BattleRoll {
    override def toString = "Miss".styled("miss")
}
case object Pain extends BattleRoll {
    override def toString = "Pain".styled("pain")
}
case object Kill extends BattleRoll {
    override def toString = "Kill".styled("kill")
}

object BattleRoll {
    def roll() = {
        val r = math.random() * 6
        if (r > 5)
            Kill
        else
        if (r > 3)
            Pain
        else
            Miss
    }
}

sealed abstract class UnitHealth(val text : String) {
    override def toString = text
}

sealed trait BaseUnitHealth extends UnitHealth

case object Alive extends UnitHealth("alive") with BaseUnitHealth
case object Killed extends UnitHealth("killed") with BaseUnitHealth
case object Pained extends UnitHealth("pained") with BaseUnitHealth
case class DoubleHP(left : BaseUnitHealth, right : BaseUnitHealth) extends UnitHealth((left, right) match {
    case (Killed, Alive) => "half-killed"
    case (Alive, Killed) => "half-killed"
    case (Pained, Alive) => "half-pained"
    case (Alive, Pained) => "half-pained"
    case (Alive, Alive) => "alive"
    case (a, b) => "" + a + "/" + b
})
case class Spared(now : BaseUnitHealth) extends UnitHealth("spared-" + now)


@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
sealed trait BattlePhase
case object BattleStart extends BattlePhase
case object AttackerPreBattle extends BattlePhase
case object DefenderPreBattle extends BattlePhase
case object PreRoll extends BattlePhase
case object RollAttackers extends BattlePhase
case object RollDefenders extends BattlePhase
case object ChannelPowerPhase extends BattlePhase
case object NecrophagyPhase extends BattlePhase
case object PostRoll extends BattlePhase
case object AssignDefenderKills extends BattlePhase
case object AssignAttackerKills extends BattlePhase
case object AllKillsAssignedPhase extends BattlePhase
case object HarbingerKillPhase extends BattlePhase
case object EternalKillPhase extends BattlePhase
case object EliminatePhase extends BattlePhase
case object BerserkergangPhase extends BattlePhase
case object UnholyGroundPhase extends BattlePhase
case object AssignDefenderPains extends BattlePhase
case object AssignAttackerPains extends BattlePhase
case object AllPainsAssignedPhase extends BattlePhase
case object HarbingerPainPhase extends BattlePhase
case object EternalPainPhase extends BattlePhase
case object MadnessPhase extends BattlePhase
case object AttackerDefenderRetreats extends BattlePhase
case object DefenderAttackerRetreats extends BattlePhase
case object PostBattlePhase extends BattlePhase
case object BattleEnd extends BattlePhase

import Action._

trait PreBattleQuestion extends FactionAction {
    def question = (g : Game) => (g.battle.attacker == self).?("Attacker").|("Defender") + " pre-battle"
}

case class BattleCancelAction(self : Faction) extends BaseFactionAction(None, "Cancel") with Cancel
case class PreBattleDoneAction(self : Faction, next : BattlePhase) extends OptionFactionAction("Done") with PreBattleQuestion
case class BattleProceedAction(next : BattlePhase) extends ForcedAction

case class BattleRollAction(f : Faction, rolls : List[BattleRoll], next : BattlePhase) extends ForcedAction

case class AssignKillAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Kill" + (count > 1).??("s")).styled("kill"), ur.short)
case class AssignPainAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Pain" + (count > 1).??("s")).styled("pain"), ur.short)

case class RetreatOrderAction(self: Faction, attackerFirst: Boolean) extends BaseFactionAction("Retreat order",
    g => {
        val b = g.battle
        val attackerLabel = if (b != null) "" + b.attacker else "Attacker"
        val defenderLabel =
            if (b != null && b.isUncontrolledFilth)
                "Uncontrolled ".styled("nt") + Filth.name.styled("nt")
            else if (b != null)
                "" + b.defender
            else
                "Defender"

        if (attackerFirst)
            attackerLabel + " then " + defenderLabel
        else
            defenderLabel + " then " + attackerLabel
    })

case class EliminateNoWayAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Nowhere to retreat, a pained unit is eliminated", ur.short)

case class RetreatAllAction(self: Faction, f: Faction, r: Region) extends BaseFactionAction(
    g => {
        val b = g.battle
        val factionLabel =
            if (b != null && b.isUncontrolledFilth && f == b.defender)
                "Uncontrolled ".styled("nt") + Filth.name.styled("nt")
            else
                f.toString

        "Retreat all pained " + factionLabel + " units to"
    },
    r
  )
case class RetreatSeparatelyAction(self : Faction, f : Faction, destinations : List[Region]) extends BaseFactionAction(None, "Retreat separately") with More

case class RetreatUnitAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction("Retreat " + ur.short, r)

// GC
case class DevourPreBattleAction(self : Faction) extends OptionFactionAction(Devour) with PreBattleQuestion
case class DevourAction(self : Faction, ur : UnitRef) extends BaseFactionAction(Devour, ur.short)

case class AbsorbPreBattleAction(self : Faction) extends OptionFactionAction(Absorb) with PreBattleQuestion with Soft
case class AbsorberAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Absorb with", ur.full) with Soft
case class AbsorbeeAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction(g => "Absorb with " + g.unit(ur).full, g => { val t = g.unit(tr); (t.uclass == Shoggoth).??("Another ") + tr.short })

// CC
case class AbductPreBattleAction(self : Faction) extends OptionFactionAction(Abduct) with PreBattleQuestion
case class AbductAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction(Abduct, tr.full)

case class InvisibilityPreBattleAction(self : Faction) extends OptionFactionAction(Invisibility) with PreBattleQuestion with Soft
case class InvisibilityAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction("Make invisible", g => g.unit(tr).full + (ur == tr).??(" (self)"))

case class SeekAndDestroyPreBattleAction(self : Faction) extends OptionFactionAction(SeekAndDestroy) with PreBattleQuestion with Soft
case class SeekAndDestroyAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction("Bring with " + self.styled(SeekAndDestroy), self.styled(uc) + " from " + r)

case class HarbingerPowerAction(self : Faction, ur : UnitRef, n : Int) extends BaseFactionAction(Harbinger.full + " for " + ur.short, "Get " + n.power)
case class HarbingerESAction(self : Faction, ur : UnitRef, e : Int) extends BaseFactionAction(Harbinger.full + " for " + ur.short, "Gain " + e.es)

// BG
case class NecrophagyAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(Necrophagy, self.styled(uc) + " from " + r)
case class NecrophagyDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

// SL
case class DemandSacrificePreBattleAction(self : Faction) extends OptionFactionAction(DemandSacrifice) with PreBattleQuestion
case class DemandSacrificeProvideESAction(self : Faction) extends BaseFactionAction(DemandSacrifice, g => "" + g.battle.opponent(self) + " gains " + 1.es)
case class DemandSacrificeKillsArePainsAction(self : Faction) extends BaseFactionAction(DemandSacrifice, "Rolled " + "Kills".styled("kill") + " become " + "Pains".styled("pain"))

// WW
case class HowlPreBattleAction(self : Faction) extends OptionFactionAction(Howl) with PreBattleQuestion
case class HowlUnitAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Retreat unit from " + Howl.full, ur.full)
case class HowlAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction("Retreat " + ur.short + " to", r)
case class HowlUncontrolledFilthAction(self : Faction, ur : UnitRef, r : Region, target : Faction) extends BaseFactionAction("Retreat " + ur.short + " to", r)

case class EternalPayAction(self : Faction, u : UnitRef, result : BattleRoll) extends BaseFactionAction("Save " + u.short + " from " + result, "Pay " + 1.power + " for " + self.styled(Eternal))
case class EternalIgnoreAction(self : Faction) extends BaseFactionAction(None, "Cancel")

case class BerserkergangAction(self : Faction, n : Int, u : UnitRef) extends BaseFactionAction(Berserkergang.full + " eliminates" + (n == 1).?("").|(" " + n + " units"), u.short)

case class CannibalismAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Spawn with " + self.styled(Cannibalism) + " in " + r, self.styled(uc))
case class CannibalismDoneAction(self : Faction) extends BaseFactionAction(None, "Cancel")

// OW
case class ChannelPowerAction(self : Faction, n : Int) extends BaseFactionAction(self.styled(ChannelPower), "Reroll " + n + " " + (n > 1).?("Misses").|("Miss").styled("miss") + " for " + 1.power)
case class ChannelPowerDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class MillionFavoredOnesAction(self : Faction, r : Region, uc : UnitClass, nw : List[UnitClass]) extends BaseFactionAction(self.styled(MillionFavoredOnes), self.styled(uc) + " in " + r + " to " + self.styled((nw.num > 1).?("" + nw.num + " " + nw(0).plural).|(nw(0).name)))
case class MillionFavoredOnesDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

// AN
case class UnholyGroundAction(self : Faction, o : Faction, cr : Region, br : Region) extends BaseFactionAction("Remove a cathedral with " + UnholyGround.full, cr)
case class UnholyGroundIgnoreAction(self : Faction) extends BaseFactionAction(None, "Cancel")
case class UnholyGroundEliminateAction(self : Faction, f : Faction, br : Region, ur : UnitRef) extends BaseFactionAction("Choose a GOO to eliminate in " + br, ur.short)

// Independent Great Old Ones
case class CosmicUnityPreBattleAction(self : Faction) extends OptionFactionAction(self.styled(CosmicUnity)) with PreBattleQuestion
case class CosmicUnityAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Target for " + self.styled(CosmicUnity), ur.short)

// Neutral Spellbooks
case class ShrivelingPreBattleAction(self : Faction) extends OptionFactionAction(Shriveling) with PreBattleQuestion
case class ShrivelingAction(self : Faction, ur : UnitRef) extends BaseFactionAction(Shriveling, ur.short)


class Side(val faction : Faction, val player : Player, var units : $[UnitFigure], var strength : Int = 0, var rolls : $[BattleRoll] = $, var opponent : Side = null) {
    def has(s : Spellbook) = player.oncePerAction.contains(s)
    def add(s : Spellbook) { player.oncePerAction :+= s }
    def remove(s : Spellbook) { player.oncePerAction = player.oncePerAction.but(s) }
    def count(s : Spellbook) = player.oncePerAction.count(s)
}

class Battle(val game : Game, val region : Region, val attacker : Faction, val defender : Faction, log : (=> String) => Unit, uncontrolledDefender : String = "") {
    def isUncontrolledFilth: Boolean = uncontrolledDefender == Filth.name

    private val attackerUnits = {
        game.of(attacker).at(region).exceptUncontrolledFilth(game)
    }

    private val defenderUnits = {
        val all = game.of(defender).at(region)

        if (isUncontrolledFilth)
            all.uncontrolledFilthOnly(game)
        else if (game.isAbhothPresent)
            all
        else
            all.exceptFilth
    }

    private def refreshDefenderUnits(): Unit = {
        val stillHere = game.of(defender).at(region)
        defenders.units = defenders.units.filter(stillHere.contains)
    }

    val attackers = new Side(attacker, game.of(attacker), attackerUnits)
    val defenders = new Side(defender, game.of(defender), defenderUnits)
    attackers.opponent = defenders
    defenders.opponent = attackers

    val sides = List(attackers, defenders)
    var hidden : List[UnitFigure] = Nil
    var cannibalism : List[Faction] = Nil
    var cosmicUnityTargets: Set[UnitRef] = Set.empty

    var unholyGroundEliminatedGOOs: Set[UnitRef] = Set.empty

    def opponent(f : Faction) = if (f == attacker) defender else if (f == defender) attacker else { log("Unknown faction in battle " + f); null }
    def units(f : Faction) = side(f).units
    def strength(f : Faction) = side(f).strength
    def side(f : Faction) = if (f == attacker) attackers else if (f == defender) defenders else { log("Unknown faction in battle " + f); null }

    def eliminate(u : UnitFigure) {
        exempt(u)
        game.eliminate(u)

        if (!u.isUncontrolledFilth(game))
            game.satisfy(u.faction, LoseUnitInBattle, "Lose " + u.short + " in battle")
    }

    def exempt(u : UnitFigure) {
        u.state = Nil
        u.add(Hidden)
        hidden :+= u
        sides.foreach(s => s.units = s.units.but(u))
    }

    def resolveHowl(u: UnitFigure, movedBy: Faction, targetSide: Side, r: Region): Continue = {
        if (!targetSide.units.contains(u)) {
            return proceed()
        }

        targetSide.units = targetSide.units.but(u)
        game.move(u, r)

        if (!side(movedBy).has(Howl))
            side(movedBy).add(Howl)

        log("" + u.short + " was howled to " + r)

        refreshDefenderUnits()

        if (defenders.units.isEmpty) {
            proceed(PostBattlePhase)
        } else {
            proceed()
        }
    }

    def retreat(u : UnitFigure, r : Region) = {
        game.move(u, r)
        u.add(Retreated)
        u.health = Alive

        sides.foreach { s =>
            if (s.units.contains(u)) {
                s.units = s.units.but(u)
            }
        }

        if (isUncontrolledFilth) {
            defenders.units = defenders.units.but(u)
        }
    }

    var phase : BattlePhase = BattleStart

    sides.foreach { s => s.strength = s.faction.strength(game, s.units, s.opponent.faction) }

    def assignedKills(unit : UnitFigure) : Int =
        unit.health match {
            case Killed => 1
            case DoubleHP(Killed, Killed) => 2
            case DoubleHP(Killed, _) => 1
            case DoubleHP(_, Killed) => 1
            case _ => 0
        }

    def canAssignKills(unit : UnitFigure) : Int =
        unit.health match {
            case DoubleHP(Killed, Killed) => 0
            case DoubleHP(Killed, _) => 1
            case DoubleHP(_, Killed) => 1
            case DoubleHP(_, _) => 2
            case Alive => 1
            case _ => 0
        }

    def assignKill(unit : UnitFigure) {
        unit.health = unit.health match {
            case Killed => log("ERROR - Assign KILL to KILLED"); Killed
            case Spared(_) => log("ERROR - Assign KILL to SPARED"); Killed
            case DoubleHP(Killed, Killed) => log("ERROR - Assign KILL to DBL-KILLED"); Killed
            case DoubleHP(Killed, _) => DoubleHP(Killed, Killed)
            case DoubleHP(_, Killed) => log("ERROR - Assign KILL to HALF-PAINED"); DoubleHP(Killed, Killed)
            case DoubleHP(Pained, _) => log("ERROR - Assign KILL to HALF-PAINED"); DoubleHP(Pained, Killed)
            case DoubleHP(_, Pained) => DoubleHP(Killed, Pained)
            case DoubleHP(Alive, Alive) => DoubleHP(Killed, Alive)
            case Pained => log("ERROR - Assign KILL to PAINED"); Killed
            case Alive => Killed
        }
    }

    def assignedPains(unit : UnitFigure) : Int =
        unit.health match {
            case Pained => 1
            case DoubleHP(Pained, Pained) => 2
            case DoubleHP(Pained, _) => 1
            case DoubleHP(_, Pained) => 1
            case _ => 0
        }

    def canAssignPains(unit : UnitFigure) : Int =
        unit.health match {
            case DoubleHP(Alive, Alive) => 2
            case DoubleHP(Alive, _) => 1
            case DoubleHP(_, Alive) => 1
            case Alive => 1
            case _ => 0
        }

    def assignPain(unit : UnitFigure) {
         unit.health = unit.health match {
            case Killed => log("ERROR - Assign PAIN to KILLED"); Pained
            case Spared(_) => log("ERROR - Assign PAIN to SPARED"); Killed
            case Pained => log("ERROR - Assign PAIN to PAINED"); Pained
            case DoubleHP(Killed, Killed) => log("ERROR - Assign PAIN to DBL-KILLED"); Pained
            case DoubleHP(Pained, Killed) => log("ERROR - Assign PAIN to PAINED-KILLED"); Pained
            case DoubleHP(Killed, Pained) => log("ERROR - Assign PAIN to KILLED-PAINED"); Pained
            case DoubleHP(Pained, Pained) => log("ERROR - Assign PAIN to DBL-PAINED"); Pained
            case DoubleHP(Killed, Alive) => DoubleHP(Killed, Pained)
            case DoubleHP(Alive, Killed) => DoubleHP(Pained, Killed)
            case DoubleHP(Pained, Alive) => DoubleHP(Pained, Pained)
            case DoubleHP(Alive, Pained) => DoubleHP(Pained, Pained)
            case DoubleHP(Alive, Alive) => DoubleHP(Pained, Alive)
            case Alive => Pained
        }
    }

    def prebattle(s : Side, next : BattlePhase) : Continue = {
        if (attackers.units.none || defenders.units.none) {
            if (attackers.units.none)
                log("" + attacker + " had no units remaining")

            if (defenders.units.none)
                log("" + defender + " had no units remaining")

            checkKillSpellbooks(attackers)
            checkKillSpellbooks(defenders)

            sides.foreach { s =>
                s.units.foreach(_.remove(Absorbed))
                s.units.foreach(_.remove(Invised))
            }

            return proceed(PostBattlePhase)
        }

        var options : List[FactionAction] = Nil

        if (s.player.has(Devour) && !s.has(Devour) && s.units.%(_.uclass == Cthulhu).any && s.opponent.units.exceptGOO.exceptTerror.any)
            options :+= DevourPreBattleAction(s.faction)

        if (s.player.has(Shriveling) && !s.has(Shriveling) && s.opponent.units.exceptGOO.exceptTerror.any)
            options :+= ShrivelingPreBattleAction(s.faction)

        if (s.player.has(Absorb)) {
            val sh = s.units.%(_.uclass == Shoggoth)
            if (sh.any)
                if (s.units.exceptGOO.exceptTerror.exceptUncontrolledFilth(game).%(_ != sh(0)).any)
                    options :+= AbsorbPreBattleAction(s.faction)
        }

        if (s.player.has(Howl) && s.units.%(_.uclass == Wendigo).nonEmpty && s.opponent.units.exceptByatis.nonEmpty && !side(s.faction).has(Howl) && !side(opponent(s.faction)).has(Howl))
            options :+= HowlPreBattleAction(s.faction)

        if (s.player.has(Abduct) && s.units.%(_.uclass == Nightgaunt).any && s.opponent.units.exceptGOO.exceptTerror.any)
            options :+= AbductPreBattleAction(s.faction)

        if (s.player.has(SeekAndDestroy) && s.player.all(HuntingHorror).%(_.region != region).any)
            options :+= SeekAndDestroyPreBattleAction(s.faction)

        if (s.player.has(Invisibility) && s.units.%(_.uclass == FlyingPolyp).%(!_.has(Invised)).any)
            options :+= InvisibilityPreBattleAction(s.faction)

        if (s.player.has(DemandSacrifice) && s.player.has(Tsathoggua) && !s.has(DemandSacrifice) && !s.opponent.has(KillsArePains) && (game.options.has(DemandTsathoggua).not || s.units.%(_.uclass == Tsathoggua).any) && !s.opponent.units.isUncontrolledFilthOnly(game))
            options :+= DemandSacrificePreBattleAction(s.faction)

        if (s.player.has(CosmicUnity) && !s.has(CosmicUnity) && s.units.exists(_.uclass == Daoloth) && s.opponent.units.%(_.uclass.utype == GOO).any)
            options :+= CosmicUnityPreBattleAction(s.faction)

        return Ask(s.faction, options :+ PreBattleDoneAction(s.faction, next))
    }

    def preroll(s : Side) {
        val strength = s.faction.strength(game, s.units, s.opponent.faction) + s.units./(_.count(Absorbed)).sum * 3

        if (game.of(s.faction).has(Nyogtha)) {
            val nyogthas = s.units.%(_.uclass == Nyogtha)
            if (nyogthas.nonEmpty && s == attackers) {
                nyogthas.foreach { _ =>
                    log("" + s.faction.styled(Nyogtha) + " increased its strength to " + 4.str + " since it was attacking")
                }
            }
        }

        if (strength != s.strength) {
            log("" + s.faction + " strength " + (if (strength > s.strength) "increased" else "decreased") + " to " + strength.str)
            s.strength = strength
        }

        s.units.foreach(_.remove(Absorbed))
        s.units.foreach(_.remove(Invised))

        if (s.player.has(Harbinger) && s.units.%(_.uclass == Nyarlathotep).any)
            s.add(Harbinger)

        if (s.player.has(Emissary) && s.units.%(_.uclass == Nyarlathotep).any && s.opponent.units.%(_.uclass.utype == GOO).none)
            s.add(Emissary)

        if (s.player.has(Vengeance) && s.units.%(_.uclass == Hastur).any)
            s.add(Vengeance)

        if (s.player.has(ChannelPower))
            s.add(ChannelPower)

        if (s.player.has(Eternal) && s.units.%(_.uclass == RhanTegoth).any)
            s.add(Eternal)

        if (s.player.has(Regenerate) && s.units.%(_.uclass == Starspawn).any)
            s.add(Regenerate)

        if (s.player.has(MillionFavoredOnes))
            s.add(MillionFavoredOnes)

        cosmicUnityTargets.foreach { ur =>
            val targetUnit = game.unit(ur)
            if (s.units.contains(targetUnit)) {
                val removed = s.faction.strength(game, $(targetUnit), s.opponent.faction)
                if (removed > 0) {
                    s.strength -= removed
                    log("" + targetUnit.short + " contributes no dice due to " + s.opponent.faction.styled(CosmicUnity) + ", new total: " + s.strength.str)
                }
            }
        }
    }

    def postroll(s : Side) {
        if (s.has(Regenerate))
            s.units.%(_.uclass == Starspawn).foreach(_.health = DoubleHP(Alive, Alive))

        if (s.has(KillsArePains)) {
            if (s.rolls.%(_ == Kill).any) {
                s.rolls = s.rolls./(r => if (r == Kill) Pain else r)
            }
        }
    }

    def hitter(s : Side) : Faction = {
        if (s.opponent.has(Vengeance))
            return s.opponent.faction
        else if (uncontrolledDefender.nonEmpty)
            return s.opponent.faction

        return s.faction
    }

    def assignKills(s : Side, next : BattlePhase) : Continue = {
        val kills = s.opponent.rolls.count(_ == Kill)
        val assigned = s.units./(assignedKills).sum
        val canAssign = s.units./(canAssignKills).sum

        if (kills <= assigned)
            return BattleProceedAction(next)

        if (kills >= assigned + canAssign) {
            s.units.foreach(d => 1.to(canAssignKills(d)).foreach(_ => assignKill(d)))
            return DelayedContinue(100, BattleProceedAction(next))
        }

        val f = hitter(s)

        if (f != s.faction && assigned == 0)
            log("" + f + " assigned kills with " + f.styled(Vengeance))

        return DelayedContinue(50, Ask(f, s.units.%(u => canAssignKills(u) > 0).sortBy(_.uclass.cost)./(u => AssignKillAction(f, kills - assigned, s.faction, u.ref))))
    }

    def assignPains(s : Side, next : BattlePhase) : Continue = {
        val pains = s.opponent.rolls.count(_ == Pain)
        val assigned = s.units./(assignedPains).sum
        val canAssign = s.units./(canAssignPains).sum

        if (pains <= assigned)
            return BattleProceedAction(next)

        if (pains >= assigned + canAssign) {
            s.units.foreach(d => 1.to(canAssignPains(d)).foreach(_ => assignPain(d)))
            return DelayedContinue(100, BattleProceedAction(next))
        }

        val f = hitter(s)

        if (f != s.faction && assigned == 0 && uncontrolledDefender.isEmpty)
            log("" + f + " assigned pains with " + f.styled(Vengeance))

        return DelayedContinue(50, Ask(f, s.units.%(u => canAssignPains(u) > 0).sortBy(_.uclass.cost)./(u => AssignPainAction(f, pains - assigned, s.faction, u.ref))))
    }

    def retreater(s: Side) : Faction = {
        game.factions.find(f => game.of(f).has(Madness)) match {
            case Some(cc) => cc
            case None =>
            if (uncontrolledDefender.nonEmpty) s.opponent.faction
            else s.faction
        }
    }

    def retreat(s : Side) : Continue = {
        val refugees = s.units.%(_.health == Pained)

        if (refugees.none)
            return proceed()

        s.units.foreach(_.remove(Retreated))

        val destinations = game.board.connected(region).filter { r =>
            if (isUncontrolledFilth)
                attackers.units.%(_.region == r).isEmpty
            else
                s.opponent.player.at(r).exceptUncontrolledFilth(game).isEmpty
        }

        val chooser: Faction = retreater(s)

        if (destinations.none) {
            QAsk(refugees.sortBy(_.uclass.cost)./(u => EliminateNoWayAction(chooser, u.ref)))
        } else if (destinations.num == 1) {
            val dest = destinations(0)
            refugees.foreach(u => retreat(u, dest))
            log("" + refugees./(_.short).mkString(", ") + " retreated to " + dest)
            proceed()
        } else if (refugees.num == 1 || s.units.exists(_.has(Retreated))) {
            val u = refugees(0)
            QAsk(destinations./(d => RetreatUnitAction(chooser, u.ref, d)))
        } else {
            val ownerFaction = refugees(0).faction
            QAsk(destinations./(d =>
                RetreatAllAction(chooser, ownerFaction, d)) :+
                RetreatSeparatelyAction(chooser, ownerFaction, destinations)
            )
        }
    }

    def checkKillSpellbooks(s : Side) {
        if (s.player.needs(KillDevour1) || s.player.needs(KillDevour2)) {
            var devoured = s.count(Devour)
            var kills = s.opponent.units.count(_.health == Killed)

            if (devoured + kills >= 2) {
                if (s.player.needs(KillDevour2)) {
                    if (kills >= 2) {
                        game.satisfy(s.faction, KillDevour2, "Kill two enemy units in a battle")
                        kills -= 2
                    }
                    else {
                        game.satisfy(s.faction, KillDevour2, "Kill and Devour two enemy units in a battle")
                        devoured -= 1
                        kills -= 1
                    }
                }
            }

            if (devoured + kills >= 1) {
                if (s.player.needs(KillDevour1)) {
                    if (devoured == 1) {
                        game.satisfy(s.faction, KillDevour1, "Devour an enemy unit in a battle")
                        devoured -= 1
                    }
                    else {
                        game.satisfy(s.faction, KillDevour1, "Kill an enemy unit in a battle")
                        kills -= 1
                    }
                }
            }
        }
    }

    def checkByatisSpellbook(s: Side): Unit = {
        val byatisHere = s.units.filter(_.uclass == Byatis)
        if (byatisHere.isEmpty || ByatisCard.hasSpellbook) return

        val killedOpponentUnits = s.opponent.units.count(_.health == Killed)
        val byatisKilled        = byatisHere.exists(_.health == Killed)

        if (killedOpponentUnits > 0 && !byatisKilled) {
            s.player.spellbooks :+= GodOfForgetfulness
            ByatisCard.hasSpellbook = true
            log("" + s.faction + " gained " + s.faction.styled(GodOfForgetfulness) + " for " + s.faction.styled(Byatis))
        }
    }

    def checkNyogthaSpellbookSingleBattle(s: Side): Unit = {
        if (NyogthaCard.hasSpellbook) return

        val nyogthasHere  = s.units.%(_.uclass == Nyogtha)
        val nyogthasAlive = nyogthasHere.forall(_.health != Killed)
        val inPair        = game.nyogthaPairByFaction.contains(s.faction)

        val hadEnemyGOO =
            s.opponent.units.exists(_.uclass.utype == GOO) ||
            game.nyogthaEnemyGOOSeen.getOrElse(s.faction, false)

        if (nyogthasHere.nonEmpty && nyogthasAlive && hadEnemyGOO && !inPair) {
            s.player.spellbooks :+= NightmareWeb
            NyogthaCard.hasSpellbook = true
            log("" + s.faction + " gained " + s.faction.styled(NightmareWeb) + " for " + s.faction.styled(Nyogtha))
        }
    }

    def proceed(bp : BattlePhase) : Continue = {
        phase = bp
        proceed()
    }

    def proceed() : Continue = {
        phase match {
            case BattleStart =>
                if (attackers.units.exists(_.uclass == Nyogtha) && defenders.units.exists(_.uclass.utype == GOO))
                    game.nyogthaEnemyGOOSeen += attackers.faction -> true
                if (defenders.units.exists(_.uclass == Nyogtha) && attackers.units.exists(_.uclass.utype == GOO))
                    game.nyogthaEnemyGOOSeen += defenders.faction -> true

                if (attackers.units.isEmpty || defenders.units.isEmpty) {
                    log("No units left to fight")
                    return proceed(PostBattlePhase)
                }

                if (attackers.units.any)
                    log("" + attacker + " attacked with " + attackers.units./(_.short).mkString(", ") + " " + attackers.strength.str)

                if (defenders.units.any) {
                    if (isUncontrolledFilth)
                        log("" + "Uncontrolled ".styled("nt") + Filth.name.styled("nt") + " defended with " + defenders.units./(_.uclass.name).mkString(", ").styled("nt") + " " + defenders.strength.str)
                    else
                        log("" + defender + " defended with " + defenders.units./(_.short).mkString(", ") + " " + defenders.strength.str)
                }

                proceed(AttackerPreBattle)

            case AttackerPreBattle =>
                prebattle(attackers, DefenderPreBattle)

            case DefenderPreBattle =>
                prebattle(defenders, PreRoll)

            case PreRoll =>
                preroll(attackers)
                preroll(defenders)

                proceed(RollAttackers)

            case RollAttackers =>
                if (isUncontrolledFilth) {
                    // Skip RollDefenders (Filth have 0 combat)
                    // If we later introduce uncontrolled units that can fight back, this needs to be adjusted.
                    RollBattle(attacker, "attack", side(attacker).strength, x => BattleRollAction(attacker, x, ChannelPowerPhase))
                }
                else {
                    RollBattle(attacker, "attack", side(attacker).strength, x => BattleRollAction(attacker, x, RollDefenders))
                }

            case RollDefenders =>
                RollBattle(defender, "defense", side(defender).strength, x => BattleRollAction(defender, x, ChannelPowerPhase))

            case ChannelPowerPhase =>
                sides.foreach { s =>
                    if (s.has(ChannelPower))
                        if (s.rolls.%(_ == Miss).any)
                            if (s.rolls.%(_ == Kill).num < s.opponent.units./(canAssignKills).sum)
                                if (s.player.power > 0)
                                    return QAsk(ChannelPowerAction(s.faction, s.rolls.%(_ == Miss).num) :: ChannelPowerDoneAction(s.faction))
                                else
                                if (s.player.want(DragonAscending) && game.factions.%(game.of(_).power > 0).any)
                                    return DragonAscendingAskAction(s.faction, Some(s.faction), ChannelPower.full, BattleCancelAction(s.faction))
                }

                proceed(PostRoll)

            case PostRoll =>
                postroll(attackers)
                postroll(defenders)

                proceed(AssignDefenderKills)

            case AssignDefenderKills =>
                assignKills(defenders, AssignAttackerKills)

            case AssignAttackerKills =>
                assignKills(attackers, AllKillsAssignedPhase)

            case AllKillsAssignedPhase =>
                sides.foreach { s =>
                    if (s.has(Emissary)) {
                        s.units.%(_.health == Killed).%(_.uclass == Nyarlathotep).foreach { u =>
                            log("" + u.uclass + " survived the kill as an " + u.faction.styled(Emissary))
                            u.health = Spared(Pained)
                        }
                    }

                    val doubleKilled = s.units.%(_.health == DoubleHP(Killed, Killed))
                    val killed = s.units.%(_.health == Killed)

                    doubleKilled.foreach { u =>
                        log("" + u.short + " was " + "killed".styled("kill") + " with two " + "Kills".styled("kill"))
                        u.health = Killed
                    }

                    if (killed.any)
                        log("" + killed./(_.short).mkString(", ") + (killed.num > 1).?(" were ").|(" was ") + "killed".styled("kill"))
                }

                proceed(HarbingerKillPhase)

            case HarbingerKillPhase =>
                sides.foreach { s =>
                    if (s.has(Harbinger)) {
                        val raw = s.opponent.units.filter(u => u.health == Killed && u.uclass.utype == GOO && !u.has(Harbinged))
                        val grouped = raw.groupBy(_.uclass).values.map(_.head).filterNot(u => u.uclass == Nyogtha && game.harbingerNyogthaAwarded.contains(s.faction)).toList

                        if (grouped.nonEmpty) {
                            val g = grouped.head

                            if (g.uclass == Nyogtha)
                                game.harbingerNyogthaAwarded += s.faction

                            return QAsk(HarbingerPowerAction(s.faction, g.ref, g.uclass.cost / 2) :: HarbingerESAction(s.faction, g.ref, 2))
                        }
                    }
                }

                proceed(EternalKillPhase)

            case EternalKillPhase =>
                sides.foreach { s =>
                    if (s.has(Eternal) && s.player.power > 0) {
                        val rt = s.units.%(_.health == Killed).%(_.uclass == RhanTegoth)

                        if (rt.any) {
                            if (game.targetDragonAscending(s.faction).any)
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s.faction, Eternal.full, BattleCancelAction(s.faction)))

                            return QAsk(rt./(u => EternalPayAction(s.faction, u.ref, Kill)) :+ EternalIgnoreAction(s.faction))
                        }
                    }
                }

                proceed(EliminatePhase)

            case EliminatePhase =>
                checkKillSpellbooks(attackers)
                checkKillSpellbooks(defenders)

                cannibalism = game.factions.%(f => game.of(f).has(Cannibalism)).%(f => sides.%(_.faction != f).%(_.units.%(_.health == Killed).any).any)

                val regionHere = region
                val nyogthaDiedHere = (attackers.units ++ defenders.units).exists(u => u.uclass == Nyogtha && u.health == Killed)
                val enemyGOOHereForAttacker = defenders.units.exists(_.uclass.utype == GOO)
                val enemyGOOHereForDefender = attackers.units.exists(_.uclass.utype == GOO)

                def foldIntoPair(f: Faction, enemyGOOHere: Boolean): Unit = {
                    game.nyogthaPairByFaction.get(f) match {
                        case Some(pair) if pair.contains(regionHere) =>
                            if (nyogthaDiedHere)
                                game.nyogthaPairNyogthaDied += f -> true
                            if (enemyGOOHere)
                                game.nyogthaPairHadEnemyGOO += f -> true

                        val newCount = game.nyogthaPairProgress.getOrElse(f, 0) + 1
                        game.nyogthaPairProgress += f -> newCount

                        if (newCount >= pair.size) {
                            val died   = game.nyogthaPairNyogthaDied.getOrElse(f, false)
                            val hadGOO = game.nyogthaPairHadEnemyGOO.getOrElse(f, false)
                            if (!NyogthaCard.hasSpellbook && hadGOO && !died) {
                                val p = game.of(f)
                                p.spellbooks :+= NightmareWeb
                                NyogthaCard.hasSpellbook = true
                                log("" + f + " gained " + f.styled(NightmareWeb) + " for " + f.styled(Nyogtha))
                            }

                            game.nyogthaPairByFaction    -= f
                            game.nyogthaPairProgress     -= f
                            game.nyogthaPairHadEnemyGOO  -= f
                            game.nyogthaPairNyogthaDied  -= f
                        }
                        case _ =>
                    }
                }

                foldIntoPair(attacker, enemyGOOHereForAttacker)
                foldIntoPair(defender, enemyGOOHereForDefender)

                sides.foreach { s =>
                    if (s.player.has(Berserkergang))
                        s.units.%(_.uclass == GnophKeh).%(_.health == Killed).foreach(_ => s.add(Berserkergang))

                    checkByatisSpellbook(s)

                    if (s.player.units.%(_.uclass == Nyogtha).any) {
                        checkNyogthaSpellbookSingleBattle(s)
                    }
                }

                sides.foreach { s =>
                    s.units.%(_.health == Killed).foreach(eliminate)
                }

                proceed(BerserkergangPhase)

            case BerserkergangPhase =>
                sides.foreach { s =>
                    if (s.has(Berserkergang)) {
                        val count = s.count(Berserkergang)
                        s.remove(Berserkergang)
                        val targets = s.opponent.units.exceptGOO.exceptTerror
                        if (targets.any)
                            if (count >= targets.num) {
                                log("" + targets./(_.short).mkString(", ") + (targets.num > 1).?(" were ").|(" was ") + "eliminated with " + s.faction.styled(Berserkergang))
                                targets.foreach(eliminate)
                            }
                            else
                                return QAsk(targets./(u => BerserkergangAction(s.opponent.faction, count, u.ref)))
                    }
                }

                proceed(UnholyGroundPhase)

            case UnholyGroundPhase =>
                sides.foreach { s =>
                    if (s.player.has(UnholyGround)) {
                        val goosInBattle = s.opponent.units.filter(u => u.uclass.utype == GOO && !unholyGroundEliminatedGOOs.contains(u.ref))
                        if (goosInBattle.nonEmpty && game.cathedrals.contains(game.battle.region)) {
                            return QAsk(game.cathedrals.map(r => UnholyGroundAction(s.faction, s.opponent.faction, r, game.battle.region)) :+ UnholyGroundIgnoreAction(s.faction))
                        }
                    }
                }

                proceed(NecrophagyPhase)

            case NecrophagyPhase =>
                val necrophagy = (attacker :: defender :: game.factions).%(game.of(_).can(Necrophagy))

                necrophagy.foreach { f =>
                    val gs = game.of(f).all(Ghoul).diff(side(attacker).units).diff(side(defender).units).diff(hidden)
                    if (gs.any)
                        return QAsk(gs./(g => NecrophagyAction(f, g.uclass, g.region)) :+ NecrophagyDoneAction(f))
                }

                proceed(AssignDefenderPains)

            case AssignDefenderPains =>
                assignPains(defenders, AssignAttackerPains)

            case AssignAttackerPains =>
                assignPains(attackers, AllPainsAssignedPhase)

            case AllPainsAssignedPhase =>
                sides.foreach { s =>
                    s.units.foreach(u => u.health match {
                        case DoubleHP(Alive, Alive) =>
                        case DoubleHP(Pained, Pained) => log("" + u.short + " was assigned two " + Pain + "s".styled("pain"))
                        case DoubleHP(Killed, Alive) => log("" + u.short + " was assigned " + Kill + " and was " + "pained".styled("pain"))
                        case DoubleHP(Pained, Alive) => log("" + u.short + " was assigned " + Pain + " and was " + "pained".styled("pain"))
                        case DoubleHP(Killed, Pained) => log("" + u.short + " was assigned " + Kill + " and " + Pain + " and was " + "pained".styled("pain"))
                        case DoubleHP(l, r) => log("" + u.short + " was assigned " + List(l, r).%(_ != Alive).mkString(" and ") + " and was " + "pained".styled("pain"))
                        case _ =>
                    })

                    s.units.foreach(u => u.health = u.health match {
                        case DoubleHP(Alive, Alive) => Alive
                        case DoubleHP(_, _) => Pained
                        case Spared(now) => now
                        case s => s
                    })

                    val pained = s.units.%(_.health == Pained)

                    if (pained.any)
                        log("" + pained./(_.short).mkString(", ") + (pained.num > 1).?(" were ").|(" was ") + "pained".styled("pain"))
                }

                proceed(HarbingerPainPhase)

            case HarbingerPainPhase =>
                sides.foreach { s =>
                    if (s.has(Harbinger)) {
                        val raw = s.opponent.units.filter(u => u.health == Pained && u.uclass.utype == GOO && !u.has(Harbinged))
                        val grouped = raw.groupBy(_.uclass).values.map(_.head).filterNot(u => u.uclass == Nyogtha && game.harbingerNyogthaAwarded.contains(s.faction)).toList

                        if (grouped.nonEmpty) {
                            val g = grouped.head

                            if (g.uclass == Nyogtha)
                                game.harbingerNyogthaAwarded += s.faction

                            return QAsk(HarbingerPowerAction(s.faction, g.ref, g.uclass.cost / 2) :: HarbingerESAction(s.faction, g.ref, 2))
                        }
                    }
                }

                proceed(EternalPainPhase)

            case EternalPainPhase =>
                sides.foreach { s =>
                    if (s.has(Eternal) && s.player.power > 0) {
                        val rt = s.units.%(_.health == Pained).%(_.uclass == RhanTegoth)
                        if (rt.any) {
                            if (game.targetDragonAscending(s.faction).any)
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s.faction, Eternal.full, BattleCancelAction(s.faction)))

                            return QAsk(rt./(u => EternalPayAction(s.faction, u.ref, Pain)) :+ EternalIgnoreAction(s.faction))
                        }
                    }
                }

                proceed(MadnessPhase)

            case MadnessPhase =>
                sides.foreach { s =>
                    s.units.foreach(u => u.remove(Harbinged))
                }

                if (retreater(attackers) == retreater(defenders) && sides.%(_.units.%(_.health == Pained).none).none) {
                    val chooser = retreater(attackers)
                    return QAsk(RetreatOrderAction(chooser, attackerFirst = true) :: RetreatOrderAction(chooser, attackerFirst = false) :: Nil)
                } else {
                    proceed(AttackerDefenderRetreats)
                }

            case AttackerDefenderRetreats =>
                if (attackers.units.%(_.health == Pained).any)
                    retreat(attackers)
                else
                if (defenders.units.%(_.health == Pained).any)
                    retreat(defenders)
                else
                    proceed(PostBattlePhase)

            case DefenderAttackerRetreats =>
                if (defenders.units.%(_.health == Pained).any)
                    retreat(defenders)
                else
                if (attackers.units.%(_.health == Pained).any)
                    retreat(attackers)
                else
                    proceed(PostBattlePhase)

            case PostBattlePhase =>
                game.checkGatesLost()

                sides.foreach { s =>
                    if (s.has(MillionFavoredOnes)) {
                        val options = s.units./~(u => u.uclass match {
                            case Acolyte if s.player.inPool(Mutant).any      => Some(MillionFavoredOnesAction(s.faction, u.region, u.uclass, List(Mutant)))
                            case Mutant if s.player.inPool(Abomination).any  => Some(MillionFavoredOnesAction(s.faction, u.region, u.uclass, List(Abomination)))
                            case Abomination if s.player.inPool(SpawnOW).any => Some(MillionFavoredOnesAction(s.faction, u.region, u.uclass, List(SpawnOW)))
                            case SpawnOW if s.player.inPool(Mutant).any      => Some(MillionFavoredOnesAction(s.faction, u.region, u.uclass, List.fill(s.player.inPool(Mutant).num)(Mutant)))
                            case _ => None
                        })

                        if (options.any)
                            return QAsk(options :+ MillionFavoredOnesDoneAction(s.faction))
                    }
                }

                cannibalism.foreach { f =>
                    val us = List(Acolyte, Wendigo).%(uc => game.of(f).inPool(uc).any)
                    if (us.any)
                        return QAsk(us./(u => CannibalismAction(f, region, u)) :+ CannibalismDoneAction(f))
                }

                proceed(BattleEnd)

            case BattleEnd =>
                sides.foreach(_.units.foreach(_.remove(Retreated)))
                hidden.foreach(_.remove(Hidden))
                game.battled :+= region
                game.battle = null
                cosmicUnityTargets = Set.empty
                game.nyogthaEnemyGOOSeen -= attacker
                game.nyogthaEnemyGOOSeen -= defender
                AfterAction(attacker)

        }
    }

    def perform(a : Action) : Continue = a match {
        // CANCEL
        case BattleCancelAction(self) =>
            proceed()

        // PROCEED
        case BattleProceedAction(bf) =>
            proceed(bf)

        // DONE
        case PreBattleDoneAction(self, bf) =>
            proceed(bf)

        // ROLL
        case BattleRollAction(f, rolls, next) =>
            side(f).rolls = side(f).rolls ++ rolls

            val sv = side(f).units.filter(_.uclass == StarVampire).num

            if (rolls.any)
                log("" + f + " rolled " + rolls.drop(sv).mkString(" "))

            0.until(sv).foreach { i =>
                log("" + f.styled(StarVampire) + " rolled " + rolls(i))

                if (!this.isUncontrolledFilth) {
                    rolls(i) match {
                        case Pain if game.of(opponent(f)).power > 0 =>
                            game.of(opponent(f)).power -= 1
                            game.of(f).power += 1
                            log("" + f.styled(StarVampire) + " drained " + ("1 Power").styled("power") + " from " + opponent(f).styled(opponent(f).name) + " with a " + "Pain".styled("pain"))
                        case Kill if game.of(opponent(f)).doom > 0 =>
                            game.of(opponent(f)).doom -= 1
                            game.of(f).doom += 1
                            log("" + f.styled(StarVampire) + " drained " + ("1 Doom").styled("doom") + " from " + opponent(f).styled(opponent(f).name) + " with a " + "Kill".styled("kill"))
                        case _ =>
                    }
                }
            }

            if (rolls.num >= 6)
                game.satisfy(f, Roll6DiceInBattle, "Roll " + rolls.num + " dice in Battle")

            proceed(next)

        // ASSIGN
        case AssignKillAction(_, _, _, ur) =>
            assignKill(game.unit(ur))
            proceed()

        case AssignPainAction(_, _, _, ur) =>
            assignPain(game.unit(ur))
            proceed()

        // RETREAT
        case RetreatOrderAction(self, attackerFirst) =>
            if (attackerFirst)
                proceed(AttackerDefenderRetreats)
            else
                proceed(DefenderAttackerRetreats)

        case RetreatAllAction(self, f, r) =>
            val refugees =
                if (isUncontrolledFilth && f == defender)
                    defenders.units.%(_.health == Pained)
                else
                    side(f).units.%(_.health == Pained)

            if (refugees.isEmpty) {
                proceed()
            } else {
                refugees.foreach(u => retreat(u, r))
                log("" + refugees./(_.short).mkString(", ") + " retreated to " + r)
                proceed()
            }

        case RetreatSeparatelyAction(self, f, l) =>
            val refugees =
                if (isUncontrolledFilth && f == defender)
                    defenders.units.%(_.health == Pained)
                else
                    side(f).units.%(_.health == Pained)

            if (refugees.isEmpty) {
                proceed()
            } else {
                QAsk(l./(RetreatUnitAction(self, refugees.head.ref, _)))
            }

        case EliminateNoWayAction(self, ur) =>
            if (side(self).has(Emissary) && ur.uclass == Nyarlathotep) {
                log("" + self + " had nowhere to retreat but " + ur.short + " remained as an " + self.styled(Emissary))
            }
            else {
                eliminate(game.unit(ur))
                if (isUncontrolledFilth)
                    log("" + "Uncontrolled ".styled("nt") + Filth.name.styled("nt") + " had nowhere to retreat and eliminated " + ur.short)
                else
                    log("" + self + " had nowhere to retreat and eliminated " + ur.short)
            }
            side(self).units.foreach(_.health = Alive)
            proceed()

        case RetreatUnitAction(self, ur, r) =>
            retreat(game.unit(ur), r)
            log("" + ur.short + " retreated to " + r)
            proceed()

        // DEVOUR
        case DevourPreBattleAction(self) =>
            val o = side(self).opponent
            QAsk(o.units.exceptGOO.exceptTerror./(u => DevourAction(o.faction, u.ref)))

        case DevourAction(self, ur) =>
            val u = game.unit(ur)
            side(u.faction).opponent.add(Devour)
            eliminate(u)
            log("" + ur.short + " was devoured by " + opponent(self))
            proceed()

        // ABSORB
        case AbsorbPreBattleAction(self) =>
            val shoggoths = side(self).units.%(_.uclass == Shoggoth)
            val actions = shoggoths./(u => AbsorberAction(self, u.ref)) :+ BattleCancelAction(self)

            if (shoggoths./(_.state.sorted).distinct.num == 1)
                QAsk(actions.take(1))
            else
                QAsk(actions)

        case AbsorberAction(self, ur) =>
            val u = game.unit(ur)
            QAsk(side(self).units.%(_ != u).exceptGOO.exceptTerror./(t => AbsorbeeAction(self, ur, t.ref)) :+ BattleCancelAction(self))

        case AbsorbeeAction(self, ur, tr) =>
            val u = game.unit(ur)
            val t = game.unit(tr)
            0.to(t.count(Absorbed)).foreach(_ => u.add(Absorbed))
            eliminate(t)
            log("" + ur.short + " absorbed " + tr.short + " and increased its strength by " + (3 + t.count(Absorbed) * 3).str)
            proceed()

        // ABDUCT
        case AbductPreBattleAction(self) =>
            val u = side(self).units.%(_.uclass == Nightgaunt).head
            QAsk(side(self).opponent.units.exceptGOO.exceptTerror./(t => AbductAction(opponent(self), u.ref, t.ref)))

        case AbductAction(self, ur, tr) =>
            val u = game.unit(ur)
            val t = game.unit(tr)
            eliminate(u)
            eliminate(t)
            log("" + tr.short + " was abducted by " + ur.short)
            proceed()

        // INVISIBILITY
        case InvisibilityPreBattleAction(self) =>
            val u = side(self).units.%(_.uclass == FlyingPolyp).%(!_.has(Invised)).head
            QAsk((side(self).opponent.units ++ side(self).units).exceptGOO.exceptTerror./(t => InvisibilityAction(self, u.ref, t.ref)) :+ BattleCancelAction(self))

        case InvisibilityAction(self, ur, tr) =>
            val u = game.unit(ur)
            val t = game.unit(tr)
            u.add(Invised)
            exempt(t)
            if (ur == tr)
                log("" + ur.short + " hid itself")
            else
                log("" + tr.short + " was hidden by " + ur.short)
            proceed()

        // SEEK AND DESTROY
        case SeekAndDestroyPreBattleAction(self) =>
            val us = side(self).player.all(HuntingHorror).%(_.region != region)
            QAsk(us./(u => SeekAndDestroyAction(self, u.uclass, u.region)) :+ BattleCancelAction(self))

        case SeekAndDestroyAction(self, uc, r) =>
            val u = game.of(self).at(r, uc).head
            game.move(u, region)
            side(self).units :+= u
            log("" + u + " flew from " + r)
            proceed()

        // DEMAND SACRIFICE
        case DemandSacrificePreBattleAction(self) =>
            QAsk(DemandSacrificeKillsArePainsAction(opponent(self)) :: DemandSacrificeProvideESAction(opponent(self)))

        case DemandSacrificeProvideESAction(self) =>
            game.giveES(opponent(self), 1)
            side(opponent(self)).add(DemandSacrifice)
            log("" + opponent(self) + " got " + 1.es + " from " + DemandSacrifice.full)
            proceed()

        case DemandSacrificeKillsArePainsAction(self) =>
            side(self).add(KillsArePains)
            log("" + self + " will roll " + "Kills".styled("kill") + " as " + "Pains".styled("pain") + " due to " + DemandSacrifice.full)
            proceed()

        // HOWL
        case HowlPreBattleAction(self) =>
            side(self).add(Howl)

            val enemySide = side(self).opponent
            val enemyUnits = enemySide.units.exceptByatis

            if (enemyUnits.isEmpty)
                return proceed()

            if (isUncontrolledFilth) {
                val u = enemyUnits.head
                QAsk(game.board.connected(region)./(r =>
                    HowlUncontrolledFilthAction(self, u.ref, r, enemySide.faction)
                ))
            } else {
                QAsk(enemyUnits./(u => HowlUnitAction(enemySide.faction, u.ref)))
            }

        case HowlUnitAction(self, ur) =>
            QAsk(game.board.connected(region)./(r => HowlAction(self, ur, r)))

        case HowlAction(self, ur, r) =>
            resolveHowl(game.unit(ur), self, side(self), r)

        case HowlUncontrolledFilthAction(self, ur, r, target) =>
            resolveHowl(game.unit(ur), self, side(target), r)

        // HARBINGER
        case HarbingerPowerAction(self, ur, n) =>
            val u = game.unit(ur)
            u.add(Harbinged)
            game.of(self).power += n
            log("" + self + " got " + n.power + " as " + Harbinger.full)
            proceed()

        case HarbingerESAction(self, ur, e) =>
            val u = game.unit(ur)
            u.add(Harbinged)
            game.giveES(self, e)
            log("" + self + " gained " + e.es + " as " + Harbinger.full)
            proceed()

        // NECROPHAGY
        case NecrophagyAction(self, uc, r) =>
            val b4 = game.of(self).all(Ghoul).diff(side(attacker).units).diff(side(defender).units).diff(hidden).num

            val u = game.of(self).at(r, uc).diff(hidden).head

            game.move(u, region)
            exempt(u)
            sides.foreach(_.rolls :+= Pain)
            log("" + self.styled(uc) + " came from " + r + ", causing additonal " + Pain + " to both sides")

            proceed()

        case NecrophagyDoneAction(self) =>
            game.of(self).oncePerAction :+= Necrophagy
            proceed()

        // ETERNAL
        case EternalPayAction(self, u, result) =>
            game.of(self).power -= 1
            game.unit(u).health = Spared(Alive)
            log("" + self + " payed " + 1.power + " for " + self.styled(Eternal) + " to cancel " + result + " on " + u.short)
            proceed()

        case EternalIgnoreAction(self) =>
            side(self).remove(Eternal)
            proceed()

        // BERSERKERGANG
        case BerserkergangAction(self, n, u) =>
            eliminate(game.unit(u))
            log("" + u.short + " was eliminated with " + self.styled(Berserkergang))
            if (n > 1)
                QAsk(side(self).units.exceptGOO.exceptTerror./(u => BerserkergangAction(self, n - 1, u.ref)))
            else
                proceed()

         case CannibalismAction(self, r, uc) =>
             cannibalism = cannibalism.%(_ != self)
             log("" + self + " spawned " + self.styled(uc) + " in " + r + " with " + self.styled(Cannibalism))
             game.place(self, uc, r)
             proceed()

         case CannibalismDoneAction(self) =>
             cannibalism = cannibalism.%(_ != self)
             proceed()

        // CHANNEL POWER
        case ChannelPowerAction(self, n) =>
            log("" + self + " rerolled " + (n > 0).?("Misses").|("Miss").styled("miss") + " with " + self.styled(ChannelPower))
            game.of(self).power -= 1
            side(self).rolls = side(self).rolls.%(_ != Miss)
            RollBattle(self, self.styled(ChannelPower), n, x => BattleRollAction(self, x, ChannelPowerPhase))

        case ChannelPowerDoneAction(self) =>
            side(self).remove(ChannelPower)
            proceed()

        // MILLION FAVORED ONES
        case MillionFavoredOnesAction(self, r, uc, nw) =>
            log("" + self + " promoted " + self.styled(uc) + " in " + r + " to " + nw./(self.styled).mkString(", "))
            val x = side(self).units.%(_.uclass == uc).head
            exempt(x)
            game.eliminate(x)
            nw.foreach(n => game.place(self, n, r))
            proceed()

        case MillionFavoredOnesDoneAction(self) =>
            side(self).remove(MillionFavoredOnes)
            proceed()

        // UNHOLY GROUND
        case UnholyGroundAction(self, o, cr, br) =>
            game.cathedrals = game.cathedrals.but(cr)
            log("Cathedral in " + cr + " was removed with " + self.styled(UnholyGround))

            val enemyGOOs = side(o).units.filter(u => u.uclass.utype == GOO && !unholyGroundEliminatedGOOs.contains(u.ref))
            if (enemyGOOs.nonEmpty)
                QAsk(enemyGOOs.map(u => UnholyGroundEliminateAction(o, self, br, u.ref)))
            else
                proceed(NecrophagyPhase)

        case UnholyGroundIgnoreAction(self) =>
            proceed(NecrophagyPhase)

        case UnholyGroundEliminateAction(self, f, br, ur) =>
            val u = game.unit(ur)

            if (u.uclass == Nyogtha) {
                val nyogthasInBattle = game.of(u.faction).all(Nyogtha).filter(game.battle.sides.flatMap(_.units).contains)

                if (nyogthasInBattle.size > 1) {
                    nyogthasInBattle.foreach { n =>
                        eliminate(n)
                        unholyGroundEliminatedGOOs += n.ref
                        log("" + n.short + " was eliminated with " + f.styled(UnholyGround))
                    }
                } else {
                    eliminate(u)
                    unholyGroundEliminatedGOOs += u.ref
                    log("" + u.short + " was eliminated with " + f.styled(UnholyGround))
                }
            } else {
                eliminate(u)
                unholyGroundEliminatedGOOs += u.ref
                log("" + u.short + " was eliminated with " + f.styled(UnholyGround))
            }

            val remainingEnemyGOOs = side(self).units.exists(u => u.uclass.utype == GOO && !unholyGroundEliminatedGOOs.contains(u.ref))
            val moreCathedrals = game.cathedrals.nonEmpty

            if (remainingEnemyGOOs && moreCathedrals)
                QAsk(game.cathedrals.map(r => UnholyGroundAction(f, self, r, br)) :+ UnholyGroundIgnoreAction(f))
            else
                proceed(NecrophagyPhase)

        // SHRIVELING
        case ShrivelingPreBattleAction(self) =>
            val o = side(self).opponent
            QAsk(o.units.exceptGOO.exceptTerror./(u => ShrivelingAction(self, u.ref)) :+ BattleCancelAction(self))

        case ShrivelingAction(self, ur) =>
            val u = game.unit(ur)
            val isUncontrolled = isUncontrolledFilth

            val p =
                if (!isUncontrolled)
                    (u.uclass.utype == Cultist).?(opponent(self).recruitCost(game, u.uclass, region)).|(opponent(self).summonCost(game, u.uclass, region))
                else 0

            side(self).add(Shriveling)
            eliminate(u)

            if (!isUncontrolled)
                side(self).opponent.player.power += p

            log(
                "" + ur.short +
                " was shriveled" +
                (if (!isUncontrolled) " and " + opponent(self) + " got " + p.power else "")
            )
            proceed()

        // COSMIC UNITY
        case CosmicUnityPreBattleAction(self) =>
            val o = side(self).opponent
            val gooUnits = o.units.%(_.uclass.utype == GOO)

            val groupedByGOO = gooUnits.groupBy(_.uclass)

            val options =
                groupedByGOO.map {
                    case (Nyogtha, _) =>
                        CosmicUnityAction(self, gooUnits.find(_.uclass == Nyogtha).get.ref)
                    case (_, units) =>
                        CosmicUnityAction(self, units.head.ref)
                }.toList

            QAsk(options :+ BattleCancelAction(self))

        case CosmicUnityAction(self, ur) =>
            val u = game.unit(ur)
            side(self).add(CosmicUnity)

            if (u.uclass == Nyogtha) {
                val allNyogthas = game.of(u.faction).all(Nyogtha)
                cosmicUnityTargets ++= allNyogthas.map(_.ref)
                log("" + self + " targeted both " + u.faction.styled(Nyogtha) + " units with " + self.styled(CosmicUnity))
            } else {
                cosmicUnityTargets += ur
                log("" + self + " targeted " + u.short + " with " + self.styled(CosmicUnity))
            }

            proceed()

    }

}
