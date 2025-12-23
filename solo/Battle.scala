package cws

import hrf.colmat._

import Html._


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
    def question(implicit game : Game) = (game.battle./(_.attacker).has(self)).?("Attacker").|("Defender") + " pre-battle"
}

case class BattleCancelAction(self : Faction) extends BaseFactionAction(None, "Cancel") with Cancel
case class PreBattleDoneAction(self : Faction, next : BattlePhase) extends OptionFactionAction("Done") with PreBattleQuestion
case class BattleProceedAction(next : BattlePhase) extends ForcedAction

case class BattleRollAction(f : Faction, rolls : List[BattleRoll], next : BattlePhase) extends ForcedAction

case class AssignKillAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Kill" + (count > 1).??("s")).styled("kill"), ur.short)
case class AssignPainAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Pain" + (count > 1).??("s")).styled("pain"), ur.short)

case class RetreatOrderAction(self : Faction, a : Faction, b : Faction) extends BaseFactionAction("Retreat order", "" + a + " then " + b)

case class EliminateNoWayAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Nowhere to retreat, a pained unit is eliminated", ur.short)

case class RetreatAllAction(self : Faction, f : Faction, r : Region) extends BaseFactionAction(
    g => {
        val b = g.battle
        val factionLabel =
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
case class DemandSacrificeProvideESAction(self : Faction) extends BaseFactionAction(DemandSacrifice, g => "" + self.opponent(g.battle.get) + " gains " + 1.es)
case class DemandSacrificeKillsArePainsAction(self : Faction) extends BaseFactionAction(DemandSacrifice, "Rolled " + "Kills".styled("kill") + " become " + "Pains".styled("pain"))

// WW
case class HowlPreBattleAction(self : Faction) extends OptionFactionAction(Howl) with PreBattleQuestion
case class HowlUnitAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Retreat unit from " + Howl.full, ur.full)
case class HowlAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction("Retreat " + ur.short + " to", r)

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


trait BattleImplicits {
    implicit class BattleFactionEx(f : Faction) {
        def opponent(implicit battle : Battle) = battle.factionToOpponent(f)
    }

    implicit def factionToSide(f : Faction)(implicit battle : Battle) : Side = battle.factionToSide(f)
}


class Side(private val self : Faction, var forces : $[UnitFigure], var str : Int, var rolls : $[BattleRoll])(implicit val game : Game) {
    def usyd(s : Spellbook) = self.oncePerAction.contains(s)
    def add(s : Spellbook) { self.oncePerAction :+= s }
    def remove(s : Spellbook) { self.oncePerAction = self.oncePerAction.but(s) }
    def count(s : Spellbook) = self.oncePerAction.count(s)
}

class Battle(val region : Region, val attacker : Faction, val defender : Faction)(implicit val game : Game) {
    implicit val battle : Battle = this

    def factionToSide(f : Faction) : Side = f match {
        case `attacker` => attackers
        case `defender` => defenders
        case _ => throw new Error("faction " + f.name + " is not a side in the battle")
    }

    def factionToOpponent(f : Faction) = f match {
        case `attacker` => defender
        case `defender` => attacker
        case _ => throw new Error("faction " + f.name + " is not a side in the battle")
    }

    private def refreshDefenderUnits() : Unit = {
        val stillHere = defender.at(region)
        defenders.forces = defenders.forces.filter(stillHere.contains)
    }

    val attackers = new Side(attacker, attacker.at(region), 0, $)
    val defenders = new Side(defender, defender.at(region), 0, $)

    val sides = $(attacker, defender)
    var hidden : $[UnitFigure] = $
    var cannibalism : $[Faction] = $

    def eliminate(u : UnitFigure) {
        exempt(u)
        game.eliminate(u)
    }

    def exempt(u : UnitFigure) {
        u.state = Nil
        u.add(Hidden)
        hidden :+= u
        sides.foreach(s => s.forces = s.forces.but(u))
    }

    def resolveHowl(u : UnitFigure, movedBy : Faction, targetSide : Side, r : Region) : Continue = {
        if (!targetSide.forces.contains(u)) {
            return proceed()
        }

        targetSide.forces = targetSide.forces.but(u)
        game.move(u, r)

        if (!movedBy.usyd(Howl))
            movedBy.add(Howl)

        log(u.short, "was howled to", r)

        refreshDefenderUnits()

        if (defenders.forces.none) {
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
            if (s.forces.contains(u)) {
                s.forces = s.forces.but(u)
            }
        }
    }

    var phase : BattlePhase = BattleStart

    sides.foreach { s => s.str = s.strength(s.forces, s.opponent) }

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

    def prebattle(s : Faction, next : BattlePhase) : Continue = {
        if (attackers.forces.none || defenders.forces.none) {
            if (attackers.forces.none)
                log(attacker, "had no units remaining")

            if (defenders.forces.none)
                log(defender, "had no units remaining")

            checkKillSpellbooks(attacker)
            checkKillSpellbooks(defender)

            sides.foreach { s =>
                s.forces.foreach(_.remove(Absorbed))
                s.forces.foreach(_.remove(Invised))
            }

            return proceed(PostBattlePhase)
        }

        var options : $[FactionAction] = $

        if (s.has(Devour) && !s.usyd(Devour) && s.forces(Cthulhu).any && s.opponent.forces.exceptGOO.exceptTerror.any)
            options :+= DevourPreBattleAction(s)

        if (s.has(Shriveling) && !s.usyd(Shriveling) && s.opponent.forces.exceptGOO.exceptTerror.any)
            options :+= ShrivelingPreBattleAction(s)

        if (s.has(Absorb)) {
            val sh = s.forces(Shoggoth)
            if (sh.any)
                if (s.forces.exceptGOO.exceptTerror.%(_ != sh(0)).any)
                    options :+= AbsorbPreBattleAction(s)
        }

        if (s.has(Howl) && s.forces(Wendigo).any && s.opponent.forces.exceptByatis.any && s.usyd(Howl).not && s.opponent.usyd(Howl).not)
            options :+= HowlPreBattleAction(s)

        if (s.has(Abduct) && s.forces(Nightgaunt).any && s.opponent.forces.exceptGOO.exceptTerror.any)
            options :+= AbductPreBattleAction(s)

        if (s.has(SeekAndDestroy) && s.all(HuntingHorror).%(_.region != region).any)
            options :+= SeekAndDestroyPreBattleAction(s)

        if (s.has(Invisibility) && s.forces(FlyingPolyp).%!(_.has(Invised)).any)
            options :+= InvisibilityPreBattleAction(s)

        if (s.has(DemandSacrifice) && s.has(Tsathoggua) && !s.usyd(DemandSacrifice) && !s.opponent.usyd(KillsArePains) && (game.options.has(DemandTsathoggua).not || s.forces(Tsathoggua).any))
            options :+= DemandSacrificePreBattleAction(s)

        if (s.has(CosmicUnity) && !s.usyd(CosmicUnity) && s.forces(Daoloth).any && s.opponent.forces.goos.any)
            options :+= CosmicUnityPreBattleAction(s)

        Ask(s).add(options).add(PreBattleDoneAction(s, next))
    }

    def preroll(s : Faction) {
        val str = s.strength(s.forces, s.opponent)

        if (s.has(Nyogtha)) {
            val nyogthas = s.forces(Nyogtha)
            if (nyogthas.any && s == attacker) {
                nyogthas.foreach { _ =>
                    log(s.styled(Nyogtha), "increased its strength to", 4.str, "since it was attacking")
                }
            }
        }

        if (str != s.str) {
            log(s, "strength", (str > s.str).?("increased").|("decreased"), "to", str.str)
            s.str = str
        }

        s.forces.foreach(_.remove(Absorbed))
        s.forces.foreach(_.remove(Invised))

        if (s.has(Harbinger) && s.forces(Nyarlathotep).any)
            s.add(Harbinger)

        if (s.has(Emissary) && s.forces(Nyarlathotep).any && s.opponent.forces.goos.none)
            s.add(Emissary)

        if (s.has(Vengeance) && s.forces(Hastur).any)
            s.add(Vengeance)

        if (s.has(ChannelPower))
            s.add(ChannelPower)

        if (s.has(Eternal) && s.forces(RhanTegoth).any)
            s.add(Eternal)

        if (s.has(Regenerate) && s.forces(Starspawn).any)
            s.add(Regenerate)

        if (s.has(MillionFavoredOnes))
            s.add(MillionFavoredOnes)

    }

    def postroll(s : Side) {
        if (s.usyd(Regenerate))
            s.forces(Starspawn).foreach(_.health = DoubleHP(Alive, Alive))

        if (s.usyd(KillsArePains)) {
            if (s.rolls.exists(_ == Kill)) {
                s.rolls = s.rolls./(_.useIf(_ == Kill)(_ => Pain))
            }
        }
    }

    def hitter(s : Faction) : Faction =
        if (s.opponent.has(Vengeance))
            s.opponent
        else
        if (s.neutral)
            s.opponent
        else
            s

    def assignKills(s : Faction, next : BattlePhase) : Continue = {
        val kills = s.opponent.rolls.count(_ == Kill)
        val assigned = s.forces./(assignedKills).sum
        val canAssign = s.forces./(canAssignKills).sum

        if (kills <= assigned)
            return BattleProceedAction(next)

        if (kills >= assigned + canAssign) {
            s.forces.foreach(d => 1.to(canAssignKills(d)).foreach(_ => assignKill(d)))
            return DelayedContinue(100, BattleProceedAction(next))
        }

        val f = hitter(s)

        if (f != s && assigned == 0)
            log(f, "assigned kills with", f.styled(Vengeance))

        return DelayedContinue(50, Ask(f, s.forces.%(u => canAssignKills(u) > 0).sortBy(_.uclass.cost)./(u => AssignKillAction(f, kills - assigned, s, u.ref))))
    }

    def assignPains(s : Faction, next : BattlePhase) : Continue = {
        val pains = s.opponent.rolls.count(_ == Pain)
        val assigned = s.forces./(assignedPains).sum
        val canAssign = s.forces./(canAssignPains).sum

        if (pains <= assigned)
            return BattleProceedAction(next)

        if (pains >= assigned + canAssign) {
            s.forces.foreach(d => 1.to(canAssignPains(d)).foreach(_ => assignPain(d)))
            return DelayedContinue(100, BattleProceedAction(next))
        }

        val f = hitter(s)

        if (f != s && assigned == 0 && s.real)
            log(f, "assigned pains with", f.styled(Vengeance))

        return DelayedContinue(50, Ask(f, s.forces.%(u => canAssignPains(u) > 0).sortBy(_.uclass.cost)./(u => AssignPainAction(f, pains - assigned, s, u.ref))))
    }

    def retreater(s : Faction) : Faction = {
        factions.%(_.has(Madness)).starting | {
            if (s.neutral)
                s.opponent
            else
                s
        }
    }

    def retreat(s : Faction) : Continue = {
        val refugees = s.forces.%(_.health == Pained)

        if (refugees.none)
            return proceed()

        s.forces.foreach(_.remove(Retreated))

        val destinations = game.board.connected(region).%(r => s.opponent.at(r).none)

        val chooser : Faction = retreater(s)

        if (destinations.none) {
            QAsk(refugees.sortBy(_.uclass.cost)./(u => EliminateNoWayAction(chooser, u.ref)))
        } else if (destinations.num == 1) {
            val dest = destinations(0)
            refugees.foreach(u => retreat(u, dest))
            log(refugees./(_.short).mkString(", "), "retreated to", dest)
            proceed()
        } else if (refugees.num == 1 || s.forces.exists(_.has(Retreated))) {
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

    def checkKillSpellbooks(s : Faction) {
        if (s.needs(KillDevour1) || s.needs(KillDevour2)) {
            var devoured = s.count(Devour)
            var kills = s.opponent.forces.count(_.health == Killed)

            if (devoured + kills >= 2) {
                if (s.needs(KillDevour2)) {
                    if (kills >= 2) {
                        game.satisfy(s, KillDevour2, "Kill two enemy units in a battle")
                        kills -= 2
                    }
                    else {
                        game.satisfy(s, KillDevour2, "Kill and Devour two enemy units in a battle")
                        devoured -= 1
                        kills -= 1
                    }
                }
            }

            if (devoured + kills >= 1) {
                if (s.needs(KillDevour1)) {
                    if (devoured == 1) {
                        game.satisfy(s, KillDevour1, "Devour an enemy unit in a battle")
                        devoured -= 1
                    }
                    else {
                        game.satisfy(s, KillDevour1, "Kill an enemy unit in a battle")
                        kills -= 1
                    }
                }
            }
        }
    }

    def checkByatisSpellbook(s : Faction) : Unit = {
        if (s.upgrades.has(GodOfForgetfulness).not)
            s.forces(Byatis).foreach { u =>
                if (u.health != Killed && s.opponent.forces.exists(_.health == Killed)) {
                    s.upgrades :+= GodOfForgetfulness

                    log(s, "gained", s.styled(GodOfForgetfulness), "for", s.styled(Byatis))
                }
            }
    }

    def checkNyogthaSpellbookSingleBattle(s : Faction) : Unit = {
        if (s.upgrades.has(NightmareWeb).not)
            s.forces(Nyogtha).some.foreach { l =>
                if (l.forall(_.health != Killed) && l.forall(_.has(Primed))) {
                    s.upgrades :+= NightmareWeb

                    log(s, "gained", s.styled(NightmareWeb), "for", s.styled(Nyogtha))
                }
            }
    }

    def proceed(bp : BattlePhase) : Continue = {
        phase = bp
        proceed()
    }

    def proceed() : Continue = {
        phase match {
            case BattleStart =>
                if (attacker.forces.none || defender.forces.none) {
                    log("No units left to fight")

                    return proceed(PostBattlePhase)
                }

                if (attacker.forces.any)
                    log(attacker, "attacked with", attacker.forces./(_.short).mkString(", "), "" + attacker.str.str)

                if (defender.forces.any)
                    log(defender, "defended with", defender.forces./(_.short).mkString(", "), "" + defender.str.str)

                sides.foreach { s =>
                    if (s.upgrades.has(NightmareWeb).not)
                        s.forces(Nyogtha).foreach { u =>
                            if (s.opponent.forces.goos.any)
                                u.add(Primed)
                        }
                }

                proceed(AttackerPreBattle)

            case AttackerPreBattle =>
                prebattle(attacker, DefenderPreBattle)

            case DefenderPreBattle =>
                prebattle(defender, PreRoll)

            case PreRoll =>
                preroll(attacker)
                preroll(defender)

                proceed(RollAttackers)

            case RollAttackers =>
                RollBattle(attacker, "attack", attacker.str, x => BattleRollAction(attacker, x, RollDefenders))

            case RollDefenders =>
                RollBattle(defender, "defense", defender.str, x => BattleRollAction(defender, x, ChannelPowerPhase))

            case ChannelPowerPhase =>
                sides.foreach { s =>
                    if (s.usyd(ChannelPower))
                        if (s.rolls.%(_ == Miss).any)
                            if (s.rolls.%(_ == Kill).num < s.opponent.forces./(canAssignKills).sum)
                                if (s.power > 0)
                                    return QAsk(ChannelPowerAction(s, s.rolls.%(_ == Miss).num) :: ChannelPowerDoneAction(s))
                                else
                                if (s.want(DragonAscending) && factions.%(_.power > 0).any)
                                    return DragonAscendingAskAction(s, Some(s), ChannelPower.full, BattleCancelAction(s))
                }

                proceed(PostRoll)

            case PostRoll =>
                postroll(attacker)
                postroll(defender)

                proceed(AssignDefenderKills)

            case AssignDefenderKills =>
                assignKills(defender, AssignAttackerKills)

            case AssignAttackerKills =>
                assignKills(attacker, AllKillsAssignedPhase)

            case AllKillsAssignedPhase =>
                sides.foreach { s =>
                    if (s.usyd(Emissary)) {
                        s.forces.%(_.health == Killed)(Nyarlathotep).foreach { u =>
                            log(u.uclass, "survived the kill as an", u.faction.styled(Emissary))
                            u.health = Spared(Pained)
                        }
                    }

                    val doubleKilled = s.forces.%(_.health == DoubleHP(Killed, Killed))
                    val killed = s.forces.%(_.health == Killed)

                    doubleKilled.foreach { u =>
                        log(u.short, "was", "killed".styled("kill"), "with two", "Kills".styled("kill"))
                        u.health = Killed
                    }

                    if (killed.any)
                        log(killed./(_.short).mkString(", ") + (killed.num > 1).?(" were ").|(" was ") + "killed".styled("kill"))
                }

                proceed(HarbingerKillPhase)

            case HarbingerKillPhase =>
                sides.foreach { s =>
                    if (s.usyd(Harbinger)) {
                        val raw = s.opponent.forces.filter(u => u.health == Killed && u.uclass.utype == GOO && !u.has(Harbinged))
                        val grouped = raw.groupBy(_.uclass).values.map(_.head).filterNot(u => u.uclass == Nyogtha && game.harbingerNyogthaAwarded.contains(s)).toList

                        if (grouped.any) {
                            val g = grouped.head

                            if (g.uclass == Nyogtha)
                                game.harbingerNyogthaAwarded += s

                            return QAsk(HarbingerPowerAction(s, g.ref, g.uclass.cost / 2) :: HarbingerESAction(s, g.ref, 2))
                        }
                    }
                }

                proceed(EternalKillPhase)

            case EternalKillPhase =>
                sides.foreach { s =>
                    if (s.usyd(Eternal) && s.power > 0) {
                        val rt = s.forces.%(_.health == Killed)(RhanTegoth)

                        if (rt.any) {
                            if (game.targetDragonAscending(s).any)
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s, Eternal.full, BattleCancelAction(s)))

                            return QAsk(rt./(u => EternalPayAction(s, u.ref, Kill)) :+ EternalIgnoreAction(s))
                        }
                    }
                }

                proceed(EliminatePhase)

            case EliminatePhase =>
                checkKillSpellbooks(attacker)
                checkKillSpellbooks(defender)

                cannibalism = factions.%(_.has(Cannibalism)).%(f => sides.but(f).%(_.forces.%(_.health == Killed).any).any)

                /*
                val regionHere = region
                val nyogthaDiedHere = (attacker.forces ++ defender.forces).exists(u => u.uclass == Nyogtha && u.health == Killed)
                val enemyGOOHereForAttacker = defender.forces.exists(_.uclass.utype == GOO)
                val enemyGOOHereForDefender = attacker.forces.exists(_.uclass.utype == GOO)

                def foldIntoPair(f : Faction, enemyGOOHere : Boolean) : Unit = {
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
                                f.spellbooks :+= NightmareWeb
                                NyogthaCard.hasSpellbook = true
                                log(f, "gained", f.styled(NightmareWeb), "for", f.styled(Nyogtha))
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

                ... TODO ...

                */

                sides.foreach { s =>
                    if (s.has(Berserkergang))
                        s.forces(GnophKeh).%(_.health == Killed).foreach(_ => s.add(Berserkergang))

                    checkByatisSpellbook(s)

                    if (s.forces(Nyogtha).any) {
                        checkNyogthaSpellbookSingleBattle(s)
                    }
                }

                sides.foreach { s =>
                    s.forces.%(_.health == Killed).foreach(eliminate)
                }

                proceed(BerserkergangPhase)

            case BerserkergangPhase =>
                sides.foreach { s =>
                    if (s.usyd(Berserkergang)) {
                        val count = s.count(Berserkergang)
                        s.remove(Berserkergang)
                        val targets = s.opponent.forces.exceptGOO.exceptTerror
                        if (targets.any)
                            if (count >= targets.num) {
                                log(targets./(_.short).mkString(", ") + (targets.num > 1).?(" were ").|(" was ") + "eliminated with", s.styled(Berserkergang))
                                targets.foreach(eliminate)
                            }
                            else
                                return QAsk(targets./(u => BerserkergangAction(s.opponent, count, u.ref)))
                    }
                }

                proceed(UnholyGroundPhase)

            case UnholyGroundPhase =>
                sides.foreach { s =>
                    if (s.has(UnholyGround)) {
                        if (s.opponent.forces.goos.inPlay.any && game.cathedrals.contains(region)) {
                            return QAsk(game.cathedrals.map(r => UnholyGroundAction(s, s.opponent, r, region)) :+ UnholyGroundIgnoreAction(s))
                        }
                    }
                }

                proceed(NecrophagyPhase)

            case NecrophagyPhase =>
                val necrophagy = (attacker :: defender :: factions).%(_.can(Necrophagy))

                necrophagy.foreach { f =>
                    val gs = f.all(Ghoul).diff(attacker.forces).diff(defender.forces).diff(hidden)
                    if (gs.any)
                        return QAsk(gs./(g => NecrophagyAction(f, g.uclass, g.region)) :+ NecrophagyDoneAction(f))
                }

                proceed(AssignDefenderPains)

            case AssignDefenderPains =>
                assignPains(defender, AssignAttackerPains)

            case AssignAttackerPains =>
                assignPains(attacker, AllPainsAssignedPhase)

            case AllPainsAssignedPhase =>
                sides.foreach { s =>
                    s.forces.foreach(u => u.health match {
                        case DoubleHP(Alive, Alive) =>
                        case DoubleHP(Pained, Pained) => log(u.short, "was assigned two", "" + Pain + "s".styled("pain"))
                        case DoubleHP(Killed, Alive) => log(u.short, "was assigned", Kill, "and was", "pained".styled("pain"))
                        case DoubleHP(Pained, Alive) => log(u.short, "was assigned", Pain, "and was", "pained".styled("pain"))
                        case DoubleHP(Killed, Pained) => log(u.short, "was assigned", Kill, "and", Pain, "and was", "pained".styled("pain"))
                        case DoubleHP(l, r) => log(u.short, "was assigned", List(l, r).%(_ != Alive).mkString(" and "), "and was", "pained".styled("pain"))
                        case _ =>
                    })

                    s.forces.foreach(u => u.health = u.health match {
                        case DoubleHP(Alive, Alive) => Alive
                        case DoubleHP(_, _) => Pained
                        case Spared(now) => now
                        case s => s
                    })

                    val pained = s.forces.%(_.health == Pained)

                    if (pained.any)
                        log(pained./(_.short).mkString(", ") + (pained.num > 1).?(" were ").|(" was ") + "pained".styled("pain"))
                }

                proceed(HarbingerPainPhase)

            case HarbingerPainPhase =>
                sides.foreach { s =>
                    if (s.usyd(Harbinger)) {
                        val raw = s.opponent.forces.filter(u => u.health == Pained && u.uclass.utype == GOO && !u.has(Harbinged))
                        val grouped = raw.groupBy(_.uclass).values.map(_.head).filterNot(u => u.uclass == Nyogtha && game.harbingerNyogthaAwarded.contains(s)).toList

                        if (grouped.any) {
                            val g = grouped.head

                            if (g.uclass == Nyogtha)
                                game.harbingerNyogthaAwarded += s

                            return QAsk(HarbingerPowerAction(s, g.ref, g.uclass.cost / 2) :: HarbingerESAction(s, g.ref, 2))
                        }
                    }
                }

                proceed(EternalPainPhase)

            case EternalPainPhase =>
                sides.foreach { s =>
                    if (s.usyd(Eternal) && s.power > 0) {
                        val rt = s.forces.%(_.health == Pained)(RhanTegoth)
                        if (rt.any) {
                            if (game.targetDragonAscending(s).any)
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s, Eternal.full, BattleCancelAction(s)))

                            return QAsk(rt./(u => EternalPayAction(s, u.ref, Pain)) :+ EternalIgnoreAction(s))
                        }
                    }
                }

                proceed(MadnessPhase)

            case MadnessPhase =>
                sides.foreach { s =>
                    s.forces.foreach(u => u.health = u.health match {
                        case Spared(now) => now
                        case s => s
                    })
                }

                sides.foreach { s =>
                    s.forces.foreach(u => u.remove(Harbinged))
                }

                if (retreater(attacker) == retreater(defender) && sides.forall(_.units.exists(_.health == Pained))) {
                    val chooser = retreater(attacker)
                    Ask(retreater(attacker))
                        .add(RetreatOrderAction(retreater(attacker), attacker, defender))
                        .add(RetreatOrderAction(retreater(attacker), defender, attacker))
                }
                else {
                    proceed(AttackerDefenderRetreats)
                }

            case AttackerDefenderRetreats =>
                if (attackers.forces.%(_.health == Pained).any)
                    retreat(attacker)
                else
                if (defenders.forces.%(_.health == Pained).any)
                    retreat(defender)
                else
                    proceed(PostBattlePhase)

            case DefenderAttackerRetreats =>
                if (defenders.forces.%(_.health == Pained).any)
                    retreat(defender)
                else
                if (attackers.forces.%(_.health == Pained).any)
                    retreat(attacker)
                else
                    proceed(PostBattlePhase)

            case PostBattlePhase =>
                game.checkGatesLost()

                sides.foreach { s =>
                    if (s.usyd(MillionFavoredOnes)) {
                        val options = s.forces./~(u => u.uclass match {
                            case Acolyte if s.inPool(Mutant).any      => |(MillionFavoredOnesAction(s, u.region, u.uclass, $(Mutant)))
                            case Mutant if s.inPool(Abomination).any  => |(MillionFavoredOnesAction(s, u.region, u.uclass, $(Abomination)))
                            case Abomination if s.inPool(SpawnOW).any => |(MillionFavoredOnesAction(s, u.region, u.uclass, $(SpawnOW)))
                            case SpawnOW if s.inPool(Mutant).any      => |(MillionFavoredOnesAction(s, u.region, u.uclass, s.inPool(Mutant).num.times(Mutant)))
                            case _ => None
                        })

                        if (options.any)
                            return QAsk(options :+ MillionFavoredOnesDoneAction(s))
                    }
                }

                cannibalism.foreach { f =>
                    val us = $(Acolyte, Wendigo).%(uc => f.inPool(uc).any)

                    if (us.any)
                        return Ask(f).each(us)(u => CannibalismAction(f, region, u)).add(CannibalismDoneAction(f))
                }

                proceed(BattleEnd)

            case BattleEnd =>
                sides.foreach(_.forces.foreach(_.remove(Retreated)))
                hidden.foreach(_.remove(Hidden))
                game.battled :+= region
                game.battle = None
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
            f.rolls = f.rolls ++ rolls

            val sv = f.forces(StarVampire).num

            if (rolls.any)
                log(f, "rolled", rolls.drop(sv).mkString(" "))

            0.until(sv).foreach { i =>
                log(f.styled(StarVampire), "rolled", rolls(i))

                if (f.opponent.real)
                    rolls(i) match {
                        case Pain if f.opponent.power > 0 =>
                            f.opponent.power -= 1
                            f.power += 1
                            log(f.styled(StarVampire), "drained", 1.power, "from", f.opponent.styled(f.opponent.name), "with a", "Pain".styled("pain"))
                        case Kill if f.opponent.doom > 0 =>
                            f.opponent.doom -= 1
                            f.doom += 1
                            log(f.styled(StarVampire), "drained", 1.doom, "from", f.opponent.styled(f.opponent.name), "with a", "Kill".styled("kill"))
                        case _ =>
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
        case RetreatOrderAction(self, a, b) =>
            if (a == attacker)
                proceed(AttackerDefenderRetreats)
            else
                proceed(DefenderAttackerRetreats)

        case RetreatAllAction(self, f, r) =>
            val refugees = f.forces.%(_.health == Pained)

            if (refugees.any) {
                refugees.foreach(u => retreat(u, r))
                log(refugees./(_.short).mkString(", "), "retreated to", r)
            }

            proceed()

        case RetreatSeparatelyAction(self, f, l) =>
            val refugees = f.forces.%(_.health == Pained)

            if (refugees.none)
                proceed()
            else
                Ask(self).each(l)(u => RetreatUnitAction(self, refugees.head.ref, u))

        case EliminateNoWayAction(self, ur) =>
            if (self.usyd(Emissary) && ur.uclass == Nyarlathotep) {
                self.log("had nowhere to retreat but", ur.short, "remained as an", self.styled(Emissary))
            }
            else {
                eliminate(game.unit(ur))

                self.log("had nowhere to retreat and eliminated", ur.short)
            }
            self.forces.foreach(_.health = Alive)
            proceed()

        case RetreatUnitAction(self, ur, r) =>
            retreat(game.unit(ur), r)
            log(ur.short, "retreated to", r)
            proceed()

        // DEVOUR
        case DevourPreBattleAction(self) =>
            val o = self.opponent
            QAsk(o.forces.exceptGOO.exceptTerror./(u => DevourAction(o, u.ref)))

        case DevourAction(self, ur) =>
            val u = game.unit(ur)
            u.faction.opponent.add(Devour)
            eliminate(u)
            log(ur.short, "was devoured by", self.opponent)
            proceed()

        // ABSORB
        case AbsorbPreBattleAction(self) =>
            val shoggoths = self.forces(Shoggoth)
            val actions = shoggoths./(u => AbsorberAction(self, u.ref)) :+ BattleCancelAction(self)

            if (shoggoths./(_.state.sorted).distinct.num == 1)
                QAsk(actions.take(1))
            else
                QAsk(actions)

        case AbsorberAction(self, ur) =>
            val u = game.unit(ur)
            QAsk(self.forces.%(_ != u).exceptGOO.exceptTerror./(t => AbsorbeeAction(self, ur, t.ref)) :+ BattleCancelAction(self))

        case AbsorbeeAction(self, ur, tr) =>
            val u = game.unit(ur)
            val t = game.unit(tr)
            0.to(t.count(Absorbed)).foreach(_ => u.add(Absorbed))
            eliminate(t)
            log(ur.short, "absorbed", tr.short, "and increased its strength by", (3 + t.count(Absorbed) * 3).str)
            proceed()

        // ABDUCT
        case AbductPreBattleAction(self) =>
            val u = self.forces(Nightgaunt).head
            QAsk(self.opponent.forces.exceptGOO.exceptTerror./(t => AbductAction(self.opponent, u.ref, t.ref)))

        case AbductAction(self, ur, tr) =>
            val u = game.unit(ur)
            val t = game.unit(tr)
            eliminate(u)
            eliminate(t)
            log(tr.short, "was abducted by", ur.short)
            proceed()

        // INVISIBILITY
        case InvisibilityPreBattleAction(self) =>
            val u = self.forces(FlyingPolyp).%(!_.has(Invised)).head
            QAsk((self.opponent.forces ++ self.forces).exceptGOO.exceptTerror./(t => InvisibilityAction(self, u.ref, t.ref)) :+ BattleCancelAction(self))

        case InvisibilityAction(self, ur, tr) =>
            val u = game.unit(ur)
            val t = game.unit(tr)
            u.add(Invised)
            exempt(t)
            if (ur == tr)
                log(ur.short, "hid itself")
            else
                log(tr.short, "was hidden by", ur.short)
            proceed()

        // SEEK AND DESTROY
        case SeekAndDestroyPreBattleAction(self) =>
            val us = self.all(HuntingHorror).%(_.region != region)
            Ask(self).each(us)(u => SeekAndDestroyAction(self, u.uclass, u.region)).battleCancel

        case SeekAndDestroyAction(self, uc, r) =>
            val u = self.at(r, uc).head
            game.move(u, region)
            self.forces :+= u
            log(u, "flew from", r)
            proceed()

        // DEMAND SACRIFICE
        case DemandSacrificePreBattleAction(self) =>
            Ask(self.opponent)
                .add(DemandSacrificeKillsArePainsAction(self.opponent))
                .add(DemandSacrificeProvideESAction(self.opponent))

        case DemandSacrificeProvideESAction(self) =>
            game.giveES(self.opponent, 1)
            self.opponent.add(DemandSacrifice)
            self.opponent.log("got", 1.es, "from", DemandSacrifice.full)
            proceed()

        case DemandSacrificeKillsArePainsAction(self) =>
            self.add(KillsArePains)
            self.log("will roll", "Kills".styled("kill"), "as", "Pains".styled("pain"), "due to", DemandSacrifice.full)
            proceed()

        // HOWL
        case HowlPreBattleAction(self) =>
            self.add(Howl)

            val e = self.opponent
            val l = e.forces.%(_.canBeMoved)

            if (l.any)
                Ask(e.real.?(e).|(self)).each(l)(u => HowlUnitAction(e, u.ref))
            else
                proceed()

        case HowlUnitAction(self, ur) =>
            QAsk(game.board.connected(region)./(r => HowlAction(self, ur, r)))

        case HowlAction(self, ur, r) =>
            val u = game.unit(ur)
            self.forces :-= u
            game.move(u, r)
            self.opponent.add(Howl)
            log(u.short, "was howled to", r)
            proceed()

        // HARBINGER
        case HarbingerPowerAction(self, ur, n) =>
            val u = game.unit(ur)
            u.add(Harbinged)
            self.power += n
            self.log("got", n.power, "as", Harbinger.full)
            proceed()

        case HarbingerESAction(self, ur, e) =>
            val u = game.unit(ur)
            u.add(Harbinged)
            game.giveES(self, e)
            self.log("gained", e.es, "as", Harbinger.full)
            proceed()

        // NECROPHAGY
        case NecrophagyAction(self, uc, r) =>
            val b4 = self.all(Ghoul).diff(attacker.forces).diff(defender.forces).diff(hidden).num

            val u = self.at(r, uc).diff(hidden).head

            game.move(u, region)
            exempt(u)
            sides.foreach(_.rolls :+= Pain)
            log(self.styled(uc), "came from", "" + r + ",", "causing additonal", Pain, "to both sides")

            proceed()

        case NecrophagyDoneAction(self) =>
            self.oncePerAction :+= Necrophagy
            proceed()

        // ETERNAL
        case EternalPayAction(self, u, result) =>
            self.power -= 1
            game.unit(u).health = Spared(Alive)
            self.log("payed", 1.power, "for", self.styled(Eternal), "to cancel", result, "on", u.short)
            proceed()

        case EternalIgnoreAction(self) =>
            self.remove(Eternal)
            proceed()

        // BERSERKERGANG
        case BerserkergangAction(self, n, u) =>
            eliminate(game.unit(u))
            log(u.short, "was eliminated with", self.styled(Berserkergang))
            if (n > 1)
                QAsk(self.forces.exceptGOO.exceptTerror./(u => BerserkergangAction(self, n - 1, u.ref)))
            else
                proceed()

         case CannibalismAction(self, r, uc) =>
             cannibalism = cannibalism.%(_ != self)
             self.log("spawned", self.styled(uc), "in", r, "with", self.styled(Cannibalism))
             game.place(self, uc, r)
             proceed()

         case CannibalismDoneAction(self) =>
             cannibalism = cannibalism.%(_ != self)
             proceed()

        // CHANNEL POWER
        case ChannelPowerAction(self, n) =>
            self.log("rerolled", (n > 0).?("Misses").|("Miss").styled("miss"), "with", self.styled(ChannelPower))
            self.power -= 1
            self.rolls = self.rolls.%(_ != Miss)
            RollBattle(self, self.styled(ChannelPower), n, x => BattleRollAction(self, x, ChannelPowerPhase))

        case ChannelPowerDoneAction(self) =>
            self.remove(ChannelPower)
            proceed()

        // MILLION FAVORED ONES
        case MillionFavoredOnesAction(self, r, uc, nw) =>
            self.log("promoted", self.styled(uc), "in", r, "to", nw./(self.styled).mkString(", "))
            val l = self.forces(uc)
            val x = l.%(_.region == r).starting | l.first // l.first for backward compatibility
            exempt(x)
            game.eliminate(x)
            nw.foreach(n => game.place(self, n, r))
            proceed()

        case MillionFavoredOnesDoneAction(self) =>
            self.remove(MillionFavoredOnes)
            proceed()

        // UNHOLY GROUND
        case UnholyGroundAction(self, o, cr, br) =>
            game.cathedrals = game.cathedrals.but(cr)

            log(self.styled("Cathedral"), "in", cr, "was removed with", self.styled(UnholyGround))

            Ask(o).each(o.forces.goos.inPlay)(u => UnholyGroundEliminateAction(o, self, br, u.ref))

        case UnholyGroundIgnoreAction(self) =>
            proceed(NecrophagyPhase)

        case UnholyGroundEliminateAction(self, f, br, ur) =>
            val u = game.unit(ur)

            eliminate(u)

            log(u.short, "was eliminated with", f.styled(UnholyGround))

            if (u.uclass == Nyogtha)
                self.forces(Nyogtha).but(u).foreach(eliminate)

            if (self.forces.goos.inPlay.any && game.cathedrals.contains(battle.region))
                Ask(f).each(game.cathedrals)(r => UnholyGroundAction(f, self, r, br)).add(UnholyGroundIgnoreAction(f))
            else
                proceed(NecrophagyPhase)

        // SHRIVELING
        case ShrivelingPreBattleAction(self) =>
            val o = self.opponent
            QAsk(o.forces.exceptGOO.exceptTerror./(u => ShrivelingAction(self, u.ref)) :+ BattleCancelAction(self))

        case ShrivelingAction(self, ur) =>
            val u = game.unit(ur)

            val p = u.cultist.?(self.opponent.recruitCost(u.uclass, region)).|(self.opponent.summonCost(u.uclass, region))

            self.add(Shriveling)

            eliminate(u)

            if (self.opponent.real) {
                self.opponent.power += p

                log(ur.short, "was shriveled and", self.opponent, "got", p.power)
            }
            else
                log(ur.short, "was shriveled")

            proceed()

        // COSMIC UNITY
        case CosmicUnityPreBattleAction(self) =>
            Ask(self).each(self.opponent.forces.goos.distinctBy(_.uclass))(u => CosmicUnityAction(self, u.ref)).battleCancel

        case CosmicUnityAction(self, ur) =>
            val u = game.unit(ur)

            self.add(CosmicUnity)

            if (u.uclass == Nyogtha && self.opponent.forces(Nyogtha).num > 1) {
                self.opponent.forces(Nyogtha).foreach(_.add(Zeroed))
                self.log("targeted both", u.faction.styled(Nyogtha), "units with", self.styled(CosmicUnity))
            } else {
                u.add(Zeroed)
                self.log("targeted", u.short, "with", self.styled(CosmicUnity))
            }

            proceed()

    }

}
