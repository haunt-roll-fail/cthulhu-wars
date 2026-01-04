package cws

import hrf.colmat._

import html._


sealed trait BattleRoll extends Record
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
    def roll() = randomInRange(1, 6) @@ {
        case 6 => Kill
        case 5 | 4 => Pain
        case 3 | 2 | 1 => Miss
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


sealed trait BattlePhase extends Record
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


trait PreBattleQuestion extends FactionAction {
    def question(implicit game : Game) = (game.battle./(_.attacker).has(self)).?("Attacker").|("Defender") + " pre-battle"
}

case class BattleDoneAction(self : Faction) extends ForcedAction
case class PreBattleDoneAction(self : Faction, next : BattlePhase) extends OptionFactionAction("Done") with PreBattleQuestion
case class BattleProceedAction(next : BattlePhase) extends ForcedAction

case class BattleRollAction(f : Faction, rolls : $[BattleRoll], next : BattlePhase) extends ForcedAction

case class AssignKillAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Kill" + (count > 1).??("s")).styled("kill"), ur.full)
case class AssignPainAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Pain" + (count > 1).??("s")).styled("pain"), ur.full)

case class RetreatOrderAction(self : Faction, a : Faction, b : Faction) extends BaseFactionAction("Retreat order", "" + a + " then " + b)

case class EliminateNoWayAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Nowhere to retreat, a pained unit is eliminated", ur.short)

case class RetreatAllAction(self : Faction, f : Faction, r : Region) extends BaseFactionAction("Retreat all pained " + f + " units to", r)
case class RetreatSeparatelyAction(self : Faction, f : Faction, destinations : $[Region]) extends BaseFactionAction(None, "Retreat separately") with More

case class RetreatUnitAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction("Retreat " + ur.short, r)


// GC
case class DevourPreBattleAction(self : Faction) extends OptionFactionAction(Devour) with PreBattleQuestion
case class DevourAction(self : Faction, ur : UnitRef) extends BaseFactionAction(Devour, ur.short)

case class AbsorbPreBattleAction(self : Faction) extends OptionFactionAction(Absorb) with PreBattleQuestion with Soft
case class AbsorberAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Absorb with", ur.full) with Soft
case class AbsorbeeAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction(g => "Absorb with " + ur.full, g => (tr.uclass == Shoggoth).??("Another ") + tr.short)

// CC
case class AbductPreBattleAction(self : Faction) extends OptionFactionAction(Abduct) with PreBattleQuestion
case class AbductAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction(Abduct, tr.full)

case class InvisibilityPreBattleAction(self : Faction) extends OptionFactionAction(Invisibility) with PreBattleQuestion with Soft
case class InvisibilityAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction("Make invisible", g => "" + tr.full + (ur == tr).??(" (self)"))

case class SeekAndDestroyPreBattleAction(self : Faction) extends OptionFactionAction(SeekAndDestroy) with PreBattleQuestion with Soft
case class SeekAndDestroyAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction("Bring with " + self.styled(SeekAndDestroy), self.styled(uc) + " from " + r)

case class HarbingerPowerAction(self : Faction, ur : UnitRef, n : Int) extends BaseFactionAction(Harbinger.full + " for " + ur.short, "Get " + n.power)
case class HarbingerESAction(self : Faction, ur : UnitRef, e : Int) extends BaseFactionAction(Harbinger.full + " for " + ur.short, "Gain " + e.es)
case class HarbingerAction(self : Faction, ur : UnitRef) extends ForcedAction

// BG
case class NecrophagyAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction(g => Necrophagy.full + " to " + g.battle.get.arena, ur.short + " from " + r)

// SL
case class DemandSacrificePreBattleAction(self : Faction) extends OptionFactionAction(DemandSacrifice) with PreBattleQuestion
case class DemandSacrificeProvideESAction(self : Faction) extends BaseFactionAction(DemandSacrifice, g => "" + self.opponent(g.battle.get) + " gains " + 1.es)
case class DemandSacrificeKillsArePainsAction(self : Faction) extends BaseFactionAction(DemandSacrifice, "Rolled " + "Kills".styled("kill") + " become " + "Pains".styled("pain"))

// WW
case class HowlPreBattleAction(self : Faction) extends OptionFactionAction(Howl) with PreBattleQuestion
case class HowlUnitAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Retreat unit from " + Howl.full, ur.full)
case class HowlAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction("Retreat " + ur.short + " to", r)

case class EternalPayAction(self : Faction, u : UnitRef, result : BattleRoll) extends BaseFactionAction("Save " + u.short + " from " + result, "Pay " + 1.power + " for " + self.styled(Eternal))

case class BerserkergangAction(self : Faction, n : Int, u : UnitRef) extends BaseFactionAction(Berserkergang.full + " eliminates" + (n == 1).?("").|(" " + n + " units"), u.short)

case class CannibalismAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Spawn with " + self.styled(Cannibalism) + " in " + r, self.styled(uc))

// OW
case class ChannelPowerAction(self : Faction, n : Int) extends BaseFactionAction(self.styled(ChannelPower), "Reroll " + n + " " + (n > 1).?("Misses").|("Miss").styled("miss") + " for " + 1.power)

case class MillionFavoredOnesAction(self : Faction, r : Region, uc : UnitClass, nw : $[UnitClass]) extends BaseFactionAction(self.styled(MillionFavoredOnes), self.styled(uc) + " in " + r + " to " + self.styled((nw.num > 1).?("" + nw.num + " " + nw(0).plural).|(nw(0).name)))

// AN
case class UnholyGroundAction(self : Faction, o : Faction, cr : Region) extends BaseFactionAction("Remove a cathedral with " + UnholyGround.full, cr)
case class UnholyGroundEliminateAction(self : Faction, f : Faction, ur : UnitRef) extends BaseFactionAction(g => "Choose a GOO to eliminate in " + g.battle.get.arena, ur.short)

// Independent Great Old Ones
case class CosmicUnityPreBattleAction(self : Faction) extends OptionFactionAction(self.styled(CosmicUnity)) with PreBattleQuestion
case class CosmicUnityAction(self : Faction, ur : UnitRef) extends BaseFactionAction("Target for " + self.styled(CosmicUnity), ur.short)

// Neutral Spellbooks
case class ShrivelingPreBattleAction(self : Faction) extends OptionFactionAction(Shriveling) with PreBattleQuestion
case class ShrivelingAction(self : Faction, ur : UnitRef) extends BaseFactionAction(Shriveling, ur.short)


trait BattleImplicits {
    implicit class BattleFactionEx(f : Faction) {
        def opponent(implicit battle : Battle) : Faction =
            f match {
                case f if f == battle.attacker => battle.defender
                case f if f == battle.defender => battle.attacker
                case _ => throw new Error("faction " + f.name + " is not a side in the battle")
            }

    }

    implicit def factionToSide(f : Faction)(implicit battle : Battle) : Side =
        f match {
            case f if f == battle.attacker => battle.attackers
            case f if f == battle.defender => battle.defenders
            case _ => throw new Error("faction " + f.name + " is not a side in the battle")
        }

}


class Side(private val self : Faction, var forces : $[UnitFigure], var str : Int, var rolls : $[BattleRoll], var effects : $[BattleSpellbook])(implicit val game : Game) {
    def tag(s : BattleSpellbook) = effects.has(s)
    def add(s : BattleSpellbook) { effects :+= s }
    def remove(s : BattleSpellbook) { effects = effects.but(s) }
    def count(s : BattleSpellbook) = effects.count(s)
}

class Battle(val arena : Region, val attacker : Faction, val defender : Faction, val effect : |[Spellbook])(implicit val game : Game) {
    implicit val battle : Battle = this

    val attackers = new Side(attacker, $, 0, $, $)
    val defenders = new Side(defender, $, 0, $, $)

    val sides = $(attacker, defender)

    var phase : BattlePhase = BattleStart

    var hidden : $[UnitFigure] = $

    def exempt(u : UnitFigure) {
        u.state = $
        u.add(Hidden)
        hidden :+= u
        sides.foreach(_.forces :-= u)
    }

    def eliminate(u : UnitFigure) {
        exempt(u)
        game.eliminate(u)
        u.faction.satisfy(LoseUnitInBattle, "Lose " + u.short + " in battle")
    }

    def retreat(u : UnitFigure, r : Region) = {
        u.region = r
        u.onGate = false
        u.add(Retreated)
        u.health = Alive
    }

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

            return jump(PostBattlePhase)
        }

        var options : $[FactionAction] = $

        if (s.has(Devour) && s.tag(Devour).not && s.forces(Cthulhu).any && s.opponent.forces.vulnerable.any)
            options :+= DevourPreBattleAction(s)

        if (s.has(Shriveling) && s.tag(Shriveling).not && s.opponent.forces.vulnerable.any)
            options :+= ShrivelingPreBattleAction(s)

        if (s.has(Absorb) && s.forces(Shoggoth).any && s.forces.vulnerable.num > 1)
            options :+= AbsorbPreBattleAction(s)

        if (s.has(Howl) && s.tag(Howl).not && s.forces(Wendigo).any && s.opponent.forces.%(_.canMove).any)
            options :+= HowlPreBattleAction(s)

        if (s.has(Abduct) && s.forces(Nightgaunt).any && s.opponent.forces.vulnerable.any)
            options :+= AbductPreBattleAction(s)

        if (s.has(SeekAndDestroy) && s.all(HuntingHorror).%(_.region != arena).any)
            options :+= SeekAndDestroyPreBattleAction(s)

        if (s.has(Invisibility) && s.forces(FlyingPolyp).not(Invised).any)
            options :+= InvisibilityPreBattleAction(s)

        if (s.has(DemandSacrifice) && s.tag(DemandSacrifice).not && s.opponent.tag(KillsArePains).not)
            if (game.options.has(DemandTsathoggua).?(s.forces(Tsathoggua).any).|(s.has(Tsathoggua)))
                options :+= DemandSacrificePreBattleAction(s)

        if (s.has(CosmicUnity) && s.tag(CosmicUnity).not && s.forces(Daoloth).any && s.opponent.forces.goos.any)
            options :+= CosmicUnityPreBattleAction(s)

        Ask(s).list(options).add(PreBattleDoneAction(s, next))
    }

    def preroll(s : Faction) {
        val str = s.strength(s.forces, s.opponent)

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

        if (s.has(UnholyGround))
            s.add(UnholyGround)
    }

    def postroll(s : Faction) {
        if (s.tag(Regenerate))
            s.forces(Starspawn).foreach(_.health = DoubleHP(Alive, Alive))

        if (s.tag(KillsArePains)) {
            if (s.rolls.exists(_ == Kill)) {
                s.rolls = s.rolls./(_.useIf(_ == Kill)(_ => Pain))
            }
        }
    }

    def assigner(s : Faction) : Faction =
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
            s.forces.foreach(u => 1.to(canAssignKills(u)).foreach(_ => assignKill(u)))
            return DelayedContinue(100, Then(BattleProceedAction(next)))
        }

        val f = assigner(s)

        if (f != s && assigned == 0)
            log(f, "assigned kills with", f.styled(Vengeance))

        return DelayedContinue(50, Ask(f, s.forces.%(u => canAssignKills(u) > 0).sortBy(_.uclass.cost)./(u => AssignKillAction(f, kills - assigned, s, u))))
    }

    def assignPains(s : Faction, next : BattlePhase) : Continue = {
        val pains = s.opponent.rolls.count(_ == Pain)
        val assigned = s.forces./(assignedPains).sum
        val canAssign = s.forces./(canAssignPains).sum

        if (pains <= assigned)
            return BattleProceedAction(next)

        if (pains >= assigned + canAssign) {
            s.forces.foreach(u => 1.to(canAssignPains(u)).foreach(_ => assignPain(u)))
            return DelayedContinue(100, Then(BattleProceedAction(next)))
        }

        val f = assigner(s)

        if (f != s && assigned == 0 && s.real)
            log(f, "assigned pains with", f.styled(Vengeance))

        return DelayedContinue(50, Ask(f, s.forces.%(u => canAssignPains(u) > 0).sortBy(_.uclass.cost)./(u => AssignPainAction(f, pains - assigned, s, u))))
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

        val destinations = arena.connected.%(r => s.opponent.at(r).none)

        val chooser : Faction = retreater(s)

        if (destinations.none)
            Ask(s).each(refugees.sortBy(_.uclass.cost))(u => EliminateNoWayAction(s, u))
        else
        if (destinations.num == 1) {
            val r = destinations.only

            refugees.foreach(u => retreat(u, r))

            log(refugees./(_.short).mkString(", "), "retreated to", r)

            proceed()
        }
        else
        if (refugees.num == 1 || s.forces.tag(Retreated).any)
            Ask(chooser).each(destinations)(d => RetreatUnitAction(chooser, refugees.first, d))
        else
            Ask(chooser).each(destinations)(d => RetreatAllAction(chooser, s, d)).add(RetreatSeparatelyAction(chooser, s, destinations))
    }

    def checkKillSpellbooks(s : Faction) {
        if (s.needs(KillDevour1) || s.needs(KillDevour2)) {
            var devoured = s.count(Devour)
            var kills = s.opponent.forces.count(_.health == Killed)

            if (devoured + kills >= 2) {
                if (s.needs(KillDevour2)) {
                    if (kills >= 2) {
                        s.satisfy(KillDevour2, "Kill two enemy units in a battle")
                        kills -= 2
                    }
                    else {
                        s.satisfy(KillDevour2, "Kill and Devour two enemy units in a battle")
                        devoured -= 1
                        kills -= 1
                    }
                }
            }

            if (devoured + kills >= 1) {
                if (s.needs(KillDevour1)) {
                    if (devoured == 1) {
                        s.satisfy(KillDevour1, "Devour an enemy unit in a battle")
                        devoured -= 1
                    }
                    else {
                        s.satisfy(KillDevour1, "Kill an enemy unit in a battle")
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

    def jump(bp : BattlePhase) : Continue = {
        phase = bp
        proceed()
    }

    def proceed() : Continue = {
        phase match {
            case BattleStart =>
                attacker.forces = attacker.at(arena)

                if (attacker.forces.none) {
                    log("No attackers left to battle")

                    return jump(PostBattlePhase)
                }

                defender.forces = defender.at(arena)

                if (defender.forces.none) {
                    log("No defenders left to battle")

                    return jump(PostBattlePhase)
                }

                sides.foreach(s => s.str = s.strength(s.forces, s.opponent))

                sides.foreach { s =>
                    if (s.upgrades.has(NightmareWeb).not)
                        s.forces(Nyogtha).foreach { u =>
                            if (s.opponent.forces.goos.any)
                                s.oncePerAction :+= NyogthaPrimed
                        }
                }

                log(attacker, "attacked with", attacker.forces./(_.short).mkString(", "), "" + attacker.str.str)

                attacker.forces(Nyogtha).foreach { u =>
                    log(u, "had its strength at", 4.str, "while attacking")
                }

                log(defender, "defended with", defender.forces./(_.short).mkString(", "), "" + defender.str.str)

                jump(AttackerPreBattle)

            case AttackerPreBattle =>
                prebattle(attacker, DefenderPreBattle)

            case DefenderPreBattle =>
                prebattle(defender, PreRoll)

            case PreRoll =>
                preroll(attacker)
                preroll(defender)

                jump(RollAttackers)

            case RollAttackers =>
                RollBattle(attacker, "attack", attacker.str, x => BattleRollAction(attacker, x, RollDefenders))

            case RollDefenders =>
                RollBattle(defender, "defense", defender.str, x => BattleRollAction(defender, x, ChannelPowerPhase))

            case ChannelPowerPhase =>
                sides.of[OW].foreach { s =>
                    if (s.tag(ChannelPower)) {
                        s.remove(ChannelPower)

                        if (s.rolls.%(_ == Miss).any)
                            if (s.rolls.%(_ == Kill).num < s.opponent.forces./(canAssignKills).sum)
                                if (s.power > 0)
                                    return Ask(s).add(ChannelPowerAction(s, s.rolls.%(_ == Miss).num)).skip(BattleDoneAction(s))
                                else
                                if (s.want(DragonAscending) && factions.%(_.power > 0).any)
                                    return DragonAscendingAskAction(s, |(s), ChannelPower.full, BattleDoneAction(s))
                    }
                }

                jump(PostRoll)

            case PostRoll =>
                postroll(attacker)
                postroll(defender)

                jump(AssignDefenderKills)

            case AssignDefenderKills =>
                assignKills(defender, AssignAttackerKills)

            case AssignAttackerKills =>
                assignKills(attacker, AllKillsAssignedPhase)

            case AllKillsAssignedPhase =>
                sides.foreach { s =>
                    if (s.tag(Emissary)) {
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

                jump(HarbingerKillPhase)

            case HarbingerKillPhase =>
                sides.foreach { s =>
                    if (s.tag(Harbinger)) {
                        s.opponent.units.goos.%(_.health == Killed).not(Harbinged).some.foreach { l =>
                            return Ask(s).add(HarbingerPowerAction(s, l.first, l.first.uclass.cost / 2)).add(HarbingerESAction(s, l.first, 2))
                        }
                    }
                }

                jump(EternalKillPhase)

            case EternalKillPhase =>
                sides.foreach { s =>
                    if (s.tag(Eternal) && s.power > 0) {
                        val rt = s.forces(RhanTegoth).%(_.health == Killed)

                        if (rt.any) {
                            if (s.power > s.enemies./(_.power).max && s.enemies.exists(_.want(DragonAscending)))
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s, Eternal.full, BattleDoneAction(s)))

                            s.remove(Eternal)

                            return Ask(s).each(rt)(u => EternalPayAction(s, u, Kill)).skip(BattleDoneAction(s))
                        }
                    }
                }

                jump(EliminatePhase)

            case EliminatePhase =>
                checkKillSpellbooks(attacker)
                checkKillSpellbooks(defender)

                factions.%(_.has(Cannibalism)).%(f => sides.but(f).%(_.forces.%(_.health == Killed).any).any).foreach { f =>
                    f.oncePerAction :+= Cannibalism
                }

                sides.foreach { s =>
                    if (s.has(Berserkergang))
                        s.forces(GnophKeh).%(_.health == Killed).foreach(_ => s.add(Berserkergang))

                    checkByatisSpellbook(s)
                }

                sides.foreach { s =>
                    s.forces.%(_.health == Killed).foreach(eliminate)
                }

                jump(BerserkergangPhase)

            case BerserkergangPhase =>
                sides.foreach { s =>
                    if (s.tag(Berserkergang)) {
                        val count = s.count(Berserkergang)
                        s.remove(Berserkergang)
                        val targets = s.opponent.forces.vulnerable
                        if (targets.any)
                            if (count >= targets.num) {
                                log(targets./(_.short).mkString(", ") + (targets.num > 1).?(" were ").|(" was ") + "eliminated with", s.styled(Berserkergang))
                                targets.foreach(eliminate)
                            }
                            else
                                return Ask(s.opponent).each(targets)(u => BerserkergangAction(s.opponent, count, u))
                    }
                }

                jump(UnholyGroundPhase)

            case UnholyGroundPhase =>
                sides.foreach { s =>
                    if (s.tag(UnholyGround)) {
                        s.remove(UnholyGround)

                        if (s.opponent.forces.goos.any && game.cathedrals.has(arena))
                            return Ask(s).each(game.cathedrals)(r => UnholyGroundAction(s, s.opponent, r)).skip(BattleDoneAction(s))
                    }
                }

                factions.%(_.has(Necrophagy)).foreach { f =>
                    f.oncePerAction :+= Necrophagy
                }

                jump(NecrophagyPhase)

            case NecrophagyPhase =>
                factions.%(_.oncePerAction.has(Necrophagy)).foreach { f =>
                    f.oncePerAction :-= Necrophagy

                    return Ask(f)
                        .each(f.all(Ghoul).diff(attacker.forces).diff(defender.forces).diff(hidden))(u => NecrophagyAction(f, u, u.region))
                        .done(BattleDoneAction(f))
                }

                jump(AssignDefenderPains)

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

                jump(HarbingerPainPhase)

            case HarbingerPainPhase =>
                sides.foreach { s =>
                    if (s.tag(Harbinger)) {
                        s.opponent.units.goos.%(_.health == Pained).not(Harbinged).some.foreach { l =>
                            return Ask(s).add(HarbingerPowerAction(s, l.first, l.first.uclass.cost / 2)).add(HarbingerESAction(s, l.first, 2))
                        }
                    }
                }

                jump(EternalPainPhase)

            case EternalPainPhase =>
                sides.foreach { s =>
                    if (s.tag(Eternal) && s.power > 0) {
                        val rt = s.forces(RhanTegoth).%(_.health == Pained)

                        if (rt.any) {
                            if (s.power > s.enemies./(_.power).max && s.enemies.exists(_.want(DragonAscending)))
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s, Eternal.full, BattleDoneAction(s)))

                            s.remove(Eternal)

                            return Ask(s).each(rt)(u => EternalPayAction(s, u, Pain)).skip(BattleDoneAction(s))
                        }
                    }
                }

                jump(MadnessPhase)

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
                    val f = retreater(attacker)

                    Ask(f).add(RetreatOrderAction(f, attacker, defender)).add(RetreatOrderAction(f, defender, attacker))
                }
                else {
                    jump(AttackerDefenderRetreats)
                }

            case AttackerDefenderRetreats =>
                if (attackers.forces.%(_.health == Pained).any)
                    retreat(attacker)
                else
                if (defenders.forces.%(_.health == Pained).any)
                    retreat(defender)
                else
                    jump(PostBattlePhase)

            case DefenderAttackerRetreats =>
                if (defenders.forces.%(_.health == Pained).any)
                    retreat(defender)
                else
                if (attackers.forces.%(_.health == Pained).any)
                    retreat(attacker)
                else
                    jump(PostBattlePhase)

            case PostBattlePhase =>
                game.checkGatesLost()

                sides.foreach { s =>
                    if (s.tag(MillionFavoredOnes)) {
                        s.remove(MillionFavoredOnes)

                        val options = s.forces./~(u => u.uclass match {
                            case Acolyte if s.pool(Mutant).any      => |(MillionFavoredOnesAction(s, u.region, u.uclass, $(Mutant)))
                            case Mutant if s.pool(Abomination).any  => |(MillionFavoredOnesAction(s, u.region, u.uclass, $(Abomination)))
                            case Abomination if s.pool(SpawnOW).any => |(MillionFavoredOnesAction(s, u.region, u.uclass, $(SpawnOW)))
                            case SpawnOW if s.pool(Mutant).any      => |(MillionFavoredOnesAction(s, u.region, u.uclass, s.pool(Mutant).num.times(Mutant)))
                            case _ => None
                        })

                        return Ask(s).list(options).done(BattleDoneAction(s))
                    }
                }

                factions.%(_.oncePerAction.has(Cannibalism)).foreach { f =>
                    f.oncePerAction :-= Cannibalism

                    return Ask(f)
                        .when(f.pool(Acolyte).any)(CannibalismAction(f, arena, Acolyte))
                        .when(f.pool(Wendigo).any)(CannibalismAction(f, arena, Wendigo))
                        .skip(BattleDoneAction(f))
                }

                jump(BattleEnd)

            case BattleEnd =>
                sides.foreach(_.forces.foreach(_.remove(Retreated)))
                sides.foreach(_.forces.foreach(_.remove(Zeroed)))
                hidden.foreach(_.remove(Hidden))

                attacker.battled :+= arena

                if (game.nexed.none && attacker.hasAllSB.not)
                    attacker.acted = true

                game.battle = None

                if (game.queue.starting.?(_.effect.has(FromBelow)))
                    ProceedBattlesAction
                else
                    AfterAction(attacker)

        }
    }

    def perform(a : Action) : Continue = a match {
        // PROCEED
        case BattleDoneAction(self) =>
            proceed()

        case BattleProceedAction(bf) =>
            jump(bf)

        case PreBattleDoneAction(self, bf) =>
            jump(bf)

        // ROLL
        case BattleRollAction(f, rolls, next) =>
            f.rolls ++= rolls

            val sv = f.forces(StarVampire)

            if (rolls.num > sv.num)
                log(f, "rolled", rolls.drop(sv.num).mkString(" "))

            0.until(sv.num).foreach { i =>
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
                f.satisfy(Roll6DiceInBattle, "Roll " + rolls.num + " dice in Battle")

            jump(next)

        // ASSIGN
        case AssignKillAction(_, _, _, u) =>
            assignKill(u)
            proceed()

        case AssignPainAction(_, _, _, u) =>
            assignPain(u)
            proceed()

        // RETREAT
        case RetreatOrderAction(self, a, b) =>
            if (a == attacker)
                jump(AttackerDefenderRetreats)
            else
                jump(DefenderAttackerRetreats)

        case RetreatUnitAction(self, u, r) =>
            retreat(u, r)
            log(u.short, "retreated to", r)
            proceed()

        case RetreatAllAction(self, f, r) =>
            val refugees = f.forces.%(_.health == Pained)

            if (refugees.any) {
                refugees.foreach(u => retreat(u, r))
                log(refugees./(_.short).mkString(", "), "retreated to", r)
            }

            proceed()

        case RetreatSeparatelyAction(self, f, l) =>
            val u = f.forces.%(_.health == Pained).first

            Ask(self).each(l)(r => RetreatUnitAction(self, u, r))

        case EliminateNoWayAction(self, u) =>
            if (self.tag(Emissary) && u.uclass == Nyarlathotep) {
                self.log("had nowhere to retreat but", u.short, "remained as an", self.styled(Emissary))
            }
            else {
                eliminate(u)

                self.log("had nowhere to retreat and eliminated", u.short)
            }
            self.forces.foreach(_.health = Alive)
            proceed()

        // DEVOUR
        case DevourPreBattleAction(self) =>
            Ask(self.opponent).each(self.opponent.forces.vulnerable)(u => DevourAction(self.opponent, u))

        case DevourAction(self, u) =>
            u.faction.opponent.add(Devour)
            eliminate(u)
            log(u.short, "was devoured by", self.opponent)
            proceed()

        // ABSORB
        case AbsorbPreBattleAction(self) =>
            val shoggoths = self.forces(Shoggoth)
            val actions = shoggoths./(u => AbsorberAction(self, u))

            if (shoggoths./(_.state.sorted).distinct.num == 1)
                Ask(self).list(actions.take(1))
            else
                Ask(self).list(actions).cancel

        case AbsorberAction(self, u) =>
            Ask(self).each(self.forces.but(u).vulnerable)(t => AbsorbeeAction(self, u, t)).cancel

        case AbsorbeeAction(self, u, t) =>
            0.to(t.count(Absorbed)).foreach(_ => u.add(Absorbed))
            eliminate(t)
            log(u.short, "absorbed", t.short, "and increased its strength by", (3 + t.count(Absorbed) * 3).str)
            proceed()

        // ABDUCT
        case AbductPreBattleAction(self) =>
            val u = self.forces(Nightgaunt).head
            Ask(self.opponent).each(self.opponent.forces.vulnerable)(t => AbductAction(self.opponent, u, t))

        case AbductAction(self, u, t) =>
            eliminate(u)
            eliminate(t)
            log(t.short, "was abducted by", u.short)
            proceed()

        // INVISIBILITY
        case InvisibilityPreBattleAction(self) =>
            val u = self.forces(FlyingPolyp).not(Invised).head
            Ask(self).each((self.opponent.forces ++ self.forces).vulnerable)(t => InvisibilityAction(self, u, t)).cancel

        case InvisibilityAction(self, u, t) =>
            u.add(Invised)
            exempt(t)
            if (u == t)
                log(u.short, "hid itself")
            else
                log(t.short, "was hidden by", u.short)
            proceed()

        // SEEK AND DESTROY
        case SeekAndDestroyPreBattleAction(self) =>
            val us = self.all(HuntingHorror).%(_.region != arena)
            Ask(self).each(us)(u => SeekAndDestroyAction(self, u.uclass, u.region)).cancel

        case SeekAndDestroyAction(self, uc, r) =>
            val u = self.at(r, uc).head
            u.region = arena
            self.forces :+= u
            log(u, "flew from", r)
            proceed()

        // DEMAND SACRIFICE
        case DemandSacrificePreBattleAction(self) =>
            Ask(self.opponent)
                .add(DemandSacrificeKillsArePainsAction(self.opponent))
                .add(DemandSacrificeProvideESAction(self.opponent))

        case DemandSacrificeProvideESAction(self) =>
            self.opponent.takeES(1)
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

            Ask(e.real.?(e).|(self)).each(e.forces.%(_.canBeMoved))(u => HowlUnitAction(e, u))

        case HowlUnitAction(self, ur) =>
            Ask(self).each(arena.connected)(r => HowlAction(self, ur, r))

        case HowlAction(self, u, r) =>
            self.forces :-= u
            u.region = r
            u.onGate = false
            log(u.short, "was howled to", r)
            proceed()

        // HARBINGER
        case HarbingerPowerAction(self, u, n) =>
            self.power += n
            self.log("got", n.power, "as", Harbinger.full)

            HarbingerAction(self, u)

        case HarbingerESAction(self, u, e) =>
            self.takeES(e)
            self.log("gained", e.es, "as", Harbinger.full)

            HarbingerAction(self, u)

        case HarbingerAction(self, u) =>
            u.add(Harbinged)

            if (u.uclass == Nyogtha)
                u.faction.forces(Nyogtha).but(u).foreach(_.add(Harbinged))

            proceed()

        // NECROPHAGY
        case NecrophagyAction(self, u, r) =>
            self.oncePerAction :+= Necrophagy

            u.region = arena
            exempt(u)
            sides.foreach(_.rolls :+= Pain)
            log(u, "came from", "" + r + ",", "causing additonal", Pain, "to both sides")

            proceed()

        // ETERNAL
        case EternalPayAction(self, u, result) =>
            self.power -= 1
            u.health = Spared(Alive)
            self.log("payed", 1.power, "for", self.styled(Eternal), "to cancel", result, "on", u.short)
            proceed()

        // BERSERKERGANG
        case BerserkergangAction(self, n, u) =>
            eliminate(u)
            log(u.short, "was eliminated with", self.styled(Berserkergang))
            if (n > 1)
                Ask(self).each(self.forces.vulnerable)(t => BerserkergangAction(self, n - 1, t))
            else
                proceed()

         case CannibalismAction(self, r, uc) =>
             self.log("spawned", self.styled(uc), "in", r, "with", self.styled(Cannibalism))
             self.place(uc, r)
             proceed()

        // CHANNEL POWER
        case ChannelPowerAction(self, n) =>
            self.add(ChannelPower)
            self.power -= 1
            self.rolls = self.rolls.%(_ != Miss)
            self.log("rerolled", (n > 0).?("Misses").|("Miss").styled("miss"), "with", self.styled(ChannelPower))
            RollBattle(self, self.styled(ChannelPower), n, x => BattleRollAction(self, x, ChannelPowerPhase))

        // MILLION FAVORED ONES
        case MillionFavoredOnesAction(self, r, uc, nw) =>
            self.add(MillionFavoredOnes)
            val t = self.forces(uc).%(_.region == r).first
            exempt(t)
            game.eliminate(t)
            nw.foreach(n => self.place(n, r))
            self.log("promoted", t, "in", r, "to", nw./(self.styled).mkString(", "))
            proceed()

        // UNHOLY GROUND
        case UnholyGroundAction(self, o, cr) =>
            self.add(UnholyGround)
            game.cathedrals :-= cr
            log(self.styled("Cathedral"), "in", cr, "was removed with", self.styled(UnholyGround))
            Ask(o).each(o.forces.goos.distinctBy(_.uclass))(u => UnholyGroundEliminateAction(o, self, u))

        case UnholyGroundEliminateAction(self, f, u) =>
            if (u.uclass == Nyogtha && self.all(Nyogtha).num > 1) {
                self.forces(Nyogtha).foreach(eliminate)

                self.all(Nyogtha).foreach(game.eliminate)

                log("All", u.short, "were eliminated with", f.styled(UnholyGround))
            }
            else {
                eliminate(u)

                log(u.short, "was eliminated with", f.styled(UnholyGround))
            }

            proceed()

        // SHRIVELING
        case ShrivelingPreBattleAction(self) =>
            Ask(self).each(self.opponent.forces.vulnerable)(u => ShrivelingAction(self, u)).cancel

        case ShrivelingAction(self, u) =>
            self.add(Shriveling)

            val p = u.cultist.?(self.opponent.recruitCost(u.uclass, arena)).|(self.opponent.summonCost(u.uclass, arena))

            eliminate(u)

            if (self.opponent.real) {
                self.opponent.power += p

                log(u.short, "was shriveled and", self.opponent, "got", p.power)
            }
            else
                log(u.short, "was shriveled")

            proceed()

        // COSMIC UNITY
        case CosmicUnityPreBattleAction(self) =>
            Ask(self).each(self.opponent.forces.goos.distinctBy(_.uclass))(u => CosmicUnityAction(self, u)).cancel

        case CosmicUnityAction(self, u) =>
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
