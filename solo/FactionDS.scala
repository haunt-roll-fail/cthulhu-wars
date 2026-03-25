package cws

import hrf.colmat._
import html._

case object LarvaThesis extends FactionUnitClass(DS, "Larva Thesis", Monster, 1)
case object LarvaAntithesis extends FactionUnitClass(DS, "Larva Antithesis", Monster, 1)
case object LarvaSynthesis extends FactionUnitClass(DS, "Larva Synthesis", Monster, 1)
case object ChaosGate extends FactionUnitClass(DS, "Chaos Gate", Building, 1)
case object AvatarThesis extends FactionUnitClass(DS, "Avatar Thesis", GOO, 0)
case object AvatarAntithesis extends FactionUnitClass(DS, "Avatar Antithesis", GOO, 8)
case object AvatarSynthesis extends FactionUnitClass(DS, "Avatar Synthesis", GOO, 8)

case object Psychosis extends FactionSpellbook(DS, "Psychosis")
case object CosmicRuler extends FactionSpellbook(DS, "Cosmic Ruler")

case object AnimateMatter extends FactionSpellbook(DS, "Animate Matter")
case object ChaosGateSB extends FactionSpellbook(DS, "Chaos Gate")
case object Consummation extends FactionSpellbook(DS, "Consummation")
case object FiendishGrowth extends FactionSpellbook(DS, "Fiendish Growth")
case object Traitors extends FactionSpellbook(DS, "Traitors!")
case object UndirectedEnergy extends FactionSpellbook(DS, "Undirected Energy")

case object OneLarvaEach extends Requirement("One of each Larva in play")
case object AbandonedGateGP extends Requirement("Abandoned gate in Gather Power")
case object PowerDoomOffer extends Requirement("Doom Phase Power/Doom giveaway")
case object AwakenAvatarThesis extends Requirement("Awaken Avatar Thesis")
case object AwakenAvatarAntithesis extends Requirement("Awaken Avatar Antithesis")
case object AwakenAvatarSynthesis extends Requirement("Awaken Avatar Synthesis")

// Avatar Thesis awakening: pick azathoth track value (cost 0-8), then distribute power
case class UndirectedEnergyAction(self : Faction) extends OptionFactionAction(UndirectedEnergy.styled(self)) with MainQuestion

case class ChaosGateSBAction(self : Faction) extends OptionFactionAction(ChaosGateSB.styled(self)) with MainQuestion with Soft
case class ChaosGateSBPlaceAction(self : Faction, r : Region) extends BaseFactionAction("Place Chaos Gate in", implicit g => r + self.iced(r))

case class ConsummationAction(self : Faction) extends OptionFactionAction(Consummation.styled(self)) with MainQuestion with Soft
case class ConsummationUnflipAction(self : Faction, sb : Spellbook) extends BaseFactionAction("Unflip", sb.styled(self))

case class CosmicRulerSacrificeAction(self : Faction, saved : UnitRef, sacrificed : UnitRef) extends ForcedAction
case class CosmicRulerDeclineAction(self : Faction) extends ForcedAction
case class CosmicRulerDeclineNoWayAction(self : Faction, u : UnitRef) extends ForcedAction

case class PowerDoomOfferAction(self : Faction) extends BaseFactionAction("Doom phase", "Power/Doom offer")
case class PowerDoomOfferChoiceAction(self : Faction, remaining : $[Faction], powerCount : Int, doomCount : Int) extends ForcedAction
case class PowerDoomChoicePowerAction(self : Faction, target : Faction, remaining : $[Faction], powerCount : Int, doomCount : Int) extends BaseFactionAction(implicit g => self + " offers you your choice of", "Gain 1 Power")
case class PowerDoomChoiceDoomAction(self : Faction, target : Faction, remaining : $[Faction], powerCount : Int, doomCount : Int) extends BaseFactionAction(implicit g => self + " offers you your choice of", "Gain 1 Doom")

case class PsychosisAction(self : Faction) extends OptionFactionAction(Psychosis.styled(self)) with MainQuestion with Soft
case class PsychosisPlaceAction(self : Faction, r : Region) extends BaseFactionAction("Place Acolyte in", implicit g => r + self.iced(r))

case class AnimateMatterAction(self : Faction) extends OptionFactionAction(AnimateMatter.styled(self)) with MainQuestion with Soft
case class AnimateMatterFromAction(self : Faction, from : Region) extends BaseFactionAction("Move Chaos Gate from", from) with Soft
case class AnimateMatterMoveAction(self : Faction, from : Region, to : Region) extends BaseFactionAction("Move Chaos Gate to", implicit g => to + self.iced(to))

case class TraitorsAction(self : Faction) extends OptionFactionAction(Traitors.styled(self)) with MainQuestion with Soft
case class TraitorsChaosGateAction(self : Faction, r : Region) extends BaseFactionAction("Convert Chaos Gate in", implicit g => r + self.iced(r))
case class TraitorsFactionAction(self : Faction, r : Region, target : Faction, uc : UnitClass) extends BaseFactionAction(implicit g => "Place " + target + " " + uc + " in", r)

case class FiendishGrowthAction(self : Faction) extends OptionFactionAction(FiendishGrowth.styled(self)) with MainQuestion with Soft
case class FiendishGrowthPlaceAction(self : Faction, r : Region, n : Int) extends ForcedAction
case class FiendishGrowthPlaceUnitAction(self : Faction, r : Region, uc : UnitClass, n : Int) extends BaseFactionAction("Place in " + r, uc.styled(self))

case class DSAvatarThesisRegionAction(self : Faction, r : Region) extends BaseFactionAction("Awaken " + AvatarThesis.styled(self) + " in", r) with Soft

case class DSAvatarThesisCostAction(self : Faction, r : Region, track : Int)
    extends BaseFactionAction(implicit g => "Set Azathoth track to " + track + ", pay " + track.power, r)

case class DSAvatarThesisPowerAction(self : Faction) extends ForcedAction

case class DSAvatarThesisAllPowerAction(self : Faction)
    extends BaseFactionAction(AvatarThesis.styled(DS), "All opponents gain 1 Power")

case class DSAvatarThesisOnePowerAction(self : Faction, target : Faction)
    extends BaseFactionAction(AvatarThesis.styled(DS), implicit g => target + " gains 2 Power")

case class DSAvatarAntithesisChoiceAction(self : Faction) extends ForcedAction

case class DSAvatarAntithesisAllDoomAction(self : Faction)
    extends BaseFactionAction(AvatarAntithesis.styled(DS), "All opponents gain 1 Doom")

case class DSAvatarAntithesisOneESAction(self : Faction, target : Faction)
    extends BaseFactionAction(AvatarAntithesis.styled(DS), implicit g => target + " gains 1 Elder Sign")

case class AzathothOffer(f : Faction, p : Int, d : Int) { def n : Int = p + d }

case class AzathothSynthesisRollAction(self : Faction, x : Int) extends ForcedAction
case class AzathothSynthesisContinueAction(ds : Faction, x : Int, offers : $[AzathothOffer], forum : $[Faction], time : Int) extends ForcedAction

case class AzathothSynthesisPowerAskAction(ds : Faction, x : Int, offers : $[AzathothOffer], forum : $[Faction], time : Int, self : Faction, p : Int) extends BaseFactionAction(
    g => "Azathoth demands " + x.styled("doom") + " Power/Doom total<br/>" + offers./(o => "" + o.f + ": " + (o.n > 0).?((o.p > 0).?((o.d > 0).?(o.p.styled("power") + "+" + o.d.styled("doom")).|((o.p.styled("power")))).|((o.d.styled("doom")))).|("none")).mkString("<br/>") + "<hr/>" + self + " — Power?",
    (p < 0).?("Refuse to negotiate").|((p > 0).?(p.styled("power")).|("0 Power"))
)

case class AzathothSynthesisDoomAskAction(ds : Faction, x : Int, offers : $[AzathothOffer], forum : $[Faction], time : Int, self : Faction, p : Int, d : Int) extends BaseFactionAction(
    g => "Azathoth demands " + x.styled("doom") + " Power/Doom total<br/>" + offers./(o => "" + o.f + ": " + (o.n > 0).?((o.p > 0).?((o.d > 0).?(o.p.styled("power") + "+" + o.d.styled("doom")).|((o.p.styled("power")))).|((o.d.styled("doom")))).|("none")).mkString("<br/>") + "<hr/>" + self + " offered " + (p > 0).?(p.styled("power") + " Power").|("0 Power") + " — Doom?",
    (d < 0).?("Refuse to negotiate").|((d > 0).?(d.styled("doom")).|("0 Doom"))
)


object DSExpansion extends Expansion {
    override def triggers()(implicit game : Game) {
        val f = DS
        f.satisfyIf(OneLarvaEach, "Have one of each Larva type in play",
            f.all(LarvaThesis).any && f.all(LarvaAntithesis).any && f.all(LarvaSynthesis).any)
        f.satisfyIf(AbandonedGateGP, "Abandoned gate in Gather Power",
            game.gatherPowerPhase && game.abandonedGates.any)
        if (game.gatherPowerPhase) {
            DS.chaosGateRegions.%(r => game.abandonedGates.has(r)).distinct.foreach { r =>
                DS.chaosGateRegions = DS.chaosGateRegions.%(c => c != r)
                DS.log("Chaos Gate in", r, "became a normal gate")
            }
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // SETUP — DS places no units and takes 4 power; first Psychosis defines their start area
        case SetupFactionsAction if game.starting.contains(DS).not =>
            // Avoid regions in any faction's small starting-option list (e.g. WW's 2-choice poles)
            // so the placeholder doesn't silently consume one of their picks via starting.values diff
            val wwRegions = game.board.starting(WW)
            val placeholder = areas.%(r => wwRegions.has(r).not).head
            game.starting = game.starting + (DS -> placeholder)
            DS.power = 4
            DS.log("starts with all Acolytes in the pool and", 4.power)
            Force(SetupFactionsAction)

        // DOOM
        case DoomAction(f : DS.type) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            if (f.needs(PowerDoomOffer) && f.enemies.any)
                + PowerDoomOfferAction(f)

            + DoomDoneAction(f)

            asking

        // ACTIONS
        case MainAction(f : DS.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : DS.type) if f.acted =>
            UnknownContinue

        case MainAction(f : DS.type) =>
            implicit val asking = Asking(f)

            if (f.pool.%(_.uclass == Acolyte).any && areas.nex.%(r => game.factions.%(_.at(r).any).none && f.affords(1)(r)).any)
                + PsychosisAction(f)

            game.moves(f)

            game.captures(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            game.summons(f)

            game.awakens(f)

            game.independents(f)

            if (f.can(ChaosGateSB) && DS.chaosGateRegions.num < 3 && areas.nex.%(r => game.gates.has(r).not && DS.at(r).%(_.canControlGate).any && f.affords(1)(r)).any)
                + ChaosGateSBAction(f)

            if (f.can(AnimateMatter) && DS.chaosGateRegions.any) {
                val allStarts = game.factions.%(fac => game.starting.contains(fac))./(fac => game.starting(fac))
                val validGates = DS.chaosGateRegions.distinct.%(cgr =>
                    DS.gates.has(cgr) &&
                    DS.at(cgr).%(_.onGate).any &&
                    game.board.connected(cgr).%(r => allStarts.has(r).not).%(r => DS.chaosGateRegions.%(c => c != cgr).has(r).not).%(r => f.affords(1)(r)).any
                )
                if (validGates.any)
                    + AnimateMatterAction(f)
            }

            if (f.can(UndirectedEnergy) && f.all(AvatarThesis).any && f.affords(1)(f.all(AvatarThesis).head.region))
                + UndirectedEnergyAction(f)

            if (f.power >= 1 && f.can(Consummation) && f.oncePerTurn.any)
                + ConsummationAction(f)

            if (f.can(FiendishGrowth) && f.all(AvatarAntithesis).any && (f.pool.monsters ++ f.pool.acolytes).any && f.affords(1)(f.all(AvatarAntithesis).head.region))
                + FiendishGrowthAction(f)

            if (f.can(Traitors) && DS.chaosGateRegions.%(r => DS.gates.has(r) && f.affords(1)(r)).any && f.enemies.%(_.pool.cultists.any).any)
                + TraitorsAction(f)

            game.neutralSpellbooks(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // AWAKEN AVATAR THESIS
        // Intercept at region-selection step to hide the placeholder "0 Power" cost
        case AwakenMainAction(self : DS.type, AvatarThesis, l) =>
            Ask(self).list(l./(r => DSAvatarThesisRegionAction(self, r))).cancel

        case DSAvatarThesisRegionAction(self, r) =>
            Ask(self).list((0 to 8).toList.%(track => self.affords(track)(r))./(track => DSAvatarThesisCostAction(self, r, track)))

        case DSAvatarThesisCostAction(self, r, track) =>
            DS.azathothTrack = track
            self.power -= track
            self.payTax(r)
            val larva = self.at(r).one(LarvaThesis)
            game.eliminate(larva)
            self.place(AvatarThesis, r)
            self.log("awakened", AvatarThesis.styled(self), "in", r, "setting Azathoth track to", track)
            self.satisfy(AwakenAvatarThesis, "Awaken Avatar Thesis")
            Force(DSAvatarThesisPowerAction(self))

        case DSAvatarThesisPowerAction(self) =>
            Ask(self)
                .add(DSAvatarThesisAllPowerAction(self))
                .list(self.enemies./(e => DSAvatarThesisOnePowerAction(self, e)))

        case DSAvatarThesisAllPowerAction(self) =>
            self.enemies.foreach { e =>
                e.power += 1
                e.log("gained", 1.power, "from", AvatarThesis.styled(self))
            }
            EndAction(self)

        case DSAvatarThesisOnePowerAction(self, target) =>
            target.power += 2
            target.log("gained", 2.power, "from", AvatarThesis.styled(self))
            EndAction(self)

        // AWAKEN AVATAR ANTITHESIS
        case AwakenAction(self : DS.type, AvatarAntithesis, r, cost) =>
            self.power -= cost
            self.payTax(r)
            val larva = self.at(r).one(LarvaAntithesis)
            game.eliminate(larva)
            self.place(AvatarAntithesis, r)
            self.log("awakened", AvatarAntithesis.styled(self), "in", r)
            self.satisfy(AwakenAvatarAntithesis, "Awaken Avatar Antithesis")
            Force(DSAvatarAntithesisChoiceAction(self))

        case DSAvatarAntithesisChoiceAction(self) =>
            Ask(self)
                .add(DSAvatarAntithesisAllDoomAction(self))
                .list(self.enemies./(e => DSAvatarAntithesisOneESAction(self, e)))

        case DSAvatarAntithesisAllDoomAction(self) =>
            self.enemies.foreach { e =>
                e.doom += 1
                e.log("gained", 1.doom, "from", AvatarAntithesis.styled(self))
            }
            EndAction(self)

        case DSAvatarAntithesisOneESAction(self, target) =>
            target.takeES(1)
            target.log("gained", 1.es, "from", AvatarAntithesis.styled(self))
            EndAction(self)

        // UNDIRECTED ENERGY
        case UndirectedEnergyAction(self) =>
            val r = self.all(AvatarThesis).head.region
            val n = game.factions.%(_.at(r).any).num
            self.power -= 1
            self.payTax(r)
            self.power += n
            self.oncePerTurn :+= UndirectedEnergy
            self.log("used", UndirectedEnergy, "gaining", n.power, "from", n, "factions in", r)
            EndAction(self)

        // FIENDISH GROWTH
        case FiendishGrowthAction(self) =>
            val r = self.all(AvatarAntithesis).head.region
            val n = game.factions.%(_.at(r).any).num
            Force(FiendishGrowthPlaceAction(self, r, n))

        case FiendishGrowthPlaceAction(self, r, n) =>
            val us = (self.pool.monsters ++ self.pool.acolytes)./(_.uclass).distinct
                .%(uc => self.all(uc).num < self.units./(_.uclass).count(uc))
            if (n > 0 && us.any)
                Ask(self).each(us)(uc => FiendishGrowthPlaceUnitAction(self, r, uc, n))
            else
                EndAction(self)

        case FiendishGrowthPlaceUnitAction(self, r, uc, n) =>
            if (self.oncePerTurn.has(FiendishGrowth).not) {
                self.power -= 1
                self.payTax(r)
                self.oncePerTurn :+= FiendishGrowth
                self.log("used", FiendishGrowth.styled(self), "in", r, ",", n, "factions present")
            }
            self.place(uc, r)
            self.log("placed", uc.styled(self), "in", r, "with", FiendishGrowth)
            Force(FiendishGrowthPlaceAction(self, r, n - 1))

        // AWAKEN AVATAR SYNTHESIS
        case AwakenAction(self : DS.type, AvatarSynthesis, r, cost) =>
            self.power -= cost
            self.payTax(r)
            val larva = self.at(r).one(LarvaSynthesis)
            game.eliminate(larva)
            self.place(AvatarSynthesis, r)
            self.log("awakened", AvatarSynthesis.styled(self), "in", r)
            self.satisfy(AwakenAvatarSynthesis, "Awaken Avatar Synthesis")
            RollD6("Azathoth Synthesis Roll", roll => AzathothSynthesisRollAction(self, roll))

        // AZATHOTH SYNTHESIS AWAKENING
        case AzathothSynthesisRollAction(self, roll) =>
            val x = roll + 2
            DS.azathothDieRoll = x
            val demand = if (game.factions.num <= 3) (x + 1) / 2 else x
            self.log("rolled the Azathoth die", "[" + x.styled("doom") + "]")
            if (demand != x)
                log("Halved to", "[" + demand.styled("doom") + "]", "for", game.factions.num + "-player game")
            log("Other factions must collectively offer", "[" + demand.styled("doom") + "]", "Power/Doom or", DS, "wins")
            Force(AzathothSynthesisContinueAction(self, demand, $, self.enemies.%(e => e.power + e.doom > 0), self.enemies.%(e => e.power + e.doom > 0).num * 6))

        case AzathothSynthesisContinueAction(ds, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropEnding

            if (offers./(_.n).sum == x) {
                offers.%(_.n > 0).reverse.foreach { o =>
                    if (o.p > 0) { o.f.power -= o.p; o.f.log("lost", o.p.power, "to Azathoth") }
                    if (o.d > 0) { o.f.doom  -= o.d; o.f.log("sacrificed", o.d.doom, "to Azathoth") }
                }
                EndAction(ds)
            }
            else
            if (time <= 0 || xforum./(e => e.power + e.doom).sum < x) {
                if (ds.enemies.%(e => e.power + e.doom > 0).num > 1)
                    log("Negotiations failed")
                log(DS, "wins!")
                GameOver($(DS))
            }
            else {
                if (xforum.num == 1)
                    time = 0

                if (time == xforum.num) {
                    time -= 1
                    log("Negotiations time was running out")
                }

                val next = xforum.first
                val forum = xforum.dropStarting :+ next
                val cleanOffers = offers.%(_.f != next)
                val offered = cleanOffers./(_.n).sum
                val maxPower = min(next.power, x - offered)

                if (next.power == 0) {
                    val maxDoom = min(next.doom, x - offered)
                    Ask(next).each(-1 +: 0 +: 1.to(maxDoom).$)(d => AzathothSynthesisDoomAskAction(ds, x, cleanOffers, forum, time - (random() * 1.0).round.toInt, next, 0, d))
                } else
                    Ask(next).each(-1 +: 0 +: 1.to(maxPower).$)(p => AzathothSynthesisPowerAskAction(ds, x, cleanOffers, forum, time, next, p))
            }

        case AzathothSynthesisPowerAskAction(ds, x, offers, forum, time, self, p) =>
            if (p < 0) {
                self.log("refused to negotiate")
                Force(AzathothSynthesisContinueAction(ds, x, offers, forum.but(self), time))
            } else {
                val remaining = x - offers./(_.n).sum - p
                if (self.doom == 0 || remaining <= 0)
                    Force(AzathothSynthesisDoomAskAction(ds, x, offers, forum, time, self, p, 0))
                else {
                    val maxDoom = min(self.doom, remaining)
                    Ask(self).each(0 +: 1.to(maxDoom).$)(d => AzathothSynthesisDoomAskAction(ds, x, offers, forum, time, self, p, d))
                }
            }

        case AzathothSynthesisDoomAskAction(ds, x, offers, forum, time, self, p, d) =>
            if (d < 0) {
                self.log("refused to negotiate")
                Force(AzathothSynthesisContinueAction(ds, x, offers, forum.but(self), time))
            } else {
                val n = p + d
                if (n == 0)
                    self.log("offered nothing to Azathoth")
                else if (p > 0 && d > 0)
                    self.log("offered", p.power, "and", d.styled("doom"), "Doom to Azathoth")
                else if (p > 0)
                    self.log("offered", p.power, "to Azathoth")
                else
                    self.log("offered", d.styled("doom"), "Doom to Azathoth")
                Force(AzathothSynthesisContinueAction(ds, x, AzathothOffer(self, p, d) +: offers, forum, time - (random() * 1.0).round.toInt))
            }

        // PSYCHOSIS
        case PsychosisAction(self) =>
            val empty = areas.nex.%(r => game.factions.%(_.at(r).any).none && self.affords(1)(r))
            Ask(self).list(empty./(r => PsychosisPlaceAction(self, r))).cancel

        case PsychosisPlaceAction(self, r) =>
            if (self.pool.%(_.uclass == Acolyte).none)
                EndAction(self)
            else {
                if (DS.cultists.none)
                    game.starting = game.starting + (DS -> r)
                self.payTax(r)
                self.place(Acolyte, r)
                self.log("used", Psychosis.styled(self), "placing an Acolyte in", r)
                EndAction(self)
            }

        // CHAOS GATE SPELLBOOK
        case ChaosGateSBAction(self) =>
            val valid = areas.nex.%(r => game.gates.has(r).not && DS.at(r).%(_.canControlGate).any && self.affords(1)(r))
            Ask(self).list(valid./(r => ChaosGateSBPlaceAction(self, r))).cancel

        case ChaosGateSBPlaceAction(self, r) =>
            self.power -= 1
            self.payTax(r)
            game.gates :+= r
            DS.chaosGateRegions :+= r
            self.oncePerTurn :+= ChaosGateSB
            self.log("placed", ChaosGate.styled(self), "in", r)
            EndAction(self)

        // ANIMATE MATTER
        case AnimateMatterAction(self) =>
            val allStarts = game.factions.%(fac => game.starting.contains(fac))./(fac => game.starting(fac))
            val validGates = DS.chaosGateRegions.distinct.%(cgr =>
                DS.gates.has(cgr) &&
                DS.at(cgr).%(_.onGate).any &&
                game.board.connected(cgr).%(r => allStarts.has(r).not).%(r => DS.chaosGateRegions.%(c => c != cgr).has(r).not).%(r => self.affords(1)(r)).any
            )
            if (validGates.num == 1) {
                val cgr = validGates.head
                val otherChaosGates = DS.chaosGateRegions.%(r => r != cgr)
                val dests = game.board.connected(cgr).%(r => allStarts.has(r).not).%(r => otherChaosGates.has(r).not).%(r => self.affords(1)(r))
                Ask(self).list(dests./(r => AnimateMatterMoveAction(self, cgr, r))).cancel
            } else {
                Ask(self).list(validGates./(cgr => AnimateMatterFromAction(self, cgr))).cancel
            }

        case AnimateMatterFromAction(self, from) =>
            val allStarts = game.factions.%(fac => game.starting.contains(fac))./(fac => game.starting(fac))
            val otherChaosGates = DS.chaosGateRegions.%(r => r != from)
            val dests = game.board.connected(from).%(r => allStarts.has(r).not).%(r => otherChaosGates.has(r).not).%(r => self.affords(1)(r))
            Ask(self).list(dests./(r => AnimateMatterMoveAction(self, from, r))).cancel

        case AnimateMatterMoveAction(self, from, to) =>
            self.power -= 1
            self.payTax(to)
            // Move the keeper cultist to the new area
            val keeper = DS.at(from).%(_.onGate)
            keeper.foreach { u => u.onGate = false; u.region = to }

            // Remove chaos gate from old area (filter removes all occurrences, not just first)
            DS.chaosGateRegions = DS.chaosGateRegions.%(r => r != from)
            game.gates = game.gates.%(r => r != from)
            DS.gates = DS.gates.%(r => r != from)

            // If a physical gate already exists at the destination, replace it
            if (game.gates.has(to)) {
                game.factions.%(_.gates.has(to)).foreach(_.gates :-= to)
                game.factions.foreach(_.at(to).%(_.onGate).foreach(_.onGate = false))
                // game.gates already contains `to`; no need to add it
            } else {
                game.gates :+= to
            }

            // Assign chaos gate to the new area
            DS.chaosGateRegions :+= to
            DS.gates :+= to
            keeper.foreach(_.onGate = true)

            self.oncePerTurn :+= AnimateMatter
            self.log("used", AnimateMatter.styled(self), "moving", ChaosGate.styled(self), "from", from, "to", to)
            EndAction(self)

        // CONSUMMATION
        case ConsummationAction(self) =>
            Ask(self).list(self.oncePerTurn./(sb => ConsummationUnflipAction(self, sb))).cancel

        case ConsummationUnflipAction(self, sb) =>
            self.power -= 1
            self.oncePerTurn :-= sb
            self.oncePerTurn :+= Consummation
            self.log("used", Consummation.styled(self), "to unflip", sb.styled(self))
            EndAction(self)

        // POWER/DOOM OFFER
        case PowerDoomOfferAction(self) =>
            self.log("offers each faction their choice of", 1.power, "or", 1.doom)
            Force(PowerDoomOfferChoiceAction(self, self.enemies, 0, 0))

        case PowerDoomOfferChoiceAction(self, remaining, powerCount, doomCount) =>
            if (remaining.none) {
                if (powerCount > 0) {
                    self.power += powerCount
                    self.log("gained", powerCount.power, "from", PowerDoomOffer)
                }
                if (doomCount > 0) {
                    self.doom += doomCount
                    self.log("gained", doomCount.doom, "from", PowerDoomOffer)
                }
                self.satisfy(PowerDoomOffer, "Doom Phase Power/Doom giveaway")
                CheckSpellbooksAction(DoomAction(self))
            } else {
                val target = remaining.head
                Ask(target)
                    .add(PowerDoomChoicePowerAction(self, target, remaining.tail, powerCount, doomCount))
                    .add(PowerDoomChoiceDoomAction(self, target, remaining.tail, powerCount, doomCount))
            }

        case PowerDoomChoicePowerAction(self, target, remaining, powerCount, doomCount) =>
            target.power += 1
            target.log("chose", 1.power, "from", PowerDoomOffer)
            Force(PowerDoomOfferChoiceAction(self, remaining, powerCount + 1, doomCount))

        case PowerDoomChoiceDoomAction(self, target, remaining, powerCount, doomCount) =>
            target.doom += 1
            target.log("chose", 1.doom, "from", PowerDoomOffer)
            Force(PowerDoomOfferChoiceAction(self, remaining, powerCount, doomCount + 1))

        // TRAITORS!
        case TraitorsAction(self) =>
            val valid = DS.chaosGateRegions.%(r => DS.gates.has(r) && self.affords(1)(r))
            Ask(self).list(valid./(r => TraitorsChaosGateAction(self, r))).cancel

        case TraitorsChaosGateAction(self, r) =>
            val targets = self.enemies./~(e => e.pool.cultists./(_.uclass).distinct./(uc => TraitorsFactionAction(self, r, e, uc)))
            Ask(self).list(targets)

        case TraitorsFactionAction(self, r, target, uc) =>
            self.power -= 1
            self.payTax(r)
            val acolyte = self.at(r).one(Acolyte)
            game.eliminate(acolyte)
            DS.chaosGateRegions = DS.chaosGateRegions.%(c => c != r)
            DS.gates = DS.gates.%(c => c != r)
            target.place(uc, r)
            game.checkGatesGained(target)
            self.log("used", Traitors.styled(self), "converting Chaos Gate in", r)
            target.log("gained", uc.styled(target), "in", r, "from", Traitors.styled(self))
            if (self.at(r).none) {
                self.takeES(1)
                self.log("gained", 1.es, "from", Traitors.styled(self))
            }
            self.oncePerTurn :+= Traitors
            EndAction(self)

        // COSMIC RULER: save a killed/eliminated Avatar by sacrificing another Avatar in its stead
        case CosmicRulerSacrificeAction(self, saved, sacrificed) =>
            game.eliminate(sacrificed)
            self.log("used", CosmicRuler.styled(self), "eliminating", sacrificed, "to save", saved)
            saved.health = Alive
            Force(BattleDoneAction(self))

        // RECRUIT — first acolyte placed via Recruit also defines DS's start area
        case RecruitAction(self : DS.type, Acolyte, r) =>
            if (DS.cultists.none)
                game.starting = game.starting + (DS -> r)
            UnknownContinue

        case _ => UnknownContinue
    }
}


case object DS extends Faction { f =>
    def name = "Daemon Sultan"
    def short = "DS"
    def style = "ds"

    var azathothTrack : Int = 0
    var azathothDieRoll : Int = 0
    var chaosGateRegions : $[Region] = $

    override def abilities = $(Psychosis, CosmicRuler)
    override def library = $(AnimateMatter, ChaosGateSB, Consummation, FiendishGrowth, Traitors, UndirectedEnergy)
    override def requirements(options : $[GameOption]) = $(OneLarvaEach, AbandonedGateGP, PowerDoomOffer, AwakenAvatarThesis, AwakenAvatarAntithesis, AwakenAvatarSynthesis)

    val allUnits =
        1.times(AvatarThesis) ++
        1.times(AvatarAntithesis) ++
        1.times(AvatarSynthesis) ++
        2.times(LarvaThesis) ++
        2.times(LarvaAntithesis) ++
        2.times(LarvaSynthesis) ++
        3.times(ChaosGate) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        // AvatarThesis: costs 0-8 (azathoth track chosen at time of awakening).
        // Return Some(0) so the region is offered; DSExpansion intercepts and shows the real selector.
        case AvatarThesis =>
            f.at(r, LarvaThesis).any.?(0)

        // AvatarAntithesis: requires AvatarThesis already awakened; costs 8 - azathothTrack
        case AvatarAntithesis =>
            (f.needs(AwakenAvatarThesis).not && f.at(r, LarvaAntithesis).any).?((8 - azathothTrack).max(0))

        // AvatarSynthesis: requires both prior awakenings; costs 8
        case AvatarSynthesis =>
            (f.needs(AwakenAvatarThesis).not && f.needs(AwakenAvatarAntithesis).not && f.at(r, LarvaSynthesis).any).?(8)

        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units.count(_.uclass == LarvaThesis)     * (f.all(AvatarThesis).any.?(2).|(0)) +
        units.count(_.uclass == LarvaAntithesis) * (f.all(AvatarAntithesis).any.?(2).|(0)) +
        units.count(_.uclass == LarvaSynthesis)  * (f.all(AvatarSynthesis).any.?(2).|(0)) +
        units.count(_.uclass == AvatarThesis)    * azathothTrack +
        units.count(_.uclass == AvatarAntithesis) * (8 - azathothTrack) +
        units.count(_.uclass == AvatarSynthesis)  * azathothDieRoll.max(1) +
        neutralStrength(units.%(_.uclass != AvatarSynthesis), opponent)
}
