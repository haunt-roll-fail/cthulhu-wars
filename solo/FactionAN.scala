package cws

import hrf.colmat._

import html._


case object UnMan extends FactionUnitClass(AN, "Un-Man", Monster, 3)
case object Reanimated extends FactionUnitClass(AN, "Reanimated", Monster, 4) {
    def alone(u : UnitFigure)(implicit game : Game) = u.faction.has(Brainless) && u.faction.at(u.region).not(Reanimated).none
    override def canMove(u : UnitFigure)(implicit game : Game) = alone(u).not
    override def canBattle(u : UnitFigure)(implicit game : Game) = alone(u).not
    override def canCapture(u : UnitFigure)(implicit game : Game) = alone(u).not
}
case object Yothan extends FactionUnitClass(AN, "Yothan", Terror, 6)
case object Cathedral extends FactionUnitClass(AN, "Cathedral", Building, 4)

case object Dematerialization extends FactionSpellbook(AN, "Dematerialization")

case object Festival extends FactionSpellbook(AN, "Festival")
case object Brainless extends FactionSpellbook(AN, "Brainless")
case object Extinction extends FactionSpellbook(AN, "Extinction")
case object UnholyGround extends FactionSpellbook(AN, "Unholy Ground") with BattleSpellbook
case object Consecration extends FactionSpellbook(AN, "Consecration")
case object WorshipServices extends FactionSpellbook(AN, "Worship Services")

case object CathedralWW extends Requirement("Cathedral in Area with ||| Glyph")
case object CathedralOO extends Requirement("Cathedral in Area with (*) Glyph")
case object CathedralAA extends Requirement("Cathedral in Area with /^\\ Glyph")
case object CathedralNG extends Requirement("Cathedral in Area without Glyph")
case object GiveWorstMonster extends Requirement("Give enemies lowest cost monster")
case object GiveBestMonster extends Requirement("Give enemies highest cost monster")


case object AN extends Faction { f =>
    def name = "The Ancients"
    def short = "AN"
    def style = "an"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def extinct = Region("Extinct", Extinct)

    override def abilities = $(Dematerialization)
    override def library = $(Festival, Brainless, Extinction, UnholyGround, Consecration, WorshipServices)
    override def requirements(options : $[GameOption]) = $(CathedralAA, CathedralOO, CathedralWW, CathedralNG, GiveWorstMonster, GiveBestMonster)

    def cathedralCost(r : Region)(implicit game : Game) : Int = 1 + game.board.connected(r).intersect(game.cathedrals).any.??(2)

    val allUnits =
        3.times(Yothan) ++
        3.times(Reanimated) ++
        3.times(UnMan) ++
        6.times(Acolyte)

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case UnMan => f.has(Festival).?(0).|(3)
        case Reanimated => f.has(Brainless).?(1).|(4)
        case Yothan => f.has(Extinction).?(3).|(6)
        case _ => u.cost
    }

    override def canAwakenIGOO(r : Region)(implicit game : Game) : Boolean = this.gates.has(r) && game.cathedrals.num == 4

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Reanimated).num * 2 +
        units(Yothan).num * 7 +
        neutralStrength(units, opponent)
}


case class GiveWorstMonsterMainAction(self : AN) extends OptionFactionAction("Give enemies lowest cost monster") with MainQuestion
case class GiveWorstMonsterContinueAction(self : AN, rest : $[Faction]) extends ForcedAction
case class GiveWorstMonsterSelectMonsterAction(self : Faction, f : AN, uc : UnitClass, l : $[Region], rest : $[Faction]) extends BaseFactionAction("Summon monster for free", self.styled(uc))
case class GiveWorstMonsterAskAction(self : Faction, f : AN, uc : UnitClass, r : Region, rest : $[Faction]) extends BaseFactionAction("Summon a " + uc + " for free at", r)

case class GiveBestMonsterMainAction(self : AN) extends OptionFactionAction("Give enemies highest cost monster") with MainQuestion
case class GiveBestMonsterContinueAction(self : AN, l : $[Faction]) extends ForcedAction
case class GiveBestMonsterSelectMonsterAction(self : Faction, f : AN, uc : UnitClass, l : $[Region], rest : $[Faction]) extends BaseFactionAction("Summon monster for free", self.styled(uc))
case class GiveBestMonsterAskAction(self : Faction, f : AN, uc : UnitClass, r : Region, rest : $[Faction]) extends BaseFactionAction("Summon a " + uc + " for free at", r)

case class BuildCathedralMainAction(self : AN, l : $[Region]) extends OptionFactionAction("Build " + AN.styled("Cathedral")) with MainQuestion with Soft
case class BuildCathedralAction(self : AN, r : Region) extends BaseFactionAction(implicit g => "Build cathedral" + g.forNPowerWithTax(r, self, AN.cathedralCost(r)) + " in", r)

case class FestivalUnManSummonAction(self : AN, f : Faction) extends BaseFactionAction(AN.styled("UnMen") + " gave power to another faction", "" + f + " gets " + 1.power)

case class DematerializationDoomAction(self : Faction) extends OptionFactionAction(Dematerialization.styled(self)) with DoomQuestion with Soft with PowerNeutral
case class DematerializationFromRegionAction(self : Faction, o : Region) extends BaseFactionAction(Dematerialization.styled(self) + " from", o)
case class DematerializationToRegionAction(self : Faction, o : Region, r : Region) extends BaseFactionAction(Dematerialization.styled(self) + " from " + o + " to", r)
case class DematerializationMoveUnitAction(self : Faction, o : Region, r : Region, uc : UnitClass) extends BaseFactionAction(Dematerialization.styled(self) + " from " + o + " to " + r, self.styled(uc))
case class DematerializationDoneAction(self : Faction) extends BaseFactionAction(None, "Done")


object ANExpansion extends Expansion {
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (u.uclass == Yothan && u.faction.has(Extinction)) {
            u.region = AN.extinct
            u.state = $
            log(u.faction.styled(Yothan), "was removed from the game permanently due to", Extinction)
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // DOOM
        case DoomAction(f : AN) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            if (f.can(Dematerialization))
                + DematerializationDoomAction(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            + DoomDoneAction(f)

            asking

        // ACTIONS
        case MainAction(f : AN) if f.active.not =>
            UnknownContinue

        case MainAction(f : AN) if f.acted =>
            UnknownContinue

        case MainAction(f : AN) =>
            implicit val asking = Asking(f)

            game.moves(f)

            game.captures(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            if (game.cathedrals.num < 4)
                areas.%(r => game.cathedrals.forall(_.glyph != r.glyph)).%(r => f.at(r).%(_.canControlGate).any).%(r => f.affords(AN.cathedralCost(r))(r)).some.foreach { l =>
                     + BuildCathedralMainAction(f, l)
                }

            game.summons(f)

            game.awakens(f)

            game.independents(f)

            if (f.needs(GiveWorstMonster))
                + GiveWorstMonsterMainAction(f)

            if (f.needs(GiveBestMonster))
                + GiveBestMonsterMainAction(f)

            game.neutralSpellbooks(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // BUILD CATHEDRAL
        case BuildCathedralMainAction(self, locations) =>
            Ask(self).each(locations.sortBy(self.taxIn))(r => BuildCathedralAction(self, r)).cancel

        case BuildCathedralAction(self, r) =>
            self.power -= AN.cathedralCost(r)
            self.payTax(r)
            game.cathedrals :+= r
            self.log("built a cathedral in", r)
            r.glyph match {
                case GlyphAA => self.satisfy(CathedralAA, "Cathedral in /^\\ ".trim)
                case GlyphOO => self.satisfy(CathedralOO, "Cathedral in (*)")
                case GlyphWW => self.satisfy(CathedralWW, "Cathedral in |||")
                case _ => self.satisfy(CathedralNG, "Cathedral in no-glyph Area")
            }
            EndAction(self)

        // GIVE WORST MONSTER
        case GiveWorstMonsterMainAction(self) =>
            self.satisfy(GiveWorstMonster, "Enemies got lowest cost monster")
            self.log("allowed enemy factions to summon their lowest cost monster for free")
            Force(GiveWorstMonsterContinueAction(self, self.enemies))

        case GiveWorstMonsterContinueAction(self, Nil) =>
            EndAction(self)

        case GiveWorstMonsterContinueAction(self, e :: rest) => {
            val monsters = e.pool.monsters./(_.uclass).distinct
            val gates = e.allGates.onMap.distinct

            if (monsters.none) {
                e.log("had no monsters available")
                Force(GiveWorstMonsterContinueAction(self, rest))
            }
            else
            if (gates.none) {
                e.log("had no gates", e.allGates.any.??("on the map"))
                Force(GiveWorstMonsterContinueAction(self, rest))
            }
            else {
                val minCost = monsters./(_.cost).min
                val ucs = monsters.%(_.cost == minCost)

                Ask(e).each(ucs)(uc => GiveWorstMonsterSelectMonsterAction(e, self, uc, gates, rest))
            }
        }

        case GiveWorstMonsterSelectMonsterAction(self, f, uc, l, rest) =>
            Ask(self).each(l)(r => GiveWorstMonsterAskAction(self, f, uc, r, rest))

        case GiveWorstMonsterAskAction(self, f, uc, r, rest) =>
            self.place(uc, r)
            // self.payTax(r) // Not sure if Ice Age affects this // probably doesn't HRF
            self.log("summoned", uc, "in", r, "for free")
            Force(GiveWorstMonsterContinueAction(f, rest))

        // GIVE BEST MONSTER
        case GiveBestMonsterMainAction(self) =>
            self.satisfy(GiveBestMonster, "Enemies got highest cost monster")
            self.log("allowed enemy factions to summon their highest cost monster for free")
            Force(GiveBestMonsterContinueAction(self, self.enemies))

        case GiveBestMonsterContinueAction(self, Nil) =>
            EndAction(self)

        case GiveBestMonsterContinueAction(self, e :: rest) => {
            val monsters = e.pool.monsters./(_.uclass).distinct
            val gates = e.allGates.onMap.distinct

            if (monsters.none) {
                e.log("had no monsters available")
                Force(GiveBestMonsterContinueAction(self, rest))
            }
            else
            if (gates.none) {
                e.log("had no gates", e.allGates.any.??("on the map"))
                Force(GiveBestMonsterContinueAction(self, rest))
            }
            else {
                val maxCost = monsters./(_.cost).max
                val ucs = monsters.%(_.cost == maxCost)

                Ask(e).each(ucs)(uc => GiveBestMonsterSelectMonsterAction(e, self, uc, gates, rest))
            }
        }

        case GiveBestMonsterSelectMonsterAction(self, f, uc, l, rest) =>
            Ask(self).each(l)(r => GiveBestMonsterAskAction(self, f, uc, r, rest))

        case GiveBestMonsterAskAction(self, f, uc, r, rest) =>
            self.place(uc, r)
            // self.payTax(r) // Not sure if Ice Age affects this // probably doesn't HRF
            self.log("summoned", uc, "in", r, "for free")
            Force(GiveBestMonsterContinueAction(f, rest))

        // SUMMONING UN-MAN WITH FESTIVAL
        case SummonedAction(self : AN, uc, r, l) if uc == UnMan && self.has(Festival) =>
            Ask(self).each(self.enemies)(f => FestivalUnManSummonAction(self, f))

        case FestivalUnManSummonAction(self, f) =>
            f.power += 1
            f.log("got", 1.power, "from", self.styled(Festival))
            EndAction(self)

        // DEMATERIALIZATION
        case DematerializationDoomAction(self) =>
            Ask(self).each(areas.%(r => self.at(r).any))(r => DematerializationFromRegionAction(self, r)).cancel

        case DematerializationFromRegionAction(self, o) =>
            Ask(self).each(areas.but(o))(r => DematerializationToRegionAction(self, o, r)).cancel

        case DematerializationToRegionAction(self, o, d) =>
            Ask(self).each(self.at(o).%(_.canMove))(u => DematerializationMoveUnitAction(self, o, d, u.uclass)).add(DematerializationDoneAction(self))

        case DematerializationMoveUnitAction(self, o, d, uc) =>
            val u = self.at(o).one(uc)
            u.region = d
            u.onGate = false
            self.log("sent", self.styled(uc), "from", o, "to", d, "with", Dematerialization.full)
            Ask(self).each(self.at(o).%(_.canMove))(u => DematerializationMoveUnitAction(self, o, d, u.uclass)).add(DematerializationDoneAction(self))

        case DematerializationDoneAction(self) =>
            self.oncePerTurn :+= Dematerialization
            game.demCaseMap = areas.map(r => r -> 0).toMap
            CheckSpellbooksAction(DoomAction(self))

        // ...
        case _ => UnknownContinue
    }
}
