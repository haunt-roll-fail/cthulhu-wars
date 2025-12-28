package cws

import hrf.colmat._

import Html._


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
case object UnholyGround extends FactionSpellbook(AN, "Unholy Ground")
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

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Reanimated).num * 2 +
        units(Yothan).num * 7 +
        neutralStrength(units, opponent)
}


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


object ANExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // BUILD CATHEDRAL
        case BuildCathedralMainAction(self, locations) =>
            Ask(self).each(locations.sortBy(self.taxIn))(r => BuildCathedralAction(self, r)).cancel

        case BuildCathedralAction(self, r) =>
            self.power -= game.getCathedralCost(r)
            self.payTax(r)
            game.cathedrals :+= r
            self.cathedrals :+= r
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
            val forum = self.enemies
            Force(GiveWorstMonsterContinueAction(self, forum))

        case GiveWorstMonsterContinueAction(self, xforum) => {
            if (xforum.num == 0) {
                EndAction(self)
            }
            else {
                val f = xforum.first
                val forum = xforum.drop(1)
                val validPool = f.pool.monsters

                if (!validPool.any) {
                    f.log("didn't have any monsters in the pool")
                    Force(GiveWorstMonsterContinueAction(f, forum))
                }
                else if (!f.allGates.onMap.any) {
                    f.log("had no way of summoning monsters")
                    Force(GiveWorstMonsterContinueAction(f, forum))
                }
                else {
                    val unitClasses = validPool./(_.uclass)
                    val minCost = unitClasses.map(_.cost).min
                    val ucs = unitClasses.filter(_.cost == minCost).distinct

                    if (ucs.num == 1) {
                        Ask(f).each(f.allGates.onMap)(r => GiveWorstMonsterAskAction(f, self, ucs.first, r, forum))
                    }
                    else {
                        Ask(f).each(ucs)(uc => GiveWorstMonsterSelectMonsterAction(f, self, uc, forum))
                    }
                }
            }
        }

        case GiveWorstMonsterSelectMonsterAction(self, f, uc, forum) =>
            Ask(self).each(self.allGates.onMap)(r => GiveWorstMonsterAskAction(self, f, uc, r, forum))

        case GiveWorstMonsterAskAction(self, f, uc, r, forum) =>
            self.place(uc, r)
            // self.payTax(r) // Not sure if Ice Age affects this // probably doesn't HRF
            self.log("summoned", uc, "in", r, "for free")
            Force(GiveWorstMonsterContinueAction(f, forum))

        // GIVE BEST MONSTER
        case GiveBestMonsterMainAction(self) =>
            self.satisfy(GiveBestMonster, "Enemies got highest cost monster")
            self.log("allowed enemy factions to summon their highest cost monster for free")
            val forum = self.enemies
            Force(GiveBestMonsterContinueAction(self, forum))

        case GiveBestMonsterContinueAction(self, xforum) => {
            if (xforum.num == 0) {
                EndAction(self)
            }
            else {
                val f = xforum.first
                val forum = xforum.drop(1)
                val validPool = f.pool.monsters

                if (!validPool.any) {
                    f.log("didn't have any monsters in the pool")
                    Force(GiveBestMonsterContinueAction(f, forum))
                }
                else if (!f.allGates.onMap.any) {
                    f.log("had no way of summoning monsters")
                    Force(GiveBestMonsterContinueAction(f, forum))
                }
                else {
                    val unitClasses = validPool./(_.uclass)
                    val maxCost = unitClasses.map(_.cost).max
                    val ucs = unitClasses.filter(_.cost == maxCost).distinct

                    if (ucs.num == 1) {
                        Ask(f).each(f.allGates.onMap)(r => GiveBestMonsterAskAction(f, self, ucs.first, r, forum))
                    }
                    else {
                        Ask(f).each(ucs)(uc => GiveBestMonsterSelectMonsterAction(f, self, uc, forum))
                    }
                }
            }
        }

        case GiveBestMonsterSelectMonsterAction(self, f, uc, forum) =>
            Ask(self).each(self.allGates.onMap)(r => GiveBestMonsterAskAction(self, f, uc, r, forum))

        case GiveBestMonsterAskAction(self, f, uc, r, forum) =>
            self.place(uc, r)
            // self.payTax(r) // Not sure if Ice Age affects this // probably doesn't HRF
            self.log("summoned", uc, "in", r, "for free")
            Force(GiveBestMonsterContinueAction(f, forum))


        // SUMMONING UN-MAN WITH FESTIVAL
        case FestivalUnManSummonAction(self, f) =>
            f.power += 1
            f.log("got", 1.power, "from", self.styled(Festival))
            EndAction(self)


        // DEMATERIALIZATION
        case DematerializationDoomAction(self) =>
            Ask(self).each(board.regions.%(r => self.at(r).any))(r => DematerializationFromRegionAction(self, r)).cancel

        case DematerializationFromRegionAction(self, o) =>
            Ask(self).each(board.regions.but(o))(r => DematerializationToRegionAction(self, o, r)).cancel

        case DematerializationToRegionAction(self, o, d) =>
            Ask(self).each(self.at(o).%(_.canMove))(u => DematerializationMoveUnitAction(self, o, d, u.uclass)).add(DematerializationDoneAction(self))

        case DematerializationMoveUnitAction(self, o, d, uc) =>
            val u = self.at(o, uc).first
            u.region = d
            self.log("sent", self.styled(uc), "from", o, "to", d, "with", Dematerialization.full)
            Ask(self).each(self.at(o).%(_.canMove))(u => DematerializationMoveUnitAction(self, o, d, u.uclass)).add(DematerializationDoneAction(self))

        case DematerializationDoneAction(self) =>
            self.oncePerTurn :+= Dematerialization
            game.demCaseMap = game.demCaseMap.keys.map(key => key -> 0).toMap
            CheckSpellbooksAction(DoomAction(self))


        case _ => UnknownContinue
    }
}
