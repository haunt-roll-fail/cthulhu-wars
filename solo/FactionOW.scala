package cws

import hrf.colmat._

import Html._


case object Mutant extends FactionUnitClass(OW, "Mutant", Monster, 2)
case object Abomination extends FactionUnitClass(OW, "Abomination", Monster, 3)
case object SpawnOW extends FactionUnitClass(OW, "Spawn of Yog-Sothoth", Monster, 4) { override def plural = "Spawns of Yog-Sothoth" }
case object YogSothoth extends FactionUnitClass(OW, "Yog-Sothoth", GOO, 6)

case object BeyondOne extends FactionSpellbook(OW, "The Beyond One")
case object KeyAndGate extends FactionSpellbook(OW, "The Key and the Gate")

case object TheyBreakThrough extends FactionSpellbook(OW, "They Break Through")
case object MillionFavoredOnes extends FactionSpellbook(OW, "The Million Favored Ones")
case object ChannelPower extends FactionSpellbook(OW, "Channel Power")
case object DreadCurse extends FactionSpellbook(OW, "Dread Curse of Azathoth")
case object DragonAscending extends FactionSpellbook(OW, "Dragon Ascending")
case object DragonDescending extends FactionSpellbook(OW, "Dragon Descending")

case object EightGates extends Requirement("8 gates on the map")
case object TenGates extends Requirement("10 gates on the map")
case object TwelveGates extends Requirement("12 gates on the map")
case object UnitsAtEnemyGates extends Requirement("Units at 2 enemy gates")
case object LoseUnitInBattle extends Requirement("Lose unit in battle")
case object GooMeetsGoo extends Requirement("GOO in area with enemy GOO")
case object AwakenYogSothoth extends Requirement("Awaken Yog-Sothoth")


case object OW extends Faction { f =>
    def name = "Opener of the Way"
    def short = "OW"
    def style = "ow"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $(BeyondOne, KeyAndGate)
    override def library = $(TheyBreakThrough, DreadCurse, MillionFavoredOnes, ChannelPower, DragonAscending, DragonDescending)
    override def requirements(options : $[GameOption]) = $(EightGates) ++
        $((options.has(PlayerCount(3)) || (options.has(PlayerCount(4)) && options.has(Opener4P10Gates))).?(TenGates).|(TwelveGates)) ++
        $(UnitsAtEnemyGates, LoseUnitInBattle, GooMeetsGoo, AwakenYogSothoth)

    val allUnits =
        1.times(YogSothoth) ++
        2.times(SpawnOW) ++
        3.times(Abomination) ++
        4.times(Mutant) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case YogSothoth => f.at(r, SpawnOW).any.?(6)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Mutant).num * 1 +
        units(Abomination).num * 2 +
        units(SpawnOW).num * 3 +
        units(YogSothoth).%!(_.has(Zeroed)).num * (2 * factions.but(f)./(_.all.factionGOOs.num).sum) +
        neutralStrength(units, opponent)
}


case class BeyondOneMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(self.styled(BeyondOne)) with MainQuestion with Soft
case class BeyondOneUnitAction(self : Faction, o : Region, uc : UnitClass) extends BaseFactionAction(self.styled(BeyondOne), self.styled(uc) + " from " + o) with Soft
case class BeyondOneAction(self : Faction, o : Region, uc : UnitClass, r : Region) extends BaseFactionAction(self.styled(BeyondOne) + " from " + o + " with " + self.styled(uc) + " to", implicit g => r + self.iced(r))

case class DreadCurseMainAction(self : Faction, n : Int, l : $[Region]) extends OptionFactionAction(self.styled(DreadCurse)) with MainQuestion with Soft
case class DreadCurseAction(self : Faction, n : Int, r : Region) extends BaseFactionAction(self.styled(DreadCurse), implicit g => r + self.iced(r))
case class DreadCurseRollAction(f : Faction, r : Region, x : $[BattleRoll]) extends ForcedAction
case class DreadCurseSplitAction(self : Faction, r : Region, x : $[BattleRoll], e : $[Faction], k : $[Faction], p : $[Faction]) extends BaseFactionAction(self.styled(DreadCurse) + " in " + r + "<br/>" + x.any.?(x.mkString(" ")).|("None"), e.%(f => k.count(f) + p.count(f) > 0)./(f => "" + f + " - " + (k.count(f).times(Kill) ++ p.count(f).times(Pain)).mkString(" ")).mkString("<br/>"))
case class DreadCurseAssignAction(f : Faction, r : Region, e : $[Faction], k : $[Faction], p : $[Faction], self : Faction, s : BattleRoll, uc : UnitClass) extends BaseFactionAction("Assign " + s + " in " + r, self.styled(uc))
case class DreadCurseRetreatAction(self : Faction, r : Region, e : $[Faction], f : Faction, uc : UnitClass) extends BaseFactionAction("Retreat from " + r, self.styled(uc))
case class DreadCurseRetreatToAction(self : Faction, r : Region, e : $[Faction], f : Faction, uc : UnitClass, d : Region) extends BaseFactionAction("Retreat " + f.styled(uc) + " from " + r + " to", d)

case class DragonDescendingDoomAction(self : Faction, n : Int) extends OptionFactionAction("Ritual with " + DragonDescending.full) with DoomQuestion

case class DragonAscendingMainAction(self : Faction) extends OptionFactionAction(self.styled(DragonAscending)) with MainQuestion with Soft
case class DragonAscendingDoomAction(self : Faction) extends OptionFactionAction(self.styled(DragonAscending)) with DoomQuestion with Soft

case class DragonAscendingAction(self : Faction, f : Option[Faction], reason : String, n : Int, then : ForcedAction) extends BaseFactionAction(self.styled(DragonAscending) + " before " + f./("" + _ + " ").|("") + reason, "Rise to " + n.power)
case class DragonAscendingAskAction(self : Faction, f : Option[Faction], reason : String, then : ForcedAction) extends ForcedAction
case class DragonAscendingInstantAction(then : ForcedAction) extends ForcedAction
case class DragonAscendingUpAction(reason : String, then : ForcedAction) extends ForcedAction
case class DragonAscendingDownAction(f : Faction, reason : String, then : ForcedAction) extends ForcedAction
case class DragonAscendingCancelAction(self : Faction, then : ForcedAction) extends BaseFactionAction(None, "Cancel")
case class DragonAscendingNotThisTurnAction(self : Faction, then : ForcedAction) extends BaseFactionAction(None, "Not in this Action Phase")


object OWExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // BEYOND ONE
        case BeyondOneMainAction(self, l) =>
            Ask(self).each(l./~(r => self.at(r).%(_.uclass.cost >= 3)).%(_.canMove))(u => BeyondOneUnitAction(self, u.region, u.uclass)).cancel

        case BeyondOneUnitAction(self, o, uc) =>
            Ask(self).each(board.regions.diff(game.gates).%(self.affords(1)))(BeyondOneAction(self, o, uc, _)).cancel

        case BeyondOneAction(self, o, uc, r) =>
            self.power -= 1
            self.payTax(r)
            game.gates :-= o
            game.gates :+= r
            factions.%(_.gates.contains(o)).foreach { f =>
                f.gates :-= o
                f.gates :+= r
                f.at(o).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && f.has(RedSign))).first.region = r
            }
            self.at(o).one(uc).region = r
            self.log("moved gate with", self.styled(uc), "from", o, "to", r)
            EndAction(self)

        // DREAD CURSE
        case DreadCurseMainAction(self, n, l) =>
            Ask(self).each(l)(DreadCurseAction(self, n, _)).cancel

        case DreadCurseAction(self, n, r) =>
            self.power -= 2
            self.payTax(r)
            self.log("sent", self.styled(DreadCurse), "to", r)
            RollBattle(self, self.styled(DreadCurse), n, x => DreadCurseRollAction(self, r, x))

        case DreadCurseRollAction(self, r, x) =>
            self.log("rolled", x.mkString(" "))
            var k = x.count(Kill)
            var p = x.count(Pain)
            if (k + p == 0)
                EndAction(self)
            else {
                val e = self.enemies.%(f => f.at(r).any).sortBy(-_.at(r).sortBy(_.uclass.cost).take(k)./(_.uclass.cost).sum)

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

                Ask(self).each(kpvb)((a, b) => DreadCurseSplitAction(self, r, x, e.%(f => a.contains(f) || b.contains(f)), a, b))
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
                Ask(self).each(uu)(u => DreadCurseAssignAction(self, r, e, k, p, f, rs.first, u))
            }
            else {
                e.foreach { f =>
                    f.at(r).%(_.health == Killed).foreach { u =>
                        log(u, "was", "killed".styled("kill"))
                        game.eliminate(u)
                    }
                }

                var m = e./~(f => f.at(r).%(_.health == Pained))

                m = m.take(1)

                if (m.any)
                    Ask(self).each(m)(u => DreadCurseRetreatAction(self, r, e, u.faction, u.uclass))
                else
                    EndAction(self)
            }

        case DreadCurseAssignAction(f, r, e, k, p, self, s, uc) =>
            val u = self.at(r, uc).%(_.health == Alive).first
            u.health = (s == Kill).?(Killed).|(Pained)
            Ask(f).add(DreadCurseSplitAction(f, r, $, e, k, p))

        case DreadCurseRetreatAction(self, r, e, f, uc) =>
            Ask(self).each(board.connected(r))(d => DreadCurseRetreatToAction(self, r, e, f, uc, d))

        case DreadCurseRetreatToAction(self, r, e, f, uc, d) =>
            val u = f.at(r, uc).%(_.health == Pained).first
            u.region = d
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
            Ask(self).add(DragonAscendingAction(self, |(self), "own action", factions./(_.power).max, MainAction(self))).cancel

        case DragonAscendingDoomAction(self) =>
            Ask(self).add(DragonAscendingAction(self, |(self), "own " + "Doom".styled("doom") + " action", factions./(_.power).max, DoomAction(self))).cancel

        case DragonAscendingAskAction(self, f, reason, then) =>
            Ask(self)
                .add(DragonAscendingAction(self, f, reason, factions./(_.power).max, then))
                .add(DragonAscendingCancelAction(self, then))
                .add(DragonAscendingNotThisTurnAction(self, then))

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
            val daf = game.targetDragonAscending(f)

            if (daf.none || (f.hibernating && then == MainAction(f))) {
                Force(then)
            }
            else {
                val self = daf(0)
                DragonAscendingAskAction(self, Some(f), reason, DragonAscendingDownAction(f, reason, then))
            }


        case _ => UnknownContinue
    }
}
