package cws

import hrf.colmat._

import Html._


case object Undead extends FactionUnitClass(YS, "Undead", Monster, 1)
case object Byakhee extends FactionUnitClass(YS, "Byakhee", Monster, 2)
case object KingInYellow extends FactionUnitClass(YS, "King in Yellow", GOO, 4)
case object Hastur extends FactionUnitClass(YS, "Hastur", GOO, 10)

case object Feast extends FactionSpellbook(YS, "Feast")
case object Desecrate extends FactionSpellbook(YS, "Desecrate")
case object Vengeance extends FactionSpellbook(YS, "Vengeance")

case object Passion extends FactionSpellbook(YS, "Passion")
case object Zingaya extends FactionSpellbook(YS, "Zingaya")
case object Shriek extends FactionSpellbook(YS, "Shriek of the Byakhee")
case object ScreamingDead extends FactionSpellbook(YS, "The Screaming Dead")
case object ThirdEye extends FactionSpellbook(YS, "The Third Eye")
case object HWINTBN extends FactionSpellbook(YS, "He Who is Not to be Named")


case object Provide3Doom extends Requirement("Provide 3 Doom")
case object AwakenKing extends Requirement("Awaken King in Yellow")
case object DesecrateAA extends Requirement("Desecrate an Area with /^\\ Glyph")
case object DesecrateOO extends Requirement("Desecrate an Area with (*) Glyph")
case object DesecrateWW extends Requirement("Desecrate an Area with ||| Glyph")
case object AwakenHastur extends Requirement("Awaken Hastur", 1)


case object YS extends Faction { f =>
    def name = "Yellow Sign"
    def short = "YS"
    def style = "ys"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $(Feast, Desecrate, Vengeance)
    override def library = $(Passion, Zingaya, Shriek, ScreamingDead, ThirdEye, HWINTBN)
    override def requirements(options : $[GameOption]) = $(Provide3Doom, AwakenKing, DesecrateAA, DesecrateOO, DesecrateWW, AwakenHastur)

    val allUnits =
        1.times(Hastur) ++
        1.times(KingInYellow) ++
        4.times(Byakhee) ++
        6.times(Undead) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case KingInYellow => (game.gates.has(r).not && game.unitGates.has(r).not && f.present(r)).?(4)
        case Hastur => (f.gates.has(r) && f.at(r, KingInYellow).any).?(10)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Undead).num.useIf(_ > 0)(_ - 1) +
        units(Byakhee).num.useIf(_ > 0)(_ + 1) +
        units(Hastur).%!(_.has(Zeroed)).num * game.ritualCost +
        neutralStrength(units, opponent)
}


case class Provide3DoomMainAction(self : Faction) extends OptionFactionAction("Get spellbook and another faction gets " + 3.doom) with MainQuestion with Soft
case class Provide3DoomAction(self : Faction, f : Faction) extends BaseFactionAction("Get spellbook", "" + f + " gets " + 3.doom)

case class DesecrateMainAction(self : Faction, r : Region, te : Boolean) extends OptionFactionAction(implicit g => "" + Desecrate + " " + r + te.??(" (" + ThirdEye + ")") + self.iced(r)) with MainQuestion
case class DesecrateRollAction(f : Faction, r : Region, te : Boolean, x : Int) extends ForcedAction
case class DesecratePlaceAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Place in " + r, self.styled(uc))

case class HWINTBNMainAction(self : Faction, o : Region, l : $[Region]) extends OptionFactionAction(HWINTBN) with MainQuestion with Soft
case class HWINTBNAction(self : Faction, o : Region, r : Region) extends BaseFactionAction(HWINTBN, implicit g => r + self.iced(r))

case class ScreamingDeadMainAction(self : Faction, o : Region, l : $[Region]) extends OptionFactionAction(ScreamingDead) with MainQuestion with Soft
case class ScreamingDeadAction(self : Faction, o : Region, r : Region) extends BaseFactionAction(ScreamingDead, implicit g => r + self.iced(r))
case class ScreamingDeadFollowAction(self : Faction, o : Region, r : Region, uc : UnitClass) extends BaseFactionAction("Follow " + KingInYellow, self.styled(uc))
case class ScreamingDeadDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class ShriekMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(Shriek) with MainQuestion with Soft
case class ShriekAction(self : Faction, r : Region) extends BaseFactionAction(Shriek, implicit g => r + self.iced(r))
case class ShriekFromAction(self : Faction, o : Region, r : Region) extends BaseFactionAction("" + Shriek.full + " to " + r, "" + Byakhee + " from " + o)
case class ShriekDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class ZingayaMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(self.styled(Zingaya)) with MainQuestion with Soft
case class ZingayaAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(self.styled(Zingaya), implicit g => f.styled(Acolyte) + " in " + r + self.iced(r))


object YSExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // PROVIDE 3 DOOM
        case Provide3DoomMainAction(self) =>
            Ask(self).each(self.enemies)(f => Provide3DoomAction(self, f)).cancel

        case Provide3DoomAction(self, f) =>
            f.doom += 3
            self.log("supplied", f, "with", 3.doom)
            self.satisfy(Provide3Doom, "Provide 3 Doom")
            EndAction(self)

        // DESECRATE
        case DesecrateMainAction(self, r, te) =>
            self.power -= te.?(1).|(2)
            self.payTax(r)
            RollD6("Roll for " + self.styled(Desecrate) + " in " + r, x => DesecrateRollAction(self, r, te, x))

        case DesecrateRollAction(self, r, te, x) =>
            if (self.at(r).num >= x) {
                log(self.styled(KingInYellow), "desecrated", r, "with roll [" + x.styled("power") + "]")
                if (te) {
                    self.log("gained", 1.es, "using", ThirdEye.full)
                    self.takeES(1)
                }
                r.glyph match {
                    case GlyphAA => self.satisfy(DesecrateAA, "Desecrated /^\\ ".trim)
                    case GlyphOO => self.satisfy(DesecrateOO, "Desecrated (*)")
                    case GlyphWW => self.satisfy(DesecrateWW, "Desecrated |||")
                    case _ =>
                }
                game.desecrated :+= r
            }
            else
                log(self.styled(KingInYellow), "failed", r, "desecration with roll [" + x.styled("power") + "]")

            val us = (self.pool.cultists ++ self.pool.monsters)./(_.uclass).distinct.%(_.cost <= 2)

            if (us.any)
                Ask(self).each(us)(DesecratePlaceAction(self, r, _))
            else
                EndAction(self)

        case DesecratePlaceAction(self, r, uc) =>
            self.place(uc, r)
            log(self.styled(uc), "appeared in", r)
            EndAction(self)

        // HWINTBN
        case HWINTBNMainAction(self, o, l) =>
            Ask(self).each(l)(HWINTBNAction(self, o, _)).cancel

        case HWINTBNAction(self, o, r) =>
            self.power -= 1
            self.payTax(r)
            self.at(o, Hastur).first.region = r
            self.oncePerRound :+= HWINTBN

            log(Hastur, "heard his name in", r)

            AfterAction(self)

        // SCREAMING DEAD
        case ScreamingDeadMainAction(self, o, l) =>
            Ask(self).each(l)(ScreamingDeadAction(self, o, _)).cancel

        case ScreamingDeadAction(self, o, r) =>
            self.power -= 1
            self.payTax(r)
            Force(ScreamingDeadFollowAction(self, o, r, KingInYellow))

        case ScreamingDeadFollowAction(self, o, r, uc) =>
            val u = self.at(o).one(uc)
            u.region = r
            if (uc == KingInYellow)
                log(KingInYellow, "screamed from", o, "to", r)
            else
                log(u, "followed along")
            Ask(self).each(self.at(o, Undead)./(_.uclass))(ScreamingDeadFollowAction(self, o, r, _)).add(ScreamingDeadDoneAction(self))

        case ScreamingDeadDoneAction(self) =>
            self.oncePerRound :+= ScreamingDead

            AfterAction(self)

        // SHRIEK
        case ShriekMainAction(self, l) =>
            Ask(self).each(l)(ShriekAction(self, _)).cancel

        case ShriekAction(self, r) =>
            val b = self.all(Byakhee)./(_.region).but(r)
            if (b.none)
                EndAction(self)
            else
                Ask(self)
                    .each(b)(ShriekFromAction(self, _, r))
                    .use(x => self.oncePerAction.contains(Shriek).?(x.add(ShriekDoneAction(self))).|(x.cancel))

        case ShriekFromAction(self, o, r) =>
            val u = self.at(o).one(Byakhee)
            if (self.oncePerAction.has(Shriek).not) {
                self.power -= 1
                self.payTax(r)
                self.oncePerAction :+= Shriek
            }
            u.region = r
            log(u, "flew to", r, "from", o)
            Force(ShriekAction(self, r))

        case ShriekDoneAction(self) =>
            EndAction(self)

        // ZINGAYA
        case ZingayaMainAction(self, l) =>
            Ask(self).each(l./~(r => self.enemies./~(f => f.at(r, Acolyte).take(1))))(u => ZingayaAction(self, u.region, u.faction)).cancel

        case ZingayaAction(self, r, f) =>
            val c = f.at(r).one(Acolyte)
            self.power -= 1
            self.payTax(r)
            game.eliminate(c)
            self.place(Undead, r)
            self.log("replaced", c, "in", r, "with", Undead)
            EndAction(self)


        case _ => UnknownContinue
    }
}
