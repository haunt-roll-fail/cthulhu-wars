package cws

import hrf.colmat._

import html._


case object Undead extends FactionUnitClass(YS, "Undead", Monster, 1)
case object Byakhee extends FactionUnitClass(YS, "Byakhee", Monster, 2)
case object KingInYellow extends FactionUnitClass(YS, "King in Yellow", GOO, 4)
case object Hastur extends FactionUnitClass(YS, "Hastur", GOO, 10)

case object Feast extends FactionSpellbook(YS, "Feast")
case object Desecrate extends FactionSpellbook(YS, "Desecrate")
case object Vengeance extends FactionSpellbook(YS, "Vengeance") with BattleSpellbook

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
        case KingInYellow => (game.allGates.has(r).not && f.present(r)).?(4)
        case Hastur => (f.gates.has(r) && f.at(r, KingInYellow).any).?(10)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Undead).num.useIf(_ > 0)(_ - 1) +
        units(Byakhee).num.useIf(_ > 0)(_ + 1) +
        units(Hastur).not(Zeroed).num * game.ritualCost +
        neutralStrength(units, opponent)
}


case class Provide3DoomMainAction(self : YS) extends OptionFactionAction("Get spellbook and another faction gets " + 3.doom) with MainQuestion with Soft
case class Provide3DoomAction(self : YS, f : Faction) extends BaseFactionAction("Get spellbook", "" + f + " gets " + 3.doom)

case class DesecrateMainAction(self : YS, r : Region, te : Boolean) extends OptionFactionAction(implicit g => "" + Desecrate + " " + r + te.??(" (" + ThirdEye + ")") + self.iced(r)) with MainQuestion
case class DesecrateRollAction(f : YS, r : Region, te : Boolean, x : Int) extends ForcedAction
case class DesecratePlaceAction(self : YS, r : Region, uc : UnitClass) extends BaseFactionAction("Place in " + r, uc.styled(self))

case class HWINTBNMainAction(self : YS, o : Region, l : $[Region]) extends OptionFactionAction(HWINTBN) with MainQuestion with Soft
case class HWINTBNAction(self : YS, o : Region, r : Region) extends BaseFactionAction(HWINTBN, implicit g => r + self.iced(r))

case class ScreamingDeadMainAction(self : YS, o : Region, l : $[Region]) extends OptionFactionAction(ScreamingDead) with MainQuestion with Soft
case class ScreamingDeadAction(self : YS, o : Region, r : Region) extends BaseFactionAction(ScreamingDead, implicit g => r + self.iced(r))
case class ScreamingDeadFollowAction(self : YS, o : Region, r : Region, uc : UnitClass) extends BaseFactionAction("Follow " + KingInYellow, uc.styled(self))
case class ScreamingDeadDoneAction(self : YS) extends BaseFactionAction(None, "Done")

case class ShriekMainAction(self : YS, l : $[Region]) extends OptionFactionAction(Shriek) with MainQuestion with Soft
case class ShriekAction(self : YS, r : Region) extends BaseFactionAction(Shriek, implicit g => r + self.iced(r))
case class ShriekFromAction(self : YS, o : Region, r : Region) extends BaseFactionAction("" + Shriek + " to " + r, "" + Byakhee + " from " + o)
case class ShriekDoneAction(self : YS) extends BaseFactionAction(None, "Done")

case class ZingayaMainAction(self : YS, l : $[Region]) extends OptionFactionAction(Zingaya) with MainQuestion with Soft
case class ZingayaAction(self : YS, r : Region, f : Faction) extends BaseFactionAction(Zingaya, implicit g => Acolyte.styled(f) + " in " + r + self.iced(r))


object YSExpansion extends Expansion {
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (u.uclass.utype == Cultist && u.faction.has(Passion) && u.region.glyph.onMap)
            u.faction.oncePerAction :+= Passion
    }

    override def afterAction()(implicit game : Game) {
        factions.%(_.has(Passion)).%(_.oncePerAction.has(Passion)).foreach { f =>
            f.power += 1

            f.log("got", 1.power, "from", Passion)
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // ACTIONS
        case MainAction(f : YS) if f.active.not =>
            UnknownContinue

        case MainAction(f : YS) if f.acted =>
            UnknownContinue

        case MainAction(f : YS) =>
            implicit val asking = Asking(f)

            game.moves(f)

            game.captures(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            game.summons(f)

            game.awakens(f)

            game.independents(f)

            if (f.has(Desecrate) && f.has(KingInYellow) && game.desecrated.num <= 12) {
                val r = f.goo(KingInYellow).region
                if (game.desecrated.has(r).not) {
                    val te = f.has(Hastur) && f.has(ThirdEye)
                    if (f.affords(te.?(1).|(2))(r))
                        + DesecrateMainAction(f, r, te)
                }
            }

            if (f.can(HWINTBN) && f.used(ScreamingDead).not && f.has(Hastur)) {
                val o = f.goo(Hastur).region
                areas.%(f.affords(1)).but(o).%(r => factions.%(_.at(r, Cultist).any).any).some.foreach { l =>
                    + HWINTBNMainAction(f, o, l)
                }
            }

            if (f.can(ScreamingDead) && f.used(HWINTBN).not && f.has(KingInYellow)) {
                val o = f.goo(KingInYellow).region
                game.board.connected(o).%(f.affords(1)).some.foreach { l =>
                    + ScreamingDeadMainAction(f, o, l)
                }
            }

            if (f.has(Zingaya) && f.pool(Undead).any)
                areas.%(f.affords(1)).%(r => f.at(r, Undead).any).%(r => f.enemies.exists(_.at(r, Acolyte).any)).some.foreach { l =>
                    + ZingayaMainAction(f, l)
                }

            if (f.has(Shriek) && f.has(Byakhee))
                areas.%(f.affords(1)).%(r => f.all(Byakhee).%(_.region != r).any).some.foreach { l =>
                    + ShriekMainAction(f, l)
                }

            if (f.needs(Provide3Doom))
                + Provide3DoomMainAction(f)

            game.neutralSpellbooks(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any || f.oncePerRound.contains(HWINTBN) || f.oncePerRound.contains(ScreamingDead))

            asking

        // AWAKEN
        case AwakenedAction(self, KingInYellow, r, cost) =>
            self.satisfy(AwakenKing, "Awaken King in Yellow")

            EndAction(self)

        case AwakenedAction(self, Hastur, r, cost) =>
            self.satisfy(AwakenHastur, "Awaken Hastur")

            EndAction(self)

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
            RollD6("Roll for " + Desecrate + " in " + r, x => DesecrateRollAction(self, r, te, x))

        case DesecrateRollAction(self, r, te, x) =>
            if (self.at(r).num >= x) {
                log(KingInYellow, "desecrated", r, "with roll [" + x.styled("power") + "]")
                if (te) {
                    self.log("gained", 1.es, "using", ThirdEye)
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
                log(KingInYellow, "failed", r, "desecration with roll [" + x.styled("power") + "]")

            val us = (self.pool.cultists ++ self.pool.monsters)./(_.uclass).distinct.%(_.cost <= 2)

            if (us.any)
                Ask(self).each(us)(uc => DesecratePlaceAction(self, r, uc))
            else
                EndAction(self)

        case DesecratePlaceAction(self, r, uc) =>
            self.place(uc, r)
            log(uc.styled(self), "appeared in", r)
            EndAction(self)

        // HWINTBN
        case HWINTBNMainAction(self, o, l) =>
            Ask(self).each(l)(r => HWINTBNAction(self, o, r)).cancel

        case HWINTBNAction(self, o, r) =>
            self.power -= 1
            self.payTax(r)
            self.at(o, Hastur).first.region = r
            self.oncePerRound :+= HWINTBN

            log(Hastur, "heard his name in", r)

            AfterAction(self)

        // SCREAMING DEAD
        case ScreamingDeadMainAction(self, o, l) =>
            Ask(self).each(l)(r => ScreamingDeadAction(self, o, r)).cancel

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
            Ask(self).each(self.at(o, Undead)./(_.uclass))(uc => ScreamingDeadFollowAction(self, o, r, uc)).add(ScreamingDeadDoneAction(self))

        case ScreamingDeadDoneAction(self) =>
            self.oncePerRound :+= ScreamingDead

            AfterAction(self)

        // SHRIEK
        case ShriekMainAction(self, l) =>
            Ask(self).each(l)(r => ShriekAction(self, r)).cancel

        case ShriekAction(self, r) =>
            val b = self.all(Byakhee)./(_.region).but(r)
            if (b.none)
                EndAction(self)
            else
                Ask(self)
                    .each(b)(o => ShriekFromAction(self, o, r))
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
            Ask(self).some(l)(r => self.enemies.%(f => f.at(r, Acolyte).any)./(e => ZingayaAction(self, r, e))).cancel

        case ZingayaAction(self, r, f) =>
            val c = f.at(r).one(Acolyte)
            self.power -= 1
            self.payTax(r)
            game.eliminate(c)
            self.place(Undead, r)
            self.log("replaced", c, "in", r, "with", Undead)
            EndAction(self)

        // ...
        case _ => UnknownContinue
    }
}
