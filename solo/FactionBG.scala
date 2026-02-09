package cws

import hrf.colmat._

import html._


case object Ghoul extends FactionUnitClass(BG, "Ghoul", Monster, 1)
case object Fungi extends FactionUnitClass(BG, "Fungi from Yuggoth", Monster, 2)
case object DarkYoung extends FactionUnitClass(BG, "Dark Young", Monster, 3) {
    override def canControlGate(u : UnitFigure)(implicit game : Game) = u.faction.has(RedSign)
}
case object ShubNiggurath extends FactionUnitClass(BG, "Shub-Niggurath", GOO, 8)

case object Fertility extends FactionSpellbook(BG, "Fertility Cult")
case object Avatar extends FactionSpellbook(BG, "Avatar")

case object ThousandYoung extends FactionSpellbook(BG, "The Thousand Young")
case object Frenzy extends FactionSpellbook(BG, "Frenzy") with BattleSpellbook
case object Necrophagy extends FactionSpellbook(BG, "Necrophagy") with BattleSpellbook
case object Ghroth extends FactionSpellbook(BG, "Ghroth")
case object RedSign extends FactionSpellbook(BG, "The Red Sign")
case object BloodSacrifice extends FactionSpellbook(BG, "Blood Sacrifice")

case object Spread4 extends Requirement("Units in 4 Areas")
case object Spread6 extends Requirement("Units in 6 Areas")
case object Spread8 extends Requirement("Units in 8 Areas")
case object SpreadSocial extends Requirement("Share Areas will all enemies")
case object EliminateTwoCultists extends Requirement("Elminiate two cultists")
case object AwakenShubNiggurath extends Requirement("Awaken Shub-Niggurath")


case object BG extends Faction { f =>
    def name = "Black Goat"
    def short = "BG"
    def style = "bg"

    override def abilities = $(Fertility, Avatar)
    override def library = $(Frenzy, Ghroth, Necrophagy, RedSign, BloodSacrifice, ThousandYoung)
    override def requirements(options : $[GameOption]) = $(Spread4, Spread6, Spread8, SpreadSocial, EliminateTwoCultists, AwakenShubNiggurath)

    val allUnits =
        1.times(ShubNiggurath) ++
        3.times(DarkYoung) ++
        4.times(Fungi) ++
        2.times(Ghoul) ++
        6.times(Acolyte)

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case Ghoul | Fungi | DarkYoung if f.has(ThousandYoung) && f.has(ShubNiggurath) => u.cost - 1
        case _ => u.cost
    }

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case ShubNiggurath => (f.gates.has(r) && (f.cultists.num >= 2)).?(8)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Acolyte).num * f.has(Frenzy).??(1) +
        units(HighPriest).num * f.has(Frenzy).??(1) +
        units(Fungi).num * 1 +
        units(DarkYoung).num * 2 +
        units(ShubNiggurath).not(Zeroed).num * (
            f.gates.num +
            f.cultists.num +
            f.all(DarkYoung).num * f.has(RedSign).??(1)
        ) +
        neutralStrength(units, opponent)
}


case class BloodSacrificeDoomAction(self : BG) extends OptionFactionAction(BloodSacrifice) with DoomQuestion with Soft with PowerNeutral
case class BloodSacrificeAction(self : BG, r : Region, u : UnitRef) extends ForcedAction

case class EliminateTwoCultistsMainAction(self : BG) extends OptionFactionAction("Eliminate two " + self.styled(Cultist.plural) + " for a spellbook") with MainQuestion with Soft
case class EliminateTwoCultistsAction(self : BG, a : UnitRef, b : UnitRef) extends BaseFactionAction("Eliminate two " + self.styled(Cultist.plural) + " for a spellbook", implicit g => (a.region == b.region).?("Two from " + a.region)|("From " + a.region + " and " + b.region))

case class AwakenEliminateTwoCultistsAction(self : BG, uc : UnitClass, l : $[Region], a : UnitRef, b : UnitRef) extends BaseFactionAction("Eliminate two " + self.styled(Cultist.plural) + " to awaken " + self.styled(uc), implicit g => (a.region == b.region).?("Two from " + a.region)|("From " + a.region + " and " + b.region))

case class AvatarMainAction(self : BG, o : Region, l : $[Region]) extends OptionFactionAction(self.styled(Avatar)) with MainQuestion with Soft
case class AvatarAction(self : BG, o : Region, r : Region, f : Faction) extends BaseFactionAction(g => self.styled(Avatar), implicit g => "" + f + " in " + r + self.iced(r))
case class AvatarReplacementAction(self : Faction, f : BG, r : Region, o : Region, u : UnitRef) extends ForcedAction

case class GhrothMainAction(self : BG) extends OptionFactionAction(self.styled(Ghroth)) with MainQuestion
case class GhrothRollAction(f : BG, x : Int) extends ForcedAction
case class GhrothAction(f : BG, x : Int) extends ForcedAction
case class GhrothContinueAction(f : BG, x : Int, offers : $[Offer], forum : $[Faction], time : Int) extends ForcedAction
case class GhrothAskAction(f : BG, x : Int, offers : $[Offer], forum : $[Faction], time : Int, self : Faction, n : Int) extends BaseFactionAction(
    g => f.styled(Ghroth) + " demand " + x.styled("power") + " Cultists<br/>" + offers./(o => "" + o.f + " offers " + (o.n > 0).?(o.n.styled("power")).|("none")).mkString("<br/>") + "<hr/>" + self,
    (n < 0).?("Refuse to negotiate").|((x == n + offers./(_.n).sum).?("Offer".styled("highlight")).|("Offer") + " " + (n > 0).?(n.styled("power") + (x == n + offers./(_.n).sum).?((" Cultist" + (n > 1).??("s")).styled("highlight")).|((" Cultist" + (n > 1).??("s")))).|((x == n + offers./(_.n).sum).?("0 Cultists".styled("highlight")).|("0 Cultists")))
)
case class GhrothSplitAction(self : BG, x : Int, factions : $[Faction]) extends BaseFactionAction((x > 1).?("Eliminate " + x.styled("hightlight") + " Cultists from").|("Eliminate a Cultist from"), factions.mkString(" and "))
case class GhrothSplitNumAction(self : BG, x : Int, factions : $[Faction], full : $[Faction]) extends BaseFactionAction((x > 1).?("Eliminate " + x.styled("hightlight") + " Cultists from").|("Eliminate a Cultist from"), factions./(f => "" + f + (full.%(_ == f).num > 0).??(f.styled(" (" + full.%(_ == f).num + ")"))).mkString(", "))
case class GhrothEliminateAction(f : BG, l : $[Faction]) extends ForcedAction
case class GhrothTargetAction(self : Faction, u : UnitRef, f : BG, l : $[Faction]) extends ForcedAction // BaseFactionAction((factions.%(_ == self).num > 1).?("Eliminate " + factions.%(_ == self).num.styled("hightlight") + " Cultists").|("Eliminate a Cultist"), self.styled(uc) + " in " + r)
case class GhrothFactionAction(self : BG, f : Faction) extends BaseFactionAction("Place " + Acolyte.name, f.styled(Acolyte))
case class GhrothPlaceAction(self : BG, f : Faction, r : Region) extends BaseFactionAction("Place " + f.styled(Acolyte) + " in", r)


object BGExpansion extends Expansion {
    override def triggers()(implicit game : Game) {
        val f = BG
        f.satisfyIf(Spread4, "Have Units in four Areas", areas.%(r => f.at(r).any).num >= 4)
        f.satisfyIf(Spread6, "Have Units in six Areas", areas.%(r => f.at(r).any).num >= 6)
        f.satisfyIf(Spread8, "Have Units in eight Areas", areas.%(r => f.at(r).any).num >= 8)
        f.satisfyIf(SpreadSocial, "Share Areas with all enemies", f.enemies.forall(e => areas.exists(r => f.at(r).any && e.at(r).any)), f.enemies.num)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // DOOM
        case DoomAction(f : BG) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            if (f.can(BloodSacrifice) && f.has(ShubNiggurath) && f.cultists.any)
                + BloodSacrificeDoomAction(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            + DoomDoneAction(f)

            asking

        // ACTIONS
        case MainAction(f : BG) if f.active.not =>
            UnknownContinue

        case MainAction(f : BG) if f.acted =>
            implicit val asking = Asking(f)

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            game.summons(f)

            game.reveals(f)

            game.endTurn(f)(true)

            asking

        case MainAction(f : BG) =>
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

            if (f.has(Avatar) && f.has(ShubNiggurath)) {
                val r = f.goo(ShubNiggurath).region
                val t = f.taxIn(r)
                areas.but(r).%(f.affords(1 + t)).%(r => factionlike.exists(_.at(r).vulnerable.any)).some.foreach { l =>
                    + AvatarMainAction(f, r, l)
                }
            }

            if (f.has(Ghroth) && f.power >= 2)
                + GhrothMainAction(f)

            if (f.needs(EliminateTwoCultists) && f.cultists.num >= 2)
                + EliminateTwoCultistsMainAction(f)

            game.neutralSpellbooks(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any || f.oncePerRound.contains(Fertility))

            asking

        // BLOOD SACRIFICE
        case BloodSacrificeDoomAction(self) =>
            Ask(self).each(self.cultists.sortP)(u => BloodSacrificeAction(self, u.region, u).as(u.full, "in", u.region)(BloodSacrifice)).cancel

        case BloodSacrificeAction(self, r, u) =>
            game.eliminate(u)
            self.oncePerTurn :+= BloodSacrifice
            self.takeES(1)
            self.log("sacrificed", u, "in", r, "for", 1.es)

            game.checkGatesLost()

            CheckSpellbooksAction(DoomAction(self))

        // FERTILITY
        case SummonedAction(self, uc, r, l) if self.has(Fertility) && !self.ignored(Fertility) =>
            self.oncePerRound :+= Fertility
            AfterAction(self)

        // ELIMINATE CULTISTS
        case EliminateTwoCultistsMainAction(self) =>
            val cultists = areas./~(r => self.at(r).cultists.sortP.take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).dropStarting./(b => (a, b))).distinct
            Ask(self).each(pairs)((a, b) => EliminateTwoCultistsAction(self, a, b)).cancel

        case EliminateTwoCultistsAction(self, a, b) =>
            $(a, b).foreach { u =>
                log(u, "in", u.region, "was sacrificed")
                game.eliminate(u)
            }
            self.satisfy(EliminateTwoCultists, "Eliminate two Cultists")
            EndAction(self)

        // AWAKEN
        case AwakenMainAction(self : BG, uc : ShubNiggurath.type, locations) =>
            val cultists = areas./~(r => self.at(r).cultists.sortP.take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).dropStarting./(b => (a, b))).distinct
            Ask(self).each(pairs)((a, b) => AwakenEliminateTwoCultistsAction(self, uc, locations, a, b)).cancel

        case AwakenEliminateTwoCultistsAction(self, uc, locations, a, b) =>
            val q = locations./~(r => self.awakenCost(uc, r)./(cost => AwakenAction(self, uc, r, cost)))
            $(a, b).foreach { u =>
                log(u, "in", u.region, "was sacrificed")
                game.eliminate(u)
            }
            Ask(self).list(q)

        case AwakenedAction(self, ShubNiggurath, r, cost) =>
            self.satisfy(AwakenShubNiggurath, "Awaken Shub-Niggurath")

            EndAction(self)

        // AVATAR
        case AvatarMainAction(self, o, l) =>
            Ask(self).some(l)(r => factionlike.%(_.at(r).vulnerable.any).map(f => AvatarAction(self, o, r, f))).cancel

        case AvatarAction(self, o, r, e) =>
            self.power -= 1
            self.payTax(r)
            self.payTax(o)

            val sn = self.goo(ShubNiggurath)
            sn.region = r
            log(sn, "avatared to", r)

            val l = e.at(r).vulnerable.preferablyNotOnGate

            Ask(e.real.?(e).|(self)).each(l)(u => AvatarReplacementAction(e, self, r, o, u).as(u.full)(Avatar, "replacement from", r, "to", o))

        case AvatarReplacementAction(self, f, r, o, u) =>
            log(u, "was sent back to", o)
            u.region = o
            u.onGate = false
            EndAction(f)

        // GHROTH
        case GhrothMainAction(self) =>
            self.power -= 2
            RollD6("Roll for " + self.styled(Ghroth), x => GhrothRollAction(self, x))

        case GhrothRollAction(f, x) =>
            var b = f.all(Fungi)./(_.region).distinct.num

            if (x > b) {
                f.log("failed", Ghroth.full, "with roll of", ("[" + x.styled("power") + "]"))

                val fs = factions.%(_.pool(Acolyte).any)
                if (fs.any)
                    Ask(f).each(fs)(e => GhrothFactionAction(f, e))
                else {
                    log("No Cultists were available to place")
                    EndAction(f)
                }
            }
            else {
                f.log("used", Ghroth.full, "and rolled", ("[" + x.styled("power") + "]"))

                val n = f.enemies./~(_.cultists.onMap).num
                if (n <= x) {
                    if (n < x)
                        log("Not enough Cultists among other factions")

                    f.enemies./~(_.cultists.onMap).foreach { c =>
                        log(c, "was eliminated in", c.region)
                        game.eliminate(c)
                    }

                    EndAction(f)
                }
                else {
                    Force(GhrothAction(f, x))
                }
            }

        case GhrothAction(f, x) =>
            f.enemies.%(_.cultists.onMap.none).foreach(f => f.log("had no Cultists"))

            val forum = f.enemies.%(_.cultists.onMap.any)
            Force(GhrothContinueAction(f, x, Nil, forum, forum.num * 3))

        case GhrothContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropEnding

            if (offers./(_.n).sum == x && offers.num == xforum.num) {
                Force(GhrothEliminateAction(f, offers./~(o => o.n.times(o.f))))
            }
            else
            if (time < 0 || xforum./(_.cultists.onMap.num).sum < x) {
                f.log("eliminated", x, "Cultist" + (x > 1).??("s"))

                val affected = f.enemies.%(_.cultists.onMap.any)
                val split = 1.to(x)./~(n => affected.combinations(n))
                val valid = split.%(_./(_.cultists.onMap.num).sum >= x)

                Ask(f).each(valid)(l => GhrothSplitAction(f, x, l))
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

                offers = offers.%(_.f != next)
                val offered = offers./(_.n).sum
                val maxp = min(next.cultists.onMap.num, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(_.cultists.onMap.num).sum
                val minp = max(1, x - maxother)

                Ask(next).each(-1 +: 0 +: minp.to(maxp).$)(n => GhrothAskAction(f, x, offers, forum, time - (random() * 1.0).round.toInt, next, n))
            }


        case GhrothAskAction(f, x, offers, forum, time, self, n) =>
            if (n < 0) {
                self.log("refused to negotiate")

                Force(GhrothContinueAction(f, x, offers, forum.but(self), time))
            }
            else {
                if (n == 0)
                    self.log("offered no Cultists")
                else
                    self.log("offered to lose", n.styled("highlight"))

                Force(GhrothContinueAction(f, x, Offer(self, n) +: offers, forum, time))
            }

        case GhrothSplitAction(self, x, ff) =>
            val split = ff./~(f => (x - ff.num).times(f)).combinations(x - ff.num)./(_ ++ ff)./(l => ff./~(f => l.count(f).times(f)))
            val valid = split.%(s => ff.%(f => f.cultists.onMap.num < s.%(_ == f).num).none)
            Ask(self).each(valid)(l => GhrothSplitNumAction(self, x, ff, l))

        case GhrothSplitNumAction(self, x, ff, l) =>
            Force(GhrothEliminateAction(self, l))

        case GhrothEliminateAction(f, Nil) =>
            EndAction(f)

        case GhrothEliminateAction(f, l) =>
            val e = l.first
            val cultists = e.cultists.preferablyNotOnGate
            val n = l.count(e)

            // If we want to allow SL to eliminate a cultist in Cursed Slumber (which you should be able to do, according to the FAQ).
            // val cultists = {
            //     val base = areas./~(r => next.at(r, Cultist))
            //     val slumberCultists = next.at(SL.slumber, Cultist)
            //     if (slumberCultists.any) {
            //         base ++ slumberCultists
            //     } else base
            // }

            Ask(e).each(cultists)(u => GhrothTargetAction(e, u, f, l.dropStarting).as(u.full, "in", u.region)("Eliminate", (n > 1).?(n.styled("hightlight")).|("a"), "Cultist".s(n)))

        case GhrothTargetAction(self, u, f, l) =>
            log(u, "was eliminated in", u.region)
            game.eliminate(u)
            Force(GhrothEliminateAction(f, l))

        case GhrothFactionAction(self, f) =>
            Ask(self).each(areas)(r => GhrothPlaceAction(self, f, r))

        case GhrothPlaceAction(self, f, r) =>
            f.place(Acolyte, r)
            log(f.styled(Acolyte), "was placed in", r)
            EndAction(self)

        // ...
        case _ => UnknownContinue
    }
}
