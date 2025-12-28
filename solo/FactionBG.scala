package cws

import hrf.colmat._

import Html._


case object Ghoul extends FactionUnitClass(BG, "Ghoul", Monster, 1)
case object Fungi extends FactionUnitClass(BG, "Fungi from Yuggoth", Monster, 2)
case object DarkYoung extends FactionUnitClass(BG, "Dark Young", Monster, 3) {
    override def canControlGate(u : UnitFigure)(implicit game : Game) = u.faction.has(RedSign)
}
case object ShubNiggurath extends FactionUnitClass(BG, "Shub-Niggurath", GOO, 8)

case object Fertility extends FactionSpellbook(BG, "Fertility Cult")
case object Avatar extends FactionSpellbook(BG, "Avatar")

case object ThousandYoung extends FactionSpellbook(BG, "The Thousand Young")
case object Frenzy extends FactionSpellbook(BG, "Frenzy")
case object Necrophagy extends FactionSpellbook(BG, "Necrophagy")
case object Ghroth extends FactionSpellbook(BG, "Ghroth")
case object RedSign extends FactionSpellbook(BG, "The Red Sign")
case object BloodSacrifice extends FactionSpellbook(BG, "Blood Sacrifice")

case object Spread4 extends Requirement("Units in 4 Areas")
case object Spread6 extends Requirement("Units in 6 Areas")
case object Spread8 extends Requirement("Units in 8 Areas")
case object SpreadSocial extends Requirement("Share Areas will all enemies")
case object Eliminate2Cultists extends Requirement("Elminiate two cultists")
case object AwakenShubNiggurath extends Requirement("Awaken Shub-Niggurath")


case object BG extends Faction { f =>
    def name = "Black Goat"
    def short = "BG"
    def style = "bg"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $(Fertility, Avatar)
    override def library = $(Frenzy, Ghroth, Necrophagy, RedSign, BloodSacrifice, ThousandYoung)
    override def requirements(options : $[GameOption]) = $(Spread4, Spread6, Spread8, SpreadSocial, Eliminate2Cultists, AwakenShubNiggurath)

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
        case ShubNiggurath => (f.gates.has(r) && (f.all.cultists.num >= 2)).?(8)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Acolyte).num * f.has(Frenzy).??(1) +
        units(HighPriest).num * f.has(Frenzy).??(1) +
        units(Fungi).num * 1 +
        units(DarkYoung).num * 2 +
        units(ShubNiggurath).%!(_.has(Zeroed)).num * (
            f.gates.num +
            f.all.cultists.num +
            f.all(DarkYoung).num * f.has(RedSign).??(1)
        ) +
        neutralStrength(units, opponent)
}


case class BloodSacrificeDoomAction(self : Faction) extends OptionFactionAction(BloodSacrifice) with DoomQuestion with Soft with PowerNeutral
case class BloodSacrificeAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(BloodSacrifice, self.styled(uc) + " in " + r)

case class Eliminate2CultistsMainAction(self : Faction) extends OptionFactionAction("Eliminate two " + self.styled(Cultist.plural) + " for a spellbook") with MainQuestion with Soft
case class Eliminate2CultistsAction(self : Faction, a : Region, b : Region) extends BaseFactionAction("Eliminate two " + self.styled(Cultist.plural) + " for a spellbook", (a == b).?("Two from " + a)|("From " + a + " and " + b))

case class AvatarMainAction(self : Faction, o : Region, l : $[Region]) extends OptionFactionAction(self.styled(Avatar)) with MainQuestion with Soft
case class AvatarAction(self : Faction, o : Region, r : Region, f : Faction) extends BaseFactionAction(g => self.styled(Avatar), implicit g => "" + f + " in " + r + self.iced(r))
case class AvatarReplacementAction(self : Faction, f : Faction, r : Region, o : Region, uc : UnitClass) extends BaseFactionAction(Avatar.full + " replacement from " + r + " to " + o, self.styled(uc))

case class GhrothMainAction(self : Faction) extends OptionFactionAction(self.styled(Ghroth)) with MainQuestion
case class GhrothRollAction(f : Faction, x : Int) extends ForcedAction
case class GhrothAction(f : Faction, x : Int) extends ForcedAction
case class GhrothContinueAction(f : Faction, x : Int, offers : $[Offer], forum : $[Faction], time : Int) extends ForcedAction
case class GhrothAskAction(f : Faction, x : Int, offers : $[Offer], forum : $[Faction], time : Int, self : Faction, n : Int) extends BaseFactionAction(
    g => f.styled(Ghroth) + " demand " + x.styled("power") + " Cultists<br/>" + offers./(o => "" + o.f + " offers " + (o.n > 0).?(o.n.styled("power")).|("none")).mkString("<br/>") + "<hr/>" + self,
    (n < 0).?("Refuse to negotiate").|((x == n + offers./(_.n).sum).?("Offer".styled("highlight")).|("Offer") + " " + (n > 0).?(n.styled("power") + (x == n + offers./(_.n).sum).?((" Cultist" + (n > 1).??("s")).styled("highlight")).|((" Cultist" + (n > 1).??("s")))).|((x == n + offers./(_.n).sum).?("0 Cultists".styled("highlight")).|("0 Cultists")))
)
case class GhrothSplitAction(self : Faction, x : Int, factions : $[Faction]) extends BaseFactionAction((x > 1).?("Eliminate " + x.styled("hightlight") + " Cultists from").|("Eliminate a Cultist from"), factions.mkString(" and "))
case class GhrothSplitNumAction(self : Faction, x : Int, factions : $[Faction], full : $[Faction]) extends BaseFactionAction((x > 1).?("Eliminate " + x.styled("hightlight") + " Cultists from").|("Eliminate a Cultist from"), factions./(f => "" + f + (full.%(_ == f).num > 0).??(f.styled(" (" + full.%(_ == f).num + ")"))).mkString(", "))
case class GhrothEliminateAction(f : Faction, factions : $[Faction]) extends ForcedAction
case class GhrothUnitAction(self : Faction, uc : UnitClass, r : Region, f : Faction, factions : $[Faction]) extends BaseFactionAction((factions.%(_ == self).num > 1).?("Eliminate " + factions.%(_ == self).num.styled("hightlight") + " Cultists").|("Eliminate a Cultist"), self.styled(uc) + " in " + r)
case class GhrothFactionAction(self : Faction, f : Faction) extends BaseFactionAction("Place " + Acolyte.name, f.styled(Acolyte))
case class GhrothPlaceAction(self : Faction, f : Faction, r : Region) extends BaseFactionAction("Place " + f.styled(Acolyte) + " in", r)


object BGExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // BLOOD SACRIFICE
        case BloodSacrificeDoomAction(self) =>
            Ask(self).each(self.all.cultists)(c => BloodSacrificeAction(self, c.region, c.uclass)).cancel

        case BloodSacrificeAction(self, r, uc) =>
            val c = self.at(r).one(uc)

            game.eliminate(c)
            self.oncePerTurn :+= BloodSacrifice
            self.takeES(1)
            self.log("sacrificed", c, "in", r, "for", 1.es)

            game.checkGatesLost()

            CheckSpellbooksAction(DoomAction(self))

        // ELIMINATE CULTISTS
        case Eliminate2CultistsMainAction(self) =>
            val cultists = board.regions./~(r => self.at(r, Cultist).take(2))
            val pairs = cultists./~(a => cultists.dropWhile(_ != a).drop(1)./(b => (a.region, b.region))).distinct
            Ask(self).each(pairs)((a, b) => Eliminate2CultistsAction(self, a, b)).cancel

        case Eliminate2CultistsAction(self, a, b) =>
            $(a, b).foreach { r =>
                val c = self.at(r).one(Cultist)
                log(c, "in", c.region, "was sacrificed")
                game.eliminate(c)
            }
            self.satisfy(Eliminate2Cultists, "Eliminate two Cultists")
            EndAction(self)

        // AVATAR
        case AvatarMainAction(self, o, l) =>
            val variants = l.flatMap { r =>
                nfactions.filter(_.at(r).vulnerable.any).map(f =>
                    AvatarAction(self, o, r, f)
                )
            }

            Ask(self).list(variants).cancel

        case AvatarAction(self, o, r, f) =>
            self.power -= 1
            self.payTax(r)
            val sn = self.goo(ShubNiggurath)
            sn.region = r
            log(sn, "avatared to", r)
            self.payTax(o)
            val units = f.at(r).vulnerable
            val l = units.useIf(units./(_.uclass).distinct.num == 1)(_.take(1))

            Ask(f.real.?(f).|(self)).each(l)(u => AvatarReplacementAction(f, self, r, o, u.uclass))

        case AvatarReplacementAction(self, f, r, o, uc) =>
            val u = self.at(r).one(uc)
            log(u, "was sent back to", o)
            u.region = o
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
                    Ask(f).each(fs)(GhrothFactionAction(f, _))
                else {
                    log("No Cultists were available to place")
                    EndAction(f)
                }
            }
            else {
                f.log("used", Ghroth.full, "and rolled", ("[" + x.styled("power") + "]"))

                val n = f.enemies./~(_.onMap.cultists).num
                if (n <= x) {
                    if (n < x)
                        log("Not enough Cultists among other factions")

                    f.enemies./~(_.onMap.cultists).foreach { c =>
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
            f.enemies.%(_.onMap.cultists.none).foreach(f => f.log("had no Cultists"))

            val forum = f.enemies.%(_.onMap.cultists.any)
            Force(GhrothContinueAction(f, x, Nil, forum, forum.num * 3))

        case GhrothContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropRight(1)

            if (offers./(_.n).sum == x && offers.num == xforum.num) {
                Force(GhrothEliminateAction(f, offers./~(o => o.n.times(o.f))))
            }
            else
            if (time < 0 || xforum./(_.onMap.cultists.num).sum < x) {
                f.log("eliminated", x, "Cultist" + (x > 1).??("s"))

                val affected = f.enemies.%(_.onMap.cultists.any)
                val split = 1.to(x)./~(n => affected.combinations(n))
                val valid = split.%(_./(_.onMap.cultists.num).sum >= x)

                Ask(f).each(valid)(GhrothSplitAction(f, x, _))
            }
            else {
                if (xforum.num == 1)
                    time = 0

                if (time == xforum.num) {
                    time -= 1

                    log("Negotiations time was running out")
                }

                val next = xforum.first
                val forum = xforum.drop(1) :+ next

                offers = offers.%(_.f != next)
                val offered = offers./(_.n).sum
                val maxp = min(next.onMap.cultists.num, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(_.onMap.cultists.num).sum
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
            val valid = split.%(s => ff.%(f => f.onMap.cultists.num < s.%(_ == f).num).none)
            Ask(self).each(valid)(l => GhrothSplitNumAction(self, x, ff, l))

        case GhrothSplitNumAction(self, x, ff, full) =>
            Force(GhrothEliminateAction(self, full))

        case GhrothEliminateAction(f, full) =>
            if (full.none)
                EndAction(f)
            else {
                val next = full.first
                val cultists = board.regions./~(r => next.at(r, Cultist))
                // If we want to allow SL to eliminate a cultist in Cursed Slumber (which you should be able to do, according to the FAQ).
                // val cultists = {
                //     val base = board.regions./~(r => next.at(r, Cultist))
                //     val slumberCultists = next.at(SL.slumber, Cultist)
                //     if (slumberCultists.any) {
                //         base ++ slumberCultists
                //     } else base
                // }
                Ask(next).each(cultists)(c => GhrothUnitAction(next, c.uclass, c.region, f, full.drop(1)))
            }

        case GhrothUnitAction(self, uc, r, f, full) =>
            val c = self.at(r).one(uc)
            log(c, "was eliminated in", c.region)
            game.eliminate(c)
            Force(GhrothEliminateAction(f, full))

        case GhrothFactionAction(self, f) =>
            Ask(self).each(board.regions)(r => GhrothPlaceAction(self, f, r))

        case GhrothPlaceAction(self, f, r) =>
            f.place(Acolyte, r)
            log(f.styled(Acolyte), "was placed in", r)
            EndAction(self)


        case _ => UnknownContinue
    }
}
