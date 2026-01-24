package cws

import hrf.colmat._

import html._


case object Nightgaunt extends FactionUnitClass(CC, "Nightgaunt", Monster, 1)
case object FlyingPolyp extends FactionUnitClass(CC, "Flying Polyp", Monster, 2)
case object HuntingHorror extends FactionUnitClass(CC, "Hunting Horror", Monster, 3)
case object Nyarlathotep extends FactionUnitClass(CC, "Nyarlathotep", GOO, 10)


case object Harbinger extends FactionSpellbook(CC, "The Harbinger") with BattleSpellbook
case object Flight extends FactionSpellbook(CC, "Flight")

case object Abduct extends FactionSpellbook(CC, "Abduct") with BattleSpellbook
case object SeekAndDestroy extends FactionSpellbook(CC, "Seek and Destroy") with BattleSpellbook
case object Invisibility extends FactionSpellbook(CC, "Invisibility") with BattleSpellbook
case object Madness extends FactionSpellbook(CC, "Madness") with BattleSpellbook
case object Emissary extends FactionSpellbook(CC, "Emissary of the Outer Gods") with BattleSpellbook
case object ThousandForms extends FactionSpellbook(CC, "The Thousand Forms")


case object Pay4Power extends Requirement("Pay 4 Power")
case object Pay6Power extends Requirement("Pay 6 Power")
case object Gates3Power12 extends Requirement("Control 3 Gates / Have 12 Power")
case object Gates4Power15 extends Requirement("Control 4 Gates / Have 15 Power")
case object CaptureCultist extends Requirement("Capture cultist")
case object AwakenNyarlathotep extends Requirement("Awaken Nyarlathotep")


case object CC extends Faction { f =>
    def name = "Crawling Chaos"
    def short = "CC"
    def style = "cc"

    override def abilities = $(Flight, Harbinger)
    override def library = $(Abduct, Invisibility, SeekAndDestroy, Emissary, ThousandForms, Madness)
    override def requirements(options : $[GameOption]) = $(Pay4Power, Pay6Power, Gates3Power12, Gates4Power15, CaptureCultist, AwakenNyarlathotep)

    val allUnits =
        1.times(Nyarlathotep) ++
        2.times(HuntingHorror) ++
        3.times(FlyingPolyp) ++
        3.times(Nightgaunt) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case Nyarlathotep => f.gates.has(r).?(10)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(FlyingPolyp).num * 1 +
        units(HuntingHorror).num * 2 +
        units(Nyarlathotep).not(Zeroed).num * (f.spellbooks.num + opponent.spellbooks.num) +
        neutralStrength(units, opponent)
}


case class ThousandFormsMainAction(self : CC) extends OptionFactionAction(self.styled(ThousandForms)) with MainQuestion
case class ThousandFormsRollAction(f : CC, x : Int) extends ForcedAction

case class ThousandFormsAction(f : CC, x : Int) extends ForcedAction
case class ThousandFormsContinueAction(f : CC, x : Int, offers : $[Offer], forum : $[Faction], time : Int) extends ForcedAction
case class ThousandFormsAskAction(f : CC, x : Int, offers : $[Offer], forum : $[Faction], time : Int, self : Faction, n : Int) extends BaseFactionAction(
    g => f.styled(ThousandForms) + " demand " + x.power + "<br/>" + offers./(o => "" + o.f + " offers " + (o.n > 0).?(o.n.styled("power")).|("none")).mkString("<br/>") + "<hr/>" + self,
    (n < 0).?("Refuse to negotiate").|((x == n + offers./(_.n).sum).?("Offer".styled("highlight")).|("Offer") + " " + (n > 0).?(n.styled("power") + (x == n + offers./(_.n).sum).?(" Power".styled("highlight")).|(" Power")).|((x == n + offers./(_.n).sum).?("0 Power".styled("highlight")).|("0 Power")))
)

case class Pay4PowerMainAction(self : CC) extends OptionFactionAction("Pay " + 4.power + " for a spellbook") with MainQuestion
case class Pay6PowerMainAction(self : CC) extends OptionFactionAction("Pay " + 6.power + " for a spellbook") with MainQuestion
case class Pay10PowerMainAction(self : CC) extends OptionFactionAction("Pay " + 10.power + " for two spellbooks") with MainQuestion


object CCExpansion extends Expansion {
    override def triggers()(implicit game : Game) {
        val f = CC
        f.satisfyIf(Gates3Power12, "Have 12 Power", f.power >= 12)
        f.satisfyIf(Gates4Power15, "Have 15 Power", f.power >= 15)

        f.satisfyIf(Gates3Power12, "Control three Gates", f.gates.num >= 3)
        f.satisfyIf(Gates4Power15, "Control four Gates", f.gates.num >= 4)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // ACTIONS
        case MainAction(f : CC) if f.active.not =>
            UnknownContinue

        case MainAction(f : CC) if f.acted =>
            UnknownContinue

        case MainAction(f : CC) =>
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

            if (f.can(ThousandForms) && f.has(Nyarlathotep))
                + ThousandFormsMainAction(f)

            if (f.needs(Pay4Power) && f.power >= 4)
                + Pay4PowerMainAction(f)

            if (f.needs(Pay6Power) && f.power >= 6)
                + Pay6PowerMainAction(f)

            if (f.needs(Pay4Power) && f.needs(Pay6Power) && f.power >= 10)
                + Pay10PowerMainAction(f)

            game.neutralSpellbooks(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // AWAKEN
        case AwakenedAction(self, Nyarlathotep, r, cost) =>
            self.satisfy(AwakenNyarlathotep, "Awaken Nyarlathotep")

            EndAction(self)

        // PAYXPOWER
        case Pay4PowerMainAction(self) =>
            self.power -= 4
            self.log("paid", 4.power)
            self.satisfy(Pay4Power, "Pay four Power")
            EndAction(self)

        case Pay6PowerMainAction(self) =>
            self.power -= 6
            self.log("paid", 6.power)
            self.satisfy(Pay6Power, "Pay six Power")
            EndAction(self)

        case Pay10PowerMainAction(self) =>
            self.power -= 10
            self.log("paid", 10.power)
            self.satisfy(Pay4Power, "Pay four Power")
            self.satisfy(Pay6Power, "Pay six Power")
            EndAction(self)

        // 1000F
        case ThousandFormsMainAction(self) =>
            self.oncePerTurn +:= ThousandForms
            RollD6("Roll for " + ThousandForms.full, x => ThousandFormsRollAction(self, x))

        case ThousandFormsRollAction(f, x) =>
            f.log("used", ThousandForms.full, "and rolled", ("[" + x.styled("power") + "]"))
            log("Other factions were to lose", x.power)
            Force(ThousandFormsAction(f, x))

        case ThousandFormsAction(f, x) =>
            val mp = factions./(_.power).max
            val sm = f.enemies./(_.power).sum

            if (sm < x) {
                log("Not enough power among other factions")
                f.log("got", x.power)
                f.power += x
                EndAction(f)
            }
            else {
                f.enemies.%(_.power == 0).foreach(f => f.log("had no power"))

                val forum = f.enemies.%(_.power > 0)
                Force(ThousandFormsContinueAction(f, x, $, forum, forum.num * 3))
            }

        case ThousandFormsContinueAction(f, x, xoffers, xforum, xtime) =>
            var offers = xoffers
            var time = xtime

            while (offers./(_.n).sum > x)
                offers = offers.dropEnding

            if (offers./(_.n).sum == x) {
                offers.%(_.n > 0).reverse.foreach { o =>
                    o.f.power -= o.n

                    log(o.f, "lost", o.n.power)
                }
                EndAction(f)
            }
            else
            if (time <= 0 || xforum./(_.power).sum < x) {
                if (f.enemies.%(_.power > 0).num > 1)
                    log("Negotiations failed")

                f.power += x

                f.log("got", x.power)

                EndAction(f)
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
                val maxp = min(next.power, x)
                val sweet = max(0, x - offered)
                val maxother = forum.but(next)./(_.power).sum
                val minp = max(1, x - maxother)

                Ask(next).each(-1 +: 0 +: minp.to(maxp).$)(n => ThousandFormsAskAction(f, x, offers, forum, time - (random() * 1.0).round.toInt, next, n))
            }


        case ThousandFormsAskAction(f, x, offers, forum, time, self, n) =>
            if (n < 0) {
                self.log("refused to negotiate")

                Force(ThousandFormsContinueAction(f, x, offers, forum.but(self), time))
            }
            else {
                if (n == 0)
                    self.log("offered no power")
                else
                    self.log("offered to lose", n.styled("highlight"))

                Force(ThousandFormsContinueAction(f, x, Offer(self, n) +: offers, forum, time))
            }

        // ...
        case _ => UnknownContinue
    }
}
