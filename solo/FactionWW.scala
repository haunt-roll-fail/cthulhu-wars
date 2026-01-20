package cws

import hrf.colmat._

import html._


case object Wendigo extends FactionUnitClass(WW, "Wendigo", Monster, 1)
case object GnophKeh extends FactionUnitClass(WW, "Gnoph-Keh", Monster, 4)
case object RhanTegoth extends FactionUnitClass(WW, "Rhan Tegoth", GOO, 6)
case object Ithaqua extends FactionUnitClass(WW, "Ithaqua", GOO, 6)

case object Hibernate extends FactionSpellbook(WW, "Hibernate")
case object Eternal extends FactionSpellbook(WW, "Eternal") with BattleSpellbook
case object Ferox extends FactionSpellbook(WW, "Ferox")

case object Cannibalism extends FactionSpellbook(WW, "Cannibalism") with BattleSpellbook
case object Howl extends FactionSpellbook(WW, "Howl") with BattleSpellbook
case object Berserkergang extends FactionSpellbook(WW, "Berserkergang") with BattleSpellbook
case object ArcticWind extends FactionSpellbook(WW, "Arctic Wind")
case object IceAge extends FactionSpellbook(WW, "Ice Age")
case object Herald extends FactionSpellbook(WW, "Herald of the Outer Gods")

case object FirstPlayer extends Requirement("Starting Player")
case object OppositeGate extends Requirement("Opposite Gate")
case object AnotherFactionAllSpellbooks extends Requirement("Another Faction All Spellbooks")
case object AnytimeGainElderSigns extends Requirement("Anytime Spellbook")
case object AwakenRhanTegoth extends Requirement("Awaken Rhan Tegoth")
case object AwakenIthaqua extends Requirement("Awaken Ithaqua")


case object WW extends Faction { f =>
    def name = "Windwalker"
    def short = "WW"
    def style = "ww"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $(Hibernate, Eternal, Ferox)
    override def library = $(Cannibalism, Howl, Berserkergang, ArcticWind, IceAge, Herald)
    override def requirements(options : $[GameOption]) = $(FirstPlayer, OppositeGate, AnotherFactionAllSpellbooks, AnytimeGainElderSigns, AwakenRhanTegoth, AwakenIthaqua)

    val allUnits =
        1.times(Ithaqua) ++
        1.times(RhanTegoth) ++
        4.times(GnophKeh) ++
        4.times(Wendigo) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case RhanTegoth => game.board.starting(f).has(r).?(6)
        case Ithaqua => (game.board.starting(f).has(r) && game.allGates.has(r) && f.needs(AwakenRhanTegoth).not).?(6)
    }

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case GnophKeh => f.pool(GnophKeh).num
        case _ => u.cost
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Wendigo).num * 1 +
        units(GnophKeh).num * 3 +
        units(RhanTegoth).not(Zeroed).num * 3 +
        units(Ithaqua).not(Zeroed).num * ((opponent.doom + 1) / 2) +
        neutralStrength(units, opponent)
}


case class HibernateMainAction(self : WW, n : Int) extends OptionFactionAction(Hibernate.full + " for extra " + n.power) with MainQuestion

case class IceAgeMainAction(self : WW, l : $[Region]) extends OptionFactionAction(IceAge) with MainQuestion with Soft
case class IceAgeAction(self : WW, r : Region) extends BaseFactionAction(self.styled(IceAge) + " region", r)

case class ArcticWindAction(self : WW, o : Region, u : UnitRef, r : Region) extends ForcedAction

case class AnytimeGainElderSignsMainAction(self : WW) extends OptionFactionAction(self.styled("Anytime Spellbook")) with MainQuestion with Soft with PowerNeutral
case class AnytimeGainElderSignsDoomAction(self : WW) extends OptionFactionAction(self.styled("Anytime Spellbook")) with DoomQuestion with Soft with PowerNeutral
case class AnytimeGainElderSignsAction(self : WW, n : Int, next : ForcedAction) extends BaseFactionAction(self.styled("Anytime Spellbook"), "Get spellbook and " + n.es)


object WWExpansion extends Expansion {
    override def triggers()(implicit game : Game) {
        val f = WW
        val o = game.board.starting(f).but(game.starting(f)).only
        f.satisfyIf(OppositeGate, "Gate exists in " + o.name, game.allGates.has(o))
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // ACTIONS
        case MainAction(f : WW) if f.active.not =>
            implicit val asking = Asking(f)

            game.reveals(f)

            if (f.needs(AnytimeGainElderSigns))
                if (f.doom + f.es./(_.value).sum + f.enemies.%(_.hasAllSB).num.upTo(3) * 3 >= 30)
                    if (f.unfulfilled.num == 1 || f.enemies.%(_.hasAllSB).none)
                        + AnytimeGainElderSignsMainAction(f)

            + NextPlayerAction(f).as("Skip")

            asking

        case MainAction(f : WW) if f.acted =>
            implicit val asking = Asking(f)

            game.controls(f)

            game.reveals(f)

            if (f.needs(AnytimeGainElderSigns))
                if (f.doom + f.es./(_.value).sum + f.enemies.%(_.hasAllSB).num.upTo(3) * 3 >= 30)
                    if (f.unfulfilled.num == 1 || f.enemies.%(_.hasAllSB).none)
                        + AnytimeGainElderSignsMainAction(f)

            + NextPlayerAction(f).as("Skip")

            asking

        case MainAction(f : WW) =>
            implicit val asking = Asking(f)

            if (f.has(Hibernate))
                + HibernateMainAction(f, min(f.power, f.enemies./~(_.goos.distinctBy(_.uclass)).num))

            game.moves(f)

            game.captures(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            game.summons(f)

            game.awakens(f)

            game.independents(f)

            if (f.needs(AnytimeGainElderSigns))
                + AnytimeGainElderSignsMainAction(f)

            if (f.has(IceAge))
                areas.%(r => f.iceAge.?(_ == r).not).%(f.affords(1)).some.foreach { l =>
                    + IceAgeMainAction(f, l)
                }

            game.neutralSpellbooks(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // AWAKEN
        case AwakenedAction(self, RhanTegoth, r, cost) =>
            self.satisfy(AwakenRhanTegoth, "Awaken Rhan Tegoth")

            EndAction(self)

        case AwakenedAction(self, Ithaqua, r, cost) =>
            if (game.gates.has(r)) {
                game.gates :-= r

                factions.foreach { e =>
                    e.gates :-= r
                    e.at(r).foreach(_.onGate = false)
                }

                self.log("destroyed gate in", r)
            }
            else {
                val u = factions./~(_.unitGate).%(_.region == r).only

                game.eliminate(u)

                self.log("eliminated", u, "in", r)
            }

            self.satisfy(AwakenIthaqua, "Awaken Ithaqua")

            EndAction(self)

        // HIBERNATE
        case HibernateMainAction(self, n) =>
            self.power += n
            self.hibernating = true
            self.active = false
            self.log("hibernated", (n != 0).??("for extra " + n.power))
            EndAction(self)

        // ICE AGE
        case IceAgeMainAction(self, l) =>
            Ask(self).each(l)(r => IceAgeAction(self, r)).cancel

        case IceAgeAction(self, r) =>
            self.power -= 1
            self.iceAge = |(r)
            game.anyIceAge = true
            self.log("started", IceAge, "in", r)
            EndAction(self)

        // ARCTIC WIND
        case MovedAction(self : WW, u, o, r) if u.uclass == Ithaqua && self.has(ArcticWind) =>
            Ask(self)
                .each(self.at(o).not(Moved).%(_.canMove).sort)(u => ArcticWindAction(self, o, u, r).as(u.full, "from", o)(ArcticWind, "to", r))
                .done(MoveContinueAction(self, true))

        case ArcticWindAction(self, o, u, r) =>
            u.region = r
            u.add(Moved)
            log(u, "followed with", ArcticWind)
            MovedAction(self, self.goo(Ithaqua), o, r)

        // ANYTIME
        case AnytimeGainElderSignsMainAction(self) =>
            Ask(self).add(AnytimeGainElderSignsAction(self, min(3, self.enemies.%(_.hasAllSB).num), PreMainAction(self))).cancel

        case AnytimeGainElderSignsDoomAction(self) =>
            Ask(self).add(AnytimeGainElderSignsAction(self, min(3, self.enemies.%(_.hasAllSB).num), DoomAction(self))).cancel

        case AnytimeGainElderSignsAction(self, n, next) =>
            self.satisfy(AnytimeGainElderSigns, "Anytime Spellbook", n)
            CheckSpellbooksAction(next)

        // ...
        case _ => UnknownContinue
    }
}
