package cws

import hrf.colmat._

import html._


// Neutral Monsters
case object GhastCard extends NeutralMonsterLoyaltyCard(GhastIcon, Ghast, cost = 2, quantity = 4, combat = 0)
case object GugCard extends NeutralMonsterLoyaltyCard(GugIcon, Gug, cost = 1, quantity = 2, combat = 3)
case object ShantakCard extends NeutralMonsterLoyaltyCard(ShantakIcon, Shantak, cost = 2, quantity = 2, combat = 2)
case object StarVampireCard extends NeutralMonsterLoyaltyCard(StarVampireIcon, StarVampire, cost = 2, quantity = 3, combat = 1)

case object GhastIcon extends UnitClass(Ghast.name + " Icon", Token, 0)
case object GugIcon extends UnitClass(Gug.name + " Icon", Token, 0)
case object ShantakIcon extends UnitClass(Shantak.name + " Icon", Token, 0)
case object StarVampireIcon extends UnitClass(StarVampire.name + " Icon", Token, 0)

trait NeutralMonster

case object Ghast extends UnitClass("Ghast", Monster, 2) with NeutralMonster { override val priority = 1001 }
case object Gug extends UnitClass("Gug", Monster, 1) with NeutralMonster {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
}
case object Shantak extends UnitClass("Shantak", Monster, 2) with NeutralMonster
case object StarVampire extends UnitClass("Star Vampire", Monster, 2) with NeutralMonster


case class LoyaltyCardDoomAction(self : Faction) extends OptionFactionAction("Obtain " + "Loyalty Card".styled("nt")) with DoomQuestion with Soft with PowerNeutral
case class NeutralMonstersAction(self : Faction, lc : NeutralMonsterLoyaltyCard) extends BaseFactionAction(g => "Obtain " + "Loyalty Card".styled("nt"), {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        lc.short +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with PowerNeutral
case class LoyaltyCardSummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(g => "" + self + " places " + self.styled(uc) + " in", implicit g => r + self.iced(r))

case class FreeSummonAction(self : Faction, uc : UnitClass, r : Region, l : $[Region]) extends BaseFactionAction(g => "" + self + " summons " + self.styled(uc) + " for free in", implicit g => r + self.iced(r))

case class ShantakCarryCultistAction(self : Faction, o : Region, ur : UnitRef, r : Region) extends ForcedAction


object NeutralMonstersExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        case LoyaltyCardDoomAction(self) =>
            val cards = game.loyaltyCards.of[NeutralMonsterLoyaltyCard].%(_.doom <= self.doom).%(_.power <= self.power).distinct

            Ask(self)
                .each(cards)(c => NeutralMonstersAction(self, c))
                .cancel

        case NeutralMonstersAction(self, lc) =>
            self.loyaltyCards :+= lc
            game.loyaltyCards :-= lc

            self.hired = true

            self.doom -= lc.doom
            self.power -= lc.power

            self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", $((lc.doom > 0).??(lc.doom.doom), (lc.power > 0).??(lc.power.power)).but("").mkString("and"))

            lc.quantity.times(lc.unit).foreach { u =>
                self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
            }

            if (self.allGates.onMap.any)
                Ask(self).each(self.allGates.onMap)(r => LoyaltyCardSummonAction(self, lc.unit, r))
            else {
                self.log("had nowhere to place", self.styled(lc.unit))

                CheckSpellbooksAction(DoomAction(self))
            }

        case LoyaltyCardSummonAction(self, uc, r) =>
            self.place(uc, r)
            self.log("placed", self.styled(uc), "in", r)

            if (uc == Ghast && self.pool(Ghast).any)
                Ask(self).each(self.allGates.onMap)(r => LoyaltyCardSummonAction(self, uc, r))
            else
                CheckSpellbooksAction(DoomAction(self))

        // GHAST
        case SummonedAction(self, uc, r, l) if uc == Ghast && self.pool(Ghast).any =>
            Ask(self).each(self.summonRegions)(r => FreeSummonAction(self, uc, r, l))

        case FreeSummonAction(self, uc, r, l) =>
            if (l.has(r).not)
                self.payTax(r)

            self.place(uc, r)
            self.log("summoned", self.styled(uc), "in", r, "for free")

            SummonedAction(self, uc, r, l :+ r)

        // SHANTAK
        case MovedAction(self, u, o, r) if u.uclass == Shantak =>
            Ask(self)
                .each(self.at(o).not(Moved).cultists.sortA)(u => ShantakCarryCultistAction(self, o, u, r).as(u.full, "from", o)(Shantak, "carries", "Cultist".styled(self), "to", r))
                .skip(MoveContinueAction(self, true))

        case ShantakCarryCultistAction(self, o, u, r) =>
            u.region = r

            u.add(Moved)
            u.add(MovedForFree)

            log(Shantak, "carried", u, "to", r)

            MoveContinueAction(self, true)

        // ...
        case _ => UnknownContinue
    }
}
