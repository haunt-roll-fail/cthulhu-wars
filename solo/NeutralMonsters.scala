package cws

import hrf.colmat._

import Html._


// Neutral Monsters

// name, doomCost (to obtain), powerCost (to obtain), quantity, cost (to summon), combat
case object GhastCard extends LoyaltyCard(Ghast.name, 2, 0, 4, 2, 0, Ghast, GhastIcon)
case object GugCard extends LoyaltyCard(Gug.name, 2, 0, 2, 1, 3, Gug, GugIcon)
case object ShantakCard extends LoyaltyCard(Shantak.name, 2, 0, 2, 2, 2, Shantak, ShantakIcon)
case object StarVampireCard extends LoyaltyCard(StarVampire.name, 2, 0, 3, 2, 1, StarVampire, StarVampireIcon)

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

// High Priests

case object HighPriestCard extends LoyaltyCard(HighPriest.name, 0, 0, 1, 3, 0, HighPriest, HighPriestIcon)
case object HighPriestIcon extends UnitClass(HighPriest.name + " Icon", Token, 0)
case object HighPriest extends UnitClass("High Priest", Cultist, 3) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(u : UnitFigure)(implicit game : Game) = true
}


case class LoyaltyCardDoomAction(self : Faction) extends OptionFactionAction("Obtain " + "Loyalty Card".styled("nt")) with DoomQuestion with Soft with PowerNeutral
case class NeutralMonstersAction(self : Faction, lc : LoyaltyCard) extends BaseFactionAction(g => "Obtain " + "Loyalty Card".styled("nt"), {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        lc.short +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with PowerNeutral
case class LoyaltyCardSummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(g => "Place " + self.styled(uc) + " in", implicit g => r + self.iced(r))

case class ShantakCarryCultistAction(self : Faction, o : Region, uc : UnitClass, r : Region) extends BaseFactionAction("Carry Cultist to " + r, self.styled(uc) + " from " + o)


object NeutralMonstersExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        case LoyaltyCardDoomAction(self) =>
            val availableCards = game.loyaltyCards
                .filter { c =>
                    val doomOK  = self.doom  >= c.doom
                    val powerOK = self.power >= c.power
                    doomOK && powerOK && c.doom > 0
                }
                .distinct

            Ask(self)
                .each(availableCards)(c => NeutralMonstersAction(self, c))
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

        // SHANTAK
        case ShantakCarryCultistAction(self, o, uc, r) =>
            val u = self.at(o, uc).%!(_.has(Moved)).first
            u.region = r

            u.add(Moved)
            u.add(MovedForFree)

            log(self.styled(Shantak), "carried", u, "to", r)

            MoveContinueAction(self, true)


        case _ => UnknownContinue
    }
}
