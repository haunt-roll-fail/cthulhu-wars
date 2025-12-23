package cws

import hrf.colmat._

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
case object HighPriestIcon extends UnitClass(HighPriest + " Icon", Token, 0)
case object HighPriest extends UnitClass("High Priest", Cultist, 3) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(u : UnitFigure)(implicit game : Game) = true
}
