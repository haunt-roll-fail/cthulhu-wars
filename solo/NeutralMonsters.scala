package cws

import hrf.colmat._

// name, doomCost (to obtain), powerCost (to obtain), quantity, cost (to summon), combat
case object GhastCard extends LoyaltyCard("Ghast", 2, 0, 4, 2, 0)
case object GugCard extends LoyaltyCard("Gug", 2, 0, 2, 1, 3)
case object ShantakCard extends LoyaltyCard("Shantak", 2, 0, 2, 2, 2)
case object StarVampireCard extends LoyaltyCard("Star Vampire", 2, 0, 3, 2, 1)

case object GhastIcon extends UnitClass("Ghast Icon", Token, 0)
case object GugIcon extends UnitClass("Ghast Icon", Token, 0)
case object ShantakIcon extends UnitClass("Shantak Icon", Token, 0)
case object StarVampireIcon extends UnitClass("Star Vampire Icon", Token, 0)

case object Ghast extends UnitClass("Ghast", Monster, 2)
case object Gug extends UnitClass("Gug", Monster, 1)
case object Shantak extends UnitClass("Shantak", Monster, 2)
case object StarVampire extends UnitClass("Star Vampire", Monster, 2)

case object HighPriestCard extends LoyaltyCard("High Priest", 0, 0, 1, 3, 0)
case object HighPriestIcon extends UnitClass("High Priest Icon", Token, 0)
case object HighPriest extends UnitClass("High Priest", Cultist, 3)