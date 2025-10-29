package cws

import hrf.colmat._

// IGOOs

// name, doomCost (to obtain), powerCost (to obtain), quantity, cost (to summon), combat
case object ByatisCard extends LoyaltyCard("Byatis", 0, 4, 1, 4, 4)
case object AbhothCard extends LoyaltyCard("Abhoth", 0, 4, 1, 4, 0)
case object DaolothCard extends LoyaltyCard("Daoloth", 0, 6, 1, 6, 0)
case object NyogthaCard extends LoyaltyCard("Nyogtha", 0, 6, 2, 6, 0)

case object ByatisIcon extends UnitClass("Byatis Icon", Token, 0)
case object AbhothIcon extends UnitClass("Abhoth Icon", Token, 0)
case object DaolothIcon extends UnitClass("Daoloth Icon", Token, 0)
case object NyogthaIcon extends UnitClass("Nyogtha Icon", Token, 0)

case object Byatis extends UnitClass("Byatis", GOO, 4) with IGOO
case object Abhoth extends UnitClass("Abhoth", GOO, 4) with IGOO
case object Daoloth extends UnitClass("Daoloth", GOO, 6) with IGOO
case object Nyogtha extends UnitClass("Nyogtha", GOO, 6) with IGOO

case object Filth extends UnitClass("Filth", Monster, 1)

// IGOO abilities (as spellbooks, like Devour)

case object CosmicUnity extends IGOOSpellbook("Cosmic Unity")                // Daoloth
case object FromBelow extends IGOOSpellbook("From Below")                    // Nyogtha

// IGOO spellbooks

case object GodOfForgetfulness extends IGOOSpellbook("God of Forgetfulness") // Byatis
case object TheBrood extends IGOOSpellbook("The Brood")                      // Abhoth
case object Interdimensional extends IGOOSpellbook("Interdimensional")       // Daoloth
case object NightmareWeb extends IGOOSpellbook("Nightmare Web")              // Nyogtha