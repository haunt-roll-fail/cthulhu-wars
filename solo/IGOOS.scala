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

trait NeutralFaction extends Faction

case object NeutralAbhoth extends NeutralFaction {
    def name = "Neutral Abhoth"
    def short = "NA"
    def style = ""
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $
    override def spellbooks = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = 12.times(Filth)
        // 1.times(Hastur) ++
        // 1.times(KingInYellow) ++
        // 4.times(Byakhee) ++
        // 6.times(Undead) ++
        // 6.times(Acolyte)

    // override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
    //     case KingInYellow => (!g.gates.contains(r) && !g.ugates.contains(r) && g.of(this).at(r).any).?(4).|(999)
    //     case Hastur => (g.of(this).gates.contains(r) && g.of(this).at(r, KingInYellow).any).?(10).|(999)
    // }

    def strength(g : Game, units : $[UnitFigure], opponent : Faction) : Int = 0

    var ignoredSacrificeHighPriest : Boolean = false
}
