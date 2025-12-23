package cws

import hrf.colmat._

// IGOOs

// name, doomCost (to obtain), powerCost (to obtain), quantity, cost (to summon), combat
case object ByatisCard extends LoyaltyCard(Byatis.name, 0, 4, 1, 4, 4, Byatis, ByatisIcon)
case object AbhothCard extends LoyaltyCard(Abhoth.name, 0, 4, 1, 4, 0, Abhoth, AbhothIcon)
case object DaolothCard extends LoyaltyCard(Daoloth.name, 0, 6, 1, 6, 0, Daoloth, DaolothIcon)
case object NyogthaCard extends LoyaltyCard(Nyogtha.name, 0, 6, 2, 6, 0, Nyogtha, NyogthaIcon)

case object ByatisIcon extends UnitClass(Byatis.name + " Icon", Token, 0)
case object AbhothIcon extends UnitClass(Abhoth.name + " Icon", Token, 0)
case object DaolothIcon extends UnitClass(Daoloth.name + " Icon", Token, 0)
case object NyogthaIcon extends UnitClass(Nyogtha.name + " Icon", Token, 0)

case object Byatis extends UnitClass("Byatis", GOO, 4) with IGOO {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) = false
}

case object Abhoth extends UnitClass("Abhoth", GOO, 4) with IGOO
case object Daoloth extends UnitClass("Daoloth", GOO, 6) with IGOO
case object Nyogtha extends UnitClass("Nyogtha", GOO, 6) with IGOO

case object Filth extends UnitClass("Filth", Monster, 1) {
    override def canBeSummoned(implicit game : Game) = false
}

// Byatis
case object GodOfForgetfulness extends NeutralSpellbook("God of Forgetfulness")

// Abhoth
case object LostAbhoth extends NeutralSpellbook("Lost Abhoth")
case object TheBrood extends NeutralSpellbook("The Brood")

// Daoloth
case object CosmicUnity extends NeutralSpellbook("Cosmic Unity")
case object Interdimensional extends NeutralSpellbook("Interdimensional")

// Nyogtha
case object FromBelow extends NeutralSpellbook("From Below")
case object NightmareWeb extends NeutralSpellbook("Nightmare Web")



trait NeutralFaction extends Faction

case object NeutralAbhoth extends NeutralFaction {
    def name = "Neutral Abhoth"
    def short = "NA"
    def style = "nt"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = 12.times(Filth)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = 0
}
