package cws

import hrf.colmat._

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

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case ShubNiggurath => (f.gates.has(r)).?((f.all(Cultist).num >= 2).?(8).|(998)).|(999)
    }

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case Ghoul | Fungi | DarkYoung if f.has(ThousandYoung) && f.has(ShubNiggurath) => u.cost - 1
        case _ => u.cost
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Acolyte).num * f.has(Frenzy).??(1) +
        units(HighPriest).num * f.has(Frenzy).??(1) +
        units(Fungi).num * 1 +
        units(DarkYoung).num * 2 +
        units(ShubNiggurath).%!(_.has(Zeroed)).num * (
            f.gates.num +
            f.all(Cultist).num +
            f.all(DarkYoung).num * f.has(RedSign).??(1)
        ) +
        neutralStrength(units, opponent)
}
