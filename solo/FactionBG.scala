package cws

import colmat._

case object Ghoul extends FactionUnitClass(BG, "Ghoul", Monster, 1)
case object Fungi extends FactionUnitClass(BG, "Fungi from Yuggoth", Monster, 2)
case object DarkYoung extends FactionUnitClass(BG, "Dark Young", Monster, 3)
case object ShubNiggurath extends FactionUnitClass(BG, "Shub-Niggurath", GOO, 8)

case object Fertility extends FactionSpellbook(BG, "Fertility Cult")
case object Avatar extends FactionSpellbook(BG, "Avatar")

case object BloodSacrifice extends FactionSpellbook(BG, "Blood Sacrifice")
case object ThousandYoung extends FactionSpellbook(BG, "The Thousand Young")
case object Ghroth extends FactionSpellbook(BG, "Ghroth")
case object RedSign extends FactionSpellbook(BG, "The Red Sign")
case object Necrophagy extends FactionSpellbook(BG, "Necrophagy")
case object Frenzy extends FactionSpellbook(BG, "Frenzy")

case object Spread4 extends Requirement("Units in 4 Areas")
case object Spread6 extends Requirement("Units in 6 Areas")
case object Spread8 extends Requirement("Units in 8 Areas")
case object SpreadSocial extends Requirement("Share Areas will all enemies")
case object Eliminate2Cultists extends Requirement("Elminiate two cultists")
case object AwakenShubNiggurath extends Requirement("Awaken Shub-Niggurath")


case object BG extends Faction {
    def name = "Black Goat"
    def short = "BG"
    def style = "bg"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities : List[Spellbook] = List(Fertility, Avatar)
    override def spellbooks : List[Spellbook] = List(Frenzy, Ghroth, Necrophagy, RedSign, BloodSacrifice, ThousandYoung)
    override def requirements : List[Requirement] = List(Spread4, Spread6, Spread8, SpreadSocial, Eliminate2Cultists, AwakenShubNiggurath)

    val allUnits =
        List.fill(1)(ShubNiggurath) ++
        List.fill(3)(DarkYoung) ++
        List.fill(4)(Fungi) ++
        List.fill(2)(Ghoul) ++
        List.fill(6)(Acolyte)

    override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
        case ShubNiggurath => (g.of(this).gates.contains(r)).?((g.of(this).all(Cultist).num >= 2).?(8).|(998)).|(999)
    }

    override def summonCost(g : Game, u : UnitClass, r : Region) = u.cost - (g.of(this).has(ThousandYoung) && g.of(this).has(ShubNiggurath)).?(1).|(0)

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == Acolyte) * g.of(this).has(Frenzy).?(1).|(0) +
        units.count(_.uclass == Fungi) * 1 +
        units.count(_.uclass == DarkYoung) * 2 +
        units.count(_.uclass == ShubNiggurath) * (
            g.of(this).gates.num +
            g.of(this).all(Acolyte).num +
            g.of(this).all(DarkYoung).num * g.of(this).has(RedSign).?(1).|(0)
        )

}