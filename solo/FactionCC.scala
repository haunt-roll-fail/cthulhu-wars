package cws

import colmat._

case object Nightgaunt extends FactionUnitClass(CC, "Nightgaunt", Monster, 1)
case object FlyingPolyp extends FactionUnitClass(CC, "Flying Polyp", Monster, 2)
case object HuntingHorror extends FactionUnitClass(CC, "Hunting Horror", Monster, 3)
case object Nyarlathotep extends FactionUnitClass(CC, "Nyarlathotep", GOO, 10)


case object Harbinger extends FactionSpellbook(CC, "The Harbinger")
case object Flight extends FactionSpellbook(CC, "Flight")

case object Abduct extends FactionSpellbook(CC, "Abduct")
case object SeekAndDestroy extends FactionSpellbook(CC, "Seek and Destroy")
case object Invisibility extends FactionSpellbook(CC, "Invisibility")
case object Madness extends FactionSpellbook(CC, "Madness")
case object Emissary extends FactionSpellbook(CC, "Emissary of the Outer Gods")
case object ThousandForms extends FactionSpellbook(CC, "The Thousand Forms")


case object Pay4Power extends Requirement("Pay 4 Power")
case object Pay6Power extends Requirement("Pay 6 Power")
case object Gates3Power12 extends Requirement("Control 3 Gates / Have 12 Power")
case object Gates4Power15 extends Requirement("Control 4 Gates / Have 15 Power")
case object CaptureCultist extends Requirement("Capture cultist")
case object AwakenNyarlathotep extends Requirement("Awaken Nyarlathotep")


case object CC extends Faction {
    def name = "Crawling Chaos"
    def short = "CC"
    def style = "cc"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities : List[Spellbook] = List(Flight, Harbinger)
    override def spellbooks : List[Spellbook] = List(Abduct, Invisibility, SeekAndDestroy, Madness, Emissary, ThousandForms)
    override def requirements : List[Requirement] = List(Pay4Power, Pay6Power, Gates3Power12, Gates4Power15, CaptureCultist, AwakenNyarlathotep)

    val allUnits =
        List.fill(1)(Nyarlathotep) ++
        List.fill(2)(HuntingHorror) ++
        List.fill(3)(FlyingPolyp) ++
        List.fill(3)(Nightgaunt) ++
        List.fill(6)(Acolyte)

    override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
        case Nyarlathotep => g.of(this).gates.contains(r).?(10).|(999)
    }

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == FlyingPolyp) * 1 +
        units.count(_.uclass == HuntingHorror) * 2 +
        units.count(_.uclass == Nyarlathotep) * (g.of(this).spellbooks.num + g.of(opponent).spellbooks.num)
}