package cws

import colmat._

case object DeepOne extends FactionUnitClass(GC, "Deep One", Monster, 1)
case object Shoggoth extends FactionUnitClass(GC, "Shoggoth", Monster, 2)
case object Starspawn extends FactionUnitClass(GC, "Starspawn", Monster, 3)
case object Cthulhu extends FactionUnitClass(GC, "Cthulhu", GOO, 4)


case object Devour extends FactionSpellbook(GC, "Devour")
case object Immortal extends FactionSpellbook(GC, "Immortal")

case object Absorb extends FactionSpellbook(GC, "Absorb")
case object Regenerate extends FactionSpellbook(GC, "Regenerate")
case object Dreams extends FactionSpellbook(GC, "Dreams")
case object Devolve extends FactionSpellbook(GC, "Devolve")
case object YhaNthlei extends FactionSpellbook(GC, "Y'ha Nthlei")
case object Submerge extends FactionSpellbook(GC, "Submerge")


case object FirstDoomPhase extends Requirement("First Doom Phase", 1)
case object KillDevour1 extends Requirement("Kill/Devour 1 enemy unit")
case object KillDevour2 extends Requirement("Kill/Devour 2 enemy units")
case object AwakenCthulhu extends Requirement("Awaken Cthulhu")
case object OceanGates extends Requirement("Ocean gates")
case object FiveSpellbooks extends Requirement("Five spellbooks", 1)


case object GC extends Faction {
    def name = "Great Cthulhu"
    def short = "GC"
    def style = "gc"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def deep = Region("Ocean Deep", Deep)

    override def abilities : List[Spellbook] = List(Immortal, Devour)
    override def spellbooks : List[Spellbook] = List(Devolve, Absorb, Regenerate, Dreams, YhaNthlei, Submerge)
    override def requirements : List[Requirement] = List(FirstDoomPhase, KillDevour1, KillDevour2, AwakenCthulhu, OceanGates, FiveSpellbooks)

    val allUnits =
        List.fill(1)(Cthulhu) ++
        List.fill(2)(Starspawn) ++
        List.fill(2)(Shoggoth) ++
        List.fill(4)(DeepOne) ++
        List.fill(6)(Acolyte)

    override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
        case Cthulhu => (r == g.starting(this) && g.gates.contains(r)).?(g.of(this).needs(AwakenCthulhu).?(10).|(4)).|(999)
    }

    override def awakenDesc(g : Game, u : UnitClass) : Option[String] = None

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == DeepOne) * 1 +
        units.count(_.uclass == Shoggoth) * 2 +
        units.count(_.uclass == Starspawn) * 3 +
        units.count(_.uclass == Cthulhu) * 6

}