package cws

import hrf.colmat._

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


case object GC extends Faction { f =>
    def name = "Great Cthulhu"
    def short = "GC"
    def style = "gc"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def deep = Region("Ocean Deep", Deep)

    override def abilities : $[Spellbook] = $(Immortal, Devour)
    override def library : $[Spellbook] = $(Devolve, Absorb, Regenerate, Dreams, YhaNthlei, Submerge)
    override def requirements(options : $[GameOption]) = $(FirstDoomPhase, KillDevour1, KillDevour2, AwakenCthulhu, OceanGates, FiveSpellbooks)

    val allUnits =
        1.times(Cthulhu) ++
        2.times(Starspawn) ++
        2.times(Shoggoth) ++
        4.times(DeepOne) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case Cthulhu => (r == game.starting(f) && game.gates.has(r)).?(f.needs(AwakenCthulhu).?(10).|(4)).|(999)
    }

    override def awakenDesc(u : UnitClass) : |[String] = None

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(DeepOne).num * 1 +
        units(Shoggoth).num * 2 + units./(_.count(Absorbed)).sum * 3 +
        units(Starspawn).num * 3 +
        units(Cthulhu).%!(_.has(Zeroed)).num * 6 +
        neutralStrength(units, opponent)
}
