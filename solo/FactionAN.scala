package cws

import hrf.colmat._

case object UnMan extends FactionUnitClass(AN, "Un-Man", Monster, 3)
case object Reanimated extends FactionUnitClass(AN, "Reanimated", Monster, 4) {
    def alone(u : UnitFigure)(implicit game : Game) = u.faction.has(Brainless) && u.faction.at(u.region).not(Reanimated).none
    override def canMove(u : UnitFigure)(implicit game : Game) = alone(u).not
    override def canBattle(u : UnitFigure)(implicit game : Game) = alone(u).not
    override def canCapture(u : UnitFigure)(implicit game : Game) = alone(u).not
}
case object Yothan extends FactionUnitClass(AN, "Yothan", Terror, 6)
case object Cathedral extends FactionUnitClass(AN, "Cathedral", Building, 4)

case object Dematerialization extends FactionSpellbook(AN, "Dematerialization")

case object Festival extends FactionSpellbook(AN, "Festival")
case object Brainless extends FactionSpellbook(AN, "Brainless")
case object Extinction extends FactionSpellbook(AN, "Extinction")
case object UnholyGround extends FactionSpellbook(AN, "Unholy Ground")
case object Consecration extends FactionSpellbook(AN, "Consecration")
case object WorshipServices extends FactionSpellbook(AN, "Worship Services")

case object CathedralWW extends Requirement("Cathedral in Area with ||| Glyph")
case object CathedralOO extends Requirement("Cathedral in Area with (*) Glyph")
case object CathedralAA extends Requirement("Cathedral in Area with /^\\ Glyph")
case object CathedralNG extends Requirement("Cathedral in Area without Glyph")
case object GiveWorstMonster extends Requirement("Give enemies lowest cost monster")
case object GiveBestMonster extends Requirement("Give enemies highest cost monster")


case object AN extends Faction { f =>
    def name = "The Ancients"
    def short = "AN"
    def style = "an"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def extinct = Region("Extinct", Extinct)

    override def abilities = $(Dematerialization)
    override def library = $(Festival, Brainless, Extinction, UnholyGround, Consecration, WorshipServices)
    override def requirements(options : $[GameOption]) = $(CathedralAA, CathedralOO, CathedralWW, CathedralNG, GiveWorstMonster, GiveBestMonster)

    val allUnits =
        3.times(Yothan) ++
        3.times(Reanimated) ++
        3.times(UnMan) ++
        6.times(Acolyte)

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case UnMan => f.has(Festival).?(0).|(3)
        case Reanimated => f.has(Brainless).?(1).|(4)
        case Yothan => f.has(Extinction).?(3).|(6)
        case _ => u.cost
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Reanimated).num * 2 +
        units(Yothan).num * 7 +
        neutralStrength(units, opponent)
}
