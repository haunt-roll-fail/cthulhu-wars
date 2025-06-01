package cws

import hrf.colmat._

case object UnMan extends FactionUnitClass(AN, "Un-Man", Monster, 3)
case object Reanimated extends FactionUnitClass(AN, "Reanimated", Monster, 4)
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


case object AN extends Faction {
    def name = "The Ancients"
    def short = "AN"
    def style = "an"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def extinct = Region("Extinct", Extinct)

    override def abilities = $(Dematerialization)
    override def spellbooks = $(Festival, Brainless, Extinction, UnholyGround, Consecration, WorshipServices)
    override def requirements(options : $[GameOption]) = $(CathedralAA, CathedralOO, CathedralWW, CathedralNG, GiveWorstMonster, GiveBestMonster)

    val allUnits =
        3.times(Yothan) ++
        3.times(Reanimated) ++
        3.times(UnMan) ++
        6.times(Acolyte)

    override def summonCost(g : Game, u : UnitClass, r : Region) = u match {
        case UnMan => g.of(this).has(Festival).?(0).|(3)
        case Reanimated => g.of(this).has(Brainless).?(1).|(4)
        case Yothan => g.of(this).has(Extinction).?(3).|(6)
        case _ => u.cost
    }

    private var strengthFn: (Game, List[UnitFigure], Faction) => Int = defaultStrength

    private def defaultStrength(g: Game, units: List[UnitFigure], opponent: Faction): Int =
        units.count(_.uclass == Reanimated) * 2 +
        units.count(_.uclass == Yothan) * 7

    override def strength(g: Game, units: List[UnitFigure], opponent: Faction): Int =
        strengthFn(g, units, opponent)

    def addToStrength(fn: (Game, List[UnitFigure], Faction) => Int): Unit = {
        val current = strengthFn
        strengthFn = (g, u, o) => current(g, u, o) + fn(g, u, o)
    }

}
