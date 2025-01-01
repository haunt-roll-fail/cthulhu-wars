package cws

import colmat._

case object UnMan extends FactionUnitClass(AN, "Un-Man", Monster, 3)
case object Reanimated extends FactionUnitClass(AN, "Reanimated", Monster, 4)
case object Yothan extends FactionUnitClass(AN, "Yothan", Terror, 6)

case object Dematerialization extends FactionSpellbook(AN, "Dematerialization")

case object Brainless extends FactionSpellbook(AN, "Brainless")
case object Consecration extends FactionSpellbook(AN, "Consecration")
case object Extinction extends FactionSpellbook(AN, "Extinction")
case object Festival extends FactionSpellbook(AN, "Festival")
case object UnholyGround extends FactionSpellbook(AN, "Unholy Ground")
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

    override def abilities : List[Spellbook] = List(Dematerialization)
    override def spellbooks : List[Spellbook] = List(Brainless, Consecration, Extinction, Festival, UnholyGround, WorshipServices)
    override def requirements : List[Requirement] = List(CathedralWW, CathedralOO, CathedralAA, CathedralNG, GiveWorstMonster, GiveBestMonster)

    val allUnits =
        List.fill(3)(Yothan) ++
        List.fill(3)(Reanimated) ++
        List.fill(3)(UnMan) ++
        List.fill(6)(Acolyte)

    override def summonCost(g : Game, u : UnitClass, r : Region) = u match {
        case UnMan => g.of(this).has(Festival).?(0).|(3)
        case Reanimated => g.of(this).has(Brainless).?(1).|(4)
        case Yothan => g.of(this).has(Extinction).?(3).|(6)
        case _ => u.cost
    }

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == Reanimated) * 2 +
        units.count(_.uclass == Yothan) * 7

}
