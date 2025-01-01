package cws

import colmat._

case object Wizard extends FactionUnitClass(SL, "Wizard", Monster, 1)
case object SerpentMan extends FactionUnitClass(SL, "Serpent Man", Monster, 2)
case object FormlessSpawn extends FactionUnitClass(SL, "Formless Spawn", Monster, 3)
case object Tsathoggua extends FactionUnitClass(SL, "Tsathoggua", GOO, 8)

case object DeathFromBelow extends FactionSpellbook(SL, "Death from Below")
case object Lethargy extends FactionSpellbook(SL, "Lethargy")

case object AncientSorcery extends FactionSpellbook(SL, "Ancient Sorcery")
case object Burrow extends FactionSpellbook(SL, "Burrow")
case object CaptureMonster extends FactionSpellbook(SL, "Capture Monster")
case object CursedSlumber extends FactionSpellbook(SL, "Cursed Slumber")
case object DemandSacrifice extends FactionSpellbook(SL, "Demand Sacrifice")
case object KillsArePains extends FactionSpellbook(SL, "Kills are Pains")
case object EnergyNexus extends FactionSpellbook(SL, "Energy Nexus")

case object Pay3SomeoneGains3 extends Requirement("Pay 3, Someone gains 3 Power")
case object Pay3EverybodyLoses1 extends Requirement("Pay 3, Everybody loses 1 Power")
case object Pay3EverybodyGains1 extends Requirement("Pay 3, Everybody gains 1 Power")
case object PerformRitual extends Requirement("Perform ritual")
case object Roll6DiceInBattle extends Requirement("Roll 6 dice in Battle")
case object AwakenTsathoggua extends Requirement("Awaken Tsathoggua")


case object SL extends Faction {
    def name = "Sleeper"
    def short = "SL"
    def style = "sl"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def slumber = Region("Cursed Slumber", Slumber)

    override def abilities : List[Spellbook] = List(DeathFromBelow, Lethargy)
    override def spellbooks : List[Spellbook] = List(AncientSorcery, Burrow, CaptureMonster, CursedSlumber, DemandSacrifice, EnergyNexus)
    override def requirements : List[Requirement] = List(Pay3SomeoneGains3, Pay3EverybodyLoses1, Pay3EverybodyGains1, PerformRitual, Roll6DiceInBattle, AwakenTsathoggua)

    val allUnits =
        List.fill(1)(Tsathoggua) ++
        List.fill(4)(FormlessSpawn) ++
        List.fill(3)(SerpentMan) ++
        List.fill(2)(Wizard) ++
        List.fill(6)(Acolyte)

    override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
        case Tsathoggua => (g.of(this).at(r, FormlessSpawn).any).?((g.of(this).has(Immortal) && !g.of(this).needs(AwakenTsathoggua)).?(4).|(8)).|(999)
    }

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == SerpentMan) * 1 +
        units.count(_.uclass == FormlessSpawn) * (g.of(this).all(FormlessSpawn).num + g.of(this).all(Tsathoggua).num) +
        units.count(_.uclass == Tsathoggua) * (max(2, g.of(opponent).power))

}
