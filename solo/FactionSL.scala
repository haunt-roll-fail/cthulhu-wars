package cws

import hrf.colmat._

case object Wizard extends FactionUnitClass(SL, "Wizard", Monster, 1)
case object SerpentMan extends FactionUnitClass(SL, "Serpent Man", Monster, 2)
case object FormlessSpawn extends FactionUnitClass(SL, "Formless Spawn", Monster, 3)
case object Tsathoggua extends FactionUnitClass(SL, "Tsathoggua", GOO, 8)

case object DeathFromBelow extends FactionSpellbook(SL, "Death from Below")
case object Lethargy extends FactionSpellbook(SL, "Lethargy")

case object Burrow extends FactionSpellbook(SL, "Burrow")
case object EnergyNexus extends FactionSpellbook(SL, "Energy Nexus")
case object AncientSorcery extends FactionSpellbook(SL, "Ancient Sorcery")
case object CaptureMonster extends FactionSpellbook(SL, "Capture Monster")
case object DemandSacrifice extends FactionSpellbook(SL, "Demand Sacrifice")
case object CursedSlumber extends FactionSpellbook(SL, "Cursed Slumber")

case object KillsArePains extends FactionSpellbook(SL, "Kills are Pains")

case object Pay3SomeoneGains3 extends Requirement("Pay 3, Someone gains 3 Power")
case object Pay3EverybodyGains1 extends Requirement("Pay 3, Everybody gains 1 Power")
case object Pay3EverybodyLoses1 extends Requirement("Pay 3, Everybody loses 1 Power")
case object Roll6DiceInBattle extends Requirement("Roll 6 dice in Battle")
case object PerformRitual extends Requirement("Perform ritual")
case object AwakenTsathoggua extends Requirement("Awaken Tsathoggua")


case object SL extends Faction { f =>
    def name = "Sleeper"
    def short = "SL"
    def style = "sl"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    def slumber = Region("Slumber", Slumber)

    override def abilities = $(DeathFromBelow, Lethargy)
    override def library = $(Burrow, EnergyNexus, AncientSorcery, CaptureMonster, DemandSacrifice, CursedSlumber)
    override def requirements(options : $[GameOption]) = $(Pay3SomeoneGains3, Pay3EverybodyGains1, Pay3EverybodyLoses1, Roll6DiceInBattle, PerformRitual, AwakenTsathoggua)

    val allUnits =
        1.times(Tsathoggua) ++
        4.times(FormlessSpawn) ++
        3.times(SerpentMan) ++
        2.times(Wizard) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case Tsathoggua => (f.at(r, FormlessSpawn).any).?((f.has(Immortal) && f.needs(AwakenTsathoggua).not).?(4).|(8)).|(999)
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(SerpentMan).num * 1 +
        units(FormlessSpawn).num * (f.all(FormlessSpawn).num + f.all(Tsathoggua).num) +
        units(Tsathoggua).%!(_.has(Zeroed)).num * max(2, opponent.power) +
        neutralStrength(units, opponent)
}
