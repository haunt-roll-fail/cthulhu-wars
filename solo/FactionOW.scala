package cws

import colmat._

case object Mutant extends FactionUnitClass(OW, "Mutant", Monster, 2)
case object Abomination extends FactionUnitClass(OW, "Abomination", Monster, 3)
case object SpawnOW extends FactionUnitClass(OW, "Spawn of Yog-Sothoth", Monster, 4) { override def plural = "Spawns of Yog-Sothoth" }
case object YogSothoth extends FactionUnitClass(OW, "Yog-Sothoth", GOO, 6)

case object BeyondOne extends FactionSpellbook(OW, "The Beyond One")
case object KeyAndGate extends FactionSpellbook(OW, "The Key and the Gate")

case object DreadCurse extends FactionSpellbook(OW, "Dread Curse of Azathoth")
case object MillionFavoredOnes extends FactionSpellbook(OW, "The Million Favored Ones")
case object TheyBreakThrough extends FactionSpellbook(OW, "They Break Through")
case object DragonAscending extends FactionSpellbook(OW, "Dragon Ascending")
case object DragonDescending extends FactionSpellbook(OW, "Dragon Descending")
case object ChannelPower extends FactionSpellbook(OW, "Channel Power")

case object EightGates extends Requirement("8 gates on the map")
case object TwelweGates extends Requirement("12 gates on the map")
case object UnitsAtEnemyGates extends Requirement("Units at 2 enemy gates")
case object LoseUnitInBattle extends Requirement("Lose unit in battle")
case object GooMeetsGoo extends Requirement("GOO in area with enemy GOO")
case object AwakenYogSothoth extends Requirement("Awaken Yog-Sothoth")


case object OW extends Faction {
    def name = "Opener of the Way"
    def short = "OW"
    def style = "ow"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities : List[Spellbook] = List(BeyondOne, KeyAndGate)
    override def spellbooks : List[Spellbook] = List(DreadCurse, MillionFavoredOnes, TheyBreakThrough, DragonAscending, DragonDescending, ChannelPower)
    override def requirements : List[Requirement] = List(EightGates, TwelweGates, UnitsAtEnemyGates, LoseUnitInBattle, GooMeetsGoo, AwakenYogSothoth)

    val allUnits =
        List.fill(1)(YogSothoth) ++
        List.fill(2)(SpawnOW) ++
        List.fill(3)(Abomination) ++
        List.fill(4)(Mutant) ++
        List.fill(6)(Acolyte)

    override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
        case YogSothoth => g.of(this).at(r, SpawnOW).any.?(6).|(999)
    }

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == Mutant) * 1 +
        units.count(_.uclass == Abomination) * 2 +
        units.count(_.uclass == SpawnOW) * 3 +
        units.count(_.uclass == YogSothoth) * (2 * g.factions.%(_ != this)./(g.of(_).all(GOO).num).sum)

}