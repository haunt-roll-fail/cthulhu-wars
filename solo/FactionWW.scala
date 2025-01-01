package cws

import colmat._

case object Wendigo extends FactionUnitClass(WW, "Wendigo", Monster, 1)
case object GnophKeh extends FactionUnitClass(WW, "Gnoph-Keh", Monster, 4)
case object RhanTegoth extends FactionUnitClass(WW, "Rhan-Tegoth", GOO, 6)
case object Ithaqua extends FactionUnitClass(WW, "Ithaqua", GOO, 6)

case object Hibernate extends FactionSpellbook(WW, "Hibernate")
case object Eternal extends FactionSpellbook(WW, "Eternal")
case object Ferox extends FactionSpellbook(WW, "Ferox")

case object Cannibalism extends FactionSpellbook(WW, "Cannibalism")
case object Howl extends FactionSpellbook(WW, "Howl")
case object Berserkergang extends FactionSpellbook(WW, "Berserkergang")
case object Herald extends FactionSpellbook(WW, "Herald of the Outer Gods")
case object ArcticWind extends FactionSpellbook(WW, "Arctic Wind")
case object IceAge extends FactionSpellbook(WW, "Ice Age")

case object FirstPlayer extends Requirement("Starting Player")
case object OppositeGate extends Requirement("Opposite Gate")
case object AnotherFactionAllSpellbooks extends Requirement("Another Faction All Spellbooks")
case object AnytimeGainElderSigns extends Requirement("Anytime Spellbook")
case object AwakenRhanTegoth extends Requirement("Awaken Rhan-Tegoth")
case object AwakenIthaqua extends Requirement("Awaken Ithaqua")


case object WW extends Faction {
    def name = "Windwalker"
    def short = "WW"
    def style = "ww"
    val poolR = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities : List[Spellbook] = List(Hibernate, Eternal, Ferox)
    override def spellbooks : List[Spellbook] = List(Cannibalism, Howl, Berserkergang, Herald, ArcticWind, IceAge)
    override def requirements : List[Requirement] = List(FirstPlayer, OppositeGate, AnotherFactionAllSpellbooks, AnytimeGainElderSigns, AwakenRhanTegoth, AwakenIthaqua)

    val allUnits =
        List.fill(1)(Ithaqua) ++
        List.fill(1)(RhanTegoth) ++
        List.fill(4)(GnophKeh) ++
        List.fill(4)(Wendigo) ++
        List.fill(6)(Acolyte)

    override def awakenCost(g : Game, u : UnitClass, r : Region) = u match {
        case RhanTegoth => g.board.starting(this).contains(r).?(6).|(999)
        case Ithaqua => (g.board.starting(this).contains(r) && (g.gates.contains(r) || g.factions.but(this).%(e => g.of(e).ugate./(_.region == r).|(false)).any) && !g.of(this).requirements.contains(AwakenRhanTegoth)).?(6).|(999)
    }

    override def summonCost(g : Game, u : UnitClass, r : Region) = u match {
        case GnophKeh => g.of(this).inPool(GnophKeh).num
        case _ => u.cost
    }

    override def strength(g : Game, units : List[UnitFigure], opponent : Faction) =
        units.count(_.uclass == Wendigo) * 1 +
        units.count(_.uclass == GnophKeh) * 3 +
        units.count(_.uclass == RhanTegoth) * 3 +
        units.count(_.uclass == Ithaqua) * ((g.of(opponent).doom + 1) / 2)

}
