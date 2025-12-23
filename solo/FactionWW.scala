package cws

import hrf.colmat._

case object Wendigo extends FactionUnitClass(WW, "Wendigo", Monster, 1)
case object GnophKeh extends FactionUnitClass(WW, "Gnoph-Keh", Monster, 4)
case object RhanTegoth extends FactionUnitClass(WW, "Rhan Tegoth", GOO, 6)
case object Ithaqua extends FactionUnitClass(WW, "Ithaqua", GOO, 6)

case object Hibernate extends FactionSpellbook(WW, "Hibernate")
case object Eternal extends FactionSpellbook(WW, "Eternal")
case object Ferox extends FactionSpellbook(WW, "Ferox")

case object Cannibalism extends FactionSpellbook(WW, "Cannibalism")
case object Howl extends FactionSpellbook(WW, "Howl")
case object Berserkergang extends FactionSpellbook(WW, "Berserkergang")
case object ArcticWind extends FactionSpellbook(WW, "Arctic Wind")
case object IceAge extends FactionSpellbook(WW, "Ice Age")
case object Herald extends FactionSpellbook(WW, "Herald of the Outer Gods")

case object FirstPlayer extends Requirement("Starting Player")
case object OppositeGate extends Requirement("Opposite Gate")
case object AnotherFactionAllSpellbooks extends Requirement("Another Faction All Spellbooks")
case object AnytimeGainElderSigns extends Requirement("Anytime Spellbook")
case object AwakenRhanTegoth extends Requirement("Awaken Rhan Tegoth")
case object AwakenIthaqua extends Requirement("Awaken Ithaqua")


case object WW extends Faction { f =>
    def name = "Windwalker"
    def short = "WW"
    def style = "ww"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $(Hibernate, Eternal, Ferox)
    override def library = $(Cannibalism, Howl, Berserkergang, ArcticWind, IceAge, Herald)
    override def requirements(options : $[GameOption]) = $(FirstPlayer, OppositeGate, AnotherFactionAllSpellbooks, AnytimeGainElderSigns, AwakenRhanTegoth, AwakenIthaqua)

    val allUnits =
        1.times(Ithaqua) ++
        1.times(RhanTegoth) ++
        4.times(GnophKeh) ++
        4.times(Wendigo) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case RhanTegoth => game.board.starting(f).has(r).?(6).|(999)
        case Ithaqua => (game.board.starting(f).has(r) && (game.gates.has(r) || game.unitGates.has(r)) && f.needs(AwakenRhanTegoth).not).?(6).|(999)
    }

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u @@ {
        case GnophKeh => f.inPool(GnophKeh).num
        case _ => u.cost
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(Wendigo).num * 1 +
        units(GnophKeh).num * 3 +
        units(RhanTegoth).%!(_.has(Zeroed)).num * 3 +
        units(Ithaqua).%!(_.has(Zeroed)).num * ((opponent.doom + 1) / 2) +
        neutralStrength(units, opponent)
}
