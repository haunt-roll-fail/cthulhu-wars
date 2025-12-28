package cws

import hrf.colmat._

import Html._


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
        case Tsathoggua => (f.at(r, FormlessSpawn).any).?((f.has(Immortal) && f.needs(AwakenTsathoggua).not).?(4).|(8))
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(SerpentMan).num * 1 +
        units(FormlessSpawn).num * (f.all(FormlessSpawn).num + f.all(Tsathoggua).num) +
        units(Tsathoggua).%!(_.has(Zeroed)).num * max(2, opponent.power) +
        neutralStrength(units, opponent)
}


case class DeathFromBelowDoomAction(self : Faction) extends OptionFactionAction(DeathFromBelow) with DoomQuestion with Soft with PowerNeutral
case class DeathFromBelowSelectMonsterAction(self : Faction, uc : UnitClass) extends BaseFactionAction(DeathFromBelow, self.styled(uc))
case class DeathFromBelowAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(DeathFromBelow, self.styled(uc) + " in " + r)

case class LethargyMainAction(self : Faction) extends OptionFactionAction(Lethargy) with MainQuestion with PowerNeutral

case class Pay3SomeoneGains3MainAction(self : Faction) extends OptionFactionAction("Pay " + 3.power + " and another faction gains " + 3.power) with MainQuestion with Soft
case class Pay3SomeoneGains3Action(self : Faction, f : Faction) extends BaseFactionAction("Get spellbook for " + 3.power, "" + f + " gets " + 3.power)

case class Pay3EverybodyLoses1MainAction(self : Faction) extends OptionFactionAction("Pay " + 3.power + ", other factions lose " + 1.power + " each") with MainQuestion
case class Pay3EverybodyGains1MainAction(self : Faction) extends OptionFactionAction("Pay " + 3.power + ", other factions gain " + 1.power + " each") with MainQuestion

case class CaptureMonsterMainAction(self : Faction) extends OptionFactionAction(CaptureMonster) with MainQuestion with Soft
case class CaptureMonsterAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(CaptureMonster, "Capture " + f + " Monster in " + r)
case class CaptureMonsterUnitAction(f : Faction, r : Region, self : Faction, uc : UnitClass) extends BaseFactionAction(CaptureMonster.full + " in " + r, self.styled(uc))

case class AncientSorceryMainAction(self : Faction) extends OptionFactionAction(AncientSorcery) with MainQuestion with Soft
case class AncientSorceryAction(self : Faction, a : Spellbook) extends BaseFactionAction(AncientSorcery, a) with Soft
case class AncientSorceryUnitAction(self : Faction, a : Spellbook, r : Region, uc : UnitClass) extends BaseFactionAction("Access " + a.full + " with", self.styled(uc) + " from " + r)
case class AncientSorceryDoomAction(self : Faction) extends OptionFactionAction(AncientSorcery) with DoomQuestion with Soft
case class AncientSorceryPlaceAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Place " + uc + " in", r)

case class CursedSlumberSaveMainAction(self : Faction) extends OptionFactionAction(CursedSlumber) with MainQuestion with Soft
case class CursedSlumberSaveAction(self : Faction, r : Region) extends BaseFactionAction("Move gate to " + CursedSlumber.full + " from", r)
case class CursedSlumberLoadMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(CursedSlumber) with MainQuestion with Soft
case class CursedSlumberLoadAction(self : Faction, r : Region) extends BaseFactionAction("Move gate from " + CursedSlumber.full + " to", implicit g => r + self.iced(r))


object SLExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // DEATH FROM BELOW
        case DeathFromBelowDoomAction(self) =>
            val unitClasses = self.pool.monsters./(_.uclass)

            val minCost = unitClasses.map(_.cost).min
            val ucs = unitClasses.filter(_.cost == minCost).distinct

            if (ucs.num == 1) {
                Ask(self).each(board.regions.%(r => self.at(r).any).some.|(board.regions))(r => DeathFromBelowAction(self, r, ucs.first)).cancel
            }
            else {
                Ask(self).each(ucs)(uc => DeathFromBelowSelectMonsterAction(self, uc)).cancel
            }

        case DeathFromBelowSelectMonsterAction(self, uc) =>
            Ask(self).each(board.regions.%(r => self.at(r).any).some.|(board.regions))(r => DeathFromBelowAction(self, r, uc)).cancel

        case DeathFromBelowAction(self, r, uc) =>
            self.place(uc, r)
            self.log("placed", uc, "in", r, "with", DeathFromBelow.full)
            self.oncePerTurn :+= DeathFromBelow
            CheckSpellbooksAction(DoomAction(self))

        // LETHARGY
        case LethargyMainAction(self) =>
            if (options.has(IceAgeAffectsLethargy))
                self.payTax(self.goo(Tsathoggua).region)

            self.log("was sleeping")
            self.battled = board.regions
            EndAction(self)

        // PAY 3 POWER
        case Pay3SomeoneGains3MainAction(self) =>
            Ask(self).each(self.enemies)(Pay3SomeoneGains3Action(self, _)).cancel

        case Pay3SomeoneGains3Action(self, f) =>
            self.power -= 3
            f.power += 3
            self.log("spent", 3.power, "and", f, "gained", 3.power)
            self.satisfy(Pay3SomeoneGains3, "Provide 3 Power")
            EndAction(self)

        case Pay3EverybodyLoses1MainAction(self) =>
            self.power -= 3
            self.enemies.%(f => f.power > 0).foreach(f => f.power -= 1)
            self.log("spent", 3.power, "and each other faction lost", 1.power)
            self.satisfy(Pay3EverybodyLoses1, "Everybody loses 1 power")
            EndAction(self)

        case Pay3EverybodyGains1MainAction(self) =>
            self.power -= 3
            self.enemies.foreach(f => f.power += 1)
            self.log("spent", 3.power, "and each other faction gained", 1.power)
            self.satisfy(Pay3EverybodyGains1, "Everybody gains 1 power")
            EndAction(self)

        // CAPTURE MONSTER
        case CaptureMonsterMainAction(self) =>
            val variants = board.regions./~ { r =>
                self.at(r, Tsathoggua).any.?? {
                    self.enemies.%(f => f.at(r).monsters.any && f.at(r, GOO).none)
                        ./(f => CaptureMonsterAction(self, r, f))
                }
            }

            Ask(self).list(variants).cancel

        case CaptureMonsterAction(self, r, f) =>
            self.power -= 1

            Ask(f).each(f.at(r).monsters.sortBy(_.uclass.cost))(u => CaptureMonsterUnitAction(self, r, u.faction, u.uclass))

        case CaptureMonsterUnitAction(self, r, f, uc) =>
            val m = f.at(r).one(uc)
            game.capture(self, m)
            self.log("captured", m, "in", r)
            EndAction(self)

        // ANCIENT SORCERY
        case AncientSorceryMainAction(self) =>
            Ask(self).each(self.enemies./(_.abilities.first).diff(self.borrowed))(a => AncientSorceryAction(self, a)).cancel

        case AncientSorceryAction(self, a) =>
            Ask(self).each(self.onMap(SerpentMan).nex)(u => AncientSorceryUnitAction(self, a, u.region, u.uclass)).cancel

        case AncientSorceryUnitAction(self, a, r, uc) =>
            self.power -= 1
            self.at(r).one(uc).region = SL.slumber
            self.borrowed :+= a
            self.log("sent", uc, "from", r, "to access", a.full)
            EndAction(self)

        case AncientSorceryDoomAction(self) =>
            Ask(self).each(board.regions)(r => AncientSorceryPlaceAction(self, r, SerpentMan)).cancel

        case AncientSorceryPlaceAction(self, r, uc) =>
            self.at(SL.slumber).one(uc).region = r
            self.power += 1
            self.log("placed", uc, "in", r, "with", AncientSorcery.full, "and gained", 1.power)
            CheckSpellbooksAction(DoomAction(self))

        // CURSED SLUMBER
        case CursedSlumberSaveMainAction(self) =>
            Ask(self).each(self.gates.nex)(CursedSlumberSaveAction(self, _)).cancel

        case CursedSlumberSaveAction(self, r) =>
            self.power -= 1

            self.gates :-= r
            self.gates :+= SL.slumber
            game.gates :-= r
            game.gates :+= SL.slumber

            self.at(r).one(Cultist).region = SL.slumber

            self.log("moved gate from", r, "to", CursedSlumber.full)

            EndAction(self)

        case CursedSlumberLoadMainAction(self, l) =>
            Ask(self).each(l)(CursedSlumberLoadAction(self, _)).cancel

        case CursedSlumberLoadAction(self, r) =>
            self.power -= 1
            self.payTax(r)

            self.gates :-= SL.slumber
            self.gates :+= r
            game.gates :-= SL.slumber
            game.gates :+= r

            if (self.at(SL.slumber, Cultist).any)
                self.at(SL.slumber).one(Cultist).region = r

            self.log("moved gate from", CursedSlumber.full, "to", r)

            EndAction(self)


        case _ => UnknownContinue
    }
}
