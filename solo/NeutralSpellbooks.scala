package cws

import hrf.colmat._

import html._


case object MaoCeremony extends NeutralSpellbook("The Mao Ceremony")
case object Recriminations extends NeutralSpellbook("Recriminations")
case object Shriveling extends NeutralSpellbook("Shriveling") with BattleSpellbook
case object StarsAreRight extends NeutralSpellbook("Stars Are Right")
case object UmrAtTawil extends NeutralSpellbook("Umr at-Tawil")
case object Undimensioned extends NeutralSpellbook("Undimensioned")


case class MaoCeremonyAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(MaoCeremony, uc.styled(self) + " in " + r)
case class MaoCeremonyDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class RecriminationsMainAction(self : Faction) extends OptionFactionAction(Recriminations) with MainQuestion with Soft
case class RecriminationsAction(self : Faction, sb : Spellbook) extends BaseFactionAction("Discard spellbook", sb)

case class UndimensionedMainAction(self : Faction) extends OptionFactionAction(Undimensioned) with MainQuestion with Soft
case class UndimensionedContinueAction(self : Faction, destinations : $[Region], moved : Boolean) extends ForcedAction with Soft
case class UndimensionedSelectAction(self : Faction, destinations : $[Region], uc : UnitClass, r : Region) extends BaseFactionAction(g => "" + Undimensioned + " move unit", uc.styled(self) + " from " + r) with Soft
case class UndimensionedAction(self : Faction, destinations : $[Region], uc : UnitClass, r : Region, dest : Region) extends BaseFactionAction(g => "" + Undimensioned + " move " + uc.styled(self) + " from " + r + " to", implicit g => dest + self.iced(dest))
case class UndimensionedDoneAction(self : Faction) extends BaseFactionAction(None, "Done")
case class UndimensionedCancelAction(self : Faction, destinations : $[Region]) extends BaseFactionAction(None, "Cancel") with Cancel


object NeutralSpellbooksExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // MAO CEREMONY
        case MaoCeremonyAction(self, r, uc) =>
            val c = self.at(r).one(uc)
            game.eliminate(c)
            self.power += 1
            self.log("sacrificed", c, "in", r, "for", 1.power)

            game.triggers() // game.checkPowerReached()

            game.checkGatesLost()

            AfterPowerGatherAction

        case MaoCeremonyDoneAction(self) =>
            self.ignorePerInstant :+= MaoCeremony

            AfterPowerGatherAction

        // RECRIMINATIONS
        case RecriminationsMainAction(self) =>
            Ask(self).each(self.spellbooks)(b => RecriminationsAction(self, b))

        case RecriminationsAction(self, sb) =>
            self.power -= 1
            self.spellbooks = self.spellbooks.but(sb)

            if (sb.is[NeutralSpellbook])
                game.neutralSpellbooks :+= sb

            self.log("discarded", sb)

            self.ignorePerInstant :+= sb

            EndAction(self)

        // UNDIMENSIONED
        case UndimensionedMainAction(self) =>
            UndimensionedContinueAction(self, self.units.onMap./(_.region).distinct, false)

        case UndimensionedContinueAction(self, destinations, moved) =>
            val units = self.units.nex.onMap.not(Moved).%(u => destinations.but(u.region).%(self.affords(self.units.onMap.tag(Moved).none.??(2))).any).sortA
            if (units.none)
                Then(UndimensionedDoneAction(self))
            else
            if (moved)
                Ask(self).add(UndimensionedDoneAction(self)).each(units)(u => UndimensionedSelectAction(u.faction, destinations, u.uclass, u.region))
            else
                Ask(self).each(units)(u => UndimensionedSelectAction(u.faction, destinations, u.uclass, u.region)).cancel

        case UndimensionedSelectAction(self, destinations, uc, r) =>
            val options = destinations.but(r).%(self.affords(self.units.onMap.tag(Moved).none.??(2)))./(d => UndimensionedAction(self, destinations, uc, r, d))

            if (self.units.onMap.tag(Moved).any)
                Ask(self).list(options).add(UndimensionedCancelAction(self, destinations))
            else
                Ask(self).list(options).cancel

        case UndimensionedDoneAction(self) =>
            self.units.foreach(_.remove(Moved))
            EndAction(self)

        case UndimensionedAction(self, destinations, uc, o, r) =>
            if (self.units.onMap.tag(Moved).none) {
                self.log("units are", Undimensioned)
                self.power -= 2
            }

            self.payTax(r)

            val u = self.at(o, uc).not(Moved).first
            u.region = r
            u.add(Moved)

            log(uc.styled(self), "from", o, "is now in", r)

            UndimensionedContinueAction(self, destinations, true)

        case UndimensionedCancelAction(self, destinations) =>
            UndimensionedContinueAction(self, destinations, true)

        // ...
        case _ => UnknownContinue
    }
}
