package cws

import hrf.colmat._

import html._

// IGOOs
case object ByatisCard extends IGOOLoyaltyCard(ByatisIcon, Byatis, power = 4, combat = 4)
case object AbhothCard extends IGOOLoyaltyCard(AbhothIcon, Abhoth, power = 4)
case object DaolothCard extends IGOOLoyaltyCard(DaolothIcon, Daoloth, power = 6)
case object NyogthaCard extends IGOOLoyaltyCard(NyogthaIcon, Nyogtha, power = 6, quantity = 2, combat = 4)

case object ByatisIcon extends UnitClass(Byatis.name + " Icon", Token, 0)
case object AbhothIcon extends UnitClass(Abhoth.name + " Icon", Token, 0)
case object DaolothIcon extends UnitClass(Daoloth.name + " Icon", Token, 0)
case object NyogthaIcon extends UnitClass(Nyogtha.name + " Icon", Token, 0)

case object Byatis extends UnitClass("Byatis", GOO, 4) with IGOO {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) = false
}

case object Abhoth extends UnitClass("Abhoth", GOO, 4) with IGOO
case object Daoloth extends UnitClass("Daoloth", GOO, 6) with IGOO
case object Nyogtha extends UnitClass("Nyogtha", GOO, 6) with IGOO

case object Filth extends UnitClass("Filth", Monster, 1) {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBattle(u : UnitFigure)(implicit game : Game) = false
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canBeSummoned(f : Faction)(implicit game : Game) = f.has(Fertility)
}

// Byatis
case object ToadOfBerkeley extends NeutralSpellbook("Toad of Berkeley")
case object GodOfForgetfulness extends NeutralSpellbook("God of Forgetfulness")

// Abhoth
case object LostAbhoth extends NeutralSpellbook("Lost Abhoth")
case object TheBrood extends NeutralSpellbook("The Brood")

// Daoloth
case object CosmicUnity extends NeutralSpellbook("Cosmic Unity") with BattleSpellbook
case object Interdimensional extends NeutralSpellbook("Interdimensional")

// Nyogtha
case object FromBelow extends NeutralSpellbook("From Below")
case object NyogthaPrimed extends NeutralSpellbook("Nyogtha Primed")
case object NyogthaMourning extends NeutralSpellbook("Nyogtha Mourning")
case object NightmareWeb extends NeutralSpellbook("Nightmare Web")


trait NeutralFaction extends Faction

case object NeutralAbhoth extends NeutralFaction {
    def name = "Neutral Abhoth"
    def short = "NA"
    def style = "nt"
    val reserve = Region(name + " Pool", Pool)
    val prison = Region(name + " Prison", Prison)

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = 12.times(Filth)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = 0
}


case class IndependentGOOMainAction(self : Faction, lc : IGOOLoyaltyCard, l : $[Region]) extends OptionFactionAction(g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Awaken " + lc.name.styled("nt") +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with MainQuestion with Soft
case class IndependentGOOAction(self : Faction, lc : LoyaltyCard, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + self.styled(lc.unit) + g.forNPowerWithTax(r, self, cost) + " in", implicit g => r + self.iced(r))

case class GodOfForgetfulnessMainAction(self : Faction, d : Region, l : $[Region]) extends OptionFactionAction(self.styled("God of Forgetfulness")) with MainQuestion with Soft
case class GodOfForgetfulnessAction(self : Faction, d : Region, r : Region) extends BaseFactionAction(g => "Move all enemy Cultists to " + self.styled(Byatis) + " " + g.forNPowerWithTax(d, self, 1) + " from", r)

case class FilthMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Place " + self.styled(Filth)) with MainQuestion with Soft
case class FilthAction(self : Faction, r : Region) extends BaseFactionAction(g => "Place " + self.styled(Filth) + " " + g.forNPowerWithTax(r, self, 1) + " in", r)

case class NightmareWebMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Awaken " + self.styled(Nyogtha) + " with " + self.styled(NightmareWeb)) with MainQuestion with Soft
case class NightmareWebAction(self : Faction, r : Region) extends BaseFactionAction(g => "Awaken " + self.styled(Nyogtha) + g.forNPowerWithTax(r, self, 2) + " in", implicit g => r + self.iced(r))


object IGOOsExpansion extends Expansion {
    def checkAbhothSpellbook()(implicit game : Game) {
        factions.foreach { f =>
            if (f.has(Abhoth) && f.upgrades.has(TheBrood).not) {
                val monsters = f.units.monsters.inPlay

                if (monsters./(_.uclass).distinct.num >= 4 || monsters.num >= 8) {
                    f.upgrades :+= TheBrood
                    f.log("gained", f.styled(TheBrood), "for", f.styled(Abhoth))
                }
            }
        }
    }

    def checkInterdimensional()(implicit game : Game) {
        factions.foreach { f =>
            if (f.has(Interdimensional)) {
                val r = f.goo(Daoloth).region

                if (r.onMap && game.gates.has(r).not) {
                    game.gates :+= r
                    log(f.styled(Daoloth), "placed a Gate in", r, "with", f.styled(Interdimensional))
                }
            }
        }
    }

    override def triggers()(implicit game : Game) {
        checkAbhothSpellbook()
        checkInterdimensional()
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        val f = u.faction

        u.uclass @@ {
            case Byatis =>
                f.units :-= u
                f.upgrades :-= GodOfForgetfulness

                f.loyaltyCards :-= ByatisCard
                game.loyaltyCards :+= ByatisCard

            case Abhoth =>
                f.units :-= u
                f.upgrades :-= TheBrood

                f.loyaltyCards :-= AbhothCard
                game.loyaltyCards :+= AbhothCard

                f.oncePerAction :+= LostAbhoth

            case Daoloth =>
                f.units :-= u
                f.upgrades :-= CosmicUnity
                f.upgrades :-= Interdimensional

                f.loyaltyCards :-= DaolothCard
                game.loyaltyCards :+= DaolothCard

            case Nyogtha if f.all(Nyogtha).but(u).any =>
                f.oncePerAction :+= NyogthaMourning

            case Nyogtha =>
                f.units = f.units.%!(_.uclass == Nyogtha)
                f.upgrades :-= FromBelow
                f.upgrades :-= NightmareWeb
                f.loyaltyCards :-= NyogthaCard
                game.loyaltyCards :+= NyogthaCard

            case _ =>
        }
    }

    override def afterAction()(implicit game : Game) {
        factions.%(_.oncePerAction.has(LostAbhoth)).foreach { f =>
            NeutralAbhoth.units = f.units(Filth)./(u => new UnitFigure(NeutralAbhoth, u.uclass, u.index, (u.region == f.reserve).?(NeutralAbhoth.reserve).|(u.region), u.onGate, u.state, u.health))

            f.units = f.units.not(Filth)
        }

        factions.%(_.oncePerAction.has(NyogthaPrimed)).%!(_.oncePerAction.has(NyogthaMourning)).foreach { f =>
            if (f.upgrades.has(NightmareWeb).not) {
                f.upgrades :+= NightmareWeb

                f.log("gained", f.styled(NightmareWeb), "for", f.styled(Nyogtha))
            }
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        case IndependentGOOMainAction(self, lc, l) =>
            Ask(self).each(l)(r => IndependentGOOAction(self, lc, r, lc.power)).cancel

        case IndependentGOOAction(self, lc, r, _) =>
            self.loyaltyCards :+= lc
            game.loyaltyCards :-= lc

            self.power -= lc.power

            self.log("awakened", lc.unit.name.styled("nt"), "in", r, "for", lc.power.power)

            self.units :+= new UnitFigure(self, lc.unit, 1, r)

            lc.unit match {
                case Abhoth =>
                    if (game.neutrals.contains(NeutralAbhoth).not)
                        game.neutrals += NeutralAbhoth -> new Player(NeutralAbhoth)

                    self.units ++= NeutralAbhoth.units./(u => new UnitFigure(self, u.uclass, u.index, (u.region == NeutralAbhoth.reserve).?(self.reserve).|(u.region), u.onGate, u.state, u.health))

                    NeutralAbhoth.units = $

                case Daoloth =>
                    self.upgrades :+= CosmicUnity

                case Nyogtha =>
                    self.units :+= new UnitFigure(self, lc.unit, 2, r)

                    self.upgrades :+= FromBelow

                case _ =>
            }

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal.full)

                self.takeES(1)
            }

            EndAction(self)

        // BYATIS
        case GodOfForgetfulnessMainAction(self, d, l) =>
            Ask(self).each(l)(r => GodOfForgetfulnessAction(self, d, r)).cancel

        case GodOfForgetfulnessAction(self, d, r) =>
            self.power -= 1
            self.payTax(r)

            self.enemies.foreach { f =>
                f.at(r).cultists.foreach { u =>
                    u.region = d
                }
            }
            log(self.styled(Byatis), "used", GodOfForgetfulness.name.styled("nt"), "to move all enemy cultist from", r, "to", d)
            EndAction(self)

        // ABHOTH
        case FilthMainAction(self, l) =>
            Ask(self).each(l)(r => FilthAction(self, r)).cancel

        case FilthAction(self, r) =>
            self.power -= 1
            self.payTax(r)

            self.place(Filth, r)
            log(self.styled(Abhoth), "placed", self.styled(Filth), "in", r)

            EndAction(self)

        // NYOGTHA
        case MovedAction(self, u, o, r) if u.uclass == Nyogtha =>
            self.all(Nyogtha).but(u).not(Moved).single./(n => MoveSelectAction(self, n, n.region, 0)).|(MoveContinueAction(self, true))

        case NightmareWebMainAction(self, regions) =>
            Ask(self).each(regions)(r => NightmareWebAction(self, r)).cancel

        case NightmareWebAction(self, r) =>
            self.power -= 2
            self.payTax(r)

            val ny = self.pool.one(Nyogtha)

            ny.region = r

            self.log("awakened", self.styled(Nyogtha), "in", r, "with", self.styled(NightmareWeb))

            EndAction(self)

        // ...
        case _ => UnknownContinue
    }
}
