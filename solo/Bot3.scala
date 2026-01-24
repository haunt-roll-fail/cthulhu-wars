package cws

import hrf.colmat._


case class Bot3(faction : Faction) {
    def cost(a : Action)(implicit game : Game) : Int = a match {
        case BuildGateAction(_, _) => 3
        case SummonAction(self, uc, r) => self.summonCost(uc, r) + self.taxIn(r)
        case AwakenAction(self, uc, r, cost) => cost + self.taxIn(r)
        case GhrothMainAction(_) => 2
        case DreamsAction(_, _, _) => 2
        case Pay4PowerMainAction(_) => 4
        case Pay6PowerMainAction(_) => 6
        case Pay10PowerMainAction(_) => 10
        case _ => 1
    }

    def ask(actions : $[Action], error : Double)(implicit game : Game) : Action =
        askE(Explode.explode(game, actions), error)

    def askE(actions : $[Action], error : Double)(implicit game : Game) : Action = {
        if (actions.num == 1)
            return actions.head

        eval(actions).maxBy(_.evaluations.map(_.weight).sum * (1 + error * (random() * 2 - 1))).action
    }

    def eval(actions : $[Action])(implicit game : Game) : $[ActionEval] = {
        if (game.factions.none)
            return actions./{ a => ActionEval(a, $) }

        val self = faction
        val others = game.factions.%(_ != self)
        val power = self.power

        implicit class FactionClassify(val f : Faction) {
            def realDoom = f.doom + f.es./(_.value).sum
            def aprxDoom = f.doom + (f.es.num * 1.67).round.toInt
            def count(uc : UnitClass) = f.all(uc).num
            def allSB = f.hasAllSB
            def numSB = f.spellbooks.num
        }

        implicit class RegionClassify(val r : Region) {
            def empty = allies.none && foes.none
            def allies = self.at(r)
            def foes = others./~(_.at(r))
            def gate = game.gates.has(r)
            def ownGate = self.gates.has(r)
            def enemyGate = others.%(_.gates.has(r)).any
            def freeGate = gate && !ownGate && !enemyGate
            def controllers : $[UnitFigure] = (ownGate || enemyGate).??(owner.at(r).%(_.canControlGate))
            def owner = game.factions.%(_.gates.has(r)).single.get
            def capturers = allies.goos.none.??(others.%(f => f.at(r).goos.any || (allies.monsterly.none && f.at(r).monsterly.%(_.canCapture).any)))
        }

        implicit class UnitClassify(val u : UnitFigure) {
            def is(uc : UnitClass) = u.uclass == uc
            def ally = u.faction == self
            def foe = u.faction != self
            def friends = u.faction.at(u.region).%(_ != u)
            def enemies = game.factions.%(_ != u.faction)./~(_.at(u.region))
            def ownGate = u.region.ownGate
            def enemyGate = u.region.enemyGate
            def gateController = u.region.gate && u.region.controllers.has(u)
            def gateKeeper = gateController && friends.%(_.canControlGate).none
            def defender = ownGate && (u.monster || u.terror || u.goo) && friends.monsterly.none
            def protector = (u.monster || u.terror || u.goo) && friends.cultists.any && friends.monsterly.none
            def preventsCaptureM = u.monsterly && friends.cultists.any && friends.monsterly.none && friends.goos.none && enemies.monsterly.any
            def preventsCaptureG = u.goo && friends.cultists.any && friends.goos.none && enemies.goos.any
            def prevents = preventsCaptureM || preventsCaptureG
            def pretender = u.cultist && !capturable && enemyGate
            def shield = friends.goos.any
            def capturable = u.cultist && capturers.%(_.power > 0).any
            def capturers = game.factions.%(_ != u.faction).%(f => friends.goos.none && (f.at(u.region).goos.any || (friends.monsterly.none && f.at(u.region).monsterly.%(_.canCapture).any)))
            def vulnerable = u.cultist && friends.goos.none && friends.monsterly.none
        }

        implicit def unitRefToUnitClassify(r : UnitRef) : UnitClassify = UnitClassify(r)
        implicit def unitRefToUnitFigureGameEx(r : UnitRef) : UnitFigureGameEx = UnitFigureGameEx(r)

        val maxEnemyPower = others./(_.power).max

        def adjustedOwnStrengthForCosmicUnity(ownStr : Int, allies : $[UnitFigure], foes : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
            val hasDaoloth = foes.exists(_.uclass == Daoloth)
            if (!hasDaoloth) return ownStr

            val allyGOOs = allies.filter(_.uclass.utype == GOO)
            if (allyGOOs.none) return ownStr

            val nyogthas = allies.filter(_.uclass == Nyogtha)
            val nyogthaReduction : Int = if (nyogthas.any) nyogthas.head.faction.strength(nyogthas, opponent) else 0

            val perGOOStrengths : $[Int] = allyGOOs.map(u => u.faction.strength($(u), opponent))
            val strongestGOOStr = perGOOStrengths.foldLeft(0)(math.max)

            val reduction = math.max(nyogthaReduction, strongestGOOStr)
            math.max(0, ownStr - reduction)
        }

        def canRitual = self.acted.not && game.ritualCost <= power

        def canSummon(u : UnitClass) = self.pool(u).any

        val has1000f = faction.as[CC].?(f => actions.has(ThousandFormsMainAction(f)))

        val otherOceanGates = others./(_.gates.%(_.glyph == Ocean).any).any

        val instantDeathNow = game.ritualTrack(game.ritualMarker) == 999 || game.factions.%(_.doom >= 30).any
        val instantDeathNext = game.ritualTrack(game.ritualMarker) != 999 && game.ritualTrack(game.ritualMarker + 1) == 999

        def validGatesForRitual : $[Region] = {
            self.gates.filter { r =>
                val filthHere = game.factions.exists { other =>
                    other != self &&
                    other.has(TheBrood) &&
                    other.at(r).exists(_.uclass == Filth)
                }
                !filthHere
            }
        }

        def maxDoomGain = validGatesForRitual.num + self.factionGOOs.num * 3
        def aprxDoomGain = validGatesForRitual.num + self.factionGOOs.num * 1.666

        def evalA(a : Action) : $[Evaluation] = {
            var result : $[Evaluation] = $

            implicit class condToEval(val bool : Boolean) {
                def |=> (e : => (Int, String)) { if (bool) result :+= Evaluation(e._1, e._2) }
            }

            if (self == GC) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case Devolve =>
                        true |=> 500 -> "must have"
                    case Submerge =>
                        self.has(Cthulhu) |=> 900 -> "cthulhu in play"
                    case Dreams =>
                        true |=> 200 -> "dreaming is ok"
                        otherOceanGates |=> 400 -> "ocean gates"
                        self.has(Cthulhu) |=> -200 -> "cthulhu in play"
                    case YhaNthlei =>
                        true |=> -600 -> "late game"
                        self.has(Cthulhu) |=> 800 -> "cthulhu in play"
                        otherOceanGates |=> 800 -> "ocean gates"
                    case Regenerate =>
                        self.count(Starspawn) >= 2 |=> 200 -> "2 starspawn"
                        self.count(Starspawn) >= 1 |=> 300 -> "one starspawn"
                    case Absorb =>
                        self.count(Shoggoth) >= 2 |=> 200 -> "shoggoth"
                        self.count(Shoggoth) >= 1 |=> 200 -> "one shoggoth"
                        self.count(DeepOne) >= 4 |=> 200 -> "many deep ones"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

                case DevolveAction(_, r, then) =>
                    val c = self.at(r, Acolyte).head
                    true |=> -100 -> "not unless needed"
                    c.gateKeeper |=> -500 -> "don't devolve gatekeeper"
                    then != PreMainAction(self) && c.capturable |=> 1000 -> "devolve to avoid capture"
                    then == PreMainAction(self) && self.acted.not && self.has(Dreams) && self.pool.cultists.none && areas.%(r => r.enemyGate && r.controllers.num == 1 && others.%(_.power > 0).%(f => f.at(r).monsterly.any || f.at(r).goos.any).none).any |=> 300 -> "devolve to allow dreams"
                    then == PreMainAction(self) && !c.gateKeeper && self.pool.cultists.none && areas.%(r => r.freeGate && r.capturers.none).any |=> 800 -> "devolve to recruit at free gate"

                case DreamsAction(_, r, f) =>
                    val c = f.at(r)(Acolyte).head

                    true |=> -100 -> "dreams are expensive"
                    r.enemyGate |=> 300 -> "enemy gate"
                    c.gateKeeper |=> 200 -> "enemy gate controller"
                    c.friends.none |=> 200 -> "no friends"
                    c.faction.power == 0 && !c.faction.has(Passion) |=> 200 -> "enemy out of power"
                    r.allies.goos.any && r.foes.goos.none |=> -300 -> "have goo there already"
                    others.%(_.power > 0 || power == 2).%(f => f.at(r).goos.any || (r.allies.none && f.at(r).monsterly.any)).any |=> -200 -> "may end captured"

                case SubmergeMainAction(_, _) =>
                    val cthulhu = self.goo(Cthulhu)

                    cthulhu.enemies.any |=> -100 -> "can fight right now"
                    true |=> 400 -> "better than moving"
                    cthulhu.friends.num >= 5 |=> 100 -> "many friends"

                case SubmergeAction(_, r, uc) =>
                    val u = self.at(r).one(uc)
                    u.gateKeeper |=> -500 -> "don't submerge gate keeper"
                    u.defender |=> -400 -> "don't submerge defender"
                    u.uclass.cost == 3 |=> 300 -> "submerge 3"
                    u.uclass.cost == 2 |=> 300 -> "submerge 2"
                    u.uclass.cost == 1 |=> 300 -> "submerge 1"
                    u.cultist && u.faction.at(GC.deep).cultists.any |=> -400 -> "one cultist is enough"

                case UnsubmergeAction(_, r) =>
                    r.enemyGate |=> 200 -> "unsubmerge on gate"
                    r.glyph == Ocean |=> 200 -> "unsubmerge in ocean"
                    r.foes.goos.any |=> 200 -> "unsubmerge to goo"
                    r.foes.num > 5 |=> 200 -> "unsubmerge to many foes"

                case SummonAction(_, m, r) =>
                    m == DeepOne && r.allies.cultists.num >= 3 && self.has(Devolve) |=> -200 -> "don't summon, devolve"

                case _ =>
            }

            if (self == CC)
                if (has1000f)
                    power - cost(a) < 1 |=> -1000 -> "don't spend last power if 1000F unused"

            if (self == CC) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case ThousandForms =>
                        self.has(Nyarlathotep) |=> 1000 -> "must have if have nyarlathotep"
                    case Emissary =>
                        self.has(Nyarlathotep) |=> 900 -> "good if have nyarlathotep"
                    case SeekAndDestroy =>
                        self.count(HuntingHorror) == 2 |=> 800 -> "all hh"
                        self.count(HuntingHorror) == 1 |=> 500 -> "one hh"
                        self.count(HuntingHorror) == 0 |=> 200 -> "too good"
                    case Invisibility =>
                        self.count(FlyingPolyp) == 3 |=> 700 -> "all fp"
                        self.count(FlyingPolyp) == 2 |=> 600 -> "two fp"
                        self.count(FlyingPolyp) == 1 |=> 300 -> "one fp"
                    case Abduct =>
                        self.count(Nightgaunt) == 3 |=> 400 -> "all ng"
                        self.count(Nightgaunt) == 2 |=> 200 -> "two ng"
                        self.count(Nightgaunt) == 1 |=> 50 -> "one ng"
                    case Madness =>
                        true |=> 100 -> "madness"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

               case Pay4PowerMainAction(_) =>
                    self.numSB == 5 |=> 500 -> "last spellbook"
                    self.numSB == 4 |=> 400 -> "pre-last spellbook"
                    power < 6 |=> -200 -> "not much power"
                    power == 4 |=> -200 -> "last power"
                    self.realDoom >= 20 |=> 400 -> "the end is near"
                    self.realDoom >= 25 |=> 200 -> "the end is very near"
                    self.realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

               case Pay6PowerMainAction(_) =>
                    self.numSB == 5 |=> 500 -> "last spellbook"
                    self.numSB == 4 |=> 200 -> "pre-last spellbook"
                    power < 9 |=> -200 -> "not much power"
                    power == 6 |=> -200 -> "last power"
                    self.realDoom >= 20 |=> 400 -> "the end is near"
                    self.realDoom >= 25 |=> 200 -> "the end is very near"
                    self.realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

                case ThousandFormsMainAction(_) =>
                    power == 1 |=> 2000 -> "spend last power on 1000F"


                case _ =>
            }

            if (self == BG) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case BloodSacrifice =>
                        self.has(ShubNiggurath) |=> 1000 -> "must have if sn"
                    case ThousandYoung =>
                        self.has(ShubNiggurath) |=> 900 -> "very good if sn"
                        self.pool.monsterly.num > 5 |=> 200 -> "summoning to do"
                    case Frenzy =>
                        self.count(Acolyte) == 6  |=> 300 -> "frenzy 6"
                        self.count(Acolyte) == 5  |=> 200 -> "frenzy 5"
                        self.count(Acolyte) == 4  |=> 100 -> "frenzy 4"
                    case Necrophagy =>
                        self.count(Ghoul) == 2  |=> 550 -> "necrophagy 2"
                        self.count(Ghoul) == 1  |=> 400 -> "necrophagy 1"
                    case Ghroth =>
                        self.count(Fungi) == 4 |=> 450 -> "ghroth 4"
                        self.count(Fungi) == 3 |=> 350 -> "ghroth 3"
                        self.count(Fungi) == 2 |=> 250 -> "ghroth 2"
                        self.count(Fungi) == 1 |=> 150 -> "ghroth 1"
                    case RedSign =>
                        self.count(DarkYoung) == 3 |=> 800 -> "red sign 3"
                        self.count(DarkYoung) == 2 |=> 700 -> "red sign 2"
                        self.count(DarkYoung) == 1 |=> 600 -> "red sign 1"
                        self.count(DarkYoung) == 0 |=> 500 -> "red sign 1"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

                case EndTurnAction(_) =>
                    self.oncePerRound.has(Fertility) || self.acted |=> 8000 -> "don't oversummon"

                case NextPlayerAction(_) =>
                    self.oncePerRound.has(Fertility) || self.acted && self.cultists.%(_.capturable).none |=> 8000 -> "don't oversummon"

                case BloodSacrificeAction(_, r, u) =>
                    instantDeathNow |=> 10000 -> "instant death now"
                    self.realDoom >= others./(_.aprxDoom).max + 10 && !self.allSB |=> -500 -> "in the lead already, and not all spellbooks yet"
                    u.gateKeeper |=> -5000 -> "don't sacrifice gatekeeper"
                    u.capturable |=> 1000 -> "sacrifice capturable"
                    u.vulnerable |=> 500 -> "sacrifice vulnerable"
                    self.cultists.num > 4 |=> 200 -> "many cultists"
                    u.friends.%(_.canControlGate).num == 8 |=> 80 -> "many fcultists 8"
                    u.friends.%(_.canControlGate).num == 7 |=> 70 -> "many fcultists 7"
                    u.friends.%(_.canControlGate).num == 6 |=> 60 -> "many fcultists 6"
                    u.friends.%(_.canControlGate).num == 5 |=> 50 -> "many fcultists 5"
                    u.friends.%(_.canControlGate).num == 4 |=> 40 -> "many fcultists 4"
                    u.friends.%(_.canControlGate).num == 3 |=> 30 -> "many fcultists 3"
                    u.friends.%(_.canControlGate).num == 2 |=> 20 -> "many fcultists 2"
                    u.friends.%(_.canControlGate).num == 1 |=> 10 -> "many fcultists 1"
                    true |=> 200 -> "sacrifice is good"

                case GhrothMainAction(_) =>
                    self.all(Fungi)./(_.region).distinct.num match {
                        case 0 => true |=> -1000 -> "0 fungi"
                        case 1 => true |=> -500 -> "1 fungi"
                        case 2 => true |=> -200 -> "2 fungi"
                        case 3 => true |=> 100 -> "3 fungi"
                        case 4 => true |=> 450 -> "4 fungi"
                    }

                case AvatarAction(_, o, r, f) if f.neutral =>
                    true |=> -100000 -> "don't avatar uncontrolled filth (for now)"

                case AvatarAction(_, o, r, f) =>
                    val shub = self.all(ShubNiggurath).only

                    r.enemyGate && f == r.owner && f.at(r).monsterly.num == 0 && (shub.friends.monsterly.any || !shub.region.ownGate) |=> 600 -> "get gate no monster"
                    r.enemyGate && f == r.owner && f.at(r).monsterly.num == 1 && (shub.friends.monsterly.any || !shub.region.ownGate) |=> 600 -> "get gate monster"

                    r.foes.goos.any |=> -500 -> "enemy gate and no goos"
                    shub.region.foes.goos.any |=> 500 -> "flee from goo"

                    game.cathedrals.has(shub.region) && AN.has(UnholyGround) && AN.strength(AN.at(shub.region), self) > 0 && (AN.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                    game.cathedrals.has(r) && AN.has(UnholyGround) && AN.strength(AN.at(r), self) > 0 && (AN.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

                case MoveAction(_, u, o, d, cost) =>
                    self.realDoom > 28 && d.allies.none && o.allies.any && self.needs(Spread8) && self.allInPlay.num >= 8 && areas.%(self.present).num < 8 |=> 3000 -> "final spread"
                    self.realDoom > 27 && self.needs(SpreadSocial) && d.allies.none && d.foes./(_.faction).%(f => areas.%(r => r.allies.any && f.at(r).any).none).any |=> 1000 -> "final social spread"

                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(Spread4) && areas.%(r => r.allies.any).num == 3 |=> 100 -> "get spread 4"
                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(Spread6) && areas.%(r => r.allies.any).num == 5 |=> 100 -> "get spread 6"
                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(Spread8) && areas.%(r => r.allies.any).num == 7 |=> 100 -> "get spread 8"
                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(SpreadSocial) && others.%(f => areas.%(r => r.allies.any && f.at(r).any).none).%(f => f.at(d).none).none |=> 100 -> "get social spread"

                case EliminateTwoCultistsAction(_, a, b) =>
                    a.gateKeeper |=> -1000 -> "don't eliminate gatekeeper a"
                    b.gateKeeper |=> -1000 -> "don't eliminate gatekeeper b"
                    (a.region == b.region) && a.ownGate && a.region.controllers.num == 2 |=> -1000 -> "don't eliminate two gatekeepers"
                    a.capturable |=> 200 -> "avoid capture a"
                    b.capturable |=> 200 -> "avoid capture b"
                    a.ownGate && a.region.controllers.num < 3 |=> -100 -> "gate a"
                    a.ownGate && b.region.controllers.num < 3 |=> -100 -> "gate b"
                    (a == b) |=> -50 -> "a == b"
                    power < self.pool.cultists.num + 2 |=> -200 -> "need power to re-recruit all cultists spellbook"
                    true |=> 400 -> "spell is good"
                    self.numSB == 5 |=> 500 -> "final spellbook"
                    self.numSB == 5 && self.realDoom >= 30 |=> 3500 -> "final spellbook"
                    self.cultists.num == 6 |=> 100 -> "too many cultists"

                case AwakenEliminateTwoCultistsAction(_, _, _, a, b) =>
                    a.gateKeeper |=> -1000 -> "don't eliminate gatekeeper a"
                    b.gateKeeper |=> -1000 -> "don't eliminate gatekeeper b"
                    (a.region == b.region) && a.ownGate && a.region.controllers.num == 2 |=> -1000 -> "don't eliminate two gatekeepers"
                    a.capturable |=> 200 -> "avoid capture a"
                    b.capturable |=> 200 -> "avoid capture b"
                    a.ownGate && a.region.controllers.num < 3 |=> -100 -> "gate a"
                    a.ownGate && b.region.controllers.num < 3 |=> -100 -> "gate b"
                    (a == b) |=> -50 -> "a == b"
                    power < 8 + self.pool.cultists.num + 2 |=> -200 -> "need power to re-recruit all cultists awaken"
                    true |=> 400 -> "awaken shub is good"
                    self.needs(AwakenShubNiggurath) && self.doom > 10 |=> 1100 -> "awaken shub is very good"
                    self.cultists.num == 6 |=> 100 -> "too many cultists"

                case _ =>
            }

            if (self == YS) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case ScreamingDead =>
                        self.has(KingInYellow) |=> 1000 -> "must have if kiy"
                    case ThirdEye =>
                        true |=> 100 -> "3rd eye good"
                        self.has(Hastur) |=> 800 -> "very good if hastur"
                    case HWINTBN =>
                        self.has(Hastur) |=> 800 -> "good if hastur"
                    case Passion =>
                        true |=> 1200 -> "passion super"
                    case Zingaya =>
                        self.pool(Undead).any && areas.%(r => self.at(r)(Undead).any && r.foes.cultists.any).any |=> 700 -> "make more undead"
                    case Shriek =>
                        self.count(Byakhee) > 1 |=> 500 -> "byakhee fly"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

                case DesecrateMainAction(_, _, te) =>
                    val kiy = self.all(KingInYellow).single.get

                    kiy.friends.num >= 5 |=> 900 -> "desecrate sure"
                    kiy.friends.num == 4 |=> 800 -> "desecrate 5"
                    kiy.friends.num == 3 |=> 700 -> "desecrate 4"
                    kiy.friends.num == 2 |=> 600 -> "desecrate 3"
                    kiy.friends.num == 1 |=> 500 -> "desecrate 2"
                    kiy.friends.num == 0 |=> 400 -> "desecrate 1"

                    kiy.region.freeGate && self.pool.cultists.any |=> 1000 -> "desecrate free gate with cultists in pool"

                    te |=> 300 -> "desecrate++"

                case DesecratePlaceAction(_, r, uc) =>
                    r.freeGate && uc == Acolyte |=> 1000 -> "place acolyte on a free gate"
                    r.allies(Byakhee).none && uc == Byakhee |=> 400 -> "new byakhee good"
                    uc == Undead |=> 200 -> "undead good default"
                    uc == Acolyte |=> 100 -> "cultist ok"

                case Provide3DoomAction(_, f) =>
                    self.power == 3 |=> 550 -> "time to give 3 doom"

                case HWINTBNAction(_, _, r) =>
                    r.enemyGate && others.%(f => f.strength(f.at(r), self) > 0 && !(r.owner == f && f.at(r).num == 1 && f.at(r).cultists.num == 1)).none |=> 1000 -> "get gate without retribution"

                case ScreamingDeadAction(_, o, r) =>
                    val kiy = self.all(KingInYellow).single.get

                    game.desecrated.has(r) |=> -500 -> "already desecrated dest"
                    game.desecrated.has(o).not |=> -500 -> "not desecrated origin"
                    self.all(KingInYellow).single.get.friends(Undead).num >= 5 |=> 800 -> "screaming sure"
                    self.all(KingInYellow).single.get.friends(Undead).num == 4 |=> 600 -> "screaming 5"
                    self.all(KingInYellow).single.get.friends(Undead).num == 3 |=> 500 -> "screaming 4"
                    self.all(KingInYellow).single.get.friends(Undead).num == 2 |=> 400 -> "screaming 3"
                    self.all(KingInYellow).single.get.friends(Undead).num == 1 |=> 300 -> "screaming 2"
                    self.all(KingInYellow).single.get.friends(Undead).num == 0 |=> 200 -> "screaming 1"

                    r.glyph == GlyphAA && self.needs(DesecrateAA) |=> 20 -> "king scream to aa"
                    r.glyph == GlyphWW && self.needs(DesecrateWW) |=> 20 -> "king scream to ww"
                    r.glyph == GlyphOO && self.needs(DesecrateOO) |=> 20 -> "king scream to oo"

                    self.needs(DesecrateAA) && r.connected.%(_.glyph == GlyphAA).any |=> 10 -> "king scream nearer to aa"
                    self.needs(DesecrateWW) && r.connected.%(_.glyph == GlyphWW).any |=> 10 -> "king scream nearer to ww"
                    self.needs(DesecrateOO) && r.connected.%(_.glyph == GlyphOO).any |=> 10 -> "king scream nearer to oo"

                    self.has(Hastur) && self.has(ThirdEye) |=> 200 -> "desecrate++"

                    kiy.enemies.goos.any |=> 500 -> "scream from goo"

                case ScreamingDeadFollowAction(_, o, r, uc) =>
                    val u = self.at(o, uc).head
                    true |=> 100 -> "scream along"
                    u.defender |=> -200 -> "don't scream defender"
                    game.desecrated.has(u.region) && u.friends.none |=> -200 -> "don't scream fiester"

                case _ =>
            }

            a match {
                case PlayDirectionAction(_, order) =>
                    order(1).power < order.last.power |=> 100 -> "low power first"

                case RitualAction(_, cost, _) =>
                    instantDeathNow |=> 10000 -> "instant death now"
                    instantDeathNext && self.allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                    instantDeathNext && !self.allSB && others.%(_.allSB).any |=> -1000 -> "don't ritual if ID next and not all SB"
                    instantDeathNext && !self.allSB && others.all(!_.allSB) && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                    self.allSB && self.realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30, and all SB"
                    !self.allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                    !self.allSB && self.doom + self.gates.num < 30 && self.realDoom <= 29 && self.realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"
                    self.numSB >= 5 && cost * 2 <= power |=> 800 -> "5 SB and less than half available power"
                    self.numSB >= 2 && aprxDoomGain / cost > 1 |=> 200 -> "very sweet deal"
                    self.numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 200 -> "sweet deal"
                    self.numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                    cost == 5 |=> 100 -> "ritual first"
                    self.pool.goos.any |=> -200 -> "not all goos in play"
                    true |=> -250 -> "don't ritual unless have reasons"

                case NeutralMonstersAction(_, _) =>
                    true |=> -100000 -> "don't obtain loyalty cards (for now)"

                case DoomDoneAction(_) =>
                    true |=> 0 -> "doom done"

                case PassAction(_) =>
                    true |=> -500 -> "wasting power bad"

                case MoveDoneAction(_) =>
                    true |=> 500 -> "move done"

                case MoveAction(_, u, o, d, cost) =>
                    true |=> 100 -> "moving is ok"
                    u.gateKeeper && (!u.capturable || u.enemies.goos.none) |=> -1000 -> "don't move gatekeeper"

                    u.monsterly && o.allies.none && d.foes.goos.any |=> 100 -> "alone vs goo"

                    u.canControlGate && !u.gateKeeper && d.freeGate && self.gates.num < self.allInPlay.%(_.canControlGate).num && d.capturers.none |=> 500 -> "ic free gate"
                    u.monsterly && !d.foes.goos.any && d.freeGate && self.gates.num < self.allInPlay.%(_.canControlGate).num && d.capturers.any && power > 1 |=> 500 -> "ic free gate"

                    d.ownGate && canSummon(u.uclass) && self.summonCost(u.uclass, d) == 1 |=> -1000 -> "why move if can summon for same"
                    d.ownGate && canSummon(u.uclass) && self.summonCost(u.uclass, d) == 2 |=> -500 -> "why move if can summon for almost same"

                    u.cultist && self.pool.cultists.any && d.allies.any && !self.allInPlay.tag(Moved).any |=> -1000 -> "why move if can recruit for same"

                    power < 3 && u.protector |=> -400 -> "don't move protector if low power"
                    power < 3 && u.defender |=> -400 -> "don't move defender if low power"

                    u.protector |=> -100 -> "don't move protector"
                    u.defender |=> -100 -> "don't move defender"

                    u.goo && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num == 1 |=> 800 -> "super-afraid lone cultist looking for goo visit"
                    u.goo && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num >= 2 |=> 790 -> "super-afraid cultists looking for goo visit"

                    u.monsterly && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num == 1 && d.foes.goos.none |=> 800 -> "afraid lone cultist looking for monster visit"
                    u.monsterly && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num >= 2 && d.foes.goos.none |=> 790 -> "afraid cultists looking for monster visit"

                    !u.cultist && d.enemyGate && d.owner.power <= 1 |=> 100 -> "enemy gate and low power owner"
                    !u.cultist && d.enemyGate && power - d.owner.power >= 3 |=> 100 -> "enemy gate and power diff"
                    !u.cultist && d.enemyGate && u.goo |=> 100 -> "enemy gate and move goo"

                    !u.cultist && o.enemyGate && o.owner.power <= 1 |=> -100 -> "enemy gate and low power owner origin"
                    !u.cultist && o.enemyGate && power - o.owner.power >= 3 |=> -100 -> "enemy gate and power diff origin"
                    !u.cultist && o.enemyGate && u.goo |=> -100 -> "enemy gate and move goo"

                    u.monsterly && d.allies.none && d.foes.cultists.%(_.vulnerable).any |=> 450 -> "vulnerable cultists"

                    u.monsterly && d.allies.none && d.enemyGate && d.owner == YS |=> -20 -> "damn passion"

                    (u.cultist || u.goo) && u.faction == GC && d.glyph == Ocean |=> 10 -> "cthulhu n cultist move to ocean"

                    u.uclass == KingInYellow && u.faction.has(ScreamingDead) |=> -1000 -> "king always scream"

                    o.allies.cultists.num == 6 && d.empty |=> 300 -> "crowded cultists 6 explore"

                    u.shield |=> -100 -> "don't move shield"
                    u.pretender |=> -100 -> "don't move pretender"
                    u.capturable |=> 150 -> "move capturable"

                    d.allies.goos.any && (d.foes.goos.any || !self.has(Emissary)) |=> 100 -> "move to shield"

                    power > 1 && u.is(Nyarlathotep) && d.foes(KingInYellow).any && d.foes(Hastur).none && YS.power == 0 |=> 1000 -> "nya likes kiy"
                    power > 1 && u.is(Nyarlathotep) && d.foes(ShubNiggurath).any && BG.power == 0 |=> 800 -> "nya likes shub"
                    power > 1 && u.is(Nyarlathotep) && d.foes(Cthulhu).any && GC.power == 0 |=> 600 -> "nya likes cthulhu"

                    u.goo |=> 30 -> "move goo"
                    u.goo && !o.gate && d.gate |=> 20 -> "move goo to gate"
                    u.monsterly && !o.gate && d.gate |=> 10 -> "move monster to gate"

                    u.goo && game.cathedrals.has(o) && AN.has(UnholyGround) && AN.strength(AN.at(o), self) > 0 && (AN.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                    u.goo && game.cathedrals.has(d) && AN.has(UnholyGround) && AN.strength(AN.at(d), self) > 0 && (AN.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

                case AttackAction(_, r, f, _) if f.neutral =>
                    true |=> -100000 -> "don't attack uncontrolled filth (for now)"

                case AttackAction(_, r, f, _) =>
                    val attackers = self.at(r)
                    val defenders = f.at(r)

                    val baseAttack = self.strength(attackers, f) +
                        (attackers(Cthulhu).any && defenders.monsterly.any).?(5).|(0) +
                        self.has(Abduct).?(3 * min(attackers(Nightgaunt).num, defenders.monsterly.num + defenders.cultists.num)).|(0)

                    val baseDefense = f.strength(defenders, self) +
                        (defenders(Cthulhu).any && attackers.monsterly.any).?(5).|(0) +
                        f.has(Abduct).?(3 * min(defenders(Nightgaunt).num, attackers.monsterly.num + attackers.cultists.num)).|(0)

                    val attack = adjustedOwnStrengthForCosmicUnity(baseAttack, attackers, defenders, opponent = f)
                    val defense = adjustedOwnStrengthForCosmicUnity(baseDefense, defenders, attackers, opponent = f)

                    self.acted || self.battled.any |=> -300 -> "unlimited battle drains power"
                    true |=> -200 -> "battle costs power"

                    attack <= defense |=> -300 -> "less attack"
                    attack > defense |=> 100 -> "more attack"
                    attack > defense * 3 + 3 |=> 300 -> "much more attack"

                    r.enemyGate && f == r.owner && others.%(_ != f).%(_.at(r).any).none |=> 200 -> "attack at enemy gate alone"

                    r.enemyGate && f == r.owner && attack > defenders.num * 2 && attackers.cultists.any |=> 600 -> "ready to take over gate"

                    r.allies.goos.none && f.at(r).goos.any && f.at(r).num <= self.strength(attackers, f) |=> 450 -> "assault goo"

                    self.needs(KillDevour1) && attack > 1 |=> 400 -> "need kill spellbook"
                    self.needs(KillDevour2) && defenders.num >= 2 && attack > 5 |=> 400 -> "need kill 2 spellbook"

                    defenders(FlyingPolyp).num == defenders.num && f.has(Invisibility) |=> -10000 -> "invisible polyps"
                    defenders(Nightgaunt).any && f.has(Abduct) && attackers.goos.none && attackers./(_.uclass.cost).sorted.take(defenders(Nightgaunt).num).sum > defenders(Nightgaunt).num |=> -10000 -> "suicide nightgaunts"

                    attackers.got(Nyarlathotep) && !defenders.got(Hastur) && defenders.goos.any && defense < attackers.num * 5 |=> 2000 -> "nya likes battle goos"

                    f.has(Abhoth) && defense == 0 && attack >= r.foes(Filth).num * 2 |=> 200 -> "get rid of filth"
                    f.has(Abhoth) && f.has(TheBrood) && defense == 0 && attack >= r.foes(Filth).num * 2 |=> 400 -> "get rid of brood filth"

                    f == AN && r.allies.goos.any && game.cathedrals.has(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                    f == AN && AN.has(Extinction) && defenders.num == 1 && defenders(Yothan).any && ((r.allies.goos.any && r.allies.num >= 3 && attack >= 6) || (r.allies.goos.none && attack >= 6)) |=> 1000 -> "attack lone extinct yothan"

                case CaptureAction(_, r, f, _) =>
                    true |=> 600 -> "capture"
                    r.enemyGate |=> 100 -> "enemy gate"
                    r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 450 -> "capture and open gate"
                    r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 400 -> "capture and nearly open gate"
                    r.enemyGate && f == r.owner && r.controllers.num == 1 && r.foes.%(_.canControlGate).num > 1 |=> -300 -> "give gate away"
                    self.needs(CaptureCultist) |=> 200 -> "spellbook good"

                case BuildGateAction(_, r) =>
                    true |=> 500 -> "building gates is good"
                    maxEnemyPower <= 1 && r.foes.none |=> 300 -> "enemies max 1p and no foes"
                    r.foes.goos.any |=> -400 -> "enemy goo present"
                    r.foes.monsterly.any |=> -100 -> "enemy monster present"
                    power == 3 && r.allies.num == 1 |=> -200 -> "lone cultist and last power"
                    self == GC && r.glyph == Ocean |=> 250 -> "cthulhu likes ocean gates"

                case RecruitAction(_, Acolyte, r) =>
                    r.freeGate && r.foes.goos.none && (r.foes.monsterly.none || r.allies.goos.any || r.allies.monsterly.any) |=> 800 -> "free gate"
                    r.foes.goos.any && r.allies.goos.none |=> -500 -> "recruit to capture"
                    r.allies.cultists.num == 1 |=> 200 -> "a cultist needs a friend"
                    r.allies.cultists.num == 2 |=> 100 -> "two cultists needs a friend"
                    self.pool.cultists.num >= power |=> 300 -> "recover lost cultists"
                    r.capturers.any |=> -1000 -> "don't recruit if can be captured"
                    true |=> 100 -> "cultist is good"

                case RecruitAction(_, HighPriest, r) =>
                    true |=> -100000 -> "inactivated"

                case SummonAction(_, uc, r) =>
                    val p = self.summonCost(uc, r)
                    true |=> 300 -> "summoning is good"

                    p == 0 && !self.oncePerRound.has(Fertility) |=> 300 -> "stalling good"
                    r.allies.got(Fungi) && self.gates.%(!_.allies.got(Fungi)).any |=> -400 -> "fungi go another gate"
                    uc == DarkYoung && self.has(RedSign) |=> 100 -> "dark young are good with red sign"
                    uc == HuntingHorror && self.has(Nyarlathotep) |=> 200 -> "hunting horrors to shield nya"

                    r.foes.goos.any && r.allies.goos.none |=> -500 -> "enemy goo"
                    r.foes.goos.any && r.allies.goos.any |=> 500 -> "enemy goo and own goo"
                    r.ownGate && r.controllers.num == 1 && r.controllers.%(_.capturable).any  |=> 250 -> "prevent losing gate"
                    r.allies.%(_.capturable).any |=> 300 -> "allies capturable"
                    r.allies.%(_.vulnerable).any |=> 100 -> "allies vulnerable"
                    r.allies.monsterly.none |=> 100 -> "no monsters"
                    r.allies.cultists.num == 1 |=> 100 -> "lone cultist"
                    r.allies.cultists.num >= 3 |=> -100 -> "many cultist"
                    p == 2 |=> -50 -> "save power 2"
                    p == 3 |=> -100 -> "save power 3"

                    r.allies.goos.any |=> 100 -> "summon to shield"
                    r.foes.cultists.%(_.vulnerable).any |=> 200 -> "summon to capture"

                case AwakenAction(_, uc, r, _) =>
                    true |=> 400 -> "yes awaken"
                    r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                    r.foes.got(Hastur) |=> -200 -> "hastur is scary"
                    r.allies.%(_.vulnerable).any |=> 150 -> "allies vulnerable"

                    uc == Cthulhu |=> 500 -> "cthulhu for es"
                    uc == Nyarlathotep && !self.oncePerTurn.has(ThousandForms) |=> 200 -> "nyarlathotep for 1000f"
                    uc == ShubNiggurath && self.pool.monsterly.num > 4 |=> 300 -> "shub niggurath for summon"
                    uc == Hastur |=> 500 -> "hastur for 3rd eye"

                    self.numSB >= 5 && uc == Cthulhu && self.needs(AwakenCthulhu) |=> 500 -> "need cthulhu"
                    self.numSB >= 5 && uc == Nyarlathotep && self.needs(AwakenNyarlathotep) |=> 500 -> "need nyarlathotep"
                    self.numSB >= 5 && uc == ShubNiggurath && self.needs(AwakenShubNiggurath) |=> 500 -> "need shub niggurath"
                    self.numSB >= 5 && uc == Hastur && self.needs(AwakenHastur) |=> 500 -> "need hastur"

                case AvatarReplacementAction(_, _, r, o, u) =>
                    u.cultist && o.capturers.%(_.power > 0).any |=> -100 -> "don't send cultist to be captured"
                    u.cultist && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                    u.monsterly && o.foes.%(_.capturable).any |=> 200 -> "send to capture"

                case RevealESAction(_, es, false, _) if self.es != es =>
                    true |=> -10000 -> "better reveal all"

                case RevealESAction(_, _, _, _) =>
                    self.allSB && self.realDoom >= 30 |=> 1000 -> "reveal and try to win"
                    self.allSB && self.realDoom < 30 && self.realDoom < self.aprxDoom && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                    !self.allSB && self.realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 800 -> "reveal so 30 broken and nobody wins"
                    true |=> -100 -> "don't reveal"
                    self.acted.not && power >= game.ritualCost |=> -1000 -> "ritual first"

                case ThousandFormsAskAction(_, _, _, _, _, _, power) =>
                    power == 0 |=> 3*3*3*3*3*3 -> "pay 0"
                    power == 1 |=> 2*3*3*3*3*3 -> "pay 1"
                    power == 2 |=> 2*2*3*3*3*3 -> "pay 2"
                    power == 3 |=> 2*2*2*3*3*3 -> "pay 3"
                    power == 4 |=> 2*2*2*2*3*3 -> "pay 4"
                    power == 5 |=> 2*2*2*2*2*3 -> "pay 5"
                    power == 6 |=> 2*2*2*2*2*2 -> "pay 6"
                    result = result.map(e => Evaluation((e.weight * Math.random()).round.toInt, e.desc))

                case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                    result = evalA(SummonAction(self, uc, r))

                case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                    result = evalA(SummonAction(self, uc, r))

                case ControlGateAction(_, r, u, _) =>
                    r.allies.%(_.onGate).foreach { c =>
                        c.uclass == u.uclass |=> -1000000 -> "remain calm"
                        c.uclass == Acolyte && u.uclass == DarkYoung |=> 1000 -> "dark young on gate"
                        u.uclass == Acolyte && c.uclass == DarkYoung |=> -1000 -> "dark young on gate"
                        c.uclass == HighPriest && u.uclass == Acolyte |=> 1000 -> "high priest not on gate"
                        u.uclass == HighPriest && c.uclass == Acolyte |=> -1000 -> "high priest not on gate"
                    }

                case AbandonGateAction(_, _, _) =>
                    true |=> -1000000 -> "never"

                case _ =>
            }

            // BATTLE
            if (game.battle.any) {
                implicit val battle = game.battle.get

                def elim(u : UnitFigure) {
                    u.uclass == Acolyte |=> 600 -> "elim acolyte"
                    u.gateKeeper |=> -1000 -> "elim gate keeper"
                    u.uclass == Acolyte && self.has(Passion) && !self.oncePerAction.has(Passion) |=> 200 -> "elim for passion"
                    u.uclass == Acolyte && self.has(Frenzy) |=> -100 -> "elim for frenzy"

                    u.uclass == DeepOne |=> 300 -> "elim do"
                    u.uclass == Shoggoth |=> 200 -> "elim sg"
                    u.uclass == Starspawn |=> 100 -> "elim ss"
                    u.uclass == Cthulhu |=> -400 -> "elim sn"

                    u.uclass == Nightgaunt |=> 300 -> "elim ng"
                    u.uclass == FlyingPolyp |=> 200 -> "elim fp"
                    u.uclass == HuntingHorror |=> 100 -> "elim hh"
                    u.uclass == Nyarlathotep |=> -1000 -> "elim sn"

                    u.is(Ghoul) |=> 1000 -> "elim ghoul"
                    u.uclass == Fungi && u.faction.has(ShubNiggurath) && u.faction.has(ThousandYoung) |=> 500 -> "elim fungi cheap"
                    u.uclass == Fungi |=> 400 -> "elim fungi"
                    u.uclass == DarkYoung |=> 100 -> "elim dy"
                    u.uclass == ShubNiggurath |=> -1000 -> "elim sn"

                    u.uclass == Undead |=> 700 -> "elim undead"
                    u.uclass == Byakhee |=> 200 -> "elim byakhee"
                    u.uclass == KingInYellow |=> -400 -> "elim kiy"
                    u.uclass == Hastur |=> -1000 -> "elim hastur"

                    if (u.faction != self)
                        result = result./(e => Evaluation(-e.weight, "neg " + e.desc))
                }

                def retreat(u : UnitFigure) {
                    u.uclass == Acolyte |=> 600 -> "retr acolyte"
                    u.gateKeeper |=> -1000 -> "retr gate keeper"

                    u.uclass == DeepOne |=> 300 -> "retr do"
                    u.uclass == Shoggoth |=> 200 -> "retr sg"
                    u.uclass == Starspawn |=> 100 -> "retr ss"
                    u.uclass == Cthulhu |=> -400 -> "retr sn"

                    u.uclass == Nightgaunt |=> 100 -> "retr ng"
                    u.uclass == FlyingPolyp |=> 200 -> "retr fp"
                    u.uclass == HuntingHorror |=> 300 -> "retr hh"
                    u.uclass == Nyarlathotep |=> -1000 -> "retr sn"

                    u.is(Ghoul) |=> 1000 -> "retr ghoul"
                    u.uclass == Fungi |=> 800 -> "retr fungi"
                    u.uclass == DarkYoung |=> 100 -> "retr dy"
                    u.uclass == ShubNiggurath |=> -1000 -> "retr sn"

                    u.uclass == Undead |=> 700 -> "retr undead"
                    u.uclass == Byakhee |=> 800 -> "retr byakhee"
                    u.uclass == KingInYellow |=> 400 -> "retr kiy"
                    u.uclass == Hastur |=> -1000 -> "retr hastur"

                    if (u.faction != self)
                        result = result./(e => Evaluation(-e.weight, "neg " + e.desc))
                }


                a match {
                    case DevourPreBattleAction(f) =>
                        true |=> 1000 -> "always devour"

                    case DevourAction(_, u) =>
                        elim(u)

                    case AbsorbeeAction(_, _, t) =>
                        t.uclass == DeepOne |=> 300 -> "absorb deep one"
                        true |=> -100 -> "don't absorb"

                    case AbductPreBattleAction(f) =>
                        true |=> 100 -> "abduct"
                        f.opponent.strength(f.opponent.units, f) == 0 |=> -200 -> "no abduct if attack zero"

                    case AbductAction(_, _, u) =>
                        elim(u)

                    case InvisibilityAction(_, _, u) =>
                        elim(u)

                    case DemandSacrificeKillsArePainsAction(_) =>
                        self.str < self.opponent.str |=> 1000 -> "less str"
                        self.str > self.opponent.str |=> -1000 -> "more str"

                    case HarbingerPowerAction(_, _, n) =>
                        n == 2 |=> 200 -> "harb 2 power"
                        n == 3 |=> 300 -> "harb 3 power"
                        n == 4 |=> 400 -> "harb 4 power"
                        n == 5 |=> 500 -> "harb 5 power"

                    case HarbingerESAction(_, _, _) =>
                        self.allSB |=> 600 -> "es all spellbooks"
                        self.realDoom > 5 + others./(_.aprxDoom).max |=> -300 -> "doom lead"

                    case NecrophagyAction(_, u, r) =>
                        battle.attacker != self |=> 100 -> "necrophagy is good"
                        battle.attacker == self |=> -100 -> "necrophagy is bad"
                        u.prevents |=> -1000 -> "prevents capture"
                        u.region.freeGate && u.friends.none |=> -1000 -> "free gate"

                    case AssignKillAction(_, _, _, u) =>
                        elim(u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(u)

                    case EliminateNoWayAction(_, u) =>
                        elim(u)

                    case RetreatOrderAction(_, a, b) =>
                        a == self |=> 1000 -> "retreat self first"
                        a.aprxDoom < b.aprxDoom |=> 100 -> "retreat less doom first"

                    case RetreatUnitAction(_, u, r) =>
                        u.foe && u.cultist && r.allies.monsterly.any |=> 1000 -> "send cultist to be captured by monsters"
                        u.foe && u.cultist && r.allies.goos.any |=> 1000 -> "send cultist to be captured by goos"
                        u.foe && u.cultist && r.foes.none |=> 200 -> "send cultist where no foes"
                        u.foe && u.cultist && u.faction.at(r).%(!_.cultist).none |=> 200 -> "send where no friends"
                        u.foe && u.cultist && u.faction.at(r).%(_.monsterly).any |=> -1000 -> "dont send where friends"
                        u.foe && u.cultist && u.faction.at(r).%(_.goo).any |=> -2000 -> "dont send to goo"
                        u.foe && u.cultist && r.freeGate |=> -1000 -> "dont send cultist to empty gate"
                        u.foe && u.cultist && r.ownGate |=> -100 -> "dont send cultist to own gate"
                        u.foe && u.cultist && r.gate |=> -100 -> "dont send cultist to gate"
                        u.foe && u.cultist && r.empty |=> 50 -> "send cultist to empty area"

                        u.ally && u.cultist && r.allies.monsterly.any |=> 1000 -> "send cultist to be protected by monsters"
                        u.ally && u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                        u.ally && u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                        u.ally && u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                        u.ally && u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                        u.ally && u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                        u.ally && u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                        u.foe && u.uclass == Fungi && u.faction.at(r)(Fungi).any |=> 1000 -> "retreat fungi to fungi"
                        u.ally && u.uclass == Fungi && u.faction.at(r)(Fungi).any |=> -800 -> "dont retreat fungi to fungi"

                        u.foe && !u.cultist && r.allies.any |=> -2000 -> "dont send non cultists to self"
                        u.foe && !u.cultist && r.ownGate |=> -2000 -> "dont send non cultists to own gate"
                        u.foe && !u.cultist && r.enemyGate && r.owner == u.faction |=> -2000 -> "dont send non cultists to their gate"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 6 |=> 600 -> "send non cultists to enemy gate 6"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 5 |=> 500 -> "send non cultists to enemy gate 5"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 4 |=> 400 -> "send non cultists to enemy gate 4"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 3 |=> 300 -> "send non cultists to enemy gate 3"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 2 |=> 200 -> "send non cultists to enemy gate 2"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 1 |=> 100 -> "send non cultists to enemy gate 1"
                        u.foe && !u.cultist && r.freeGate |=> -100 -> "dont send non cultists to free gate"
                        u.foe && !u.cultist && r.empty |=> 200 -> "send non cultists to empty"

                        u.ally && u.monsterly && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                        u.ally && u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                        u.ally && u.monsterly && r.foes.%(_.vulnerable).any && !r.foes.goos.any |=> 1000 -> "send monster to capture"
                        u.ally && u.goo && r.foes.%(_.vulnerable).any |=> 1000 -> "send goo to capture"

                        u.ally && u.monsterly && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                        u.ally && u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                        u.ally && u.monsterly && r.ownGate |=> 400 -> "send monster to own gate"
                        u.ally && u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                        u.ally && u.monsterly && r.freeGate |=> 300 -> "send monster to free gate"
                        u.ally && u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                        u.ally && u.monsterly && r.enemyGate |=> 300 -> "send monster to enemy gate"
                        u.ally && u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                    case _ =>

                }
            }

            result
        }

        actions./{ a => ActionEval(a, evalA(a)) }
    }
}
