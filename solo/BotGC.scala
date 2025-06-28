package cws

import hrf.colmat._

object BotGC extends BotX(g => new GameEvaluationGC(g))

class GameEvaluationGC(game : Game) extends GameEvaluation(game, GC) {
    def eval(a : Action) : List[Evaluation] = eval(a, 0)

    def eval(a : Action, extra : Int) : List[Evaluation] = {
        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val power = max(self.power + extra, 0)

        def cthulhu = self.player.units.%(_.uclass == Cthulhu).single.get

        def needKill = need(KillDevour1) || need(KillDevour2)

        def ofinale(f : Faction) = f.allSB && (3 * f.doom + 3 * f.gates.num + 5 * (f.es + (f match {
            case YS =>
                var p = f.power

                if (f.has(KingInYellow))
                    p += 4
                if (f.has(Hastur))
                    p += 10

                if (p < 4)
                    0
                else
                if (p < 14)
                    1
                else
                    2 + (p - 14) / 2

            case BG =>
                var p = f.power

                if (f.has(ShubNiggurath))
                    p += 8

                if (p < 8)
                    0
                else
                    2

            case CC =>
                var p = f.power

                if (f.has(Nyarlathotep))
                    p += 10

                if (p < 10)
                    0
                else
                    1 + min(p - 10, game.factions.%(_ != f)./(_.all.goos.num).sum * 2)

            case SL =>
                var p = f.power

                if (f.has(Tsathoggua))
                    p += 8

                if (p < 8)
                    0
                else
                if (p < 14)
                    1
                else
                if (p < 18)
                    2
                else
                    3

            case WW =>
                var p = f.power

                if (f.has(RhanTegoth))
                    p += 6

                if (f.has(Ithaqua))
                    p += 6

                val e = f.needs(AnytimeGainElderSigns).?(3).|(0)

                if (p < 6)
                    0 + e
                else
                if (p < 12)
                    1 + e
                else
                    2 + e

            case OW =>
                1

            case AN =>
                1
        }))) >= 30 * 3

        def finale(extra : Int) = allSB && game.gates.contains(game.starting(self)) && ((power - extra + 2) / 6 match {
            case 0 => self.realDoom >= 30
            case 1 => self.realDoom >= 28
            case 2 => self.realDoom >= 26
            case n => self.realDoom >= 30 - (1 + n * 5) / 3
        })

        def gateNeedsMonster(r : Region) = r.ownGate && r.allies.goos.none && r.allies.monsters.none && r.foes.goos.active.none && (
            r.foes.monsters.active.any
        )

        a match {
            case FirstPlayerAction(_, f) =>
                f == self && game.board.regions.%(_.capturers.any).%(_.allies.cultists.any).any |=> 100 -> "play first prevent capture"
                f == self && game.board.regions.%(_.allies.goos.any).%(_.foes.goos.any).any |=> 100 -> "play first goos together"
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

                f == CC |=> 10 -> "cc first"

            case PlayDirectionAction(_, order) =>
                order(1) == CC |=> 100 -> "cc second"
                order(2) == CC |=> 50 -> "cc third"

            case SpellbookAction(_, sb, _) => sb match {
                case Devolve =>
                    true |=> 1000 -> "1000"
                case Submerge =>
                    true |=> 800 -> "800"
                case Regenerate =>
                    units(Starspawn).any |=> 700 -> "700"
                case Absorb =>
                    units(Shoggoth).any |=> 600 -> "600"
                case YhaNthlei =>
                    true |=> 400 -> "400"
                case Dreams =>
                    true |=> 200 -> "200"
                case _ =>
                    true |=> -1000 -> "unknown"
            }

            case RitualAction(_, cost, _) =>
                instantDeathNow |=> 10000 -> "instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "dont ritual if ID next and not all SB"
                instantDeathNext && !allSB && others.all(!_.allSB) && realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30, and all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom <= 29 && realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"

                numSB >= 5 && cost * 2 <= power && self.gates.num >= 3 |=> 800 -> "5 SB and less than half available power"
                numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "very sweet deal"
                numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                cost == 5 |=> 100 -> "ritual first"
                self.pool.goos.any |=> -200 -> "not all goos in play"
                true |=> -250 -> "dont ritual unless have reasons"

            case LoyaltyCardAction(_, _, _) =>
                true |=> -10000 -> "don't obtain loyalty cards (for now)"

            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"
                active.none |=> 5000000 -> "move done"

            case MoveAction(_, Acolyte, o, d) =>
                active.none && o.ownGate && o.allies.cultists.num == 1 |=> - 200000 -> "gatekeeper"
                active.none && d.freeGate |=> (2 * 100000 / 1) -> "safe move and get gate"
                active.none && d.noGate && power > 3 |=> (2 * 100000 / 4) -> "safe move and build gate"

                o.ownGate && o.allies.cultists.num == 1 |=> -500 -> "gatekeeper"
                (power > 4 || (power > 3 && others.%(_.power > 1).none)) && d.allies.cultists.none && d.ocean && d.noGate && d.foes.none |=> 400 -> "move cultist to empty ocean"
                (power > 4 || (power > 3 && others.%(_.power > 1).none)) && o.ocean && o.noGate && o.foes.none |=> -450 -> "already in empty ocean"
                need(OceanGates) && d.ocean && d.noGate && d.foes.none && d.allies.cultists.none && power > 3 && self.gates.%(g => g.allies.goos.none && g.allies.monsters.none).none |=> 440 -> "private sea"
                d.allies.goos.any |=> 30 -> "goo will protect"
                d.near.%(_.foes.active.any).none |=> 10 -> "no near foes"

            case MoveAction(_, uc, o, d) if uc.utype == Monster =>
                // Works best in early game when it's "safe" to leave cultists along
                wwLoneCultistPolarGate(1) && (WW.exists.?(d == game.board.starting(WW).but(game.starting(WW)).head)|(false)) |=> 1500 -> "move monster to block ww getting 2nd gate at opp pole"

            case MoveAction(_, Cthulhu, o, d) =>
                finale(2) && !game.battled.contains(d) |=> others./(f => f.strength(game, f.at(d), self)).max * 100000 -> "finale move attack"
                power > 1 && others.%(ofinale).%(f => f != CC && f.at(d).goos.any && (power > 5 || f.strength(game, f.at(d), self) < 5)).any |=> 550000 -> "others finale goo attack"
                power > 1 && others.%(ofinale).%(f => f.gates.contains(d)).any |=> 500000 -> "others finale gate attack"

                val hasturThreat = YS.has(Hastur) && YS.player.goo(Hastur).region.allies.cultists.any
                val kiyThreat = YS.has(KingInYellow) && YS.player.goo(KingInYellow).region.allies.cultists.any

                active.none && power > 1 && d.enemyGate && (d.owner != YS || (!hasturThreat && !kiyThreat)) && d.controllers.num == 1 && d.of(d.owner).goos.none |=> (5 * 100000 / 3) -> "go get a gate 5/3"
                active.none && power > 2 && d.enemyGate && (d.owner != YS || (!hasturThreat && !kiyThreat)) && d.controllers.num == 2 && d.of(d.owner).goos.none |=> (5 * 100000 / 4) -> "go get a gate 5/4"

                (allSB || YS.power == 0) && YS.at(d, KingInYellow).any && YS.at(d, Hastur).none && YS.at(d).monsters.num < 2 |=> 1200 -> "kill kiy"

                numSB >= 5 && d.foes.goos.none && d.freeGate && d.foes.none && d.ocean && self.at(GC.deep).cultists.any |=> 1150 -> "empty ocean gate"

                d.enemyGate && d.owner.has(Passion) && d.owner.power == 0 && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any |=> -1200 -> "dont go capture if passion allows reverse capture"
                d.enemyGate && d.controllers.num + 1 > power |=> -1200 -> "wont open the gate"

                d.ocean && numSB >= 5 && d.foes.goos.none && d.ownGate && d.foes.cultists.num > 1 |=> 1100 -> "cultvisit"
                d.ocean && numSB >= 5 && d.foes.goos.none && d.enemyGate && d.foes.cultists.num > 1 |=> 1090 -> "cultvisit"
                d.ocean && numSB >= 5 && d.foes.goos.none && d.freeGate && d.foes.cultists.num > 1 |=> 1080 -> "cultvisit"
                d.ocean && numSB >= 5 && d.foes.goos.none && d.ownGate && d.foes.cultists.num == 1 |=> 1070 -> "cultvisit"
                d.ocean && numSB >= 5 && d.foes.goos.none && d.enemyGate && d.foes.cultists.num == 1 |=> 1060 -> "cultvisit"
                d.ocean && numSB >= 5 && d.foes.goos.none && d.freeGate && d.foes.cultists.num == 1 |=> 1050 -> "cultvisit"

                d.ocean.not && numSB >= 5 && d.foes.goos.none && d.ownGate && d.foes.cultists.num > 1 |=> 1050 -> "cultvisit"
                d.ocean.not && numSB >= 5 && d.foes.goos.none && d.enemyGate && d.foes.cultists.num > 1 |=> 1040 -> "cultvisit"
                d.ocean.not && numSB >= 5 && d.foes.goos.none && d.freeGate && d.foes.cultists.num > 1 |=> 1030 -> "cultvisit"
                d.ocean.not && numSB >= 5 && d.foes.goos.none && d.ownGate && d.foes.cultists.num == 1 |=> 1020 -> "cultvisit"
                d.ocean.not && numSB >= 5 && d.foes.goos.none && d.enemyGate && d.foes.cultists.num == 1 |=> 1010 -> "cultvisit"
                d.ocean.not && numSB >= 5 && d.foes.goos.none && d.freeGate && d.foes.cultists.num == 1 |=> 1000 -> "cultvisit"

                d.ocean && o.ocean.not |=> 450 -> "ocean"
                d.gate && !o.gate |=> 400 -> "gate"
                numSB >= 5 && d.ownGate && d.foes.goos.active.any |=> 11000 -> "goos at my gate"

                needKill && others.%(f => f.at(d).monsters.num + f.at(d).cultists.num > 2 && f.gates.contains(d)).any |=> 9000 -> "not all sb"
                needKill && others.%(f => f.at(d).monsters.num + f.at(d).cultists.num > 3 && f.gates.contains(d)).any |=> 9900 -> "not all sb"
                needKill && others.%(f => f.at(d).monsters.num + f.at(d).cultists.num > 4 && f.gates.contains(d)).any |=> 9990 -> "not all sb"

                needKill && d.enemyGate && d.owner == CC |=> 1000 -> "beat cc"

                power == 1 && others.%(_.power > 1).any && numSB >= 5 && !d.ownGate && self.gates.any |=> -20000 -> "dont end not on own gate"
                power == 1 && others.%(_.power > 1).any && d.ownGate && !o.ownGate |=> 5000 -> "end on own gate"
                d.freeGate |=> 50 -> "free gate"
                d.enemyGate |=> 40 -> "enemy gate"
                d.ownGate |=> 30 -> "own gate"

                o.ocean.not && game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                o.ocean && !have(Submerge) && game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.player.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val enemyStr = f.strength(game, foes, self)
                val ownStr = self.strength(game, allies, f)

                allies.goos.any && foes.goos.none && f.gates.contains(r) && foes.monsters.any && ofinale(f) && !game.acted && !game.battled.any  |=> 600000 -> "others finale gate attack"
                allies.goos.any && foes.goos.any && f != CC && ofinale(f) && (power > 4 || f.strength(game, f.at(r), self) < 5 + allies.num * 2) |=> 700000 -> "others finale goo attack"
                allies.goos.any && finale(1) && enemyStr > 6 |=> 1000000 -> "finale attack >6"
                allies.goos.any && finale(1) && enemyStr > 11 |=> 2000000 -> "finale attack >11"

                game.acted && foes.goos.none |=> -100000 -> "unlimited battle only vs goos"

                ownStr >= 6 && (foes.goos.any || f.gates.contains(r)) |=> 250 -> "pound"

                power > 4 + game.tax(r, self) && allSB && allies.goos.any && foes.goos.any && (foes.monsters.num + foes.cultists.num <= 1) |=> 11000 -> "heavy pound"

                ownStr >= 6 && need(KillDevour1) |=> 12000 -> "pound"
                allies.goos.any && need(KillDevour2) && foes.num > 1 |=> 12300 -> "pound"
                allies.goos.any && need(KillDevour1) && need(KillDevour2) && foes.num > 2 |=> 12500 -> "pound"

                allies.goos.any && enemyStr < allies.num && (foes.goos.any || (r.ocean.not && foes.num > 1 && enemyStr > 0)) |=> 12000 -> "battle"
                allies.goos.any && power > 5 && enemyStr <= allies.num * 3 && (foes.goos.any || (r.ocean.not && foes.num > 1 && enemyStr > 0)) |=> 11500 -> "battle with possible respawn"

                allies.goos.any && foes.actualMonsters.any && foes./(_.uclass.cost).min >= 2 |=> 1000 -> "eat 2p monster"
                allies.goos.any && foes.actualMonsters.any && foes./(_.uclass.cost).min >= 3 |=> 11000 -> "eat 3p monster"
                allies.goos.any && foes.actualMonsters.any && foes./(_.uclass.cost).sum >= 2 && ofinale(f) |=> 1500 -> "eat 2p monster in finale"
                allies.goos.any && foes.actualMonsters.any && foes./(_.uclass.cost).sum >= 3 && ofinale(f) |=> 550000 -> "eat 3p monster in finale"

                r.ownGate && allies.goos.any && allies.monsters.none && foes.num <= 2 && f.active && active.%(_ != f).%(_.at(r).any).none |=> 1100 -> "clean up gate"

                allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                // Ok to risk Cthulhu with this, I think. Other factions should make sure any own goo has some shield:
                f == AN && AN.has(Extinction) && foes.monsters.num == 1 && foes(Yothan).any && ownStr >= 6 |=> 1000 -> "attack lone extinct yothan"

            case CaptureAction(_, r, f) =>
                val safe = active.none
                safe && !r.gateOf(f) |=> (1 * 100000 / 1) -> "safe capture"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 1 && power > 0                    |=> (2 * 100000 / 1) -> "safe capture and open gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 2 && power > 1 && !f.has(Passion) |=> (3 * 100000 / 2) -> "safe capture and half gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 3 && power > 2 && !f.has(Passion) |=> (4 * 100000 / 3) -> "safe capture and third of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 4 && power > 3 && !f.has(Passion) |=> (5 * 100000 / 4) -> "safe capture and fourth of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 5 && power > 4 && !f.has(Passion) |=> (6 * 100000 / 5) -> "safe capture and fifth of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 6 && power > 5 && !f.has(Passion) |=> (7 * 100000 / 6) -> "safe capture and sixth of gate"

                others./(_.aprxDoom).max > 15 && (f.aprxDoom + 9 < others./(_.aprxDoom).max) |=> -1650 -> "bash leader instead"
                others./(_.aprxDoom).max > 23 && (f.aprxDoom + 5 < others./(_.aprxDoom).max) |=> -1650 -> "bash leader instead"

                ofinale(f) && f.gates.contains(r) && (!f.has(Passion) || f.at(r).cultists.num == 1) |=> 600000 -> "others finale at gate capture"

                r.enemyGate && f == r.owner && r.controllers.num == 1 && r.allies.cultists.none && active.but(f).%(_.at(r).%(_.canControlGate).any).%(_.at(r).goos.any).any |=> -700 -> "give gate away"
                numSB >= 5 |=> 16000 -> "capture with all sb"

                !f.has(Passion) |=> 1600 -> "capture"
                f.has(Passion) && f.power == 0 && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any |=> -1500000 -> "dont capture if passion allows reverse capture"
                f.has(Passion) && (f.power == 0 || !YS.has(Hastur) || !YS.has(ThirdEye)) |=> 1100 -> "capture with passion safe"
                f.has(Passion) |=> 850 -> "capture with passion"
                r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 450 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 400 -> "capture and nearly open gate"
                r.enemyGate |=> 100 -> "enemy gate"

            case BuildGateAction(_, r) =>
                active.none && self.gates.num < 6 && (self.allSB || self.doom + self.gates.num < 29) |=> (10 * 100000 / 9) -> "safe build gate"

                WW.exists && game.board.starting(WW).contains(r) && !(need(OceanGates) && r.ocean && WW.has(Ithaqua)) |=> -10000000 -> "starting ww"

                r.ocean && need(OceanGates) |=> 1100 -> "need ocean gates"
                r.ocean && need(AwakenCthulhu) && power < 10 |=> 1100 -> "cant awaken build gates"
                YS.has(Hastur) && YS.power > 1 |=> -1000 -> "hastur in play"
                YS.has(KingInYellow) && YS.power > 1 && game.board.connected(YS.player.goo(KingInYellow).region).contains(r) |=> -1000 -> "kiy is near"
                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -800 -> "shub in play and lone cultist"

                (power >= 3 + maxEnemyPower || self.gates.num <= 1 || (r.ocean && need(OceanGates))) && r.capturers.none |=> 500 -> "building gates is good"

            case RecruitAction(_, Acolyte, r) =>
                active.none && r.freeGate |=> (3 * 100000 / 1) -> "safe recruit and get gate"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && r.allies.cultists.none |=> (1 * 100000 / 1) -> "safe recruit"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && r.allies.cultists.any |=> (8 * 100000 / 9) -> "safe recruit"
                active.none && power > 1 && r.near.%(_.freeGate).any |=> (3 * 100000 / 2) -> "safe recruit to move to gate"
                active.none && r.ownGate && r.allies.cultists.num >= 1 |=> (8 * 100000 / 9) -> "safe recruit"
                active.none && r.ownGate && r.allies.cultists.num == 1 |=> (7 * 100000 / 8) -> "safe recruit"

                r.capturers.%(_.power > 0).any |=> -2000 -> "dont recruit to be captured"
                r.freeGate && r.allies.goos.any && (r.allies.monsters.any || power == 1 || power >= others./(_.power).max || r.foes.none) |=> 17000 -> "free gate"
                r.freeGate && r.foes.goos.none  && (r.allies.monsters.any || power == 1 || power >= others./(_.power).max || r.foes.none) |=> 17000 -> "free gate"

                power > 4 && r.freeGate && active.none && others.%(ofinale).any |=> 10000000 -> "gate is a gate"

                r.freeGate |=> 330 -> "free gate"
                self.pool.cultists.num >= power |=> 300 -> "recover lost cultists"
                r.ownGate && others.all(_.power < power) |=> -250 -> "dont recruit if max power"
                r.ownGate && r.allies.goos.any |=> 200 -> "a cultist needs a big friend"

                need(OceanGates) && r.ocean && r.noGate && r.foes.none && r.allies.cultists.none |=> 490 -> "private sea"
                r.allies.goos.any |=> 30 -> "goo will protect"
                r.foes.goos.any |=> -20 -> "goo will capture"

            case SummonAction(_, DeepOne, r) =>
                // Defense against WW setting up 2nd gate at opposite pole. Only worth it if adjacent to location for new gate
                r.distanceToWWOppPole == 1 && wwLoneCultistPolarGate(2) && r.allies.monsters.none |=> 1500 ->"cheap summon to stop WW 2nd gate at opp pole"

                true |=> 20 -> "summon deep one"
                r == game.starting(self) && need(AwakenCthulhu) |=> 150 -> "deepone for cthulhu"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 300 -> "protect gate"
                r.allies.monsters.any |=> -100 -> "already have monsters"

            case SummonAction(_, Shoggoth, r) =>
                true |=> 15 -> "summon shoggoth"
                r == game.starting(self) && r.allies(Shoggoth).none && need(AwakenCthulhu) |=> 250 -> "starspawn for cthulhu"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 300 -> "protect gate"

            case SummonAction(_, Starspawn, r) =>
                true |=> 10 -> "summon starspawn"
                r == game.starting(self) && need(AwakenCthulhu) |=> 200 -> "starspawn for cthulhu"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 300 -> "protect gate"

            case AwakenAction(_, _, r, _) =>
                finale(0) |=> 1000000 -> "finale awaken"

                need(AwakenCthulhu) && others.%(f => f.power > 3 && f.all.goos.any).any |=> 20000 -> "awaken cthulhu"

                !need(OceanGates) |=> 10000 -> "awaken cthulhu"

            case DevolveAction(_, r, then) =>
                val c = r.allies(Acolyte).head
                true |=> -100 -> "not unless needed"
                c.gateKeeper |=> -500 -> "dont devolve gatekeeper"
                c.capturable && !(c.gateKeeper && power > 0 && cthulhu.region == GC.deep) |=> 1200 -> "devolve to avoid capture"
                r.allies.cultists.num > self.pool(DeepOne).num |=> -2500 -> "cant save everyone"
                self.all.cultists.%(_.capturable).num > self.pool(DeepOne).num |=> -2500 -> "cant save everyone 2"
                r.ownGate && power > 0 && cthulhu.region == GC.deep && self.all.cultists.%(_.capturable)./(_.region).distinct.num == 1 |=> -1500 -> "better unsubmerge to protect"
                power > 1 && r.ownGate && r.capturers.any && r.capturers./(_.power).sum <= 1 |=> -2000 -> "let them have it"
                then == MainCancelAction(self) && power > 1 && !game.acted && self.has(Dreams) && self.pool.cultists.none && game.board.regions.%(r => r.enemyGate && r.controllers.num == 1 && others.%(_.power > 0).%(f => f.at(r).monsters.any || f.at(r).goos.any).none).any |=> 300 -> "devolve to allow dreams"

            case AvatarReplacementAction(_, _, r, o, uc) =>
                val u = self.at(r, uc).head
                u.cultist && o.capturers.%(_.power > 0).none && o.freeGate && power > 0 |=> 300 -> "free gate"
                u.cultist && r.allies.goos.any |=> -250 -> "stay with goo"
                u.cultist && o.allies.goos.any |=> 250 -> "go to goo"
                u.gateKeeper && have(Devolve) && self.pool(DeepOne).any |=> -250 -> "keep the gate"
                u.cultist && o.capturers.%(_.power > 0).%(_ != BG).any |=> -100 -> "dont send cultist to be captured"
                u.cultist && o.capturers.none |=> 150 -> "no capturers"
                u.cultist && o.capturers.any && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                u.monster && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monster && u.friends.cultists.any && u.friends.monsters.none && r.foes.monsters./(_.faction).%(_ != BG).%(_.power > 0).any |=> -200 -> "dont sent temp defender"
                u.monster && o.allies.cultists.any && o.allies.monsters.none |=> 50 -> "protect cultist"

            case RevealESAction(_, es, false, _) if game.of(self).es != es =>
                true |=> -10000 -> "better reveal all"

            case RevealESAction(_, _, _, _) =>
                allSB && realDoom >= 30 |=> 1100 -> "reveal and try to win"
                allSB && realDoom < 30 && realDoom < self.aprxDoom && realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                !allSB && realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 1100 -> "reveal so 30 broken and nobody wins"
                true |=> -100 -> "dont reveal"
                canRitual |=> -2000 -> "ritual first"

            case ThousandFormsAskAction(_, r, offers, _, _, _, power) =>
                r < power + offers./(_.n).sum |=> -6*6*6*6*6*6 -> "dont overpay"
                power == 0 |=> (5*5*5*5*5*5 * Math.random()).round.toInt -> "pay 0"
                power == 1 |=> (2*5*5*5*5*5 * Math.random()).round.toInt -> "pay 1"
                power == 2 |=> (2*2*5*5*5*5 * Math.random()).round.toInt -> "pay 2"
                power == 3 |=> (2*2*2*5*5*5 * Math.random()).round.toInt -> "pay 3"
                power == 4 |=> (2*2*2*2*5*5 * Math.random()).round.toInt -> "pay 4"
                power == 5 |=> (2*2*2*2*2*5 * Math.random()).round.toInt -> "pay 5"
                power == 6 |=> (2*2*2*2*2*2 * Math.random()).round.toInt -> "pay 6"

            case GhrothAskAction(_, _, _, _, _, _, n) =>
                n == -1 |=> 1000 -> "refuse"
                n == 0 |=> 1000 -> "wait"

            case GhrothUnitAction(_, uc, r, f, _) =>
                val c = self.at(r, uc).head
                c.friends.cultists.none && c.region.capturers.%(!_.blind(f)).any |=> 1000 -> "will be captured anyway"
                c.gateKeeper |=> -900 -> "gate keeper"
                c.friends.cultists.none && c.region.capturers.any |=> 800 -> "can be captured one"
                c.friends.cultists.any && c.region.capturers.any |=> 700 -> "can be captured many"
                c.friends.goos.any |=> -500 -> "nya huggers"
                c.friends.cultists.any |=> 400 -> "cultist friends"
                c.friends.monsters.none |=> 300 -> "cultist friends"

            case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case DreamsAction(_, r, f) =>
                val c = f.at(r)(Acolyte).head

                active.none && c.gateKeeper |=> 170000 -> "safe dreams and get gate"
                active.none && c.gateKeeper && ofinale(f) |=> 300000 -> "finale dreams and get gate"
                active.none && r.ocean.not |=> 50 -> "not ocean"
                active.none && r.allies.monsters.any |=> 30 -> "have monsters there"

                f.has(Passion) && f.at(r).goos.any

                true |=> -100 -> "dreams are expensive"

                f.has(Passion) && f.power == 0 && (r +: self.all.cultists./(_.region)).%(_.capturers.contains(f)).any |=> -1500000 -> "dont dream if passion allows reverse capture"

            case SubmergeMainAction(_, r) =>
                finale(2) |=> 1500000 -> "finale submerge"
                others.%(ofinale).any |=> 500000 -> "others finale submerge"

                active.none |=> 50000 -> "active none"

                needKill |=> 10000 -> "not all sb"
                numSB >= 5 |=> 10000 -> "nearly all sb"

                power == 1 && others.%(_.power > 1).any && self.gates.any |=> -20000 -> "dont end with submerge"
                cthulhu.region.foes.any |=> -100 -> "can fight right now"
                true |=> 400 -> "better than moving"
                cthulhu.friends.num >= 5 |=> 100 -> "many friends"

            case SubmergeAction(_, r, uc) =>
                finale(1) |=> 1500000 -> "finale submerge"

                needKill |=> 10000 -> "not all sb"

                r.foes.goos.active.any |=> 10000 -> "get away from goo"

                val u = self.at(r, uc).head
                u.gateKeeper && r.capturers.%(_.active).none |=> -500 -> "don't submerge gate keeper"
                u.defender |=> -400 -> "don't submerge defender"
                u.uclass.cost == 3 |=> 300 -> "submerge 3"
                u.uclass.cost == 2 |=> 300 -> "submerge 2"
                u.uclass.cost == 1 |=> 300 -> "submerge 1"

                r.ocean && game.cathedrals.contains(r) && AN.has(UnholyGround) && r.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"

            case UnsubmergeAction(_, r) =>
                finale(1) && !game.battled.contains(r) |=> others./(f => f.strength(game, f.at(r), self)).max * 100000 -> "finale unsubmerge attack"

                result = eval(MoveAction(self, Cthulhu, GC.deep, r), 1 - game.tax(r, self))

                game.cathedrals.contains(r) && AN.has(UnholyGround) && r.str(AN) > 0 && (AN.player.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

            case MainDoneAction(_) =>
                game.battled.any |=> 20000 -> "unlimited battle drains power"
                others.%(ofinale).any |=> 666000 -> "extend finale"
                true |=> 500 -> "main done"

            case _ =>
        }

        // BATTLE
        if (game.battle != null) {
            val battle = game.battle

            val opponent = battle.opponent(self)
            val allies = battle.units(self)
            val foes = battle.units(opponent)
            val first = battle.attacker == self

            def elim(battle : Battle, u : UnitFigure) {
                u.is(DeepOne)   |=> 800 -> "elim dee"
                u.is(Acolyte)   |=> 400 -> "elim aco"
                u.is(Shoggoth)  |=> 300 -> "elim sho"
                u.is(Starspawn) |=> 200 -> "elim sta"
                u.is(Cthulhu)   |=> 100 -> "elim cth"
                u.is(Cthulhu) && finale(0) |=> 1000000 -> "finale immortal"
            }

            def retreat(battle : Battle, u : UnitFigure) {
                u.gateKeeper && battle.region.allies.num - battle.side(self).opponent.rolls.%(_ == Pain).num >= 2 |=> -1000 -> "retr gate keeper"

                u.is(DeepOne)   && !CC.has(Madness) |=> 100 -> "retr dee"
                u.is(Shoggoth)  && !CC.has(Madness) |=> 200 -> "retr sho"
                u.is(Starspawn) && !CC.has(Madness) |=> 300 -> "retr sta"
                u.is(Acolyte)   && !CC.has(Madness) |=> 600 -> "retr acolyte"
                u.is(Cthulhu)   && !CC.has(Madness) |=> 800 -> "retr cthulhu"

                u.is(DeepOne)   && CC.has(Madness) |=> 800 -> "retr dee"
                u.is(Shoggoth)  && CC.has(Madness) |=> 600 -> "retr sho"
                u.is(Starspawn) && CC.has(Madness) |=> 300 -> "retr sta"
                u.is(Acolyte)   && CC.has(Madness) |=> 200 -> "retr acolyte"
                u.is(Cthulhu)   && CC.has(Madness) |=> 100 -> "retr cthulhu"

                u.is(Cthulhu) && battle.side(self).opponent.units(Nyarlathotep).any |=> -1000 -> "dont pain if harbinger"
                u.is(Cthulhu) && u.region.ownGate && (u.region.allies.monsters.none || u.region.foes.goos.any) && u.region.foes.any |=> -1000 -> "hold to gate"
            }

            a match {
                case DevourPreBattleAction(_) =>
                    true |=> 1000 -> "devour"

                case AbsorbeeAction(_, s, u) =>
                    u.is(DeepOne) && needKill |=> 100 -> "absorb"
                    (u.is(DeepOne) || u.is(Acolyte)) && foes.goos.any && (allies.goos.any || !opponent.has(Emissary)) && (allies.goos.none || battle.strength(opponent) > battle.strength(self)) |=> 100 -> "absorb"
                    true |=> -100 -> "absorb"

                case DemandSacrificeKillsArePainsAction(_) =>
                    battle.strength(self) < battle.strength(battle.opponent(self)) |=> 1000 -> "less str"
                    battle.strength(self) > battle.strength(battle.opponent(self)) |=> -1000 -> "more str"

                case AssignKillAction(_, _, _, u) =>
                    u.is(Starspawn) && u.health == DoubleHP(Alive, Alive) |=> 1000 -> "weather kill"
                    elim(battle, u)

                case AssignPainAction(_, _, _, u) =>
                    retreat(battle, u)

                case EliminateNoWayAction(_, u) =>
                    elim(battle, u)

                case RetreatUnitAction(_, u, r) =>
                    u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                    u.cultist && r.foes.goos.any |=> -1500 -> "dont send cultist to feed enemy goo"
                    u.cultist && r.allies.monsters.any |=> 1000 -> "send cultist to be protected by monsters"
                    u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                    u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                    u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                    u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                    u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                    u.monster && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                    u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                    u.monster && r.foes.%(_.vulnerableM).any && !r.foes.goos.any |=> 1000 -> "send monster to capture"
                    u.goo && r.foes.%(_.vulnerableG).any |=> 1000 -> "send goo to capture"

                    u.monster && r.allies.goos.any && r.foes.goos.any |=> 1500 -> "send monster to friendly goo with enemy goo"
                    u.monster && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                    u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                    u.monster && r.ownGate |=> 400 -> "send monster to own gate"
                    u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                    u.monster && r.freeGate |=> 300 -> "send monster to free gate"
                    u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                    u.monster && r.enemyGate |=> 300 -> "send monster to enemy gate"
                    u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                    if (u.goo)
                        result = eval(MoveAction(u.faction, u.uclass, u.region, r), game.tax(r, self))

                    true |=> game.board.connected(r).distinct.num -> "reachable regions"
                    r.ocean |=> 1 -> "ocean"
                    r.desecrated && r.of(YS).none |=> -2 -> "empty desecrated"

                case _ =>
            }
        }

        result.none |=> 0 -> "none"

        true |=> (math.random() * 4).round.toInt -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
