package cws

import hrf.colmat._

object BotSL extends BotX(g => new GameEvaluationSL(g))

class GameEvaluationSL(game : Game) extends GameEvaluation(game, SL) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        a match {
            case FirstPlayerAction(_, f) =>
                f == self && game.board.regions.%(_.capturers.any).%(_.allies.cultists.any).any |=> 100 -> "play first prevent capture"
                f == self && game.board.regions.%(_.allies.goos.any).%(_.foes.goos.any).any |=> 100 -> "play first goos together"
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

                (game.factions.indexOf(f) - game.factions.indexOf(self)).abs == 2 |=> 10 -> "stall opposite"
                f == YS |=> 1 -> "ys first"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            case SpellbookAction(_, sb, _) => sb match {
                case CursedSlumber =>
                    true |=> 1000 -> "1000"
                case AncientSorcery =>
                    true |=> 1800 -> "1800"
                case Burrow =>
                    true |=> 700 -> "700"
                case CaptureMonster =>
                    true |=> 600 -> "600"
                case DemandSacrifice =>
                    true |=> 400 -> "400"
                case EnergyNexus =>
                    true |=> 200 -> "200"
                case _ =>
                    true |=> -1000 -> "unknown"
            }

            case RevealESAction(_, es, false, _) if game.of(self).es != es =>
                true |=> -10000 -> "better reveal all"

            case RevealESAction(_, _, _, _) =>
                allSB && realDoom >= 30 |=> 1100 -> "reveal and try to win"
                allSB && realDoom < 30 && realDoom < self.aprxDoom && realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                !allSB && realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 1100 -> "reveal so 30 broken and nobody wins"
                true |=> -100 -> "dont reveal"
                canRitual |=> -2000 -> "ritual first"

            case RitualAction(_, cost, _) =>
                val allSB = self.allSB || (numSB == 5 && need(PerformRitual))

                instantDeathNow |=> 10000 -> "instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "dont ritual if ID next and not all SB"
                instantDeathNext && !allSB && others.all(!_.allSB) && realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30, and all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom <= 29 && realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"

                maxDoomGain > 5 |=> 300 -> "good enough for sleeper"

                numSB >= 5 && cost * 2 <= power |=> 800 -> "5 SB and less than half available power"
                numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "very sweet deal"
                numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                cost == 5 |=> 100 -> "ritual first"
                self.pool.goos.any |=> -200 -> "not all goos in play"
                true |=> -250 -> "dont ritual unless have reasons"

            case DoomDoneAction(_) =>
                true |=> 0 -> "doom done"


            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"
                active.none |=> 5000000 -> "move done"

            case MoveAction(_, Tsathoggua, o, d) =>
                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                active.any || power < 2 |=> -10000 -> "dont move sleeper"

                val hasturThreat = YS.has(Hastur) && YS.player.goo(Hastur).region.ownGate && YS.power == 0
                val kiyThreat = YS.has(KingInYellow) && YS.player.goo(KingInYellow).region.ownGate && YS.power == 0
                val ihh = have(SeekAndDestroy).?(self.all(FormlessSpawn).%(_.region != d).num).|(0)

                d.enemyGate && others./(_.aprxDoom).max > 15 && (d.owner.aprxDoom + 5 < others./(_.aprxDoom).max) && d.foes.goos.none |=> -1500 -> "bash leader instead"
                d.enemyGate && others./(_.aprxDoom).max > 23 && (d.owner.aprxDoom + 1 < others./(_.aprxDoom).max) && d.foes.goos.none |=> -1500 -> "bash leader instead"

                val kiyScreamCapture = YS.power > 1 && YS.has(KingInYellow) && YS.has(ScreamingDead)

                if (kiyScreamCapture && !self.allSB) {
                    val r = YS.player.goo(KingInYellow).region
                    o.allies.cultists.any && o.allies.cultists.num < 4 && r.near.%(_ == o).any && self.strength(game, o.allies.monsters, YS) <= r.foes(Undead).num + 1 |=> -900 -> "kiy scream capture"
                    d.allies.cultists.any && o.allies.cultists.num < 4 && r.near.%(_ == d).any && d.ownStr <= r.foes(Undead).num + 1 |=> 900 -> "kiy scream capture"
                }

                power == 1 && o.ownGate && o.allies.num == 2 && others.%(_.power > 0).%(_.at(o).goos.any).none && o.foes.monsters.%(_.faction.power > 0).any |=> -2000 -> "dont lose the gate"
                power == 1 && o.ownGate && o.allies.num == 2 && others.%(_.power > 0).%(_.at(o).goos.any).none && o.near.%(_.foes.monsters.%(_.faction.power > 1).any).any |=> -1900 -> "dont lose the gate"
                power == 1 && o.ownGate && o.allies.num == 2 && others.%(_.power > 0).%(_.at(o).goos.any).none && o.near.%(_.near.%(_.foes.monsters.%(_.faction.power > 2).any).any).any |=> -1800 -> "dont lose the gate"

                val canCaptureYS = self.all.cultists./(_.region).%(_.capturers.contains(YS)).none

                active.none && power > 1 && d.enemyGate && (d.owner != YS || canCaptureYS) && d.controllers.num == 1 && d.of(d.owner).goos.none |=> (5 * 100000 / 3) -> "go get a gate"
                active.none && power > 2 && d.enemyGate && (d.owner != YS || canCaptureYS) && d.controllers.num == 2 && d.of(d.owner).goos.none |=> (5 * 100000 / 4) -> "go get a gate"
                active.none && power > 3 && d.enemyGate && (d.owner != YS || canCaptureYS) && d.controllers.num == 3 && d.of(d.owner).goos.none |=> (5 * 100000 / 5) -> "go get a gate"

                d.enemyGate && self.gates.num < 4 && d.owner.power < power && (d.owner != YS || !hasturThreat) && d.controllers.num == 1 && d.controllers.monsters.none && d.foes.goos.none && !d.owner.active && power > 1 |=> 999 -> "go get a gate"
                d.enemyGate && self.gates.num < 4 && d.owner.power < power && (d.owner != YS || !hasturThreat) && d.controllers.num == 2 && d.controllers.monsters.none && d.foes.goos.none && !d.owner.active && power > 2 |=> 990 -> "go get a gate"
                d.enemyGate && self.gates.num < 4 && d.owner.power < power && (d.owner != YS || !hasturThreat) && d.owner.power + 1 < power && d.foes.goos.none && have(Emissary) && power > 2 |=> 900 -> "go get a gate"
                d.enemyGate |=> -d.controllers.num -> "controllers"

                d.foes(Hastur).any |=> -3000 -> "no not hastur"
                o.foes(Hastur).any |=> 2800 -> "no not hastur"
                d.foes(Cthulhu).any && d.of(GC).num > d.allies.num |=> -2600 -> "no not cthulhu"
                o.foes(Cthulhu).any && o.of(GC).num > o.allies.num && GC.power > 0 |=> 2400 -> "no not cthulhu"
                o.ownGate && others.%(_.power > 0).%(_.at(o).monsters.any).any && o.allies.monsters.none |=> -1500 -> "protect gate"

                d.foes(Hastur).any && power > 1 && YS.power == 0 && have(Abduct).?(d.allies(Wizard).num).|(0) + have(Invisibility).?(d.allies(SerpentMan).num).|(0) >= d.of(YS).num - 1 |=> 3333 -> "assassinate hastur"

                ((d.ownGate && d.allies.cultists.num <= YS.power) || ((self.allSB || YS.power == 0 || (YS.power == 0 && !d.desecrated        )) && power > 1)) && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= (d.allies.num + ihh    ) * 4 && YS.power >= 0 |=> 2000 -> "nya defend from kiy"
                ((d.ownGate && d.allies.cultists.num <= BG.power) || ((self.allSB || BG.power == 0 || (BG.power == 0 && d.allies.cultists.any)) && power > 1)) && d.foes(ShubNiggurath).any                       && d.str(BG) <= (d.allies.num + ihh    ) * 4 && BG.power >= 0 |=> 1800 -> "nya defend from shub"
                ((d.ownGate && d.allies.cultists.num <= GC.power) || ((self.allSB || GC.power == 0 || (GC.power == 0 && d.allies.cultists.any)) && power > 1)) && d.foes(Cthulhu).any                             && d.str(GC) <= (d.allies.num + ihh - 1) * 4 && GC.power >= 0 |=> 1600 -> "nya defend from cthulhu"

                val shield = d.allies.num + have(SeekAndDestroy).?(self.all(FormlessSpawn).num).|(0) >= 2

                shield && d.foes(KingInYellow).any && d.foes(Hastur).none && !game.desecrated.contains(d) |=> 300 -> "nya chase kiy"
                shield && d.foes(ShubNiggurath).any && d.enemyGate && d.owner == BG |=> 290 -> "nya chase shub"
                shield && d.foes(Cthulhu).any && d.ocean.not |=> 280 -> "nya chase cthulhu"

                active.none && power > 1 && d.enemyGate && (d.owner != YS || (!hasturThreat && !kiyThreat)) && d.controllers.num == 1 && d.of(d.owner).goos.none |=> (5 * 100000 / 3) -> "go get a gate"

                d.enemyGate && power > d.owner.power && d.foes.goos.none |=> 110 -> "enemy gate"
                d.foes.%(_.vulnerableG).any && power > 1 |=> 90 -> "maybe capture"
                d.freeGate |=> 75 -> "free gate"
                others.%(_.power > 0).any && d.ownGate |=> 50 -> "own gate"
                others.%(_.power > 0).any && d.allies.cultists.any |=> 25 -> "hug cultist"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.player.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

            case MoveAction(_, Acolyte, o, d) =>
                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                d.allies.goos.any && o.foes.goos.%(_.faction.power > 0).any |=> 1100 -> "go to daddy"

                val u = self.at(o, Acolyte).%(!_.has(Moved)).head

                active.none && d.enemyGate && d.allies.goos.any && !u.gateKeeper |=> 60000 -> "go to freeing gate"

                val needGate = self.allSB || self.doom + self.gates.num < 29

                needGate && active.none && self.gates.num < 4 && !u.gateKeeper && d.freeGate && d.allies.cultists.none |=> (2 * 100000 / 1) -> "safe move and get gate"

                needGate && active.none && self.gates.num < 4 && !u.gateKeeper && d.noGate && power > 3 && (d.ocean.not || !GC.needs(OceanGates)) |=> (2 * 100000 / 4) -> "safe move and build gate"

                u.gateKeeper && (!u.capturable || u.enemies.goos.none) |=> -500 -> "dont move gatekeeper"
                self.pool.cultists.any && d.allies.any && !self.all.%(_.has(Moved)).any |=> -500 -> "why move if can recruit for same"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d.near.all(_.empty) && d.near2.all(_.empty) |=> 999 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d.near.all(_.empty) && d.near2.all(_.of(YS).none) |=> 990 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d.near.all(_.empty) |=> 909 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d.near.all(_.foes.none) |=> 908 -> "crowded cultists 6 explore all friendly around"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d.near.all(_.of(YS).none) |=> 900 -> "crowded cultists 6 explore all no-ys around"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d == EarthMap4v35.Antarctica |=> 800 -> "crowded cultists 6 explore - antarctica"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d == EarthMap4v35.NorthAmerica |=> 750 -> "crowded cultists 6 explore - north america"
                o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d == EarthMap4v35.Arabia |=> 700 -> "crowded cultists 6 explore - arabia"

                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 400 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && (active.none || d.capturers.%(f => f.power > 0 || f.has(Passion)).none) |=> 300 -> "ic temporary free gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.ownGate |=> 60 -> "flee from capture to own gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.allies.monsters.any |=> 59 -> "flee from capture to monster"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.empty |=> 58 -> "flee from capture"

                others.%(f => f.power > 0 || f.has(Passion)).%(f => o.of(f).goos.none && o.of(f).monsters.none).none |=> -300 -> "why move"

                d.foes.goos.any && d.allies.goos.none |=> -250 -> "dont move to enemy goos"
                o.capturers.%(_.power > 0).any && d.capturers.none && o.allies.cultists.num == 1 |=> 220 -> "move from capture"
                o.ownGate && o.allies.cultists.num == 2 |=> -210 -> "move from own gate"
                o.ownGate && o.allies.cultists.num == 3 |=> -110 -> "move from own gate"
                d.ownGate && d.allies.cultists.num >= 2 |=> -210 -> "move to own gate"
                !o.ownGate && d.allies.goos.any |=> 200 -> "move to goo"
                d.allies.cultists.any |=> -90 -> "move to cultists"
                d.allies.monsters.any |=> 80 -> "move to monsters"
                d.ownGate |=> -70 -> "move to own gate"
                d.foes.any |=> -50 -> "dont move to foes"
                o.gate |=> -10 -> "move from gate"

            case MoveAction(_, uc, o, d) =>
                // Works best in early game when it's "safe" to leave cultists along
                wwLoneCultistPolarGate(1) && (WW.exists.?(d == game.board.starting(WW).but(game.starting(WW)).head)|(false)) |=> 1500 -> "move monster to block ww getting 2nd gate at opp pole"

                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                val u = self.at(o, uc).%(!_.has(Moved)).headOption.getOrElse(new UnitFigure(self, uc, -1, o))

                o.ownGate && o.allies.goos.none && o.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(o).goos.none).%(_.at(o).monsters.any).any |=> -2000 -> "dont abandon gate"

                d.ownGate && d.capturers.any && d.capturers.%(_.power > 0).any && d.capturers.%(_.power > 0).%(_.at(d).goos.any).none |=> 1100 -> "protect gate"

                // Nyarlathotep is wrong here, right?
                uc == Wizard && (o.noGate || o.enemyGate || o.foes.none) && have(Nyarlathotep) && power > 2 && o.foes.goos.none && d.foes.goos.any && d.foes(Hastur).none && (d.foes(Cthulhu).none || d.allies.any || power > 3) && (others.%(_.at(d).goos.any).%(_.power == 0).any) |=> 700 -> "maybe shield"
                uc == SerpentMan && (o.noGate || o.enemyGate || o.foes.none) && have(Nyarlathotep) && power > 2 && o.foes.goos.none && d.foes.goos.any && d.foes(Hastur).none && (d.foes(Cthulhu).none || d.allies.any || power > 3) && (others.%(_.at(d).goos.any).%(_.power == 0).any) |=> 700 -> "maybe shield"

                o.ownGate && u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.num == 1 |=> -600 -> "dont leave lone cultist on gate"

                o.foes.goos.any |=> -500 -> "stay with enemy goos"

                val canCaptureYS = self.all.cultists./(_.region).%(_.capturers.contains(YS)).none
                val canCaptureWW = !WW.has(Ithaqua)

                d.allies.none && d.foes.cultists.%(_.vulnerableM).map(_.faction).%(f => !f.active && (f != YS || canCaptureYS) && (f != WW || canCaptureWW)).any |=> 250 -> "go for capture"

                d.ownGate && canSummon(uc) && !game.hasMoved(self) |=> -1200 -> "why move if can summon"

                o.allies.cultists.none && d.ownGate && d.allies.monsters.none |=> 50 -> "move"

                o.allies.cultists.none && d.enemyGate && d.allies.none |=> 2 -> "move"

                active.none && power > 1 && u.is(Wizard) && d.allies.goos.any && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) > d.allies(Acolyte).num + d.allies(Wizard).num |=> 1100000 -> "shield from kiy"

                u.is(SerpentMan) |=> -1 -> "sm stay"
                u.is(FormlessSpawn) |=> 1 -> "fs move"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val enemyStr = f.strength(game, foes, self)
                val ownStr = self.strength(game, allies, f)

                ownStr >= 6 && need(Roll6DiceInBattle) && foes(Cthulhu).none |=> 1000 -> "get spellbook"
                ownStr >= 6 && need(Roll6DiceInBattle) |=> 1000000 -> "get spellbook"

                val igh = BG.has(Necrophagy).?(f.all(Ghoul).diff(foes).num).|(0)

                var ac = allies(Acolyte).num
                var ng = allies(Wizard).num
                var fp = allies(SerpentMan).num
                var hh = allies(FormlessSpawn).num
                val tsa = allies.has(Tsathoggua)

                val nya = allies.has(Nyarlathotep)

                var ihh = have(SeekAndDestroy).?(self.all(FormlessSpawn).diff(allies).num).|(0)

                def emissary = have(Emissary) && nya && foes.goos.none

                f.has(Invisibility) &&            foes(FlyingPolyp).num >= foes.num    |=> -10000000 -> "polyp can invis all"
                f.has(Invisibility) && tsa.not && foes(FlyingPolyp).num >= allies.num  |=> -10000000 -> "polyp can invis all"

                r.ownGate && allies.num + ihh < 2 + igh |=> -1000 -> "ghouls will knock off the gate"

                f match {
                    case GC =>
                        var ec = foes(Acolyte).num
                        var dp = foes(DeepOne).num
                        var sh = foes(Shoggoth).num
                        var ss = foes(Starspawn).num
                        var cth = foes.has(Cthulhu)

                        if (have(Abduct)) 1.to(ng).foreach { x =>
                            ng -= 1
                            if (ec > 0)
                                ec -= 1
                            else
                            if (dp > 0)
                                dp -= 1
                            else
                            if (sh > 0)
                                sh -= 1
                            else
                            if (ss > 0)
                                ss -= 1
                            else
                                ng += 1
                        }

                        if (have(Invisibility)) 1.to(fp).foreach { x =>
                            if (ec + dp + sh > 1 && sh > 0)
                                sh -= 1
                            else
                            if (ss > 0)
                                ss -= 1
                            else
                            if (sh > 0)
                                sh -= 1
                            else
                            if (dp > 0)
                                ss -= 1
                            else
                            if (ec > 0)
                                ec -= 1
                        }

                        if (cth) {
                            if (ac > 0)
                                ac -= 1
                            else
                            if (ng > 0)
                                ng -= 1
                            else
                            if (fp > 0)
                                fp -= 1
                            else
                            if (hh > 0)
                                hh -= 1
                            else
                            if (ihh > 0)
                                ihh -= 1
                        }

                        val enemyAttack = cth.?(6).|(0) + ss * 3 + (f.has(Absorb) && ss > 0).?(ss * 2 + ec * 3 + dp * 3).|(dp)
                        val enemyDefense = ss * f.has(Regenerate).?(2).|(1) + sh + dp + ec

                        val myAttack = nya.?(self.numSB + f.numSB).|(0) + hh * 2 + ihh * 2 + fp * 1
                        val myDefense = ihh + hh + fp + ng + ac

                        nya && cth && myAttack > enemyDefense * 2 && myDefense * 3 >= enemyAttack * 2 |=> 1200 -> "good cthulhu fight"
                        nya && cth && myAttack > enemyDefense * 2 && myDefense * 2 >= enemyAttack * 1 |=> 800 -> "ok cthulhu fight"

                        active.none && nya && cth && (enemyStr <= (allies.num + ihh - 2) * 4) |=> 800000 -> "attack cthulhu 800000"

                        nya && cth && enemyStr <= (allies.num + ihh - 2) * 4 |=> 5555 -> "attack cthulhu 5555"


                    case BG =>
                        def ec = foes(Acolyte).num
                        def fr = (f == BG && f.has(Frenzy)).?(ec).|(0)
                        def gh = foes(Ghoul).num
                        def fu = foes(Fungi).num
                        def dy = foes(DarkYoung).num
                        def shu = foes.has(ShubNiggurath)

                        nya && shu && enemyStr <= (allies.num + ihh - 1) * 4 |=> 5555 -> "attack shub 5555"

                        active.none && nya && shu && enemyStr <= (allies.num + ihh - 1) * 4 |=> 900000 -> "attack shub 900000"

                        emissary && dy > 0 && (ec + fu + gh) * 4 < ownStr |=> 1000 -> "kill dark youngs"

                        r.ownGate && shu && !nya && dy + fu + gh + ec == 0 && (ownStr > 1 || ac < BG.power) |=> 1111 -> "chase bg away"

                        gh > 0 && ec + fu + dy == 0 && BG.has(ShubNiggurath) && BG.has(ThousandYoung) && BG.power > 0 |=> -1000 -> "dont fight free ghouls"

                    case YS =>
                        def ec = foes(Acolyte).num
                        def un = foes(Undead).num
                        def by = foes(Byakhee).num
                        def kiy = foes.has(KingInYellow)
                        def has = foes.has(Hastur)

                        active.none && nya && kiy && !has && (enemyStr <= (allies.num + ihh - 1) * 4) |=> 1000000 -> "attack kiy 1000000"

                        nya && kiy && !has && enemyStr <= (allies.num + ihh - 1) * 4 |=> 5555 -> "attack kiy 5555"

                        r.ownGate && !nya && kiy && (ownStr > enemyStr || ownStr >= foes.num * 2) && !(have(Nyarlathotep) && self.allSB && power > 1) |=> 4444 -> "chase kiy away"

                        r.ownGate && !nya && has && un <= 1 && by == 0 && ownStr > un |=> 1222 -> "chase has away"

                        un == 1 && by == 0 && !kiy && !has && (ac == 0 || !YS.has(Zingaya)) && !game.desecrated.contains(r) |=> -1000 -> "dont fight lone undead on undesecrated"

                        has && have(Abduct).?(ng).|(0) + have(Invisibility).?(fp).|(0) >= ec + un + by && ownStr + ihh * 2 > 4 |=> 3334 -> "assassinate has"

                        f.power == 0 && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any && f.has(Passion) && ec > 1 |=> -1000 -> "dont attack if passion allows reverse capture"
                        emissary && r.enemyGate && r.owner == f && r.owner.has(Passion) && ec > 1 |=> 900 -> "better skirmish ys than capture"

                    case CC =>
                        0 -> "todo"

                    case WW =>
                        0 -> "todo"

                    case OW =>
                        0 -> "todo"

                    case AN =>
                        allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                        AN.has(Extinction) && foes.monsters.num == 1 && foes(Yothan).any && ((tsa && allies.num >= 3 && ownStr >= 6) || (allies.goos.none && ownStr >= 6)) |=> 1000 -> "attack lone extinct yothan"
                }

                game.acted || game.battled.any |=> -1000 -> "unlimited battle drains power"

                (game.acted || game.battled.any) && r.enemyGate && foes.goos.none |=> -10000 -> "unlimited battle drains power"

                emissary && hh == 0 && (fp == 0 || have(Invisibility)) && r.enemyGate && r.owner == f |=> 610 -> "ok emissary skirmish at gate"

                enemyStr == 0 && r.gateOf(f) && ownStr >= 2 |=> (300 + ownStr) -> "enemy rolls none at gate"
                enemyStr == 0 && f.power > 0 && r.freeGate && others.%(_ != f).%(_.at(r).any).none |=> (300 + ownStr) -> "enemy rolls none at free gate"

                val ysThreat = if (f == YS) (
                    (YS.has(Hastur) && YS.player.goo(Hastur).region.allies.cultists.any) ||
                    (YS.has(KingInYellow) && YS.player.goo(KingInYellow).region.allies.cultists.any)
                ) else false

                active.none && (game.acted || game.battled.any) |=> -1000000 -> "unlimited battle drains power"
                active.none && r.gateOf(f) && emissary && !ysThreat |=> (7 * 100000 / 4) -> "drive from gate"

                r.enemyGate && r.gateOf(f) && enemyStr <= ownStr |=> (5 + (ownStr - enemyStr)) -> "attack at gate"

            case CaptureAction(_, r, f) =>
                val safe = active.none
                safe && !r.gateOf(f) |=> (1 * 100000 / 1) -> "safe capture"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 1 && power > 0                    |=> (2 * 100000 / 1) -> "safe capture and open gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 2 && power > 1 && !f.has(Passion) |=> (3 * 100000 / 2) -> "safe capture and half gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 3 && power > 2 && !f.has(Passion) |=> (4 * 100000 / 3) -> "safe capture and third of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 4 && power > 3 && !f.has(Passion) |=> (5 * 100000 / 4) -> "safe capture and fourth of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 5 && power > 4 && !f.has(Passion) |=> (6 * 100000 / 5) -> "safe capture and fifth of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 6 && power > 5 && !f.has(Passion) |=> (7 * 100000 / 6) -> "safe capture and sixth of gate"

                !need(Gates4Power15) && others./(_.aprxDoom).max > 15 && (f.aprxDoom + 9 < others./(_.aprxDoom).max) |=> -1650 -> "bash leader instead"
                !need(Gates4Power15) && others./(_.aprxDoom).max > 23 && (f.aprxDoom + 5 < others./(_.aprxDoom).max) |=> -1650 -> "bash leader instead"

                r.enemyGate && f == r.owner && r.controllers.num == 1 && r.allies.cultists.none && others.%(_.power > 0).%(_.at(r).%(_.canControlGate).any).any |=> -700 -> "give gate away"
                !f.has(Passion) |=> 1600 -> "capture"
                f.has(Passion) && f.power == 0 && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any |=> -1500000 -> "dont capture if passion allows reverse capture"
                f.has(Passion) && (f.power == 0 || !YS.has(Hastur) || !YS.has(ThirdEye)) |=> 1100 -> "capture with passion safe"
                f.has(Passion) |=> 850 -> "capture with passion"
                r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 450 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 400 -> "capture and nearly open gate"
                r.enemyGate |=> 100 -> "enemy gate"

            case BuildGateAction(_, r) =>
                active.none && self.gates.num < 4 && (self.allSB || self.doom + self.gates.num < 29) |=> (10 * 100000 / 9) -> "safe build gate"

                WW.exists && game.board.starting(WW).contains(r) |=> -10000000 -> "starting ww"
                GC.exists && game.board.starting(GC).contains(r) |=> -10000000 -> "starting gc"

                YS.has(Hastur) && YS.power > 1 |=> -1000 -> "hastur in play"
                YS.has(KingInYellow) && YS.power > 1 && game.board.connected(YS.player.goo(KingInYellow).region).contains(r) |=> -1000 -> "kiy is near"
                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && !r.allies.has(Nyarlathotep) |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && !r.allies.has(Nyarlathotep) |=> -700 -> "cthulhu has dreams"

                power > 5 && r.near.%(n => active.%(_.at(n).any).any).none |=> -600 -> "building can wait"
                (power >= 3 + maxEnemyPower || self.gates.num <= 1) && r.capturers.none |=> 500 -> "building gates is good"

            case RecruitAction(_, Acolyte, r) =>
                active.none && r.freeGate |=> (3 * 100000 / 1) -> "safe recruit and get gate"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && r.allies.cultists.none |=> (1 * 100000 / 1) -> "safe recruit"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && r.allies.cultists.any |=> (8 * 100000 / 9) -> "safe recruit"
                active.none && r.ownGate && r.allies.cultists.num == 1 |=> (7 * 100000 / 8) -> "safe recruit"

                r.capturers.%(_.power > 0).any |=> -2000 -> "dont recruit to be captured"
                r.freeGate && !have(Tsathoggua) |=> 1700 -> "free gate"
                self.pool.cultists.num >= power && !have(Tsathoggua) |=> 300 -> "recover lost cultists"
                r.ownGate && others.all(_.power < power) |=> -250 -> "dont recruit if max power"
                r.ownGate && r.allies.goos.any |=> 200 -> "a cultist needs a big friend"

            case SummonAction(_, Wizard, r) =>
                // Defense against WW setting up 2nd gate at opposite pole. Only worth it if adjacent to location for new gate
                r.distanceToWWOppPole == 1 && wwLoneCultistPolarGate(2) && r.allies.monsters.none |=> 1500 ->"cheap summon to stop WW 2nd gate at opp pole"

                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                true |=> (-r.allies.monsters.num) -> "monsters count"

                power == 1 && r.allies.num == 1 && others.%(_.power > 0).%(_.at(r).goos.any).none && r.foes.monsters.%(_.faction.power > 0).any |=> 2000 -> "dont lose the gate"
                power == 1 && r.allies.num == 1 && others.%(_.power > 0).%(_.at(r).goos.any).none && r.near.%(_.foes.monsters.%(_.faction.power > 1).any).any |=> 1900 -> "dont lose the gate"
                power == 1 && r.allies.num == 1 && others.%(_.power > 0).%(_.at(r).goos.any).none && r.near.%(_.near.%(_.foes.monsters.%(_.faction.power > 2).any).any).any |=> 1800 -> "dont lose the gate"

                power > 1 && r.allies.goos.any && r.foes.goos(Hastur).none && others.%(_.at(r).goos.any).%(_.power == 0).any |=> 850 -> "shield"

                r.allies(Wizard).none |=> 20 -> "ng cheap"
                r.allies(Wizard).any |=> 10 -> "ng cheap"
                r.foes(KingInYellow).any |=> -150 -> "ng bad against kiy"
                r.near.%(_.foes(KingInYellow).any).any |=> -110 -> "ng bad against kiy"
                r.near.%(n => n.noGate && n.of(YS).any && YS.power > 5).any |=> -100 -> "ng bad against kiy"
                YS.has(KingInYellow) && YS.power >= 2 && r.allies.monsters.num == r.allies.monsters(Wizard).num && r.allies.goos.none |=> -90 -> "ng bad against kiy"
                YS.power >= 6 |=> -80 -> "ng bad against kiy"

                self.pool(Wizard).num == 1 && self.pool(SerpentMan).num > 0 && power == 2 |=> -500 -> "summon fp instead"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.capturers.any && r.capturers.%(_.at(r).goos.any).none |=> 250 -> "prevent later capture"
                r.foes.cultists.%(_.vulnerableM).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsters.none |=> 150 -> "lone cultist"
                r.allies.monsters.none |=> 140 -> "no monsters"
                r.allies.cultists.num > 2 && r.allies.monsters.any |=> 130 -> "many cultists"
                r.allies.goos.any |=> 100 -> "summon to nya"

                true |=> 3000 -> "wizards out"

            case SummonAction(_, SerpentMan, r) =>
                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                true |=> (-r.allies.monsters.num) -> "monsters count"

                power > 2 && r.allies.goos.any && r.foes.goos(Hastur).none && others.%(_.at(r).goos.any).%(_.power == 0).any |=> 800 -> "shield"

                true |=> 25 -> "dfb useful"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.foes(KingInYellow).any |=> 230 -> "fight kiy"
                r.foes.cultists.%(_.vulnerableM).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsters.none |=> 150 -> "lone cultist"
                r.allies.monsters.none |=> 140 -> "no monsters"
                r.allies.cultists.num > 2 && r.allies.monsters.any |=> 130 -> "many cultists"
                r.allies.goos.any |=> 100 -> "summon to nya"

            case SummonAction(_, FormlessSpawn, r) =>
                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                true |=> (-r.allies.monsters.num) -> "monsters count"

                have(Nyarlathotep) && !have(ThousandForms) && need(Pay4Power) && power > 4 && power < 8 |=> -300 -> "better pay 4 power for 1000f"

                !have(Nyarlathotep) |=> 10 -> "hh expensive"
                have(Nyarlathotep) |=> 30 -> "hh to shield nya"

                r.foes(KingInYellow).any && power > 3 |=> -150 -> "ng bad againgt kiy"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.foes.cultists.%(_.vulnerableG).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsters.none |=> 150 -> "lone cultist"
                r.allies.monsters.none |=> 140 -> "no monsters"
                r.allies.cultists.num > 2 && r.allies.monsters.any |=> 130 -> "many cultists"
                r.allies.goos.any |=> 100 -> "summon to nya"

            case AwakenAction(_, _, r, _) =>
                numSB >= 5 && need(AwakenTsathoggua) |=> 5000 -> "need nyarlathotep"
                r.foes.has(Hastur) |=> -3000 -> "hastur is scary"
                numSB < 5 && numSB > 3 && need(AwakenTsathoggua) |=> 2500 -> "need nyarlathotep"
                power > 8 |=> 2000 -> "yes awaken"
                power == 8 |=> 1800 -> "maybe awaken"
                r.foes(KingInYellow).any |=> 400 -> "awaken to battle kiy"
                r.foes.goos.any |=> 300 -> "awaken to battle"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsters.any |=> 100 -> "awaken to defend"
                r.near.%(_.foes(KingInYellow).any).any |=> 50 -> "awaken near kiy"

            case DeathFromBelowAction(_, r, FormlessSpawn) =>
                self.at(r).goos.any |=> 2000 -> "hug tsathoggua"
                r.allies.cultists.num >= 2 |=> 1000 -> "hug many cultists"
                r.allies.cultists.num == 2 |=> 500 -> "hug two cultists"

            case DeathFromBelowAction(_, r, uc) =>
                result = eval(SummonAction(self, uc, r))

            case Pay3SomeoneGains3Action(_, f) =>
                self.numSB == 5 && power > 14 |=> 3000 -> "last spellbooks and plenty power"
                self.numSB == 5 && realDoom >= 30 |=> 5000 -> "last spellbook and end"
                self.numSB == 4 |=> 400 -> "pre-last spellbook"
                realDoom >= 20 |=> 600 -> "the end is near"
                realDoom >= 25 |=> 450 -> "the end is very near"
                realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

                active.none && realDoom + self.gates.num * 2 + self.goos.num >= 30 |=> 1000000 -> "the end is now"

                f.power == 0 && f.all.goos.none |=> 270 -> "good power conf"
                f.power == 0 && game.board.regions.%(r => r.allies.cultists.any && r.capturers.contains(f)).any |=> -1000 -> "dont give power to capture"


                self.count(SerpentMan) == 3 && !have(AncientSorcery) |=> 300 -> "get ancient sorcery"

                power > 3 && !have(CursedSlumber) && self.gates.%(r => r.allies.goos.none && r.foes.goos.active.any && r.allies.cultists.num > 1).any |=> 200 -> "get cursed slumber"

                self.gates.%(r => r.of(f).goos.any).any |=> -1000 -> "threatening gates"

                true |=> 3 -> "if wiped out"

            case Pay3EverybodyLoses1MainAction(_) =>
                self.numSB == 5 && power > 14 |=> 3000 -> "last spellbooks and plenty power"
                self.numSB == 5 && realDoom >= 30 |=> 5000 -> "last spellbook and end"
                self.numSB == 4 |=> 400 -> "pre-last spellbook"
                realDoom >= 20 |=> 600 -> "the end is near"
                realDoom >= 25 |=> 450 -> "the end is very near"
                realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

                active.none && realDoom + self.gates.num * 2 + self.goos.num >= 30 |=> 1000000 -> "the end is now"

                others.%(_.power == 0).none |=> 250 -> "good power conf"
                others.%(_.power == 0).num == 1 |=> 150 -> "ok power conf"

                self.count(SerpentMan) == 3 && !have(AncientSorcery) |=> 300 -> "get ancient sorcery"

                true |=> 5 -> "if wiped out"

            case Pay3EverybodyGains1MainAction(_) =>
                self.numSB == 5 && power > 14 |=> 3000 -> "last spellbooks and plenty power"
                self.numSB == 5 && realDoom >= 30 |=> 5000 -> "last spellbook and end"
                self.numSB == 4 |=> 400 -> "pre-last spellbook"
                realDoom >= 20 |=> 600 -> "the end is near"
                realDoom >= 25 |=> 450 -> "the end is very near"

                realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

                active.none && realDoom + self.gates.num * 2 + self.goos.num >= 30 |=> 1000000 -> "the end is now"

                others.%(_.power > 0).none |=> 250 -> "good power conf"
                others.%(_.power > 0).num == 1 |=> 150 -> "ok power conf"
                others.%(f => f.power > 0 && game.board.regions.%(r => r.allies.cultists.any && r.capturers.contains(f)).any).any |=> -1000 -> "dont give power to capture"

                self.count(SerpentMan) == 3 && !have(AncientSorcery) |=> 300 -> "get ancient sorcery"

                true |=> 4 -> "if wiped out"

            case CaptureMonsterAction(_, r, f) =>
                val uc = f.at(r).actualMonsters.minBy(_.uclass.cost)

                uc.uclass.cost == 1 |=> 600 -> "capture monster 1"
                // uc.uclass.cost == 2 |=> 900 -> "capture monster 2"
                uc.uclass.cost == 2 |=> 1100 -> "capture monster 2"
                uc.uclass.cost == 3 |=> 1700 -> "capture monster 3"

                uc.faction.active |=> 100 -> "active"

            case AncientSorceryUnitAction(_, Immortal, r, _) =>
                !have(Tsathoggua) && need(AwakenTsathoggua) && have(FormlessSpawn) && power > 8 |=> 800 -> "get es"
                !have(Tsathoggua) && !need(AwakenTsathoggua) && have(FormlessSpawn) && power > 4 |=> 2400 -> "resummon cheap"
                true |=> 100 -> "move power"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsters.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"
                r.allies.num == 1 && r.desecrated && have(Feast) |=> -200 -> "dont leave feast"

            case AncientSorceryUnitAction(_, Hibernate, r, _) =>
                true |=> 100 -> "move power"
                power > 1 && others./~(_.goos).num >= power - 1 |=> 1000 -> "partake in hibernate"
                r.allies.num == 1 && r.desecrated && have(Feast) |=> -200 -> "dont leave feast"

            case AncientSorceryUnitAction(_, Feast, r, _) =>
                game.desecrated.%(r => r.of(self).any).any |=> 700 -> "partake in feast"
                active.none && game.desecrated.%(r => r.of(self).any).num > 1 |=> 100000 -> "partake in feast"
                true |=> 150 -> "move power"
                r.desecrated && r.allies.num == 1 |=> -50 -> "stay to feast"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsters.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"

            case AncientSorceryUnitAction(_, Flight, r, _) =>
                active.none && power > 1 |=> 100000 -> "move safe options"

                power > 1 |=> 300 -> "sleepflying"
                power > 5 && have(Burrow) |=> 1300 -> "burrowflying"
                power > 5 && have(Tsathoggua) && game.board.regions.%(r => r.allies.cultists.any && r.allies.goos.none && r.near.%(_.allies.goos.any).none).any |=> 1200 -> "fly back cultists"
                true |=> 100 -> "move power"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsters.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"
                r.allies.num == 1 && r.desecrated && have(Feast) |=> -200 -> "dont leave feast"

            case AncientSorceryUnitAction(_, Fertility, r, _) =>
                true |=> 100 -> "move power"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsters.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"
                r.allies.num == 1 && r.desecrated && have(Feast) |=> -200 -> "dont leave feast"

            case AncientSorceryPlaceAction(self, r, uc) =>
                val o = game.board.regions.minBy(r => r.foes.num + r.allies.num)
                result = eval(MoveAction(self, uc, o, r))

            case HibernateMainAction(_, n) =>
                power == n |=> 300 -> "optimal power"
                power == n + 1 |=> 200 -> "if nothing better 1"
                power == n + 2 |=> 40 -> "if nothing better 2"
                power == n + 3 |=> 30 -> "if nothing better 3"
                power == n + 4 |=> 20 -> "if nothing better 4"
                power == n + 5 |=> 10 -> "if nothing better 5"
                others.%(ofinale).any |=> -2000000 -> (others.%(ofinale).mkString("/") + " ofinale")

                active.none |=> n * 30000 -> "hibernate x"

            case LethargyMainAction(_) =>
                true |=> 1000 -> "sweet dreams"

            case CursedSlumberSaveAction(_, r) =>
                r.allies.goos.none && r.foes.goos.any |=> 1800 -> "save gate from goo"
                r.allies.goos.none && r.allies.monsters.none && r.foes.monsters.any |=> 1700 -> "save gate from monsters"

            case CursedSlumberLoadAction(_, r) =>
                r.allies.goos.any && others.%(_.power > 1).any |=> 1200 -> "load gate to goo"




            case AvatarReplacementAction(_, _, r, o, uc) =>
                val u = self.at(r, uc).head
                u.cultist && o.capturers.%(_.power > 0).none && o.freeGate |=> 300 -> "free gate"
                u.cultist && r.allies.goos.any |=> -250 -> "stay with goo"
                u.cultist && o.allies.goos.any |=> 250 -> "go to goo"
                u.cultist && o.capturers.%(_.power > 0).%(_ != BG).any |=> -100 -> "dont send cultist to be captured"
                u.cultist && o.capturers.none |=> 150 -> "no capturers"
                u.cultist && o.capturers.any && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                u.monster && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monster && u.friends.cultists.any && u.friends.monsters.none && r.foes.monsters./(_.faction).%(_ != BG).%(_.power > 0).any |=> -200 -> "dont sent temp defender"
                u.monster && o.allies.cultists.any && o.allies.monsters.none |=> 50 -> "protect cultist"

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

            case MainDoneAction(_) =>
                game.battled.any |=> -1000 -> "unlimited battle drains power"
                true |=> 500 -> "main done"

            case _ =>
        }

        // Battle
        def elim(battle : Battle, u : UnitFigure) {
            u.is(Nyarlathotep) && have(Emissary) && battle.units(battle.opponent(self)).goos.none |=> 1000 -> "emissary or what"
            u.is(Wizard) |=> 400 -> "elim ng"
            u.is(Acolyte) |=> 800 -> "elim acolyte"
            u.is(SerpentMan) |=> 200 -> "elim fp"
            u.is(FormlessSpawn) |=> 100 -> "elim hh"
            u.is(Nyarlathotep) |=> -1 -> "elim nya"
        }

        def retreat(battle : Battle, u : UnitFigure) {
            u.uclass == Acolyte |=> 600 -> "retr acolyte"
            u.gateKeeper && battle.region.allies.num - battle.side(self).opponent.rolls.%(_ == Pain).num >= 2 |=> -1000 -> "retr gate keeper"

            u.uclass == Wizard |=> 100 -> "retr ng"
            u.uclass == SerpentMan |=> 200 -> "retr fp"
            u.uclass == FormlessSpawn |=> 300 -> "retr hh"
            u.uclass == Nyarlathotep && u.region.foes.goos(Hastur).%(_.health == Alive).any |=> 2000 -> "retr nya from hastur"
            u.uclass == Nyarlathotep && u.region.foes.goos.%(!_.is(Hastur)).%(_.health == Alive).any |=> -1500 -> "nya stays with goo"
            u.uclass == Nyarlathotep && u.region.ownGate && (u.region.allies.monsters.none || u.region.foes.goos.any) && u.region.foes.any |=> -500 -> "nya holds to gate"
            u.uclass == Nyarlathotep |=> 400 -> "nya moves anyway"
        }

        // BATTLE
        if (game.battle != null) {
            val battle = game.battle

            val opponent = battle.opponent(self)
            val allies = battle.units(self)
            val enemies = battle.units(opponent)
            val first = battle.attacker == self

            def ac = allies(Acolyte).num
            def ng = allies(Wizard).num
            def fp = allies(SerpentMan).num
            def hh = allies(FormlessSpawn).num
            def ihh = have(SeekAndDestroy).?(self.all(FormlessSpawn).diff(allies).num).|(0)
            def nya = allies.has(Nyarlathotep)

            def ec = enemies(Acolyte).num

            def fr = opponent.has(Frenzy).?(enemies(Acolyte).num).|(0)

            def dp = enemies(DeepOne).num
            def sh = enemies(Shoggoth).num
            def ss = enemies(Starspawn).num
            def cth = enemies.has(Cthulhu)

            def gh = enemies(Ghoul).num
            def fu = enemies(Fungi).num
            def dy = enemies(DarkYoung).num
            def shu = enemies.has(ShubNiggurath)

            def un = enemies(Undead).num
            def by = enemies(Byakhee).num
            def kiy = enemies.has(KingInYellow)
            def has = enemies.has(Hastur)

            // Battle
            def elim(battle : Battle, u : UnitFigure) {
                u.is(Nyarlathotep) && have(Emissary) && battle.units(battle.opponent(self)).goos.none |=> 1000 -> "emissary or what"
                u.is(Wizard) |=> 400 -> "elim ng"
                u.is(Acolyte) |=> 800 -> "elim acolyte"
                u.is(SerpentMan) |=> 200 -> "elim fp"
                u.is(FormlessSpawn) |=> 100 -> "elim hh"
                u.is(Nyarlathotep) |=> -1 -> "elim nya"
            }

            def retreat(battle : Battle, u : UnitFigure) {
                u.uclass == Acolyte |=> 600 -> "retr acolyte"
                u.gateKeeper && battle.region.allies.num - battle.side(self).opponent.rolls.%(_ == Pain).num >= 2 |=> -1000 -> "retr gate keeper"

                u.uclass == Wizard |=> 100 -> "retr ng"
                u.uclass == SerpentMan |=> 200 -> "retr fp"
                u.uclass == FormlessSpawn |=> 300 -> "retr hh"
                u.uclass == Nyarlathotep && u.region.foes.goos(Hastur).%(_.health == Alive).any |=> 2000 -> "retr nya from hastur"
                u.uclass == Nyarlathotep && u.region.foes.goos.%(!_.is(Hastur)).%(_.health == Alive).any |=> -1500 -> "nya stays with goo"
                u.uclass == Nyarlathotep && u.region.ownGate && (u.region.allies.monsters.none || u.region.foes.goos.any) && u.region.foes.any |=> -500 -> "nya holds to gate"
                u.uclass == Nyarlathotep |=> 400 -> "nya moves anyway"
            }


            a match {
                case DevourAction(_, u) =>
                    elim(battle, u)

                case AssignKillAction(_, _, _, u) =>
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
                        result = eval(MoveAction(u.faction, u.uclass, u.region, r))

                    true |=> (game.board.connected(r) ++ game.board.connected(r).flatMap(game.board.connected)).distinct.num -> "reachable regions"
                    r.ocean |=> -1 -> "ocean"
                    r.desecrated && r.of(YS).none |=> -2 -> "empty desecrated"

                case _ =>
            }
        }

        result.none |=> 0 -> "none"

        result.sortBy(v => -v.weight.abs)
    }

}
