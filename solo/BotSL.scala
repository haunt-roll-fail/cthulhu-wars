package cws

import hrf.colmat._

object BotSL extends BotX(implicit g => new GameEvaluationSL)

class GameEvaluationSL(implicit game : Game) extends GameEvaluation(SL)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

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

            case RevealESAction(_, es, false, _) if self.es != es =>
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

            case NeutralMonstersAction(_, _, _) =>
                true |=> -100000 -> "don't obtain loyalty cards (for now)"

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

                val hasturThreat = YS.has(Hastur) && YS.goo(Hastur).region.ownGate && YS.power == 0
                val kiyThreat = YS.has(KingInYellow) && YS.goo(KingInYellow).region.ownGate && YS.power == 0

                d.enemyGate && others./(_.aprxDoom).max > 15 && (d.owner.aprxDoom + 5 < others./(_.aprxDoom).max) && d.foes.goos.none |=> -1500 -> "bash leader instead"
                d.enemyGate && others./(_.aprxDoom).max > 23 && (d.owner.aprxDoom + 1 < others./(_.aprxDoom).max) && d.foes.goos.none |=> -1500 -> "bash leader instead"

                val kiyScreamCapture = YS.power > 1 && YS.has(KingInYellow) && YS.has(ScreamingDead)

                if (kiyScreamCapture && !self.allSB) {
                    val r = YS.goo(KingInYellow).region
                    o.allies.cultists.any && o.allies.cultists.num < 4 && r.near.%(_ == o).any && self.strength(o.allies.monsterly, YS) <= r.foes(Undead).num + 1 |=> -900 -> "kiy scream capture"
                    d.allies.cultists.any && o.allies.cultists.num < 4 && r.near.%(_ == d).any && d.ownStr <= r.foes(Undead).num + 1 |=> 900 -> "kiy scream capture"
                }

                power == 1 && o.ownGate && o.allies.num == 2 && others.%(_.power > 0).%(_.at(o).goos.any).none && o.foes.monsterly.%(_.faction.power > 0).any |=> -2000 -> "dont lose the gate"
                power == 1 && o.ownGate && o.allies.num == 2 && others.%(_.power > 0).%(_.at(o).goos.any).none && o.near.%(_.foes.monsterly.%(_.faction.power > 1).any).any |=> -1900 -> "dont lose the gate"
                power == 1 && o.ownGate && o.allies.num == 2 && others.%(_.power > 0).%(_.at(o).goos.any).none && o.near.%(_.near.%(_.foes.monsterly.%(_.faction.power > 2).any).any).any |=> -1800 -> "dont lose the gate"

                val canCaptureYS = self.all.cultists./(_.region).%(_.capturers.contains(YS)).none

                active.none && power > 1 && d.enemyGate && (d.owner != YS || canCaptureYS) && d.controllers.num == 1 && d.of(d.owner).goos.none |=> (5 * 100000 / 3) -> "go get a gate"
                active.none && power > 2 && d.enemyGate && (d.owner != YS || canCaptureYS) && d.controllers.num == 2 && d.of(d.owner).goos.none |=> (5 * 100000 / 4) -> "go get a gate"
                active.none && power > 3 && d.enemyGate && (d.owner != YS || canCaptureYS) && d.controllers.num == 3 && d.of(d.owner).goos.none |=> (5 * 100000 / 5) -> "go get a gate"

                d.enemyGate && self.gates.num < 4 && d.owner.power < power && (d.owner != YS || !hasturThreat) && d.controllers.num == 1 && d.controllers.monsterly.none && d.foes.goos.none && !d.owner.active && power > 1 |=> 999 -> "go get a gate"
                d.enemyGate && self.gates.num < 4 && d.owner.power < power && (d.owner != YS || !hasturThreat) && d.controllers.num == 2 && d.controllers.monsterly.none && d.foes.goos.none && !d.owner.active && power > 2 |=> 990 -> "go get a gate"
                d.enemyGate |=> -d.controllers.num -> "controllers"

                d.foes(Hastur).any |=> -3000 -> "no not hastur"
                o.foes(Hastur).any |=> 2800 -> "no not hastur"
                d.foes(Cthulhu).any && d.of(GC).num > d.allies.num |=> -2600 -> "no not cthulhu"
                o.foes(Cthulhu).any && o.of(GC).num > o.allies.num && GC.power > 0 |=> 2400 -> "no not cthulhu"
                o.ownGate && others.%(_.power > 0).%(_.at(o).monsterly.any).any && o.allies.monsterly.none |=> -1500 -> "protect gate"

                d.foes(Hastur).any && power > 1 && YS.power == 0 && 0 >= d.of(YS).num - 1 |=> 3333 -> "assassinate hastur"

                ((d.ownGate && d.allies.cultists.num <= YS.power) || ((self.allSB || YS.power == 0 || (YS.power == 0 && !d.desecrated        )) && power > 1)) && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= d.allies.num * 4 && YS.power >= 0 |=> 2000 -> "tsa defend from kiy"
                ((d.ownGate && d.allies.cultists.num <= BG.power) || ((self.allSB || BG.power == 0 || (BG.power == 0 && d.allies.cultists.any)) && power > 1)) && d.foes(ShubNiggurath).any                       && d.str(BG) <= d.allies.num * 4 && BG.power >= 0 |=> 1800 -> "tsa defend from shub"
                ((d.ownGate && d.allies.cultists.num <= GC.power) || ((self.allSB || GC.power == 0 || (GC.power == 0 && d.allies.cultists.any)) && power > 1)) && d.foes(Cthulhu).any                             && d.str(GC) <= (d.allies.num - 1) * 4 && GC.power >= 0 |=> 1600 -> "tsa defend from cthulhu"

                val shield = d.allies.num >= 2

                shield && d.foes(KingInYellow).any && d.foes(Hastur).none && !game.desecrated.contains(d) |=> 300 -> "tsa chase kiy"
                shield && d.foes(ShubNiggurath).any && d.enemyGate && d.owner == BG |=> 290 -> "tsa chase shub"
                shield && d.foes(Cthulhu).any && d.ocean.not |=> 280 -> "tsa chase cthulhu"

                active.none && power > 1 && d.enemyGate && (d.owner != YS || (!hasturThreat && !kiyThreat)) && d.controllers.num == 1 && d.of(d.owner).goos.none |=> (5 * 100000 / 3) -> "go get a gate"

                d.enemyGate && power > d.owner.power && d.foes.goos.none |=> 110 -> "enemy gate"
                d.foes.%(_.vulnerableG).any && power > 1 |=> 90 -> "maybe capture"
                d.freeGate |=> 75 -> "free gate"
                others.%(_.power > 0).any && d.ownGate |=> 50 -> "own gate"
                others.%(_.power > 0).any && d.allies.cultists.any |=> 25 -> "hug cultist"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

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
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d.near.all(_.empty) && d.near2.all(_.empty) |=> 999 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d.near.all(_.empty) && d.near2.all(_.of(YS).none) |=> 990 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d.near.all(_.empty) |=> 909 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d.near.all(_.foes.none) |=> 908 -> "crowded cultists 6 explore all friendly around"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d.near.all(_.of(YS).none) |=> 900 -> "crowded cultists 6 explore all no-ys around"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d == EarthMap4v35.Antarctica |=> 800 -> "crowded cultists 6 explore - antarctica"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d == EarthMap4v35.NorthAmerica |=> 750 -> "crowded cultists 6 explore - north america"
                o.allies.cultists.num == 6 && !self.all.monsterly.none && d.empty && d == EarthMap4v35.Arabia |=> 700 -> "crowded cultists 6 explore - arabia"

                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 400 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && (active.none || d.capturers.%(f => f.power > 0 || f.has(Passion)).none) |=> 300 -> "ic temporary free gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.ownGate |=> 60 -> "flee from capture to own gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.allies.monsterly.any |=> 59 -> "flee from capture to monster"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.empty |=> 58 -> "flee from capture"

                others.%(f => f.power > 0 || f.has(Passion)).%(f => o.of(f).goos.none && o.of(f).monsterly.none).none |=> -300 -> "why move"

                d.foes.goos.any && d.allies.goos.none |=> -250 -> "dont move to enemy goos"
                o.capturers.%(_.power > 0).any && d.capturers.none && o.allies.cultists.num == 1 |=> 220 -> "move from capture"
                o.ownGate && o.allies.cultists.num == 2 |=> -210 -> "move from own gate"
                o.ownGate && o.allies.cultists.num == 3 |=> -110 -> "move from own gate"
                d.ownGate && d.allies.cultists.num >= 2 |=> -210 -> "move to own gate"
                !o.ownGate && d.allies.goos.any |=> 200 -> "move to goo"
                d.allies.cultists.any |=> -90 -> "move to cultists"
                d.allies.monsterly.any |=> 80 -> "move to monsters"
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

                o.ownGate && o.allies.goos.none && o.allies.monsterly.num == 1 && others.%(_.power > 0).%(_.at(o).goos.none).%(_.at(o).monsterly.any).any |=> -2000 -> "dont abandon gate"

                d.ownGate && d.capturers.any && d.capturers.%(_.power > 0).any && d.capturers.%(_.power > 0).%(_.at(d).goos.any).none |=> 1100 -> "protect gate"

                o.ownGate && u.friends.goos.none && u.friends.monsterly.none && u.friends.cultists.num == 1 |=> -600 -> "dont leave lone cultist on gate"

                o.foes.goos.any |=> -500 -> "stay with enemy goos"

                val canCaptureYS = self.all.cultists./(_.region).%(_.capturers.contains(YS)).none
                val canCaptureWW = !WW.has(Ithaqua)

                d.allies.none && d.foes.cultists.%(_.vulnerableM).map(_.faction).%(f => !f.active && (f != YS || canCaptureYS) && (f != WW || canCaptureWW)).any |=> 250 -> "go for capture"

                d.ownGate && canSummon(uc) && !game.hasMoved(self) |=> -1200 -> "why move if can summon"

                o.allies.cultists.none && d.ownGate && d.allies.monsterly.none |=> 50 -> "move"

                o.allies.cultists.none && d.enemyGate && d.allies.none |=> 2 -> "move"

                active.none && power > 1 && u.is(Wizard) && d.allies.goos.any && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) > d.allies(Acolyte).num + d.allies(Wizard).num |=> 1100000 -> "shield from kiy"

                u.is(SerpentMan) |=> -1 -> "sm stay"
                u.is(FormlessSpawn) |=> 1 -> "fs move"

            case AttackAction(_, r, f) if f.neutral =>
                true |=> -100000 -> "don't attack uncontrolled filth (for now)"

            case FromBelowAttackAction(_, r, f) if f.neutral =>
                true |=> -100000 -> "don't use from below (for now)"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val enemyStr = f.strength(foes, self)
                val ownStr = adjustedOwnStrengthForCosmicUnity(self.strength(allies, f), allies, foes, opponent = f)

                ownStr >= 6 && need(Roll6DiceInBattle) && foes(Cthulhu).none |=> 1000 -> "get spellbook"
                ownStr >= 6 && need(Roll6DiceInBattle) |=> 1000000 -> "get spellbook"

                val igh = BG.has(Necrophagy).?(f.all(Ghoul).diff(foes).num).|(0)

                var ac = allies(Acolyte).num
                var wz = allies(Wizard).num
                var sm = allies(SerpentMan).num
                var fs = allies(FormlessSpawn).num
                val tsa = allies.has(Tsathoggua)

                f.has(Invisibility) &&            foes(FlyingPolyp).num >= foes.num    |=> -10000000 -> "polyp can invis all"
                f.has(Invisibility) && tsa.not && foes(FlyingPolyp).num >= allies.num  |=> -10000000 -> "polyp can invis all"

                r.ownGate && allies.num < 2 + igh |=> -1000 -> "ghouls will knock off the gate"

                var eby = foes.has(Byatis)
                var eab = foes.has(Abhoth)
                var eny = foes(Nyogtha).num
                var eght = foes(Ghast).num
                var egug = foes(Gug).num
                var esht = foes(Shantak).num
                var esv = foes(StarVampire).num
                var efi = if (eab) foes(Filth).num else 0

                f match {
                    case GC =>
                        // None of the earlier conditions here could trigger.
                        0 -> "todo"

                    case BG =>
                        def ec = foes(Acolyte).num
                        def fr = (f == BG && f.has(Frenzy)).?(ec).|(0)
                        def gh = foes(Ghoul).num
                        def fu = foes(Fungi).num
                        def dy = foes(DarkYoung).num
                        def shu = foes.has(ShubNiggurath)

                        gh > 0 && ec + fu + dy + eght + egug + esht + esv + eby.??(4) + eab.??(efi) + eny == 0 && BG.has(ShubNiggurath) && BG.has(ThousandYoung) && BG.power > 0 |=> -1000 -> "dont fight free ghouls"

                    case YS =>
                        def ec = foes(Acolyte).num
                        def un = foes(Undead).num
                        def by = foes(Byakhee).num
                        def kiy = foes.has(KingInYellow)
                        def has = foes.has(Hastur)

                        r.ownGate && kiy && (ownStr > enemyStr || ownStr >= foes.num * 2) && !(self.allSB && power > 1) |=> 4444 -> "chase kiy away"

                        r.ownGate && has && un <= 1 && by == 0 && eght == 0 && egug == 0 && esht == 0 && esv == 0 && ownStr > un |=> 1222 -> "chase has away"

                        un == 1 && by == 0 && eght == 0 && egug == 0 && esht == 0 && esv == 0 && !kiy && !has && (ac == 0 || !YS.has(Zingaya)) && !game.desecrated.contains(r) |=> -1000 -> "dont fight lone undead on undesecrated"

                        has && 0 >= ec + un + by + eght + egug + esht + esv + eby.??(4) + eab.??(efi) + eny && ownStr > 4 |=> 3334 -> "assassinate has"

                        f.power == 0 && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any && f.has(Passion) && ec > 1 |=> -1000 -> "dont attack if passion allows reverse capture"

                    case CC =>
                        0 -> "todo"

                    case WW =>
                        0 -> "todo"

                    case OW =>
                        0 -> "todo"

                    case AN =>
                        allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                        AN.has(Extinction) && foes.num == 1 && foes(Yothan).any && ((tsa && allies.num >= 3 && ownStr >= 6) || (allies.goos.none && ownStr >= 6)) |=> 1000 -> "attack lone extinct yothan"
                }

                f.has(Abhoth) && enemyStr == 0 && ownStr >= foes(Filth).num * 2 |=> 200 -> "get rid of filth"
                f.has(Abhoth) && f.has(TheBrood) && enemyStr == 0 && ownStr >= foes(Filth).num * 2 |=> 400 -> "get rid of brood filth"

                game.acted || game.battled.any |=> -1000 -> "unlimited battle drains power"

                (game.acted || game.battled.any) && r.enemyGate && foes.goos.none |=> -10000 -> "unlimited battle drains power"

                enemyStr == 0 && r.gateOf(f) && ownStr >= 2 |=> (300 + ownStr) -> "enemy rolls none at gate"
                enemyStr == 0 && f.power > 0 && r.freeGate && others.%(_ != f).%(_.at(r).any).none |=> (300 + ownStr) -> "enemy rolls none at free gate"

                active.none && (game.acted || game.battled.any) |=> -1000000 -> "unlimited battle drains power"

                r.enemyGate && r.gateOf(f) && enemyStr <= ownStr |=> (5 + (ownStr - enemyStr)) -> "attack at gate"

            case FromBelowAttackAction(_, r, f) =>
                true |=> -100000 -> "don't use from below (for now)"

            case CaptureAction(_, r, f, _) =>
                val safe = active.none
                safe && !r.gateOf(f) |=> (1 * 100000 / 1) -> "safe capture"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 1 && power > 0                    |=> (2 * 100000 / 1) -> "safe capture and open gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 2 && power > 1 && !f.has(Passion) |=> (3 * 100000 / 2) -> "safe capture and half gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 3 && power > 2 && !f.has(Passion) |=> (4 * 100000 / 3) -> "safe capture and third of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 4 && power > 3 && !f.has(Passion) |=> (5 * 100000 / 4) -> "safe capture and fourth of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 5 && power > 4 && !f.has(Passion) |=> (6 * 100000 / 5) -> "safe capture and fifth of gate"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 6 && power > 5 && !f.has(Passion) |=> (7 * 100000 / 6) -> "safe capture and sixth of gate"

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
                YS.has(KingInYellow) && YS.power > 1 && game.board.connected(YS.goo(KingInYellow).region).contains(r) |=> -1000 -> "kiy is near"

                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -700 -> "cthulhu has dreams"

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

            case RecruitAction(_, HighPriest, r) =>
                true |=> -100000 -> "inactivated"

            case SummonAction(_, Wizard, r) =>
                // Defense against WW setting up 2nd gate at opposite pole. Only worth it if adjacent to location for new gate
                r.distanceToWWOppPole == 1 && wwLoneCultistPolarGate(2) && r.allies.monsterly.none |=> 1500 ->"cheap summon to stop WW 2nd gate at opp pole"

                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                true |=> (-r.allies.monsterly.num) -> "monsters count"

                power == 1 && r.allies.num == 1 && others.%(_.power > 0).%(_.at(r).goos.any).none && r.foes.monsterly.%(_.faction.power > 0).any |=> 2000 -> "dont lose the gate"
                power == 1 && r.allies.num == 1 && others.%(_.power > 0).%(_.at(r).goos.any).none && r.near.%(_.foes.monsterly.%(_.faction.power > 1).any).any |=> 1900 -> "dont lose the gate"
                power == 1 && r.allies.num == 1 && others.%(_.power > 0).%(_.at(r).goos.any).none && r.near.%(_.near.%(_.foes.monsterly.%(_.faction.power > 2).any).any).any |=> 1800 -> "dont lose the gate"

                power > 1 && r.allies.goos.any && r.foes.goos(Hastur).none && others.%(_.at(r).goos.any).%(_.power == 0).any |=> 850 -> "shield"

                r.allies(Wizard).none |=> 20 -> "wz cheap"
                r.allies(Wizard).any |=> 10 -> "wz cheap"
                r.foes(KingInYellow).any |=> -150 -> "wz bad against kiy"
                r.near.%(_.foes(KingInYellow).any).any |=> -110 -> "wz bad against kiy"
                r.near.%(n => n.noGate && n.of(YS).any && YS.power > 5).any |=> -100 -> "wz bad against kiy"
                YS.has(KingInYellow) && YS.power >= 2 && r.allies.monsterly.num == r.allies(Wizard).num && r.allies.goos.none |=> -90 -> "wz bad against kiy"
                YS.power >= 6 |=> -80 -> "wz bad against kiy"

                self.pool(Wizard).num == 1 && self.pool(SerpentMan).num > 0 && power == 2 |=> -500 -> "summon sm instead"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.capturers.any && r.capturers.%(_.at(r).goos.any).none |=> 250 -> "prevent later capture"
                r.foes.cultists.%(_.vulnerableM).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsterly.none |=> 150 -> "lone cultist"
                r.allies.monsterly.none |=> 140 -> "no monsters"
                r.allies.cultists.num > 2 && r.allies.monsterly.any |=> 130 -> "many cultists"
                r.allies.goos.any |=> 100 -> "summon to tsa"

                true |=> 3000 -> "wizards out"

            case SummonAction(_, SerpentMan, r) =>
                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                true |=> (-r.allies.monsterly.num) -> "monsters count"

                power > 2 && r.allies.goos.any && r.foes.goos(Hastur).none && others.%(_.at(r).goos.any).%(_.power == 0).any |=> 800 -> "shield"

                true |=> 25 -> "dfb useful"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.foes(KingInYellow).any |=> 230 -> "fight kiy"
                r.foes.cultists.%(_.vulnerableM).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsterly.none |=> 150 -> "lone cultist"
                r.allies.monsterly.none |=> 140 -> "no monsters"
                r.allies.cultists.num > 2 && r.allies.monsterly.any |=> 130 -> "many cultists"
                r.allies.goos.any |=> 100 -> "summon to tsa"

            case SummonAction(_, FormlessSpawn, r) =>
                if (have(Burrow) && self.all.%(_.has(Moved)).num == 1) {
                    true |=> 1000 -> "move free"
                    active.none |=> 5000000 -> "move free"
                }

                true |=> (-r.allies.monsterly.num) -> "monsters count"

                true |=> 10 -> "fs expensive"

                r.foes(KingInYellow).any && power > 3 |=> -150 -> "fs bad against kiy" // Is it?

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.foes.cultists.%(_.vulnerableG).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsterly.none |=> 150 -> "lone cultist"
                r.allies.monsterly.none |=> 140 -> "no monsters"
                r.allies.cultists.num > 2 && r.allies.monsterly.any |=> 130 -> "many cultists"
                r.allies.goos.any |=> 100 -> "summon to tsa"

            case AwakenAction(_, _, r, _) =>
                numSB >= 5 && need(AwakenTsathoggua) |=> 5000 -> "need tsathoggua"
                r.foes.has(Hastur) |=> -3000 -> "hastur is scary"
                numSB < 5 && numSB > 3 && need(AwakenTsathoggua) |=> 2500 -> "need tsathoggua"
                power > 8 |=> 2000 -> "yes awaken"
                power == 8 |=> 1800 -> "maybe awaken"
                r.foes(KingInYellow).any |=> 400 -> "awaken to battle kiy"
                r.foes.goos.any |=> 300 -> "awaken to battle"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsterly.any |=> 100 -> "awaken to defend"
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

            case CaptureMonsterAction(_, r, f) if f.neutral =>
                true |=> -100000 -> "don't capture uncontrolled filth (for now)"

            case CaptureMonsterAction(_, r, f) =>
                val uc = f.at(r).monsters.minBy(_.uclass.cost)

                uc.uclass.cost == 1 |=> 600 -> "capture monster 1"
                // uc.uclass.cost == 2 |=> 900 -> "capture monster 2"
                uc.uclass.cost == 2 |=> 1100 -> "capture monster 2"
                uc.uclass.cost == 3 |=> 1700 -> "capture monster 3"

                uc.faction.active |=> 100 -> "active"

            case AncientSorceryUnitAction(_, Immortal, r, _) =>
                !have(Tsathoggua) && need(AwakenTsathoggua) && have(FormlessSpawn) && power > 8 |=> 800 -> "get es"
                !have(Tsathoggua) && !need(AwakenTsathoggua) && have(FormlessSpawn) && power > 4 |=> 2400 -> "resummon cheap"
                true |=> 100 -> "move power"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsterly.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"
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
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsterly.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"

            case AncientSorceryUnitAction(_, Flight, r, _) =>
                active.none && power > 1 |=> 100000 -> "move safe options"

                power > 1 |=> 300 -> "sleepflying"
                power > 5 && have(Burrow) |=> 1300 -> "burrowflying"
                power > 5 && have(Tsathoggua) && game.board.regions.%(r => r.allies.cultists.any && r.allies.goos.none && r.near.%(_.allies.goos.any).none).any |=> 1200 -> "fly back cultists"
                true |=> 100 -> "move power"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsterly.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"
                r.allies.num == 1 && r.desecrated && have(Feast) |=> -200 -> "dont leave feast"

            case AncientSorceryUnitAction(_, Fertility, r, _) =>
                true |=> 100 -> "move power"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 0).%(_.at(r).goos.none).%(_.at(r).monsterly.any).any |=> -2000 -> "dont abandon gate"
                r.ownGate && r.allies.goos.none && r.allies.monsterly.num == 1 && others.%(_.power > 1).any |=> -1000 -> "dont open gate"
                r.allies.num == 1 && r.desecrated && have(Feast) |=> -200 -> "dont leave feast"

            // TODO: Add AncientSorceryUnitAction for OW:s BeyondOne?
            // TODO: Add AncientSorceryUnitAction for AN:s Dematerialization?

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
                r.allies.goos.none && r.allies.monsterly.none && r.foes.monsterly.any |=> 1700 -> "save gate from monsters"

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
                u.monsterly && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monsterly && u.friends.cultists.any && u.friends.monsterly.none && r.foes.monsterly./(_.faction).%(_ != BG).%(_.power > 0).any |=> -200 -> "dont sent temp defender"
                u.monsterly && o.allies.cultists.any && o.allies.monsterly.none |=> 50 -> "protect cultist"

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
                c.friends.goos.any |=> -500 -> "tsa huggers"
                c.friends.cultists.any |=> 400 -> "cultist friends"
                c.friends.monsterly.none |=> 300 -> "cultist friends"

            case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case MainDoneAction(_) =>
                game.battled.any |=> -1000 -> "unlimited battle drains power"
                true |=> 500 -> "main done"

            case _ =>
        }

        // BATTLE
        if (game.battle.any) {
            if (game.battle./~(_.sides).has(self).not) {
                a match {
                    case _ =>
                        true |=> 1000 -> "todo"
                }
            }
            else {
                implicit val battle = game.battle.get

                val opponent = self.opponent
                val allies = self.forces
                val enemies = self.opponent.forces
                val attacking = battle.attacker == self

                def elim(u : UnitFigure) {
                    u.is(Wizard) |=> 400 -> "elim wz"
                    u.is(Acolyte) |=> 800 -> "elim acolyte"
                    u.is(SerpentMan) |=> 200 -> "elim sm"
                    u.is(FormlessSpawn) |=> 100 -> "elim fs"
                    u.is(Tsathoggua) |=> -1 -> "elim tsa"
                }

                def retreat(u : UnitFigure) {
                    u.uclass == Acolyte |=> 600 -> "retr acolyte"
                    u.gateKeeper && battle.region.allies.num - opponent.rolls.%(_ == Pain).num >= 2 |=> -1000 -> "retr gate keeper"

                    u.uclass == Wizard |=> 100 -> "retr wz"
                    u.uclass == SerpentMan |=> 200 -> "retr sm"
                    u.uclass == FormlessSpawn |=> 300 -> "retr fs"
                    u.is(Tsathoggua) |=> -100000 -> "retr tsa"
                }

                a match {
                    case DevourAction(_, u) =>
                        elim(u)

                    case AssignKillAction(_, _, _, u) =>
                        elim(u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(u)

                    case EliminateNoWayAction(_, u) =>
                        elim(u)

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                        u.cultist && r.foes.goos.any |=> -1500 -> "dont send cultist to feed enemy goo"
                        u.cultist && r.allies.monsterly.any |=> 1000 -> "send cultist to be protected by monsters"
                        u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                        u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                        u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                        u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                        u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                        u.monsterly && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                        u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                        u.monsterly && r.foes.%(_.vulnerableM).any && !r.foes.goos.any |=> 1000 -> "send monster to capture"
                        u.goo && r.foes.%(_.vulnerableG).any |=> 1000 -> "send goo to capture"

                        u.monsterly && r.allies.goos.any && r.foes.goos.any |=> 1500 -> "send monster to friendly goo with enemy goo"
                        u.monsterly && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                        u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                        u.monsterly && r.ownGate |=> 400 -> "send monster to own gate"
                        u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                        u.monsterly && r.freeGate |=> 300 -> "send monster to free gate"
                        u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                        u.monsterly && r.enemyGate |=> 300 -> "send monster to enemy gate"
                        u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                        if (u.goo)
                            result = eval(MoveAction(u.faction, u.uclass, u.region, r))

                        true |=> (game.board.connected(r) ++ game.board.connected(r).flatMap(game.board.connected)).distinct.num -> "reachable regions"
                        r.ocean |=> -1 -> "ocean"
                        r.desecrated && r.of(YS).none |=> -2 -> "empty desecrated"

                    case _ =>
                }
            }
        }

        result.none |=> 0 -> "none"

        result.sortBy(v => -v.weight.abs)
    }

}
