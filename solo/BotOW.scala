package cws

import hrf.colmat._

object BotOW extends BotX(implicit g => new GameEvaluationOW)

class GameEvaluationOW(implicit game : Game) extends GameEvaluation(OW)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val impunity = others.active.none || (self.goos.any && others.active.%(_.goos.any).none && others.%(_.power > power).none)

        def canStrikeCC(r : Region) = (r.foes.has(Nyarlathotep) && CC.power > 0) || (CC.power > 1 && CC.allSB && r.near012.%(_.foes(Nyarlathotep).any).any)
        def canStrikeGC(r : Region) = (r.foes.has(Cthulhu) && GC.power > 0) || (GC.power > 1 && GC.allSB && r.near.%(_.foes(Cthulhu).any).any) || (GC.power > 0 && GC.allSB && GC.at(GC.deep).any)

        def checkAttack(r : Region, f : Faction, allies : $[UnitFigure], foes : $[UnitFigure], d : Int) {
            val enemyStr = f.strength(foes, self)
            val ownStr = adjustedOwnStrengthForCosmicUnity(self.strength(allies, f), allies, foes, opponent = f)

            val igh = others.%(_.has(Necrophagy))./(_.all(Ghoul).diff(foes).num).sum

            var ac = allies(Acolyte).num
            var mu = allies(Mutant).num
            var ab = allies(Abomination).num
            var sp = allies(SpawnOW).num
            val ygs = allies.has(YogSothoth)

            var eby = foes.has(Byatis)
            var eab = foes.has(Abhoth)
            var eny = foes(Nyogtha).num
            var egug = foes(Gug).num
            var esht = foes(Shantak).num
            var esv = foes(StarVampire).num
            var efi = eab.??(foes(Filth).num)

            f match {
                case GC =>
                    var ec = foes(Acolyte).num
                    var dp = foes(DeepOne).num
                    var sh = foes(Shoggoth).num
                    var ss = foes(Starspawn).num
                    var cth = foes.has(Cthulhu)

                    var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(6) + egug * 3 + esht * 2 + esv + eby.??(4) + eab.??(efi) + eny
                    var shield = ac + mu + ab - 1

                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                    cth && ygs && f.power < 4 && !f.needs(KillDevour1) && !f.needs(KillDevour2) |=> 11000/d -> "attack cth no respawn"
                    cth && ygs && f.power == 0 && f.needs(KillDevour1) && f.needs(KillDevour2) |=> 11000/d -> "attack cth no power"
                    cth && ygs && f.power == 0 && f.needs(OceanGates) |=> 11000/d -> "attack cth no gates"

                    0 -> "todo"

                case BG =>
                    0 -> "todo"

                case CC =>
                    var ec = foes(Acolyte).num
                    var ng = foes(Nightgaunt).num
                    var fp = foes(FlyingPolyp).num
                    var hh = foes(HuntingHorror).num
                    var nya = foes.has(Nyarlathotep)

                    var abd = f.has(Abduct).??(ng)

                    while (abd > 0) {
                        abd -= 1
                        if (ac > 0)
                            ac -= 1
                        else
                        if (mu > 0)
                            mu -= 1
                        else
                        if (ab > 0)
                            ab -= 1
                    }

                    var inv = f.has(Invisibility).??(fp)

                    while (inv > 0) {
                        inv -= 1
                        if (ab > 0)
                            ab -= 1
                        else
                        if (mu > 0)
                            mu -= 1
                        else
                        if (ac > 0)
                            ac -= 1
                    }

                    val shield = ac + mu + ab

                    val ownStr = adjustedOwnStrengthForCosmicUnity((mu > 0).??(mu - 1) + (ab > 0).??(ab + 1) + ygs.??(game.ritualCost), allies, foes, opponent = f)

                    var ihh = f.has(SeekAndDestroy).??(f.all(HuntingHorror).diff(foes).num)

                    var enemyStr = fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB) + egug * 3 + esht * 2 + esv + eby.??(4) + eab.??(efi) + eny

                    val enough = shield * 5 > enemyStr * 4

                    f.has(Invisibility) && fp == foes.num |=> -1000000 -> "invis"
                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"
                    sp > 0 && nya && enough && (ownStr > 2 + (foes.num - 1) * 6 || (!f.active && ownStr > enemyStr && power > 1)) |=> 13000/d -> "sp attack nya"

                    ygs && ec < foes.num |=> 12000/d -> "attack cc"
                    ygs && nya |=> 19000/d -> "attack nya"
                    hh == foes.num && enough && ownStr > 3 |=> 11000/d -> "attack hh"
                    hh + fp == foes.num && enough && ownStr > 4 |=> 10000/d -> "attack hhfp"

                    0 -> "todo"

                case SL =>
                    0 -> "todo"

                case YS =>
                    0 -> "todo"

                case WW =>
                    0 -> "todo"

                case AN =>
                    allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                    AN.has(Extinction) && foes.num == 1 && foes(Yothan).any && ((ygs && allies.num >= 3 && ownStr >= 6) || (allies.goos.none && ownStr >= 6)) |=> 1000 -> "attack lone extinct yothan"
            }
        }

        a match {
            case StartingRegionAction(_, r) =>
                r.near.%(_.of(YS).any).any |=> -10000 -> "ys non-empty near"
                r.near2.%!(_.empty).%(_.of(YS).any).any |=> -9000 -> "ys non-empty near 2"

                r.empty |=> 10000 -> "empty"
                r.near.%!(_.empty).none |=> 1000 -> "empty near"
                r.near2.%!(_.empty).none |=> 100 -> "empty near 2"
                r.near2.%!(_.empty).num == 1 |=> 90 -> "1 non-empty near 2"
                r.near2.%!(_.empty).num == 2 |=> 80 -> "2 non-empty near 2"
                game.factions.contains(GC) && r.ocean |=> -1 -> "gc no ocean"
                game.factions.contains(WW) && game.board.starting(WW).has(r) |=> -1 -> "ww no pole"

            case FirstPlayerAction(_, f) =>
                f == self && game.board.regions.%(_.allies.goos.any).%(_.foes.goos.any).any |=> 100 -> "play first goos together"
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

                (game.factions.indexOf(f) - game.factions.indexOf(self)).abs == 2 |=> 10 -> "stall opposite"
                f == CC && !CC.allSB |=> 1 -> "cc first"
                CC.allSB |=> 1000 -> "first, cc allsb"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            case SpellbookAction(_, sb, _) => sb match {
                case MillionFavoredOnes =>
                    true |=> 1000 -> "must have if have sp and ygs"
                case TheyBreakThrough =>
                    need(UnitsAtEnemyGates) |=> 900 -> "too good"
                case DreadCurse =>
                    true |=> 800 -> "very good"
                case DragonAscending =>
                    true |=> 700 -> "just good"
                case DragonDescending =>
                    true |=> 600 -> "almost good"
                case ChannelPower =>
                    true |=> 500 -> "not good"
                case _ =>
                    true |=> -1000 -> "unknown"
            }

            case RitualAction(_, cost, _) =>
                instantDeathNow |=> 10000 -> "instant death now"
                instantDeathNext && self.allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                self.allSB && self.realDoom + maxDoomGain >= 30 |=> 1100 -> "can break 30, and all SB"
                instantDeathNext && !self.allSB && others.%(_.allSB).any |=> -1000 -> "don't ritual if ID next and not all SB"
                instantDeathNext && !self.allSB && others.all(!_.allSB) && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                !self.allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                !self.allSB && self.doom + self.gates.num < 30 && self.realDoom <= 29 && self.realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"
                self.numSB >= 5 && cost * 2 <= power |=> 800 -> "5 SB and less than half available power"
                self.numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "very sweet deal"
                self.numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                self.numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                cost == 5 |=> 100 -> "ritual first"
                self.pool.goos.any |=> -200 -> "not all goos in play"
                true |=> -250 -> "don't ritual unless have reasons"

                !self.allSB |=> -1000 -> "spellbooks first"

            case NeutralMonstersAction(_, _) =>
                true |=> -100000 -> "don't obtain loyalty cards (for now)"

            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                impunity |=> 1000000 -> "impunity move done"
                true |=> 500 -> "move done"

            case MoveAction(_, Abomination, o, d) =>
                true |=> -100 -> "abomination dont move"
                o.ownGate && o.foes.monsterly.active.any && o.allies.monsterly.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, Mutant, o, d) =>
                o.allies(SpawnOW).any |=> -200 -> "mutant dont leave sp"
                o.ownGate && o.foes.monsterly.active.any && o.allies.monsterly.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, SpawnOW, o, d) =>
                true |=> -100 -> "spawns dont move"
                o.ownGate && o.foes.monsterly.active.any && o.allies.monsterly.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, YogSothoth, o, d) =>
                true |=> -1000 -> "stay"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

                need(GooMeetsGoo) && d.foes.goos.any && d.foes.goos.active.none |=> 100000 -> "meet up"
                need(GooMeetsGoo) && d.foes.goos.any |=> 3000 -> "scared meet"

            case MoveAction(_, Acolyte, o, d) =>
                val unemployed = !((o.ownGate || o.freeGate) && o.allies.cultists.num == 1)
                val safe = d.allies.goos.any || (d.foes.goos.active.none && (d.allies.monsterly.any || d.foes.monsterly.active.none))

                impunity && unemployed && safe && power > 3 && d.noGate && d.allies.cultists.none && (d.allies.any || d.foes.active.none) |=> 40000 -> "impunity move build gate"

                impunity && unemployed && safe && d.freeGate |=> 200000 -> "impunity move take gate"

                impunity && unemployed && safe && power > 1 && o.allies.cultists.num > o.capturers.active.num + 1 && d.near.%(n => n.freeGate && n.capturers.none && n.allies.cultists.none).any && d.allies.cultists.%(!_.gateKeeper).none && d.capturers.active.none && active.none |=> 105000 -> "impunity move take gate 2 step"

                val u = self.at(o, Acolyte).%!(_.has(Moved)).head

                !unemployed && (!u.capturable || u.enemies.goos.none) |=> -120 -> "dont move gatekeeper"
                self.pool.cultists.any && d.allies.any && !self.all.%(_.has(Moved)).any |=> -1000 -> "why move if can recruit for same"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d == EarthMap4v35.NorthAsia |=> 800 -> "crowded cultists - north asia"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d.ocean.not |=> 800 -> "crowded cultists 6 explore"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d.ocean |=> 790 -> "crowded cultists 6 explore ocean"

                unemployed && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.allies.goos.any && d.foes.goos.active.none |=> 5500 -> "ic free gate"
                unemployed && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 1400 -> "ic free gate"
                unemployed && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && d.capturers.%(_.power > 0).none |=> 1300 -> "ic temporary free gate"

                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.ownGate |=> 60 -> "flee from capture to own gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.allies.monsterly.any |=> 59 -> "flee from capture to monster"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.empty |=> 58 -> "flee from capture"

                d.capturers.%(_.power > 0).any && o.capturers.none |=> -200 -> "dont go to be captured"

                o.capturers.%(_.power > 0).any && d.capturers.none |=> 10 -> "move from capture"

                o.allies.goos.any && o.allies.cultists.num < 5 |=> -310 -> "move from own goo"
                d.allies.goos.any && power == 1 && d.allies.cultists.none |=> 100 -> "move to goo first"
                d.allies.goos.any && power == 1 && d.allies.cultists.any |=> 50 -> "move to goo"
                d.ownGate && d.allies.cultists.num == 1 |=> 190 -> "move to own gate with single cultist"
                d.ownGate && d.allies.cultists.num == 2 |=> 180 -> "move to own gate with double cultist"
                d.allies.cultists.num + 1 >= o.allies.cultists.num |=> -3000 -> "dont group cultists"
                o.allies.cultists.num > 1 |=> 30 -> "ungroup cultists"
                o.allies.cultists.num > 2 |=> 40 -> "ungroup cultists"
                o.allies.cultists.num > 3 |=> 50 -> "ungroup cultists"
                o.allies.cultists.num > 4 |=> 60 -> "ungroup cultists"
                o.allies.cultists.num > 5 |=> 70 -> "ungroup cultists"

                power > 4 && need(EightGates) && !u.gateKeeper && o.gate && d.noGate && d.foes.monsterly.none && d.foes.goos.none && d.allies.none |=> 300 -> "go more gates 8"

                power > 4 && (need(TwelveGates) || need(TenGates)) && need(AwakenYogSothoth) && !u.gateKeeper && o.gate && d.noGate && d.foes.monsterly.none && d.foes.goos.none && d.allies.none |=> 200 -> "go more gates 12 no ygs"

                power > 4 && (need(TwelveGates) || need(TenGates)) && !need(AwakenYogSothoth) && !u.gateKeeper && o.gate && d.noGate && d.foes.monsterly.none && d.foes.goos.none && d.allies.none |=> 300 -> "go more gates 12"

                power > 1 && o.allies.cultists.num > o.capturers.active.num + 1 && d.near.%(n => n.freeGate && n.capturers.none && n.allies.cultists.none).any && d.allies.cultists.%(!_.gateKeeper).none && d.capturers.none && active.none |=> 450 -> "ic free gate 2 steps"
                power > 1 && o.allies.cultists.num > o.capturers.active.num + 1 && d.near.%(n => n.freeGate && n.allies.goos.any && n.foes.goos.none && n.allies.cultists.none).any && d.allies.cultists.%(!_.gateKeeper).none && d.capturers.none && self.all.%(_.has(Moved)).none |=> 1100 -> "ic free gate and goo 2 steps"

            case AttackAction(_, r, f, _) if f.neutral =>
                true |=> -100000 -> "don't attack uncontrolled filth (for now)"

            case AttackAction(_, r, f, _) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val enemyStr = f.strength(foes, self)
                val ownStr = adjustedOwnStrengthForCosmicUnity(self.strength(allies, f), allies, foes, opponent = f)

                val upg1 = allies(SpawnOW).any && self.pool(Mutant).num > 2
                val upg2 = allies(Abomination).any && self.pool(SpawnOW).any
                val upg3 = allies(Mutant).any && self.pool(Abomination).any
                val upg = self.has(MillionFavoredOnes) && (upg1 || upg2 || upg3)

                checkAttack(r, f, allies, foes, 1)

                f.has(Abhoth) && enemyStr == 0 && ownStr >= foes(Filth).num * 2 |=> 200 -> "get rid of filth"
                f.has(Abhoth) && f.has(TheBrood) && enemyStr == 0 && ownStr >= foes(Filth).num * 2 |=> 400 -> "get rid of brood filth"

                upg && f.strength(foes, self) == 0 |=> 1900 -> "zero attack"
                upg && f.strength(foes, self) == 1 && allies./(_.uclass.cost).min < 3 |=> 1500 -> "one attack"
                upg && f.strength(foes, self) == 2 && allies./(_.uclass.cost).min == 2 |=> 1500 -> "one attack"

                allies.cultists.any && foes.goos.any |=> 1200 -> "battle to prevent capture"

            case CaptureAction(_, r, f, _, _) =>
                val safe = active.%(f => f.strength(f.at(r).diff(f.at(r).cultists.take(1)), self) > r.allies.num).none

                safe && impunity && !r.enemyGate |=> 105000 -> "impunity capture"
                safe && impunity && r.enemyGate && f != r.owner  |=> 110000 -> "impunity capture"
                safe && impunity && r.enemyGate && f == r.owner && r.controllers.num >= 6 |=> 120000 -> "impunity capture"
                safe && impunity && r.enemyGate && f == r.owner && r.controllers.num == 5 |=> 140000 -> "impunity capture"
                safe && impunity && r.enemyGate && f == r.owner && r.controllers.num == 4 |=> 160000 -> "impunity capture"
                safe && impunity && r.enemyGate && f == r.owner && r.controllers.num == 3 |=> 180000 -> "impunity capture"
                safe && impunity && r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 200000 -> "impunity capture"
                safe && impunity && r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 220000 -> "impunity capture"

                safe && r.enemyGate && f == r.owner && r.controllers.num == 1 && r.allies.cultists.none && r.foes.%(_.canControlGate).num > 1 |=> -700 -> "give gate away"
                safe && r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 4000 -> "capture and open gate"
                safe && r.enemyGate && f == r.owner && r.controllers.num == 1 && f.power > 0 |=> 6000 -> "capture and open gate"
                safe && r.enemyGate && f == r.owner && f.active && power > active./(_.power).max |=> 6000 -> "capture and still lead on power"
                safe && r.enemyGate && f == r.owner && r.controllers.num == 1 && f.has(CursedSlumber) && f.power > 0 && f.gates.%(_.glyph == Slumber).none |=> 7000 -> "capture and prevent slumber"
                safe && r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 3000 -> "capture and nearly open gate"

                true |=> 2000 -> "capture"
                r.enemyGate |=> 100 -> "enemy gate"

            case BuildGateAction(_, r) =>
                impunity && r.foes.goos.active.none && (r.allies.goos.any || r.allies.monsterly.any || (active.%(_.at(r).monsterly.any).num < r.allies.cultists.num)) && (r.allies.goos.any || power > 3 || (active.%(f => f.power > 2 || f.at(r).cultists.any).none)) && self.gates.num < 4 |=> 70000 -> "impunity build gate"

                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -700 -> "cthulhu ygs dreams"
                power >= 3 + maxEnemyPower && maxEnemyPower < 10 && r.capturers.none && !canStrikeCC(r) && !canStrikeGC(r) |=> 150 -> "building gates is ok"
                (need(TwelveGates) || need(TenGates)) && r.capturers.none && !canStrikeCC(r) && !canStrikeGC(r) |=> 500 -> "building gates is good"
                r.capturers.none && r.allies.cultists.num >= 3 |=> 1500 -> "building gates is very good"

            case RecruitAction(_, Acolyte, r) =>
                impunity && r.freeGate |=> 300000 -> "impunity recruit cultist"
                impunity && !r.freeGate |=> 30000 -> "impunity recruit cultist"

                r.capturers.%(_.power > 0).any |=> -2000 -> "don't recruit to be captured"
                r.freeGate |=> 1700 -> "free gate"
                r.freeGate && r.allies.goos.any && r.foes.goos.active.none |=> 5700 -> "free gate"
                self.pool.cultists.num >= power && self.pool.goos.any |=> 300 -> "recover lost cultists"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 1 |=> 250 -> "near goo 1"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 2 |=> 240 -> "near goo 2"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 3 |=> 230 -> "near goo 3"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 4 |=> 220 -> "near goo 4"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 5 |=> 210 -> "near goo 5"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 6 |=> 200 -> "near goo 6"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 7 |=> 190 -> "near goo 7"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 8 |=> 180 -> "near goo 8"
                r.ownGate && r.allies.cultists.num == 1 |=> -100 -> "cultists not friends"
                r.ownGate && r.allies.cultists.num == 2 |=> -200 -> "cultists not friends"
                r.ownGate && r.allies.cultists.num >= 3 |=> -250 -> "own gate"

            case RecruitAction(_, HighPriest, r) =>
                true |=> -100000 -> "inactivated"

            case SummonAction(_, uc, r) if r.enemyGate && r.allies.none =>
                need(UnitsAtEnemyGates) |=> 1000 -> "need units at enemy gates"
                r.foes.num == 1 |=> 1000 -> "only one foe"
                r.foes.goos.none && uc.cost > 2 |=> 750 -> "beyondable"
                r.foes.goos.none && uc.cost == 2 && r.owner.strength(r.owner.at(r), self) < 2 && have(MillionFavoredOnes) |=> 900 -> "upgradable"
                uc.cost == 2 |=> 100 -> "cost 2"
                uc.cost == 3 |=> 50 -> "cost 3"
                uc.cost == 4 |=> 25 -> "cost 4"
                r.owner.power * 2 < power |=> 1200 -> "enemy more power"
                r.foes(Cthulhu).any && GC.power > 0 |=> -10000 -> "dont feed cth"

            case SummonAction(_, uc, r) if r.enemyGate =>
                true |=> -1000 -> "have presence already"

            case SummonAction(_, Mutant, r) =>
                impunity && r.controllers.num == 1 && r.allies.num == r.controllers.num && r.foes.goos.none && r.foes.monsterly.active.any |=> 300000 -> "prevent losing gate"
                impunity && r.controllers.num >= 2 && r.allies.num == r.controllers.num && r.foes.goos.none && r.foes.monsterly.active.any && self.all.cultists.num < 6 |=> 200000 -> "prevent losing gate"

                r.allies(YogSothoth).any |=> 50 -> "summon to ygs"
                r.controllers.num == 1 && r.foes.monsterly.any && r.foes.goos.none |=> 60 -> "prevent losing gate"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 600 -> "prevent losing gate"
                r.controllers.num == 1 && r.allies.num == 1 && r.foes.goos.none && r.foes.monsterly.active.any |=> 4200 -> "prevent losing gate"
                r.allies.goos.num == 2 && r.foes.goos.any |=> 9000 -> "summon for defense"
                r.ownGate && r.allies.monsterly.none |=> 100 -> "protect gate"
                r.controllers.num == 1 |=> 10 -> "one controller"

            case SummonAction(_, Abomination, r) =>
                true |=> -1000 -> "lets dont"

                r.allies.cultists.num == 6 && r.allies.monsterly.none |=> 2000 -> "move gate away"

                r.allies(YogSothoth).any |=> 100 -> "summon to ygs"
                !have(YogSothoth) |=> -200 -> "no yogsothoth no abomination"
                r.controllers.num == 1 |=> 10 -> "one controller"

            case SummonAction(_, SpawnOW, r) =>
                true |=> -1000 -> "lets dont"

                r.allies(YogSothoth).any |=> 100 -> "summon to ygs"
                !have(YogSothoth) && power > 9 |=> 1200 -> "no yogsothoth need spawn"
                r.controllers.num == 1 |=> 10 -> "one controller"
                r.ownGate && r.allies.monsterly.none |=> 100 -> "no monsters"

            case AwakenAction(_, YogSothoth, r, _) =>
                power >= 6 |=> 4000 -> "yes awaken"
                power >= 6 && others.%(_.allSB).any |=> 3400 -> "maybe awaken"
                r.foes.goos.any |=> 300 -> "awaken to battle"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsterly.any |=> 100 -> "awaken to defend"

            case BeyondOneAction(_, o, uc, r) =>
                uc == YogSothoth && need(GooMeetsGoo) && r.foes.goos.any && r.foes.goos.active.none |=> 120000 -> "meet up"
                uc == YogSothoth && game.cathedrals.contains(r) |=> -5000 -> "not to cathedral"

                o.enemyGate && r.foes.none |=> 1600 -> "enemy beyond free"
                true |=> -1000 -> "dont"
                o.enemyGate && o.owner == BG && o.foes(DarkYoung).any |=> -10000 -> "leave dark young alone"

                o.ownGate && o.allies.cultists.num >= 2 && o.foes.none && power > 3 |=> 3000 -> "move to free to build"
                o.ownGate && o.allies.cultists.num == 1 && o.foes.monsterly.any && o.allies.goos.none |=> 2000 -> "evacuate gate"

                r.foes.none |=> 100 -> "free"
                r.allies.none && r.foes.monsterly.none && r.foes.goos.none && r.foes.cultists.any |=> 1000 -> "hi cultist"
                r.allies.none && r.foes.monsterly.none && r.foes.goos.none && r.foes.cultists.num >= 2 |=> 1200 -> "hi cultists"

                game.factions.contains(GC) && GC.needs(OceanGates) && r.ocean |=> -1000000 -> "gc no ocean"
                game.factions.contains(WW) && game.board.starting(WW).has(r) && (WW.needs(OppositeGate) || uc == YogSothoth) |=> -1000000 -> "ww no pole"

            case DreadCurseAction(_, n, r) =>
                val v1 = others./(_.at(r)./(_.uclass.cost).sorted.take(1).sum).max
                val v2 = others./(_.at(r)./(_.uclass.cost).sorted.take(2).sum).max
                val v3 = others./(_.at(r)./(_.uclass.cost).sorted.take(3).sum).max

                val v = n match {
                    case 5 => v1 * 402 + v2 * 196 + v3 * 32
                    case 4 => v1 * 386 + v2 * 132 + v3 * 15
                    case 3 => v1 * 347 + v2 *  74 + v3 *  4
                    case 2 => v1 * 278 + v2 *  28
                    case 1 => v1 * 167
                }

                v > 4500 |=> 100000 -> "> 4500"
                v > 4000 |=>  50000 -> "> 4000"
                v > 3500 |=>  25000 -> "> 3500"
                v > 3000 |=>  12500 -> "> 3000"
                v > 2500 |=>   6250 -> "> 2500"
                v > 2000 |=>   3125 -> "> 2000"
                v > 1500 |=>    150 -> "> 1500"
                v > 1000 |=>    100 -> "> 1000"

                true |=> -100 -> "dont"

            case DreadCurseSplitAction(_, r, _, e, k, p) => {
                val v = e./{ f =>
                    val u = r.of(f)./(_.uclass.cost)

                    f.aprxDoom * (10 * u.take(k.count(f)).sum + u.drop(k.count(f)).take(p.count(f)).sum)
                }.sum

                true |=> v -> ("calculated-" + v) }

            case DreadCurseRetreatToAction(_, r, l, e, uc, d) =>
                val u = e.at(r, uc).head

                d.freeGate |=> -100 -> "cultist not to free gate"
                d.gate |=> -200 -> "cultist not to gate"
                e.at(d).any |=> -300 -> "cultist not to frieds"
                u.cultist && d.allies.monsterly.any |=> 400 -> "cultist to monster capture"
                u.cultist && d.allies.goos.any |=> 500 -> "cultist to goo capture"
                e.at(d).monsterly.any |=> -600 -> "cultist not to frieds"
                e.at(d).goos.any |=> -1000 -> "cultist not to his goo"

            case DragonAscendingAction(_, _, _, n, _) =>
                n > power + 7 |=> 2000 -> "well ok"

            case DragonAscendingCancelAction(_, _) =>
                true |=> 1000 -> "not now"

            case DragonAscendingNotThisTurnAction(_, _) =>
                true |=> -1000 -> "never"

            case AvatarReplacementAction(_, _, r, o, uc) =>
                val u = self.at(r, uc).head
                u.cultist && o.capturers.%(_.power > 0).any |=> -100 -> "don't send cultist to be captured"
                u.cultist && o.capturers.none |=> 150 -> "no capturers"
                u.cultist && o.capturers.any && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                u.monsterly && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monsterly && u.friends.cultists.num > 1 && u.friends.monsterly.none && r.foes.monsterly./(_.faction).%(_ != BG).%(_.power > 0).any |=> -200 -> "dont sent temp defender"

            case RevealESAction(_, _, _, next) if next == DoomAction(self) =>
                self.allSB && self.realDoom >= 30 |=> 1000 -> "reveal and try to win"
                true |=> -100 -> "don't reveal"
                canRitual |=> -2000 -> "ritual first"

            case RevealESAction(_, _, _, _) =>
                self.allSB && self.realDoom >= 30 |=> 1000 -> "reveal and try to win"
                self.allSB && self.realDoom < 30 && self.realDoom < self.aprxDoom && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                !self.allSB && self.realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 800 -> "reveal so 30 broken and nobody wins"
                true |=> -100 -> "don't reveal"

            case ThousandFormsAskAction(f, r, offers, _, _, _, p) =>
                r < p + offers./(_.n).sum |=> -6*6*6*6*6*6 -> "dont overpay"
                p == -1 && power >= f.power + r && !f.allSB |=> (4*4*4*4*4*4 * Math.random()).round.toInt -> "refuse pay"
                p == 0 |=> (3*3*3*3*3*3 * Math.random()).round.toInt -> "pay 0"
                p == 1 |=> (2*3*3*3*3*3 * Math.random()).round.toInt -> "pay 1"
                p == 2 |=> (2*2*3*3*3*3 * Math.random()).round.toInt -> "pay 2"
                p == 3 |=> (2*2*2*3*3*3 * Math.random()).round.toInt -> "pay 3"
                p == 4 |=> (2*2*2*2*3*3 * Math.random()).round.toInt -> "pay 4"
                p == 5 |=> (2*2*2*2*2*3 * Math.random()).round.toInt -> "pay 5"
                p == 6 |=> (2*2*2*2*2*2 * Math.random()).round.toInt -> "pay 6"

            case GhrothAskAction(_, _, _, _, _, _, n) =>
                n == -1 |=> 1000 -> "refuse"
                n == 0 |=> 1000 -> "wait"
                n == 1 && have(Passion) |=> 2000 -> "cash passion"

            case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case GhrothUnitAction(_, uc, r, f, _) =>
                val c = self.at(r, uc).head
                c.friends.cultists.none && c.region.capturers.%(!_.blind(f)).any |=> 1000 -> "will be captured anyway"
                c.gateKeeper |=> -900 -> "gate keeper"
                c.friends.cultists.none && c.region.capturers.any |=> 800 -> "can be captured one"
                c.friends.cultists.any && c.region.capturers.any |=> 700 -> "can be captured many"
                c.friends.cultists.any |=> 600 -> "cultist friends"
                c.friends.goos.any |=> -500 -> "goo huggers"

            case MainDoneAction(_) =>
                (oncePerRound.contains(HWINTBN) || oncePerRound.contains(ScreamingDead)) && active.%(_.allSB).none |=> 5000 -> "double action done"
                true |=> 500 -> "main done"



            case _ if game.battle == null =>
                true |=> 1000 -> "todo"

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
                    u.is(Mutant) |=> 800 -> "elim mutant"
                    u.is(Acolyte) && u.faction == self |=> 400 -> "elim acolyte"
                    u.is(Abomination) |=> 200 -> "elim abomination"
                    u.is(SpawnOW) |=> 100 -> "elim sp"
                    u.is(SpawnOW) && opponent.has(Harbinger) && opponent.rolls.count(Pain) + opponent.rolls.count(Kill) >= self.forces.num && opponent.power > 1 |=> 10000 -> "sacrifice sp"
                    u.is(YogSothoth) |=> -1 -> "elim ygs"

                    u.faction != self |=> u.uclass.cost * 100 -> "cost * 100"
                    u.is(Acolyte) && u.faction != self |=> 150 -> "elim enemy acolyte"
                }

                def retreat(u : UnitFigure) {
                    u.gateKeeper && u.faction == self && self.forces./(battle.canAssignPains).sum > 2 |=> -1000 -> "retr gate keeper"
                    u.is(Acolyte) && u.faction == self |=> 800 -> "retr acolyte"
                    u.is(Abomination) |=> 400 -> "retr abomination"
                    u.is(Mutant) |=> 200 -> "retr mutant"
                    u.is(SpawnOW) |=> -10000 -> "retr sp"
                    u.is(YogSothoth) |=> -100000 -> "retr ygs"

                    u.faction != self |=> u.uclass.cost * 100 -> "cost * 100"
                    u.is(Acolyte) && u.faction != self |=> 50 -> "pain enemy acolyte"
                }

                a match {
                    case DevourAction(_, u) =>
                        elim(u)

                    case AbductAction(_, _, u) =>
                        elim(u)

                    case DemandSacrificeKillsArePainsAction(_) =>
                        self.str < opponent.str |=> 1000 -> "less str"
                        self.str > opponent.str |=> -1000 -> "more str"

                    case AssignKillAction(_, _, _, u) =>
                        elim(u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(u)

                    case EliminateNoWayAction(_, u) =>
                        elim(u)

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.monsterly.any |=> 1000 -> "send cultist to be protected by monsters"
                        u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                        u.cultist && r.allies.cultists.any |=> -3000 -> "dont group cultists"
                        u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                        u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                        u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                        u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                        u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                        u.is(Mutant) && r.allies(SpawnOW).any |=> 3000 -> "mutant go sp"
                        u.is(Mutant) && r.allies(Mutant).num >= 4 |=> 2400 -> "groupundead"
                        u.is(Mutant) && r.allies(Mutant).num == 3 |=> 2300 -> "groupundead"
                        u.is(Mutant) && r.allies(Mutant).num == 2 |=> 2200 -> "groupundead"
                        u.is(Mutant) && r.allies(Mutant).num == 1 |=> 2100 -> "groupundead"

                        u.goo && r.allies.num >= 9 |=> 3100 -> "regrouphug"
                        u.goo && r.allies.num == 8 |=> 3000 -> "regrouphug"
                        u.goo && r.allies.num == 7 |=> 2900 -> "regrouphug"
                        u.goo && r.allies.num == 6 |=> 2800 -> "regrouphug"
                        u.goo && r.allies.num == 5 |=> 2700 -> "regrouphug"
                        u.goo && r.allies.num == 4 |=> 2600 -> "regrouphug"
                        u.goo && r.allies.num == 3 |=> 2500 -> "regrouphug"
                        u.goo && r.allies.num == 2 |=> 2400 -> "regrouphug"
                        u.goo && r.allies.num == 1 |=> 2300 -> "regrouphug"

                        u.monsterly && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                        u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                        u.monsterly && r.foes.%(_.vulnerableM).any && !r.foes.goos.any && r.allies.monsterly.none |=> 1000 -> "send monster to capture"
                        u.goo && r.foes.%(_.vulnerableG).any |=> 1000 -> "send goo to capture"

                        u.monsterly && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                        u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                        u.monsterly && r.ownGate |=> 400 -> "send monster to own gate"
                        u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                        u.monsterly && r.freeGate |=> 300 -> "send monster to free gate"
                        u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                        u.monsterly && r.enemyGate |=> 300 -> "send monster to enemy gate"
                        u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                        u.monsterly && r.foes(Tsathoggua).any |=> -450 -> "dont send monster to tsa"

                        if (u.goo)
                            result ++= eval(MoveAction(u.faction, u.uclass, u.region, r))

                        true |=> (game.board.connected(r) ++ game.board.connected(r).flatMap(game.board.connected)).distinct.num -> "reachable regions"

                    case MillionFavoredOnesAction(_, r, uc, nw) =>
                        uc == Abomination |=> 5000 -> "ab -> sp"
                        uc == Mutant |=> 4000 -> "mu -> ab"
                        uc == SpawnOW && nw.num > 1 && r.gate && r.foes.any |=> 3000 -> "sp -> 4mu"
                        uc == Acolyte && r.foes.goos.any && r.allies.goos.none |=> 2000 -> "save from goo"
                        uc == Acolyte && r.allies.monsterly.none && r.allies.cultists.num > 1 |=> 1000 -> "diversify"

                    case MillionFavoredOnesDoneAction(_) =>
                        true |=> 0 -> "done"

                    case ChannelPowerAction(_, n) =>

                    case ChannelPowerDoneAction(_) =>
                        true |=> 10000 -> "done"

                    case _ =>
                        true |=> 1000 -> "todo"
                }
            }
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
