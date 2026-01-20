package cws

import hrf.colmat._

object BotYSOld extends BotX(implicit g => new GameEvaluationYSOld)

class GameEvaluationYSOld(implicit game : Game) extends GameEvaluation(YS)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        a match {
            case FirstPlayerAction(_, f) =>
                f == self && areas.%(_.allies.goos.any).%(_.foes.goos.any).any |=> 100 -> "play first goos together"
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

                (game.factions.indexOf(f) - game.factions.indexOf(self)).abs == 2 |=> 10 -> "stall opposite"
                f == CC && !CC.allSB |=> 1 -> "cc first"
                CC.allSB |=> 1000 -> "first, cc allsb"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            case SpellbookAction(_, sb, _) => sb match {
                case ThirdEye =>
                    self.has(Hastur) && self.has(KingInYellow) |=> 1000 -> "must have if have kiy and has"
                case Passion =>
                    true |=> 900 -> "too good"
                case ScreamingDead =>
                    true |=> 800 -> "very good"
                case HWINTBN =>
                    true |=> 700 -> "just good"
                case Shriek =>
                    true |=> 600 -> "almost good"
                case Zingaya =>
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
                true |=> 500 -> "move done"

            case MoveAction(_, u, o, d, cost) if u.uclass == Byakhee =>
                true |=> -100 -> "byakhee dont move"
                power == 1 && d.desecrated && d.allies.none && o.allies.goos.none && (!o.desecrated || o.allies.num > 1) |=> 120 -> "come feast"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, u, o, d, cost) if u.uclass == Undead =>
                o.allies(KingInYellow).any |=> -200 -> "undead dont leave kiy"
                power == 1 && d.desecrated && d.allies.none && o.allies.goos.none && (!o.desecrated || o.allies.num > 1) |=> 220 -> "come feast"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, u, o, d, cost) if u.uclass == KingInYellow =>
                self.has(ScreamingDead) && !oncePerRound.contains(ScreamingDead) |=> -1000 -> "dont walk just scream"

                power > 10 && !have(Hastur) && d.ownGate |=> 1300 -> "go awaken hastur"

                o.desecrated && !d.desecrated |=> 200 -> "already desecated origin"
                d.desecrated && !o.desecrated |=> -200 -> "already desecated dest"

                val protecting = o.ownGate && o.allies.goos.num == 1 && o.allies.monsterly.none && o.foes.monsterly.active.any

                d.enemyGate && !(protecting) && d.controllers.num == 1 && d.controllers.monsterly.none && d.foes.goos.none && d.owner.blind(self) && power > 1 |=> 950 -> "go get a gate"
                d.enemyGate && !(protecting) && d.controllers.num == 2 && d.controllers.monsterly.none && d.foes.goos.none && d.owner.blind(self) && power > 2 |=> 900 -> "go get a gate"

                d.foes(Nyarlathotep).any |=> -3000 -> "no not nya"
                CC.power > 0 && o.foes(Nyarlathotep).any && o.allies(Hastur).none |=> 2800 -> "no not nya"
                d.foes(Cthulhu).any |=> -2900 -> "no not cthulhu"
                GC.power > 0 && o.foes(Cthulhu).any && o.allies(Hastur).none  |=> 2700 -> "no not cthulhu"
                d.foes(ShubNiggurath).any |=> -2800 -> "no not shub"
                BG.power > 0 && o.foes(ShubNiggurath).any && o.allies(Hastur).none |=> 2600 -> "no not shub"
                d.foes(Tsathoggua).any |=> -2800 -> "no not tsa"
                SL.power > 0 && o.foes(Tsathoggua).any && o.allies(Hastur).none |=> 2600 -> "no not tsa"

                o.ownGate && o.foes.any && o.allies.monsterly.none |=> -2100 -> "protect gate"

                !have(Hastur) && d.foes.%(_.vulnerableG).any && active.%(f => f.strength(f.at(d).diff(f.at(d).cultists.take(1)), self) > d.allies.num).none && power > 1 |=> 600 -> "maybe capture"

                power == 1 && o.allies(Hastur).any |=> -5000 -> "stay with hastur"

                active.none && d.enemyGate && d.foes.goos.none |=> 150 -> "enemy gate imp"
                active.none && d.near.%(n => n.enemyGate && n.foes.goos.none).any |=> 125 -> "enemy gate near imp"

                d.ownGate |=> 100 -> "own gate"
                d.freeGate |=> 75 -> "free gate"
                d.enemyGate |=> 50 -> "enemy gate"
                d.allies.cultists.any |=> 25 -> "hug cultist"

            case MoveAction(_, u, o, d, cost) if u.uclass == Hastur =>
                true |=> -1000 -> "stay"

                active./(f => f.strength(o.of(f), YS)).maxOr(0) > (o.allies.num - 1) * 2 + active./(f => f.strength(d.of(f), YS)).maxOr(0) && d.allies.num >= o.allies.num && o.allies.num < 4 |=> 7000 -> "less agr, more allies"

            case MoveAction(_, u, o, d, cost) if u.uclass == Acolyte =>
                u.onGate |=> -10 -> "on gate"

                u.gateKeeper && (!u.capturable || u.enemies.goos.none) |=> -100 -> "don't move gatekeeper"
                self.pool.cultists.any && d.allies.any && !self.all.tag(Moved).any |=> -1000 -> "why move if can recruit for same"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d == EarthMap4v35.NorthAsia |=> 800 -> "crowded cultists - north asia"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d.near.%(_.enemyGate).num > 1 |=> 800 -> "crowded cultists 6 explore near other gates"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d.glyph != Ocean |=> 800 -> "crowded cultists 6 explore"
                o.allies.cultists.num == 6 && self.all.monsterly.none && d.empty && d.ocean |=> 790 -> "crowded cultists 6 explore ocean"
                !u.gateKeeper && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.allies.goos.any && d.foes.goos.active.none |=> 5500 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 1400 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && d.capturers.%(_.power > 0).none |=> 1300 -> "ic temporary free gate"

                o.capturers.%(_.power > 0).any && d.capturers.none |=> 10 -> "move from capture"
                o.allies.goos.any |=> -310 -> "move from own goo"
                d.allies.goos.any |=> 200 -> "move to goo"
                d.ownGate && d.allies.cultists.num == 1 |=> -190 -> "move to own gate with single cultist"
                d.allies.cultists.any |=> -3000 -> "dont group cultists"
                d.desecrated && d.allies.none && d.capturers.%(_.power > 0).none && !(o.desecrated && o.allies.num == 1) |=> 100 -> "move to feast"
                o.allies.cultists.num > 1 |=> 30 -> "ungroup cultists"
                o.allies.cultists.num > 2 |=> 40 -> "ungroup cultists"
                o.allies.cultists.num > 3 |=> 50 -> "ungroup cultists"
                o.allies.cultists.num > 4 |=> 60 -> "ungroup cultists"
                o.allies.cultists.num > 5 |=> 70 -> "ungroup cultists"

                !have(KingInYellow) && !u.gateKeeper && o.gate && d.noGate && d.foes.monsterly.none && d.foes.goos.none && d.allies.none |=> 300 -> "go awaken kiy"

                power > 1 && !u.gateKeeper && d.near.%(n => n.freeGate && n.capturers.none && n.allies.cultists.none).any && d.allies.cultists.none && d.capturers.none && active.none |=> 450 -> "ic free gate 2 steps"
                power > 1 && !u.gateKeeper && d.near.%(n => n.freeGate && n.allies.goos.any && n.foes.goos.none && n.allies.cultists.none).any && d.allies.cultists.none && d.capturers.none && self.all.tag(Moved).none |=> 1100 -> "ic free gate and goo 2 steps"

            case AttackAction(_, r, f, _) if f.neutral =>
                true |=> -100000 -> "don't attack uncontrolled filth (for now)"

            case AttackAction(_, r, f, _) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val igh = BG.has(Necrophagy).?(BG.all(Ghoul).diff(foes).num).|(0)

                var ac = allies(Acolyte).num
                val un = allies(Undead).num
                val by = allies(Byakhee).num
                val kiy = allies.got(KingInYellow)
                val has = allies.got(Hastur)

                f match {
                    case GC =>
                        var ec = foes(Acolyte).num
                        var dp = foes(DeepOne).num
                        var sh = foes(Shoggoth).num
                        var ss = foes(Starspawn).num
                        var cth = foes.got(Cthulhu)

                        var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(12)
                        var shield = ac + un + by

                        enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                        cth && has && f.power < 4 && !f.needs(KillDevour1) && !f.needs(KillDevour2) |=> 11000 -> "attack cth no respawn"
                        cth && has && f.power == 0 && f.needs(KillDevour1) && f.needs(KillDevour2) |=> 11000 -> "attack cth no power"
                        cth && has && f.power == 0 && f.needs(OceanGates) |=> 11000 -> "attack cth no gates"

                        0 -> "todo"

                    case BG =>
                        0 -> "todo"

                    case CC =>
                        var ec = foes(Acolyte).num
                        var ng = foes(Nightgaunt).num
                        var fp = foes(FlyingPolyp).num
                        var hh = foes(HuntingHorror).num
                        var nya = foes.got(Nyarlathotep)
                        var shield = ac + un + by

                        var ihh = f.has(SeekAndDestroy).?(f.all(HuntingHorror).diff(foes).num).|(0)

                        var enemyStr = 5 * min(shield, f.has(Abduct).??(ng) + f.has(Invisibility).??(fp)) + fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB)

                        f.has(Invisibility) && fp == foes.num |=> -1000000 -> "invis"
                        enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                        has |=> 12000 -> "attack cc"
                        has && nya |=> 19000 -> "attack nya"
                        hh == foes.num && un + by > 5 |=> 11000 -> "attack hh"

                        0 -> "todo"

                    case SL =>
                        0 -> "todo"

                    case WW =>
                        0 -> "todo"

                    case OW =>
                        0 -> "todo"

                    case AN =>
                        0 -> "todo"
                }

            case CaptureAction(_, r, f, _) =>
                r.enemyGate && f == r.owner && r.controllers.num == 1 && r.allies.cultists.none && r.foes.%(_.canControlGate).num > 1 |=> -700 -> "give gate away"
                r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 6000 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 3000 -> "capture and nearly open gate"
                true |=> 2000 -> "capture"
                r.enemyGate |=> 100 -> "enemy gate"

            case BuildGateAction(_, r) =>
                YS.has(Hastur) && YS.power > 1 |=> -1000 -> "hastur in play"
                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && !r.allies.got(Nyarlathotep) |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && !r.allies.got(Nyarlathotep) |=> -700 -> "cthulhu has dreams"
                (power >= 3 + maxEnemyPower || self.gates.num <= 1) && r.capturers.none |=> 500 -> "building gates is good"

            case RecruitAction(_, Acolyte, r) =>
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

            case SummonAction(_, Undead, r) =>
                r.allies(Hastur).any |=> 50 -> "summon to has"
                r.allies(KingInYellow).any |=> 150 -> "summon to kiy"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 600 -> "prevent losing gate"
                r.controllers.num == 1 && power == 1 && r.allies.num == 1 && r.foes.goos.none |=> 600 -> "prevent losing gate"
                r.allies.goos.num == 2 && r.foes.goos.any |=> 9000 -> "summon for defense"

            case SummonAction(_, Byakhee, r) =>
                r.allies(Hastur).any |=> 100 -> "summon to has"
                r.allies(KingInYellow).any |=> 50 -> "summon to kiy"

            case AwakenAction(_, KingInYellow, r, _) =>
                self.needs(DesecrateWW) || self.needs(DesecrateOO) || self.needs(DesecrateAA) |=> 5000 -> "need kiy to desecrate"
                r.desecrated |=> -1500 -> "already desecrated"
                r.allies.num >= 5 |=> 500 -> "many allies"
                r.allies.num == 4 |=> 400 -> "4 allies"
                r.allies.num == 3 |=> 300 -> "3 allies"
                r.allies.num == 2 |=> 200 -> "2 allies"

                r.foes.got(Nyarlathotep) && CC.power > 0 |=> -7000 -> "don't feed nya"
                r.allies(Hastur).none && CC.power > 1 && CC.allSB && r.near012.%(_.foes(Nyarlathotep).any).any |=> -6000 -> "nya can crush"

                r.foes.got(Cthulhu) && GC.power > 0 |=> -7000 -> "don't feed cth"
                r.allies(Hastur).none && GC.power > 1 && GC.allSB && r.near.%(_.foes(Cthulhu).any).any |=> -6000 -> "cth can crush"
                r.allies(Hastur).none && GC.power > 0 && GC.allSB && GC.at(GC.deep).any |=> -6000 -> "cth can crush"

                power > 4 |=> 2000 -> "yes awaken"
                power == 4 |=> 1400 -> "maybe awaken"
                r.foes.goos.any |=> -300 -> "don't feed to goos"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsterly.any |=> -100 -> "don't feed to monsters"

            case AwakenAction(_, Hastur, r, _) =>
                power > 10 |=> 4000 -> "yes awaken"
                power == 10 |=> 3400 -> "maybe awaken"
                r.foes.goos.any |=> 300 -> "awaken to battle"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsterly.any |=> 100 -> "awaken to defend"

            case Provide3DoomAction(_, f) =>
                others.%(f => f.power == 1).any && self.power > 1 |=> 300 -> "maybe"
                self.numSB == 5 |=> 10000 -> "finally"
                f.aprxDoom != others./(_.aprxDoom).min |=> -100 -> "not in the last place"
                true |=> 10000000 -> "start with it"

            case DesecrateMainAction(_, r, te) =>
                val n = r.allies.num
                val h = te

                n >= 6 && power == 2 |=> 2000 -> "desecrate six on last power"

                h && n >= 6 |=> 4000 -> "desecrate max"
                h && n == 5 |=> 4000 -> "desecrate 5"
                h && n == 4 |=> 4000 -> "desecrate 4"
                h && n == 3 |=> 3000 -> "desecrate 3"
                h && n == 2 |=> 2000 -> "desecrate 2"
                h && n == 1 |=> 1000 -> "desecrate 1"

                h && r.freeGate && self.pool.cultists.any |=> 6000 -> "desecrate cultist"

                r.glyph == GlyphWW && self.needs(DesecrateWW) && self.numSB >= 4 |=> 800 -> "need desecrate ww"
                r.glyph == GlyphAA && self.needs(DesecrateAA) && self.numSB >= 4 |=> 800 -> "need desecrate aa"
                r.glyph == GlyphOO && self.needs(DesecrateOO) && self.numSB >= 4 |=> 800 -> "need desecrate oo"

                h && self.realDoom >= 28 && allSB |=> 1000000 -> "go go go"

                r.allies.goos.num == 2 && r.foes.goos.any |=> 10000 -> "desecrate for defense"

            case DesecratePlaceAction(_, r, Acolyte) =>
                r.allies.cultists.none |=> 800 -> "no cultists"
                self.pool.cultists.num >= power |=> 700 -> "recover lost cultists"
                true |=> 400 -> "cultist good"

            case DesecratePlaceAction(_, r, Undead) =>
                true |=> 600 -> "more undead"

            case DesecratePlaceAction(_, r, Byakhee) =>
                r.allies(Byakhee).none |=> 750 -> "no byakhee"
                true |=> 200 -> "byakhee ok?"

            case HWINTBNAction(_, o, d) =>
                active./(f => f.strength(o.of(f), YS)).maxOr(0) > (o.allies.num - 1) * 2 + o.allies.num + active./(f => f.strength(d.of(f), YS)).maxOr(0) && d.allies.num >= o.allies.num && o.allies.num < 4 |=> 8000 -> "less agr, more allies"

                CC.power > 1 && CC.allSB && CC.has(Nyarlathotep) && d.allies.num >= o.allies.num && o.allies.num < 4 |=> 8000 -> "less agr, more allies"

                o.noGate && d.ownGate && d.allies.num >= o.allies.num |=> 1000 -> "go own gate"

                power > 5 && o.noGate && !have(KingInYellow) && d.allies.num + self.all(Byakhee).num >= 4 && !o.desecrated |=> 2000 -> "go awaken kiy"
                power > 5 && o.noGate && !have(KingInYellow) && d.allies.num + self.all(Byakhee).num >= 4 && o.desecrated |=> 1000 -> "go awaken kiy"

                d.enemyGate |=> 10 -> "if nothing else"

                if (power > 1 && d.foes(Nyarlathotep).any && (CC.power == 0 || (allSB && self.battled.has(d).not)) && CC.aprxDoom >= self.realDoom) {
                    val f = CC
                    val allies = self.at(d)
                    val foes = f.at(d)

                    val ac = allies(Acolyte).num
                    val un = allies(Undead).num
                    val by = allies(Byakhee).num
                    val kiy = allies.got(KingInYellow)
                    val has = allies.got(Hastur)

                    val iby = (power > 2 && have(Shriek)).??(self.all(Byakhee).%(_.region != d).num)

                    val ec = foes(Acolyte).num
                    val ng = foes(Nightgaunt).num
                    val fp = foes(FlyingPolyp).num
                    val hh = foes(HuntingHorror).num
                    val nya = foes.got(Nyarlathotep)
                    val shield = ac + un + by + iby - f.has(Abduct).??(ng) - f.has(Invisibility).??(fp)

                    val ihh = f.has(SeekAndDestroy).??(f.all(HuntingHorror).diff(foes).num)

                    val enemyStr = fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB)

                    enemyStr <= shield * 4 |=> 9000 -> "kill nya"
                    enemyStr <= (shield - iby) * 4 |=> 30000 -> "kill nya"

                }

                if (power > 1 && d.foes(Cthulhu).any && (GC.power == 0 || (allSB && self.battled.has(d).not)) && GC.power < 4 && GC.aprxDoom >= self.realDoom) {
                    val f = GC
                    val allies = self.at(d)
                    val foes = f.at(d)

                    val ac = allies(Acolyte).num
                    val un = allies(Undead).num
                    val by = allies(Byakhee).num
                    val kiy = allies.got(KingInYellow)
                    val has = allies.got(Hastur)

                    val iby = (power > 2 && have(Shriek)).??(self.all(Byakhee).%(_.region != d).num)

                    var ec = foes(Acolyte).num
                    var dp = foes(DeepOne).num
                    var sh = foes(Shoggoth).num
                    var ss = foes(Starspawn).num
                    var cth = foes.got(Cthulhu)

                    var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(6)
                    var shield = ac + un + by - 1

                    enemyStr <= shield * 4 |=> 9000 -> "kill cth"
                    enemyStr <= (shield - iby) * 4 |=> 30000 -> "kill cth"

                }

            case ScreamingDeadAction(_, o, d) =>
                power > 10 && !have(Hastur) && d.ownGate && !o.ownGate |=> 1300 -> "go awaken hastur"

                active.%(_.allSB).any && !have(Hastur) && o.ownGate |=> -2000 -> "wait hastur"
                power == 1 && !have(Hastur) && !d.ownGate && o.ownGate |=> -2000 -> "wait hastur"
                power == 1 && o.allies(Hastur).any |=> -5000 -> "stay with hastur"
                power == 1 && d.allies.none && o.allies(Undead).num < 2 |=> -2000 -> "wait no undead"

                val cost = if (have(Hastur) && have(ThirdEye)) 1 else 2
                d.glyph == GlyphWW && self.needs(DesecrateWW) && !need(AwakenHastur) |=> 150 -> "desecrate ww"
                d.glyph == GlyphAA && self.needs(DesecrateAA) && !need(AwakenHastur) |=> 150 -> "desecrate aa"
                d.glyph == GlyphOO && self.needs(DesecrateOO) && !need(AwakenHastur) |=> 150 -> "desecrate oo"

                d.glyph == GlyphWW && self.needs(DesecrateWW) && power > cost && numSB == 5 |=> 750 -> "desecrate ww last"
                d.glyph == GlyphAA && self.needs(DesecrateAA) && power > cost && numSB == 5 |=> 750 -> "desecrate aa last"
                d.glyph == GlyphOO && self.needs(DesecrateOO) && power > cost && numSB == 5 |=> 750 -> "desecrate oo last"

                o.desecrated && !d.desecrated |=> 220 -> "already desecated origin"
                d.desecrated && !o.desecrated |=> -220 -> "already desecated dest"

                self.needs(DesecrateWW) && d.near.%(_.glyph == GlyphWW).any && !need(AwakenHastur) |=> 140 -> "closer to ww"
                self.needs(DesecrateAA) && d.near.%(_.glyph == GlyphAA).any && !need(AwakenHastur) |=> 140 -> "closer to aa"
                self.needs(DesecrateOO) && d.near.%(_.glyph == GlyphOO).any && !need(AwakenHastur) |=> 140 -> "closer to oo"

                d.allies(Undead).num >= 4 |=> 320 -> "gather the band 4"
                d.allies(Undead).num == 3 |=> 300 -> "gather the band 3"
                d.allies(Undead).num == 2 |=> 280 -> "gather the band 2"

                d.enemyGate && d.controllers.num == 1 && d.controllers.monsterly.none && d.foes.goos.none && d.owner.blind(self) && power > 1 |=> 1000 -> "go get a gate"
                d.enemyGate && d.controllers.num == 2 && d.controllers.monsterly.none && d.foes.goos.none && d.owner.blind(self) && power > 2 |=> 900 -> "go get a gate"

                d.foes(Nyarlathotep).any |=> -3000 -> "no not nya"
                o.foes(Nyarlathotep).any |=> 2800 -> "no not nya"
                d.foes(Cthulhu).any |=> -2900 -> "no not cthulhu"
                o.foes(Cthulhu).any |=> 2700 -> "no not cthulhu"
                d.foes(ShubNiggurath).any |=> -2800 -> "no not shub"
                o.foes(ShubNiggurath).any |=> 2600 -> "no not shub"
                d.foes(Tsathoggua).any |=> -2800 -> "no not tsa"
                o.foes(Tsathoggua).any |=> 2600 -> "no not tsa"
                o.ownGate && o.foes.any && o.allies.monsterly.none |=> -2100 -> "protect gate"

                !have(Hastur) && d.foes.%(_.vulnerableG).any && active.%(f => f.strength(f.at(d).diff(f.at(d).cultists.take(1)), self) > d.allies.num + o.allies(Undead).num).none && power > 1 |=> 600 -> "maybe capture"


                d.ownGate |=> 100 -> "own gate"
                d.freeGate && self.pool.cultists.any && have(Hastur) && have(ThirdEye) && !d.desecrated && power > 1 |=> 2000 -> "scream desecrate cultist at free gate"
                d.enemyGate |=> 50 -> "enemy gate"
                d.allies.cultists.any |=> 25 -> "hug cultist"

                active./(f => f.strength(f.at(d).diff(f.at(d).cultists.take(1)), self)).maxOr(0) > (d.allies.num + o.allies(Undead).num) * 2 |=> -700 -> "dangerous"

                cost == 1 && self.realDoom >= 28 && power > 1 && o.desecrated && !d.desecrated && allSB |=> 1000000 -> "go go go"

            case ScreamingDeadFollowAction(_, o, d, _) =>
                true |=> 1000 -> "always"
                o.ownGate && o.allies(Undead).num == 1 && o.foes.monsterly.active.any && o.foes.goos.active.none && active.%(_.allSB).none |=> -2000 -> "stay to protect gate"
                o.allies(Hastur).any && o.allies.num < 4 && active.%(_.allSB).any |=> -2000 -> "stay to protect hastur"

            case ShriekAction(_, d) =>
                val count = self.all(Byakhee).num
                count < 2 |=> -10000 -> "dont even bother"
                count == 2 |=> -5000 -> "dont bother"
                true |=> -1000 -> "nead reason"

                d.ownGate && d.allies(Hastur).any && d.foes.goos.%(_.faction.active).any |=> 9000 -> "shield hastur"

                d.noGate && d.allies(Hastur).any && !have(KingInYellow) && power > 5 |=> 8000 -> "shield hastur for kiy awakening"

                d.allies(KingInYellow).any && (!d.desecrated || oncePerRound.contains(ScreamingDead)) && d.allies.num < 3 |=> 4000 -> "shield kiy"

                if (power > 1 && d.allies(Hastur).any && d.foes(Nyarlathotep).any && (CC.power == 0 || (allSB && self.battled.has(d).not)) && CC.aprxDoom >= self.realDoom) {
                    val f = CC
                    val allies = self.at(d)
                    val foes = f.at(d)

                    val ac = allies(Acolyte).num
                    val un = allies(Undead).num
                    val by = allies(Byakhee).num
                    val kiy = allies.got(KingInYellow)
                    val has = allies.got(Hastur)

                    val iby = self.all(Byakhee).%(_.region != d).num

                    val ec = foes(Acolyte).num
                    val ng = foes(Nightgaunt).num
                    val fp = foes(FlyingPolyp).num
                    val hh = foes(HuntingHorror).num
                    val nya = foes.got(Nyarlathotep)
                    val shield = ac + un + by + iby - f.has(Abduct).??(ng) - f.has(Invisibility).??(fp)

                    val ihh = f.has(SeekAndDestroy).??(f.all(HuntingHorror).diff(foes).num)

                    val enemyStr = fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB)

                    enemyStr <= shield * 4 |=> 20000 -> "kill nya"
                }

                if (power > 1 && d.allies(Hastur).any && d.foes(Cthulhu).any && (GC.power == 0 || (allSB && self.battled.has(d).not)) && GC.power < 4 && GC.aprxDoom >= self.realDoom) {
                    val f = GC
                    val allies = self.at(d)
                    val foes = f.at(d)

                    val ac = allies(Acolyte).num
                    val un = allies(Undead).num
                    val by = allies(Byakhee).num
                    val kiy = allies.got(KingInYellow)
                    val has = allies.got(Hastur)

                    val iby = (power > 2 && have(Shriek)).??(self.all(Byakhee).%(_.region != d).num)

                    var ec = foes(Acolyte).num
                    var dp = foes(DeepOne).num
                    var sh = foes(Shoggoth).num
                    var ss = foes(Starspawn).num
                    var cth = foes.got(Cthulhu)

                    var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(6)
                    var shield = ac + un + by - 1

                    enemyStr <= shield * 4 |=> 10000 -> "kill cth"
                }



            case ShriekFromAction(_, o, r) =>
                true |=> 1000 -> "go"


            case AvatarReplacementAction(_, _, r, o, u) =>
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

            case GhrothTargetAction(_, c, f, _) =>
                c.friends.cultists.none && c.region.capturers.%(!_.blind(f)).any |=> 1000 -> "will be captured anyway"
                c.gateKeeper |=> -900 -> "gate keeper"
                c.friends.cultists.none && c.region.capturers.any |=> 800 -> "can be captured one"
                c.friends.cultists.any && c.region.capturers.any |=> 700 -> "can be captured many"
                c.friends.cultists.any |=> 600 -> "cultist friends"
                c.friends.goos.any |=> -500 -> "goo huggers"
                c.friends.none && c.region.desecrated |=> -400 -> "sole feast"

            case EndTurnAction(_) =>
                (oncePerRound.contains(HWINTBN) || oncePerRound.contains(ScreamingDead)) && active.%(_.allSB).none |=> 5000 -> "double action done"
                true |=> 500 -> "main done"

            case ControlGateAction(_, r, u, _) =>
                r.allies.%(_.onGate).foreach { c =>
                    c.uclass == u.uclass |=> -1000000 -> "remain calm"
                    c.uclass == HighPriest && u.uclass == Acolyte |=> 1000 -> "high priest not on gate"
                }

            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "never"

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
                    u.is(Acolyte) && u.faction == self && self.has(Passion) && !self.oncePerAction.contains(Passion) && self.forces.%(u => u.is(Acolyte) && u.health == Killed).none |=> 1200 -> "elim acolyte with passion"
                    u.is(Undead) |=> 800 -> "elim undead"
                    u.is(Acolyte) && u.faction == self |=> 400 -> "elim acolyte"
                    u.is(Byakhee) |=> 200 -> "elim byakhee"
                    u.is(KingInYellow) |=> 100 -> "elim kiy"
                    u.is(Hastur) |=> -1 -> "elim has"

                    u.faction != self |=> u.uclass.cost * 100 -> "cost * 100"
                    u.is(Acolyte) && u.faction != self |=> 150 -> "elim enemy acolyte"
                }

                def retreat(u : UnitFigure) {
                    u.gateKeeper && u.faction == self |=> -1000 -> "retr gate keeper"
                    u.is(Acolyte) && u.faction == self |=> 800 -> "retr acolyte"
                    u.is(Byakhee) |=> 400 -> "retr byakhee"
                    u.is(Undead) |=> 200 -> "retr undead"
                    u.is(KingInYellow) |=> -10000 -> "retr kiy"
                    u.is(Hastur) |=> -100000 -> "retr has"

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

                        u.is(Undead) && r.allies(KingInYellow).any |=> 3000 -> "undead go kiy"
                        u.is(Undead) && r.allies(Undead).num >= 4 |=> 2400 -> "groupundead"
                        u.is(Undead) && r.allies(Undead).num == 3 |=> 2300 -> "groupundead"
                        u.is(Undead) && r.allies(Undead).num == 2 |=> 2200 -> "groupundead"
                        u.is(Undead) && r.allies(Undead).num == 1 |=> 2100 -> "groupundead"

                        u.goo && r.allies.num >= 9 |=> 3100 -> "regrouphug"
                        u.goo && r.allies.num == 8 |=> 3000 -> "regrouphug"
                        u.goo && r.allies.num == 7 |=> 2900 -> "regrouphug"
                        u.goo && r.allies.num == 6 |=> 2800 -> "regrouphug"
                        u.goo && r.allies.num == 5 |=> 2700 -> "regrouphug"
                        u.goo && r.allies.num == 4 |=> 2600 -> "regrouphug"
                        u.goo && r.allies.num == 3 |=> 2500 -> "regrouphug"
                        u.goo && r.allies.num == 2 |=> 2400 -> "regrouphug"
                        u.goo && r.allies.num == 1 |=> 2300 -> "regrouphug"

                        r.desecrated && r.allies.none |=> 2500 -> "go desecrated"

                        u.monsterly && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                        u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                        u.monsterly && r.foes.%(_.vulnerableM).any && !r.foes.goos.any |=> 1000 -> "send monster to capture"
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
                            result ++= eval(MoveAction(u.faction, u, u.region, r, 0))

                        true |=> (r.connected ++ r.connected./~(_.connected)).distinct.num -> "reachable regions"

                    case _ =>
                }
            }
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
