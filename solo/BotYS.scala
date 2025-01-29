package cws

import hrf.colmat._

object BotYS extends BotX(g => new GameEvaluationYS(g))

class GameEvaluationYS(game : Game) extends GameEvaluation(game, YS) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        def canStrikeCC(r : Region) = (r.foes.has(Nyarlathotep) && CC.power > 0) || (CC.power > 1 && CC.allSB && r.near012.%(_.foes(Nyarlathotep).any).any)
        def canStrikeGC(r : Region) = (r.foes.has(Cthulhu) && GC.power > 0) || (GC.power > 1 && GC.allSB && r.near.%(_.foes(Cthulhu).any).any) || (GC.power > 0 && GC.allSB && GC.at(GC.deep).any)

        val forgetGates = have(Hastur) && have(KingInYellow) && self.realDoom + power / 3 * 2 >= 30 && game.desecrated.num < 12

        val impunity = !forgetGates && (others.active.none || (self.goos.any && others.active.%(_.goos.any).none && others.%(_.power > power).none))

        def hasCultistAround(r : Region) = self.pool(Acolyte).any || r.allies.cultists.any || r.near.%(n => n.allies.cultists.num > n.ownGate.??(1)).any

        def hasCultistAround2(r : Region) = self.pool(Acolyte).any || r.near012.%(n => n.allies.cultists.num > n.ownGate.??(1)).any

        def needRegion(r : Region) {
            need(DesecrateWW) && r.glyph == GlyphWW |=> 25 -> "need desecrate ww"
            need(DesecrateAA) && r.glyph == GlyphAA |=> 25 -> "need desecrate aa"
            need(DesecrateOO) && r.glyph == GlyphOO |=> 25 -> "need desecrate oo"

            need(DesecrateWW) && r.near.%(_.glyph == GlyphWW).any |=> 20 -> "need desecrate ww near"
            need(DesecrateAA) && r.near.%(_.glyph == GlyphAA).any |=> 20 -> "need desecrate aa near"
            need(DesecrateOO) && r.near.%(_.glyph == GlyphOO).any |=> 20 -> "need desecrate oo near"

            !have(KingInYellow) && r.noGate |=> 30 -> "need kiy no gate"
        }

        def checkAttack(r : Region, f : Faction, allies : List[UnitFigure], foes : List[UnitFigure], d : Int) {
            val enemyStr = f.strength(game, foes, self)
            val ownStr = self.strength(game, allies, f)

            val igh = others.%(_.has(Necrophagy))./(_.all(Ghoul).diff(foes).num).sum

            var ac = allies(Acolyte).num
            var un = allies(Undead).num
            var by = allies(Byakhee).num
            val kiy = allies.has(KingInYellow)
            val has = allies.has(Hastur)

            f match {
                case GC =>
                    var ec = foes(Acolyte).num
                    var dp = foes(DeepOne).num
                    var sh = foes(Shoggoth).num
                    var ss = foes(Starspawn).num
                    var cth = foes.has(Cthulhu)

                    var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(6)
                    var shield = ac + un + by - 1

                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                    cth && has && f.power < 4 && !f.needs(KillDevour1) && !f.needs(KillDevour2) |=> 11000/d -> "attack cth no respawn"
                    cth && has && f.power == 0 && f.needs(KillDevour1) && f.needs(KillDevour2) |=> 11000/d -> "attack cth no power"
                    cth && has && f.power == 0 && f.needs(OceanGates) |=> 11000/d -> "attack cth no gates"

                case BG =>
                    var ec = foes(Acolyte).num
                    var gh = foes(Ghoul).num
                    var fu = foes(Fungi).num
                    var dy = foes(DarkYoung).num
                    var shu = foes.has(ShubNiggurath)

                    var enemyStr = f.strength(game, foes, self)

                    var shield = ac + un + by

                    val enough = shield * 4 > enemyStr

                    val ownStr = (un > 0).??(un - 1) + (by > 0).??(by + 1) + has.??(game.ritualCost)

                    enough && has && shu |=> 11000/d -> "attack shu"
                    enough && has && dy > 0 |=> 2000/d -> "attack dy"
                    enough && shu && (ownStr > 4 + (foes.num - 1 + gh) * 4 || (!f.active && ownStr > enemyStr && power > 1)) |=> 13000/d -> "attack shu"

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
                        if (un > 0)
                            un -= 1
                        else
                        if (by > 0)
                            by -= 1
                    }

                    var inv = f.has(Invisibility).??(fp)

                    while (inv > 0) {
                        inv -= 1
                        if (by > 0)
                            by -= 1
                        else
                        if (un > 0)
                            un -= 1
                        else
                        if (ac > 0)
                            ac -= 1
                    }

                    val shield = ac + un + by

                    val ownStr = (un > 0).??(un - 1) + (by > 0).??(by + 1) + has.??(game.ritualCost)

                    var ihh = f.has(SeekAndDestroy).??(f.all(HuntingHorror).diff(foes).num)

                    var enemyStr = fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB)

                    val enough = shield * 4 > enemyStr

                    f.has(Invisibility) && fp == foes.num |=> -1000000 -> "invis"
                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"
                    kiy && nya && enough && (ownStr > 4 + (foes.num - 1 + ihh) * 4 || (!f.active && ownStr > enemyStr && power > 1)) |=> 13000/d -> "kiy attack nya"

                    has && ec + f.has(Invisibility).??(fp) + f.has(Abduct).??(ng) < foes.num |=> 12000/d -> "attack cc"
                    has && nya |=> 19000/d -> "attack nya"
                    hh == foes.num && enough && ownStr > 3 |=> 11000/d -> "attack hh"
                    hh + fp == foes.num && enough && ownStr > 4 |=> 10000/d -> "attack hhfp"

                case SL =>
                    val ec = foes(Acolyte).num
                    val wi = foes(Wizard).num
                    val sm = foes(SerpentMan).num
                    val fs = foes(FormlessSpawn).num
                    val tsa = foes.has(Tsathoggua)

                    val dms = f.has(DemandSacrifice) && f.has(Tsathoggua)

                    dms && !tsa |=> -1000000 -> "demand sacrifice and no tsa"

                    val ownStr = (un > 0).??(un - 1) + (by > 0).??(by + 1) + has.??(game.ritualCost)

                    val shield = ac + un + by

                    val enemyStr = sm + fs * (f.count(FormlessSpawn) + f.count(Tsathoggua)) + tsa.??(max(2, power - 1))

                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                    !has && ec + wi > 0 && f.power > 0 |=> -500000 -> "shielded"

                    sm + fs + tsa.??(1) == 0 && f.power > 0 |=> -500000 -> "no target"

                    ownStr > 5 && ownStr > enemyStr |=> 5000/d -> "attack sl"

                case WW =>
                    0 -> "todo"

                case OW =>
                    0 -> "todo"

                case AN =>
                    allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                    AN.has(Extinction) && foes.monsters.num == 1 && foes(Yothan).any && ((has && allies.num >= 3 && ownStr >= 6) || (allies.goos.none && ownStr >= 6)) |=> 1000 -> "attack lone extinct yothan"
            }
        }

        a match {
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
                !self.allSB && self.doom + self.gates.num < 30 && self.realDoom < 29 && self.realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"
                self.numSB >= 5 && cost * 2 <= power && have(Hastur) |=> 800 -> "5 SB and less than half available power"
                self.numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "very sweet deal"
                self.numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                self.numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                cost == 5 |=> 100 -> "ritual first"
                self.pool.goos.any |=> -200 -> "not all goos in play"
                true |=> -250 -> "don't ritual unless have reasons"

                !self.allSB |=> -1000 -> "spellbooks first"

            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                impunity |=> 1000000 -> "impunity move done"
                true |=> 500 -> "move done"

            case MoveAction(_, Acolyte, o, d) =>
                val unemployed = !((o.ownGate || o.freeGate) && o.allies.cultists.num == 1)
                val safe = d.allies.goos.any || (d.foes.goos.active.none && (d.allies.monsters.any || d.foes.monsters.active.none))

                impunity && unemployed && safe && d.desecrated && d.allies.none && (!o.desecrated || o.allies.num > 1) |=> 50000 -> "impunity move feast"

                impunity && unemployed && safe && power > 3 && d.noGate && d.allies.cultists.none && (d.allies.any || d.foes.active.none) |=> 40000 -> "impunity move build gate"

                impunity && unemployed && safe && d.freeGate |=> 200000 -> "impunity move take gate"

                impunity && unemployed && safe && power > 1 && o.allies.cultists.num > o.capturers.active.num + 1 && d.near.%(n => n.freeGate && n.capturers.none && n.allies.cultists.none).any && d.allies.cultists.%(!_.gateKeeper).none && d.capturers.active.none && active.none |=> 105000 -> "impunity move take gate 2 step"

                val u = self.at(o, Acolyte).%!(_.has(Moved)).head

                !unemployed && (!u.capturable || u.enemies.goos.none) |=> -120 -> "dont move gatekeeper"
                self.pool.cultists.any && d.allies.any && !self.all.%(_.has(Moved)).any |=> -1000 -> "why move if can recruit for same"
                o.allies.cultists.num == 6 && self.all.monsters.none && d.empty && d == EarthMap4v35.NorthAsia |=> 800 -> "crowded cultists - north asia"
                o.allies.cultists.num == 6 && self.all.monsters.none && d.empty && d.near.%(_.enemyGate).num > 1 |=> 800 -> "crowded cultists 6 explore near other gates"
                o.allies.cultists.num == 6 && self.all.monsters.none && d.empty && d.ocean.not |=> 800 -> "crowded cultists 6 explore"
                o.allies.cultists.num == 6 && self.all.monsters.none && d.empty && d.ocean |=> 790 -> "crowded cultists 6 explore ocean"
                unemployed && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.allies.goos.any && d.foes.goos.active.none |=> 5500 -> "ic free gate"
                unemployed && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none && (have(KingInYellow) || allSB) |=> 1400 -> "ic free gate"
                unemployed && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && d.capturers.%(_.power > 0).none && (have(KingInYellow) || allSB) |=> 1300 -> "ic temporary free gate"

                o.capturers.%(_.power > 0).any && d.capturers.none |=> 10 -> "move from capture"
                o.allies.goos.any && o.allies.cultists.num < 5 |=> -310 -> "move from own goo"
                d.allies.goos.any && power == 1 && d.allies.cultists.none |=> 100 -> "move to goo first"
                d.allies.goos.any && power == 1 && d.allies.cultists.any |=> 50 -> "move to goo"
                d.ownGate && d.allies.cultists.num == 1 |=> -190 -> "move to own gate with single cultist"
                d.allies.cultists.num >= o.allies.cultists.num |=> -3000 -> "dont group cultists"
                d.desecrated && d.allies.none && d.capturers.%(_.power > 0).none && !(o.desecrated && o.allies.num == 1) |=> 100 -> "move to feast"
                o.allies.cultists.num > 1 |=> 30 -> "ungroup cultists"
                o.allies.cultists.num > 2 |=> 40 -> "ungroup cultists"
                o.allies.cultists.num > 3 |=> 50 -> "ungroup cultists"
                o.allies.cultists.num > 4 |=> 60 -> "ungroup cultists"
                o.allies.cultists.num > 5 |=> 70 -> "ungroup cultists"

                (o.allies.num > 1 || !o.desecrated) && !have(KingInYellow) && !u.gateKeeper && o.gate && d.noGate && d.foes.monsters.none && d.foes.goos.none && d.allies.none |=> 300 -> "go awaken kiy"

                power > 1 && o.allies.cultists.num > o.capturers.active.num + 1 && d.near.%(n => n.freeGate && n.capturers.none && n.allies.cultists.none).any && d.allies.cultists.%(!_.gateKeeper).none && d.capturers.none && active.none |=> 450 -> "ic free gate 2 steps"
                power > 1 && o.allies.cultists.num > o.capturers.active.num + 1 && d.near.%(n => n.freeGate && n.allies.goos.any && n.foes.goos.none && n.allies.cultists.none).any && d.allies.cultists.%(!_.gateKeeper).none && d.capturers.none && self.all.%(_.has(Moved)).none |=> 1100 -> "ic free gate and goo 2 steps"

                needRegion(d)

            case MoveAction(_, Byakhee, o, d) =>
                impunity && d.desecrated && d.allies.none && (!o.desecrated || o.allies.num > 1) |=> 50000 -> "impunity move feast"

                true |=> -100 -> "byakhee dont move"
                power == 1 && d.desecrated && d.allies.none && o.allies.goos.none && (!o.desecrated || o.allies.num > 1) && !(o.allies.monsters.num == 1 && o.foes.monsters.active.any && o.allies.cultists.any) |=> 120 -> "come feast"
                o.ownGate && o.foes.monsters.active.any && o.allies.monsters.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

                needRegion(d)

            case MoveAction(_, Undead, o, d) =>
                impunity && d.desecrated && d.allies.none && (!o.desecrated || o.allies.num > 1) |=> 50000 -> "impunity move feast"

                o.allies(KingInYellow).any |=> -200 -> "undead dont leave kiy"
                power == 1 && d.desecrated && d.allies.none && (o.allies.goos.none || o.allies.num > 8) && (!o.desecrated || o.allies.num > 1) && !(o.allies.monsters.num == 1 && o.foes.monsters.active.any && o.allies.cultists.any) |=> 220 -> "come feast"
                o.ownGate && o.foes.monsters.active.any && o.allies.monsters.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

                needRegion(d)

            case MoveAction(_, KingInYellow, o, d) =>
                self.has(ScreamingDead) && !oncePerRound.contains(ScreamingDead) |=> -1000 -> "dont walk just scream"

                power > 11 && !have(Hastur) && d.ownGate && !(o.freeGate && o.allies.cultists.any) |=> 1300 -> "go awaken hastur"

                o.desecrated && !d.desecrated |=> 200 -> "already desecated origin"
                d.desecrated && !o.desecrated |=> -200 -> "already desecated dest"

                val protecting = o.ownGate && o.allies.goos.num == 1 && o.allies.monsters.none && o.foes.monsters.active.any

                d.foes(Nyarlathotep).any |=> -3000 -> "no not nya"
                CC.power > 0 && o.foes(Nyarlathotep).any && o.allies(Hastur).none |=> 2800 -> "no not nya"
                d.foes(Cthulhu).any |=> -2900 -> "no not cthulhu"
                GC.power > 0 && o.foes(Cthulhu).any && o.allies(Hastur).none  |=> 2700 -> "no not cthulhu"
                d.foes(ShubNiggurath).any |=> -2800 -> "no not shub"
                BG.power > 0 && o.foes(ShubNiggurath).any && o.allies(Hastur).none |=> 2600 -> "no not shub"
                d.foes(Tsathoggua).any |=> -2800 -> "no not tsa"
                SL.power > 0 && o.foes(Tsathoggua).any && o.allies(Hastur).none |=> 2600 -> "no not tsa"

                o.ownGate && o.foes.any && o.allies.monsters.none |=> -2100 -> "protect gate"

                !have(Hastur) && (d.enemyGate || d.foes.%(_.vulnerableG).%(u => !u.faction.has(Devolve) || u.faction.pool(DeepOne).num >= u.faction.at(d).cultists.num).any) && active.%(f => f.strength(game, f.at(d).diff(f.at(d).cultists.take(1)), self) > d.allies.num).none && power > 2 |=> 450 -> "maybe capture"

                power == 1 && o.allies(Hastur).any |=> -5000 -> "stay with hastur"

                active.none && d.enemyGate && d.foes.goos.none |=> 150 -> "enemy gate imp"
                active.none && d.near.%(n => n.enemyGate && n.foes.goos.none).any |=> 125 -> "enemy gate near imp"

                d.ownGate && power == 1 |=> 100 -> "own gate"
                d.freeGate |=> 75 -> "free gate"
                d.enemyGate && d.owner.at(d).goos.any |=> -60 -> "enemy gate"
                d.enemyGate |=> 50 -> "enemy gate"

                needRegion(d)

                power < 2 && o.allies(Hastur).any |=> -1000000 -> "hastur shadow"

                o.near.%(_.allies(Hastur).any).any && d.allies(Hastur).none && d.near.%(_.allies(Hastur).any).none |=> -1000000 -> "hastur shadow"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.player.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

            case ScreamingDeadAction(_, o, d) =>
                val undead = o.allies(Undead).num
                val followers = undead - (o.ownGate && o.allies.monsters.num == undead && o.foes.monsters.active.any && o.foes.goos.active.none).??(1)
                def isSafe(r : Region) = active.%(f => f.strength(game, f.at(r).diff(f.at(r).cultists.take(1)), self) > d.allies.num + followers).none
                val safe = isSafe(d)

                impunity && safe && power > 1 && !d.enemyGate && d.foes.cultists.any && d.foes.goos.none |=> 40000 -> "impunity scream"

                impunity && safe && d.enemyGate && d.controllers.monsters.none && d.of(d.owner).goos.none && power - 1 <  d.controllers.num |=> 100000 * (power - 1) / power -> "impunity scream"
                impunity && safe && d.enemyGate && d.controllers.monsters.none && d.of(d.owner).goos.none && power - 1 == d.controllers.num |=> 100000 -> "impunity scream"
                impunity && safe && d.enemyGate && d.controllers.monsters.none && d.of(d.owner).goos.none && power - 1 >  d.controllers.num |=> 120000 -> "impunity scream"

                impunity && safe && d.near.%(n => isSafe(n) && n.enemyGate && n.controllers.monsters.none && n.of(n.owner).goos.none && power - 2 <  n.controllers.num).any |=> 100000 * (power - 2) / power -> "impunity scream"
                impunity && safe && d.near.%(n => isSafe(n) && n.enemyGate && n.controllers.monsters.none && n.of(n.owner).goos.none && power - 2 == n.controllers.num).any |=> 60000 -> "impunity scream"
                impunity && safe && d.near.%(n => isSafe(n) && n.enemyGate && n.controllers.monsters.none && n.of(n.owner).goos.none && power - 2 >  n.controllers.num).any |=> 110000 -> "impunity scream"

                power > 11 && !have(Hastur) && d.ownGate && !o.ownGate && !(o.freeGate && o.allies.cultists.any) |=> 1300 -> "go awaken hastur"

                active.%(_.allSB).any && !have(Hastur) && o.ownGate |=> -2000 -> "wait hastur"
                power == 1 && !have(Hastur) && !d.ownGate && o.ownGate |=> -2000 -> "wait hastur"
                power == 1 && o.allies(Hastur).any |=> -5000 -> "stay with hastur"
                power == 1 && d.allies.none && o.allies(Undead).num < 2 |=> -2000 -> "wait no undead"

                val cost = if (have(Hastur) && have(ThirdEye)) 1 else 2
                d.glyph == GlyphWW && self.needs(DesecrateWW) && !need(AwakenHastur) |=> 450 -> "desecrate ww"
                d.glyph == GlyphAA && self.needs(DesecrateAA) && !need(AwakenHastur) |=> 450 -> "desecrate aa"
                d.glyph == GlyphOO && self.needs(DesecrateOO) && !need(AwakenHastur) |=> 450 -> "desecrate oo"

                d.glyph == GlyphWW && self.needs(DesecrateWW) && power > cost && numSB == 5 |=> 1250 -> "desecrate ww last"
                d.glyph == GlyphAA && self.needs(DesecrateAA) && power > cost && numSB == 5 |=> 1250 -> "desecrate aa last"
                d.glyph == GlyphOO && self.needs(DesecrateOO) && power > cost && numSB == 5 |=> 1250 -> "desecrate oo last"

                d.glyph == GlyphWW && self.needs(DesecrateWW) && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 7500 -> "desecrate ww super last"
                d.glyph == GlyphAA && self.needs(DesecrateAA) && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 7500 -> "desecrate aa super last"
                d.glyph == GlyphOO && self.needs(DesecrateOO) && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 7500 -> "desecrate oo super last"

                o.desecrated && !d.desecrated |=> 120 -> "already desecated origin"
                d.desecrated && !o.desecrated |=> -120 -> "already desecated dest"

                need(DesecrateWW) && d.near.%(_.glyph == GlyphWW).any && o.near.%(_.glyph == GlyphWW).none && !need(AwakenHastur) |=> 170 -> "closer to ww"
                need(DesecrateAA) && d.near.%(_.glyph == GlyphWW).any && o.near.%(_.glyph == GlyphAA).none && !need(AwakenHastur) |=> 170 -> "closer to aa"
                need(DesecrateOO) && d.near.%(_.glyph == GlyphWW).any && o.near.%(_.glyph == GlyphOO).none && !need(AwakenHastur) |=> 170 -> "closer to oo"

                need(DesecrateWW) && d.near.%(_.glyph == GlyphWW).any && o.near.%(_.glyph == GlyphWW).none && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 6500 -> "near desecrate ww super last"
                need(DesecrateAA) && d.near.%(_.glyph == GlyphAA).any && o.near.%(_.glyph == GlyphAA).none && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 6500 -> "near desecrate aa super last"
                need(DesecrateOO) && d.near.%(_.glyph == GlyphOO).any && o.near.%(_.glyph == GlyphOO).none && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 6500 -> "near desecrate oo super last"

                d.allies(Undead).num >= 4 |=> 320 -> "gather the band 4"
                d.allies(Undead).num == 3 |=> 300 -> "gather the band 3"
                d.allies(Undead).num == 2 |=> 280 -> "gather the band 2"

                !forgetGates && d.enemyGate && d.controllers.num == 1 && d.controllers.monsters.none && (d.foes.monsters.num < o.allies(Undead).num || d.owner.blind(self)) && d.foes.goos.none && power > 1 |=> 1000 -> "go get a gate"
                !forgetGates && d.enemyGate && d.controllers.num == 2 && d.controllers.monsters.none && (d.foes.monsters.num < o.allies(Undead).num || d.owner.blind(self)) && d.foes.goos.none && power > 2 && power > d.owner.power |=> 900 -> "go get a gate"

                d.foes(Nyarlathotep).active.any |=> -3000 -> "no not nya"
                o.foes(Nyarlathotep).active.any |=> 2800 -> "no not nya"
                d.foes(Cthulhu).active.any |=> -2900 -> "no not cthulhu"
                o.foes(Cthulhu).active.any |=> 2700 -> "no not cthulhu"
                d.foes(ShubNiggurath).active.any |=> -2800 -> "no not shub"
                o.foes(ShubNiggurath).active.any |=> 2600 -> "no not shub"
                d.foes(Tsathoggua).active.any |=> -2800 -> "no not tsa"
                o.foes(Tsathoggua).active.any |=> 2600 -> "no not tsa"

                power < 3 && d.foes(Nyarlathotep).any |=> -7000 -> "no not nya"
                power < 3 && o.foes(Nyarlathotep).any |=> 6800 -> "no not nya"
                power < 3 && d.foes(Cthulhu).any |=> -6900 -> "no not cthulhu"
                power < 3 && o.foes(Cthulhu).any |=> 6700 -> "no not cthulhu"
                power < 3 && d.foes(ShubNiggurath).any |=> -6800 -> "no not shub"
                power < 3 && o.foes(ShubNiggurath).any |=> 6600 -> "no not shub"
                power < 3 && d.foes(Tsathoggua).any |=> -6800 -> "no not tsa"
                power < 3 && o.foes(Tsathoggua).any |=> 6600 -> "no not tsa"

                o.ownGate && o.foes.any && o.allies.monsters.none |=> -2100 -> "protect gate"

                o.allies(Hastur).none && o.allies.notGoos.num < 5 && canStrikeCC(o) |=> 400 -> "nya can crush"
                o.allies(Hastur).none && o.allies.notGoos.num < 5 && canStrikeGC(o) |=> 400 -> "cth can crush"

                o.allies(Hastur).none && o.allies.notGoos.num < 6 && canStrikeCC(o) |=> 200 -> "nya can crush"
                o.allies(Hastur).none && o.allies.notGoos.num < 6 && canStrikeGC(o) |=> 200 -> "cth can crush"

                val iby = (power > 2 && have(Shriek)).??(self.all(Byakhee).%(_.region != d).num)

                d.near.%(_.allies(Hastur).any).none && d.allies(Hastur).none && d.allies.notGoos.num + o.allies(Undead).num + iby < 5 && canStrikeCC(d) |=> -400 -> "nya can crush"
                d.near.%(_.allies(Hastur).any).none && d.allies(Hastur).none && d.allies.notGoos.num + o.allies(Undead).num + iby < 5 && canStrikeGC(d) |=> -400 -> "cth can crush"

                d.near.%(_.allies(Hastur).any).none && d.allies(Hastur).none && d.allies.notGoos.num + o.allies(Undead).num + iby < 6 && canStrikeCC(d) |=> -200 -> "nya can crush"
                d.near.%(_.allies(Hastur).any).none && d.allies(Hastur).none && d.allies.notGoos.num + o.allies(Undead).num + iby < 6 && canStrikeGC(d) |=> -200 -> "cth can crush"

                !forgetGates && !have(Hastur) && d.foes.%(_.vulnerableG).%(u => !u.faction.has(Devolve) || u.faction.pool(DeepOne).none).any && active.%(f => f.strength(game, f.at(d).diff(f.at(d).cultists.take(1)), self) > d.allies.num + o.allies(Undead).num).none && power > 2 |=> 450 -> "maybe capture"
                !forgetGates && d.foes.%(_.vulnerableG).%(u => !u.faction.has(Devolve) || u.faction.pool(DeepOne).none).any && active.%(f => f.strength(game, f.at(d).diff(f.at(d).cultists.take(1)), self) > d.allies.num + o.allies(Undead).num).none && power > 1 |=> 150 -> "maybe capture"

                d.ownGate && power == 1 |=> 100 -> "own gate"
                d.freeGate && self.pool.cultists.any && have(Hastur) && have(ThirdEye) && !d.desecrated && power > 1 |=> 2000 -> "scream desecrate cultist at free gate"
                d.enemyGate |=> 50 -> "enemy gate"
                d.allies.cultists.any |=> 25 -> "hug cultist"

                active./(f => f.strength(game, f.at(d).diff(f.at(d).cultists.take(1)), self)).maxOr(0) > (d.allies.num + o.allies(Undead).num) * 2 |=> -700 -> "dangerous"

                cost == 1 && self.realDoom >= 28 && power > 1 && o.desecrated && !d.desecrated && allSB |=> 1000000 -> "go go go"

                needRegion(d)

                if (power > 1 && d.foes(Nyarlathotep).any) {
                    checkAttack(d, CC, d.of(self) ++ o.of(self)(KingInYellow) ++ o.of(self)(Undead), d.of(CC), 2)
                }

                power < 2 && o.allies(Hastur).any |=> -1000000 -> "hastur shadow"

                o.near.%(_.of(self)(Hastur).any).any && d.near.%(_.of(self)(Hastur).any).none && (!impunity || power < 2) |=> -1000000 -> "hastur shadow"


            case ScreamingDeadFollowAction(_, o, d, _) =>
                true |=> 1000 -> "always"
                o.ownGate && o.allies.monsters.num == 1 && o.foes.monsters.active.any && o.foes.goos.active.none |=> -2000 -> "stay to protect gate"
                o.allies(Hastur).any && o.allies.num < 4 && active.%(_.allSB).any && power == 0 |=> -2000 -> "stay to protect hastur"
                d.foes(Nyarlathotep).any |=> 3000 -> "all undead to nya"

            case MoveAction(_, Hastur, o, d) =>
                needRegion(d)

                oncePerRound.contains(ScreamingDead) && d.allies(KingInYellow).any && active.%(_.allSB).any |=> 1000000 -> "hastur shadow kiy"

                d.allies(KingInYellow).any && d.foes.goos.active.any |=> 1000000 -> "hastur shadow kiy"

                d.allies(KingInYellow).any && o.foes.goos.active.any |=> 1000000 -> "hastur shadow kiy"

                power == 1 && d.allies(KingInYellow).any |=> 1000000 -> "hastur shadow kiy"

                !oncePerRound.contains(ScreamingDead) && d.allies(KingInYellow).any |=> 100 -> "hastur shadow kiy"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.player.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                checkAttack(r, f, allies, foes, 1)

            case CaptureAction(_, r, f) =>
                val safe = active.%(f => f.strength(game, f.at(r).diff(f.at(r).cultists.take(1)), self) > r.allies.num).none

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
                safe && r.enemyGate && f == r.owner && r.controllers.num == 1 && f.has(CursedSlumber) && f.power > 0 && f.player.gates.%(_.glyph == Slumber).none |=> 7000 -> "capture and prevent slumber"
                safe && r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 3000 -> "capture and nearly open gate"

                true |=> 2000 -> "capture"
                r.enemyGate |=> 100 -> "enemy gate"
                power == 1 && oncePerRound.contains(ScreamingDead) |=> 6000 -> "capture now"
                forgetGates |=> -7000 -> "forget gatesncapt"

            case BuildGateAction(_, r) =>
                impunity && r.foes.goos.active.none && (r.allies.goos.any || r.allies.monsters.any || (active.%(_.at(r).monsters.any).num < r.allies.cultists.num)) && (r.allies.goos.any || power > 3 || (active.%(f => f.power > 2 || f.at(r).cultists.any).none)) && self.gates.num < 4 |=> 70000 -> "impunity build gate"

                WW.exists && game.board.starting(WW).contains(r) |=> -10000000 -> "starting ww"
                GC.exists && game.board.starting(GC).contains(r) |=> -10000000 -> "starting gc"

                have(Hastur) && have(KingInYellow) && game.desecrated.num < 12 |=> -1000000 -> "go desecrate instead"

                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -700 -> "cthulhu has dreams"
                power >= 3 + maxEnemyPower && maxEnemyPower < 10 && r.capturers.none && !canStrikeCC(r) && !canStrikeGC(r) |=> 150 -> "building gates is ok"
                self.gates.num <= 1 && !have(Hastur) && r.capturers.none && !canStrikeCC(r) && !canStrikeGC(r) |=> 500 -> "building gates is good"
                r.allies.goos.num == 2 && r.foes.goos.none && r.desecrated && active.%(_.allSB).any |=> 600 -> "tower defense"
                forgetGates |=> -3000 -> "forget gates"

                needRegion(r)

            case RecruitAction(_, Acolyte, r) =>
                impunity && r.freeGate |=> 300000 -> "impunity recruit cultist"
                impunity && !r.freeGate |=> 100000 -> "impunity recruit cultist"

                r.capturers.%(_.power > 0).any |=> -2000 -> "don't recruit to be captured"
                r.freeGate && !forgetGates |=> 1700 -> "free gate"
                r.freeGate && r.allies.goos.any && r.foes.goos.active.none && !forgetGates |=> 5700 -> "free gate"
                r.allies.goos(Hastur).any && r.allies.num == 1 |=> 600 -> "lone hastur"
                self.pool.cultists.num >= power && self.pool.goos.any |=> 300 -> "recover lost cultists"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 1 |=> 250 -> "near goo 1"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 2 |=> 240 -> "near goo 2"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 3 |=> 230 -> "near goo 3"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 4 |=> 220 -> "near goo 4"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 5 |=> 210 -> "near goo 5"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 6 |=> 200 -> "near goo 6"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 7 |=> 190 -> "near goo 7"
                self.pool.cultists.num >= power && r.allies.goos.any && r.allies.num == 8 |=> 180 -> "near goo 8"
                have(Hastur) && r.ownGate && r.allies.cultists.num == 1 |=> -100 -> "cultists not friends"
                have(Hastur) && r.ownGate && r.allies.cultists.num == 2 |=> -200 -> "cultists not friends"
                have(Hastur) && r.ownGate && r.allies.cultists.num >= 3 |=> -250 -> "own gate"

                allSB && self.realDoom >= 27 && r.allies.goos.any |=> 300 -> "shields up"

                have(Hastur) && !have(KingInYellow) && r.noGate && r.allies.cultists.none |=> 100 -> "recruit to awaken kiy"

                needRegion(r)

            case SummonAction(_, Undead, r) =>
                have(Hastur) && have(ThirdEye) && r.allies(KingInYellow).any && !r.desecrated |=> -10000000 -> "desecrate instead"

                impunity && r.controllers.num == 1 && r.allies.num == r.controllers.num && r.foes.goos.none && r.foes.monsters.active.any |=> 300000 -> "prevent losing gate"

                impunity && r.controllers.num >= 2 && r.allies.num == r.controllers.num && r.foes.goos.none && r.foes.monsters.active.any && self.all.cultists.num < 6 |=> 200000 -> "prevent losing gate"

                r.capturers.%(f => f.needs(CaptureCultist) && r.of(f).goos.none).any |=> 200 -> "prevent capture spellbook"

                r.allies(Hastur).any |=> 50 -> "summon to has"
                r.allies(Hastur).any && r.allies.num < 3 && active.%(_.allSB).any |=> 7000 -> "shield hastur"
                r.allies(Hastur).any && r.allies.num < 4 && active.%(_.allSB).any |=> 3000 -> "shield hastur"

                r.allies(KingInYellow).any |=> 150 -> "summon to kiy"
                r.controllers.num == 1 && r.allies.monsters.none && r.foes.monsters.any && r.foes.goos.none |=> 60 -> "prevent losing gate"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 600 -> "prevent losing gate"
                r.controllers.num == r.allies.num && r.allies.num == 1 && r.foes.goos.active.none && r.foes.monsters.active.any && (power < 10 || have(Hastur)) |=> 6200 -> "prevent losing gate"
                r.controllers.num == r.allies.num && r.allies.num == 2 && r.foes.goos.active.none && r.foes.monsters.active.any && (power < 10 || have(Hastur)) |=> 5200 -> "prevent losing gate"
                r.controllers.num == r.allies.num && r.allies.num >= 3 && r.foes.goos.active.none && r.foes.monsters.active.any && (power < 10 || have(Hastur)) |=> 4200 -> "prevent losing gate"

                r.allies.goos.num == 2 && r.foes.goos.any |=> 9000 -> "summon for defense"
                r.controllers.num == 1 && r.allies.num == 1 && r.foes.goos.none |=> 160 -> "prevent losing gate"

                allSB && self.realDoom >= 27 && r.allies.goos.any |=> 300 -> "shields up"

                needRegion(r)

            case SummonAction(_, Byakhee, r) =>
                have(Hastur) && have(ThirdEye) && r.allies(KingInYellow).any && !r.desecrated |=> -10000000 -> "desecrate instead"

                r.allies.goos.any && r.allies(Byakhee).none |=> 100 -> "summon to goo"
                r.allies.goos.any && r.allies(Byakhee).any |=> 10 -> "summon another"

                allSB && self.realDoom >= 27 && r.allies.goos.any |=> 300 -> "shields up"

                needRegion(r)

            case AwakenAction(_, KingInYellow, r, _) =>
                need(DesecrateWW) || need(DesecrateOO) || need(DesecrateAA) |=> 5000 -> "need kiy to desecrate"
                r.desecrated |=> -1500 -> "already desecrated"
                r.allies.num >= 5 |=> 500 -> "many allies"
                r.allies.num == 4 |=> 400 -> "4 allies"
                r.allies.num == 3 |=> 300 -> "3 allies"
                r.allies.num == 2 |=> 200 -> "2 allies"

                power > 4 && r.glyph == GlyphWW && need(DesecrateWW) && self.numSB >= 5 && (r.allies(Hastur).any || r.allies.num > 3) |=> 100000 -> "need kiy desecrate ww"
                power > 4 && r.glyph == GlyphAA && need(DesecrateAA) && self.numSB >= 5 && (r.allies(Hastur).any || r.allies.num > 3) |=> 100000 -> "need kiy desecrate aa"
                power > 4 && r.glyph == GlyphOO && need(DesecrateOO) && self.numSB >= 5 && (r.allies(Hastur).any || r.allies.num > 3) |=> 100800 -> "need kiy desecrate oo"

                r.allies(Hastur).any && r.allies.notGoos.num < 2 && canStrikeCC(r) |=> -6000 -> "nya can crush"
                r.allies(Hastur).none && r.allies.notGoos.num < 4 && canStrikeCC(r) |=> -6000 -> "nya can crush"

                r.allies(Hastur).any && r.allies.notGoos.num < 2 && canStrikeGC(r) |=> -6000 -> "cth can crush"
                r.allies(Hastur).none && r.allies.notGoos.num < 4 && canStrikeGC(r) |=> -6000 -> "cth can crush"

                power > 4 |=> 2000 -> "yes awaken"
                power == 4 |=> 1400 -> "maybe awaken"
                r.foes.goos.any |=> -300 -> "don't feed to goos"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsters.any |=> -100 -> "don't feed to monsters"

                needRegion(r)

            case AwakenAction(_, Hastur, r, _) =>
                power > 10 |=> 400000 -> "yes awaken"
                power >= 10 && others.%(_.power >= 10).any |=> 400000 -> "yes awaken"
                power >= 10 && others.%(_.allSB).any |=> 3400 -> "maybe awaken"
                r.foes.goos.any |=> 300 -> "awaken to battle"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsters.any |=> 100 -> "awaken to defend"

            case Provide3DoomAction(_, f) =>
                others.%(f => f.power == 1).any && self.power > 1 |=> 300 -> "maybe"
                self.numSB == 5 |=> 10000 -> "finally"
                f.aprxDoom != others./(_.aprxDoom).min |=> -100 -> "not in the last place"
                true |=> 10000000 -> "start with it"

            case DesecrateMainAction(_, r, te) =>
                val n = r.allies.num
                val h = te

                n >= 6 && power == 2 |=> 950 -> "desecrate six on last power"

                h && n >= 6 |=> 4000 -> "desecrate max"
                h && n == 5 |=> 4000 -> "desecrate 5"
                h && n == 4 |=> 4000 -> "desecrate 4"
                h && n == 3 |=> 3000 -> "desecrate 3"
                h && n == 2 |=> 2000 -> "desecrate 2"
                h && n == 1 |=> 1000 -> "desecrate 1"

                h && r.freeGate && self.pool.cultists.any |=> 6000 -> "desecrate cultist"

                impunity && self.pool.cultists.any && r.freeGate && h |=> 400000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && r.freeGate && !h && n >= 6 |=> 200000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && r.freeGate && !h && n == 5 |=> 190000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && r.freeGate && !h && n == 4 |=> 180000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && r.freeGate && !h && n == 3 |=> 170000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && r.freeGate && !h && n == 2 |=> 160000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && r.freeGate && !h && n == 1 |=> 150000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && h && n >= 6 |=> 200000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && h && n == 5 |=> 180000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && h && n == 4 |=> 160000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && h && n == 3 |=> 140000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && h && n == 2 |=> 130000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && h && n == 1 |=> 120000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && !h && n >= 6 |=> 110000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && !h && n == 5 |=>  90000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && !h && n == 4 |=>  70000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && !h && n == 3 |=>  50000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && !h && n == 2 |=>  30000 -> "impunity desecrate cultist"
                impunity && self.pool.cultists.any && !r.freeGate && !h && n == 1 |=>  10000 -> "impunity desecrate cultist"

                impunity && self.pool.cultists.none && !r.freeGate && h && n >= 6 |=> 110000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && h && n == 5 |=>  90000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && h && n == 4 |=>  70000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && h && n == 3 |=>  50000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && h && n == 2 |=>  30000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && h && n == 1 |=>  10000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && !h && n >= 6 |=> 55000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && !h && n == 5 |=> 45000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && !h && n == 4 |=> 35000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && !h && n == 3 |=> 25000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && !h && n == 2 |=> 15000 -> "impunity desecrate"
                impunity && self.pool.cultists.none && !r.freeGate && !h && n == 1 |=>  5000 -> "impunity desecrate"

                r.glyph == GlyphWW && need(DesecrateWW) && self.numSB >= 4 |=> 800 -> "need desecrate ww"
                r.glyph == GlyphAA && need(DesecrateAA) && self.numSB >= 4 |=> 800 -> "need desecrate aa"
                r.glyph == GlyphOO && need(DesecrateOO) && self.numSB >= 4 |=> 800 -> "need desecrate oo"

                r.glyph != GlyphWW && need(DesecrateWW) && game.desecrated.num == 11 |=> -1000000 -> "dont waste last desecrate"
                r.glyph != GlyphAA && need(DesecrateAA) && game.desecrated.num == 11 |=> -1000000 -> "dont waste last desecrate"
                r.glyph != GlyphOO && need(DesecrateOO) && game.desecrated.num == 11 |=> -1000000 -> "dont waste last desecrate"

                need(DesecrateWW) && r.glyph == GlyphWW && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 75000 -> "desecrate ww super last"
                need(DesecrateAA) && r.glyph == GlyphAA && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 75000 -> "desecrate aa super last"
                need(DesecrateOO) && r.glyph == GlyphOO && numSB == 5 && (self.realDoom >= 24 || game.desecrated.num > 10) |=> 75000 -> "desecrate oo super last"

                need(DesecrateWW) && r.glyph == GlyphWW && numSB == 5 |=> 7500 -> "desecrate ww last"
                need(DesecrateAA) && r.glyph == GlyphAA && numSB == 5 |=> 7500 -> "desecrate aa last"
                need(DesecrateOO) && r.glyph == GlyphOO && numSB == 5 |=> 7500 -> "desecrate oo last"

                r.allies(Hastur).any && r.allies.num < 3 && active.%(_.allSB).any |=> 8000 -> "shield hastur"

                h && self.realDoom >= 28 && allSB |=> 1000000 -> "go go go"

                r.allies.notGoos.none && r.foes.monsters.active.any |=> 100 -> "desecrate for protection"

                r.allies.goos.num == 2 && r.foes.goos.any |=> 10000 -> "desecrate for defense"

            case DesecratePlaceAction(_, r, Acolyte) =>
                impunity |=> 1000000 -> "impunity place cultist"

                r.allies.cultists.none |=> 800 -> "no cultists"
                self.pool.cultists.num >= power |=> 700 -> "recover lost cultists"
                true |=> 400 -> "cultist good"

            case DesecratePlaceAction(_, r, Undead) =>
                self.pool(Undead).num > self.pool(Byakhee).num |=> 650 -> "undead out"
                true |=> 500 -> "more undead"

            case DesecratePlaceAction(_, r, Byakhee) =>
                r.allies(Byakhee).none && r.allies.cultists.none |=> 750 -> "no byakhee and no cultist"
                r.allies(Byakhee).none |=> 400 -> "no byakhee"
                true |=> 200 -> "byakhee ok"

            case HWINTBNAction(_, o, d) =>
                o.allies(KingInYellow).any && power == 1 |=>  -1000 -> "stay with kiy"

                o.allies(KingInYellow).any && active.%(_.allSB).any |=> -1000 -> "stay with kiy"

                (canStrikeCC(o) || canStrikeGC(o)) && !canStrikeCC(d) && !canStrikeGC(d) && d.allies.num > o.allies.num && o.allies.num < 4 |=> 8000 -> "less agr, more allies"

                o.noGate && d.ownGate && d.allies.num >= o.allies.num |=> 1000 -> "go own gate"

                power > 5 && d.noGate && !have(KingInYellow) && d.allies.num + self.all(Byakhee).num >= 4 && !d.desecrated |=> 200000 -> "go awaken kiy"
                power > 5 && d.noGate && !have(KingInYellow) && d.allies.num + self.all(Byakhee).num >= 4 && d.desecrated |=> 50000 -> "go awaken kiy"

                d.enemyGate && d.allies.any |=> 20 -> "if nothing else enemy gate"

                d.ownGate && d.allies.num >= o.allies.num |=> 10 -> "if nothing else own gate"


                d.enemyGate && d.foes.num == 1 && d.foes.cultists == 1 && d.allies.cultists.any && power > 1 |=> 200 -> "get gate"

                d.enemyGate && d.foes.num == 1 && d.foes.cultists == 1 && self.pool.cultists.any && power > 1 |=> 150 -> "get gate"

                needRegion(d)

                if (power > 1 && d.foes(Nyarlathotep).any && (CC.power == 0 || (allSB && !game.battled.contains(d))) && CC.aprxDoom > others./(_.aprxDoom).min + 3 && GC.power == 0) {
                    val bbbb = (power > 2 && have(Shriek)).??(self.all(Byakhee).%(_.region != d))

                    checkAttack(d, CC, d.of(self) ++ o.of(self)(Hastur) ++ bbbb, d.of(CC), bbbb.any.?(3).|(2))
                }

                if (power > 1 && d.foes(Cthulhu).any && (GC.power == 0 || (allSB && !game.battled.contains(d))) && GC.power < 4 && GC.aprxDoom > others./(_.aprxDoom).max - 3 && CC.power == 0) {
                    val bbbb = (power > 2 && have(Shriek)).??(self.all(Byakhee).%(_.region != d))

                    checkAttack(d, GC, d.of(self) ++ o.of(self)(Hastur) ++ bbbb, d.of(GC), bbbb.any.?(3).|(2))
                }

            case ShriekAction(_, d) =>
                val count = self.all(Byakhee).%(_.region != d).num
                count == 1 |=> -10000 -> "dont even bother"
                count == 2 |=> -4000 -> "dont bother"
                true |=> -1000 -> "nead reason"

                power > 1 && need(DesecrateWW) && d.glyph == GlyphWW && numSB == 5 && d.allies.num < 3 && count > 2 && (self.realDoom >= 24 || game.desecrated.num > 10) && (d.allies(KingInYellow).any || (!have(KingInYellow) && d.noGate)) |=> 80000 -> "shriek desecrate ww super last"
                power > 1 && need(DesecrateAA) && d.glyph == GlyphAA && numSB == 5 && d.allies.num < 3 && count > 2 && (self.realDoom >= 24 || game.desecrated.num > 10) && (d.allies(KingInYellow).any || (!have(KingInYellow) && d.noGate)) |=> 80000 -> "shriek desecrate aa super last"
                power > 1 && need(DesecrateOO) && d.glyph == GlyphOO && numSB == 5 && d.allies.num < 3 && count > 2 && (self.realDoom >= 24 || game.desecrated.num > 10) && (d.allies(KingInYellow).any || (!have(KingInYellow) && d.noGate)) |=> 80000 -> "shriek desecrate oo super last"

                d.allies.num == 1 |=> 10 -> "allies 1"
                d.allies.num == 2 |=> 10 -> "allies 2"
                d.allies.num == 3 |=> 10 -> "allies 3"
                d.allies.num >= 4 |=> 10 -> "allies 4"

                needRegion(d)

                d.allies(Hastur).any && d.allies(KingInYellow).any && power == 1 |=> 8000 -> "shield haskiy"

                d.allies(Hastur).any && d.allies(KingInYellow).any && d.foes.goos.active.any |=> 8000 -> "shield haskiy"

                d.allies(Hastur).any && d.allies(KingInYellow).any && (canStrikeCC(d) || canStrikeGC(d)) |=> 800 -> "shield haskiy"

                d.ownGate && d.allies(Hastur).any && d.foes.goos.%(_.faction.active).any |=> 9000 -> "shield hastur"

                d.noGate && d.allies(Hastur).any && !have(KingInYellow) && power > 5 |=> 8000 -> "shield hastur for kiy awakening"

                d.allies(KingInYellow).any && ((!d.desecrated && have(Hastur)) || oncePerRound.contains(ScreamingDead)) && d.allies.num < 4 |=> 4500 -> "shield kiy"

                if (power > 1 && d.allies(Hastur).any && d.foes(Nyarlathotep).any && (CC.power == 0 || (allSB && !game.battled.contains(d))) && CC.aprxDoom > others./(_.aprxDoom).min + 3) {
                    val bbbb = self.all(Byakhee).%(_.region != d)

                    checkAttack(d, CC, d.of(self) ++ bbbb, d.of(CC), 2)
                }

                if (power > 1 && d.allies(Hastur).any && d.foes(Cthulhu).any && (GC.power == 0 || (allSB && !game.battled.contains(d))) && GC.power < 4 && GC.aprxDoom > others./(_.aprxDoom).max - 3) {
                    val bbbb = self.all(Byakhee).%(_.region != d)

                    checkAttack(d, GC, d.of(self) ++ bbbb, d.of(GC), 2)
                }

                allSB && self.realDoom >= 27 && d.allies.goos.any |=> 300 -> "shields up"

            case ShriekFromAction(_, o, r) =>
                true |=> 1000 -> "go"

            case ZingayaAction(_, r, f) =>
                true |=> 20 -> "flat"
                r.enemyGate && r.owner == f && r.allies.cultists.any && r.controllers.num == 1 |=> 1500 -> "zingaya off gate"
                r.enemyGate && r.owner == f && r.controllers.num == 1 |=> 150 -> "zingaya off gate"

            case AvatarReplacementAction(_, _, r, o, uc) =>
                val u = self.at(r, uc).head
                u.cultist && o.capturers.%(_.power > 0).any |=> -100 -> "don't send cultist to be captured"
                u.cultist && o.capturers.none |=> 150 -> "no capturers"
                u.cultist && o.capturers.any && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                u.monster && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monster && u.friends.cultists.num > 1 && u.friends.monsters.none && r.foes.monsters./(_.faction).%(_ != BG).%(_.power > 0).any |=> -200 -> "dont sent temp defender"

            case RevealESAction(_, es, false, _) if game.of(self).es != es =>
                true |=> -10000 -> "better reveal all"

            case RevealESAction(_, _, _, next) if next == DoomCancelAction(self) =>
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

            case GhrothUnitAction(_, uc, r, f, _) =>
                val c = self.at(r, uc).head
                c.friends.cultists.none && c.region.capturers.%(!_.blind(f)).any |=> 1000 -> "will be captured anyway"
                c.gateKeeper |=> -900 -> "gate keeper"
                c.friends.cultists.none && c.region.capturers.any |=> 800 -> "can be captured one"
                c.friends.cultists.any && c.region.capturers.any |=> 700 -> "can be captured many"
                c.friends.cultists.any |=> 600 -> "cultist friends"
                c.friends.goos.any |=> -500 -> "goo huggers"
                c.friends.none && r.desecrated |=> -400 -> "sole feast"

            case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case MainDoneAction(_) =>
                (oncePerRound.contains(HWINTBN) || oncePerRound.contains(ScreamingDead)) && impunity |=> 500000 -> "double action impunity done"
                (oncePerRound.contains(HWINTBN) || oncePerRound.contains(ScreamingDead)) && active.%(_.allSB).none |=> 5000 -> "double action done"
                true |=> 500 -> "main done"

            case _ =>
        }

        // BATTLE
        if (game.battle != null) {
            val battle = game.battle

            def elim(battle : Battle, u : UnitFigure) {
                val opponent = battle.side(self).opponent
                u.is(Acolyte) && u.faction == self && self.has(Passion) && !game.of(self).oncePerAction.contains(Passion) && battle.units(self).%(u => u.is(Acolyte) && u.health == Killed).none |=> 1200 -> "elim acolyte with passion"
                u.is(Undead) |=> 800 -> "elim undead"
                u.is(Acolyte) && u.faction == self |=> 400 -> "elim acolyte"
                u.is(Byakhee) |=> 200 -> "elim byakhee"
                u.is(KingInYellow) |=> 100 -> "elim kiy"
                u.is(KingInYellow) && opponent.has(Harbinger) && opponent.rolls.count(Pain) + opponent.rolls.count(Kill) >= battle.side(self).units.num && opponent.faction.power > 1 |=> 10000 -> "sacrifice kiy"
                u.is(Hastur) |=> -1 -> "elim has"

                u.faction != self |=> u.uclass.cost * 100 -> "cost * 100"
                u.is(Acolyte) && u.faction != self |=> 150 -> "elim enemy acolyte"
            }

            def retreat(battle : Battle, u : UnitFigure) {
                u.gateKeeper && u.faction == self && battle.side(self).units./(battle.canAssignPains).sum > 2 |=> -1000 -> "retr gate keeper"
                u.is(Acolyte) && u.faction == self |=> 800 -> "retr acolyte"
                u.is(Byakhee) |=> 400 -> "retr byakhee"
                u.is(Undead) |=> 200 -> "retr undead"
                u.is(KingInYellow) |=> -10000 -> "retr kiy"
                u.is(Hastur) |=> -100000 -> "retr has"

                u.faction != self |=> u.uclass.cost * 100 -> "cost * 100"
                u.is(Acolyte) && u.faction != self |=> 50 -> "pain enemy acolyte"
            }

            val opponent = battle.opponent(self)
            val allies = battle.units(self)
            val enemies = battle.units(opponent)
            val first = battle.attacker == self

            a match {
                case DevourAction(_, u) =>
                    elim(battle, u)

                case AbductAction(_, _, u) =>
                    elim(battle, u)

                case DemandSacrificeKillsArePainsAction(_) =>
                    battle.strength(self) < battle.strength(battle.opponent(self)) |=> 1000 -> "less str"
                    battle.strength(self) > battle.strength(battle.opponent(self)) |=> -1000 -> "more str"

                case UnholyGroundEliminateAction(_, _, r, ur) =>
                    val u = game.unit(ur)
                    u.uclass == KingInYellow |=> 1000 -> "kiy yes"
                    u.uclass == Hastur |=> -1000 -> "hastur no"

                case AssignKillAction(_, _, _, u) =>
                    elim(battle, u)

                case AssignPainAction(_, _, _, u) =>
                    retreat(battle, u)

                case EliminateNoWayAction(_, u) =>
                    elim(battle, u)

                case RetreatUnitAction(_, u, r) =>
                    u.cultist && r.allies.monsters.any |=> 1000 -> "send cultist to be protected by monsters"
                    u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                    u.cultist && r.allies.cultists.any |=> -3000 -> "dont group cultists"
                    u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                    u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                    u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                    u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                    u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                    u.is(Undead) && r.allies(Hastur).any |=> 4000 -> "undead go has"
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

                    u.monster && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                    u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                    u.monster && r.foes.%(_.vulnerableM).any && !r.foes.goos.any && r.allies.monsters.none |=> 1000 -> "send monster to capture"
                    u.goo && r.foes.%(_.vulnerableG).any |=> 1000 -> "send goo to capture"

                    u.monster && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                    u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                    u.monster && r.ownGate && r.foes.goos.active.none |=> 400 -> "send monster to own gate"
                    u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                    u.monster && r.freeGate |=> 300 -> "send monster to free gate"
                    u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                    u.monster && r.enemyGate |=> 300 -> "send monster to enemy gate"
                    u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                    u.monster && r.foes(Tsathoggua).any |=> -450 -> "dont send monster to tsa"

                    power > 1 && u.is(KingInYellow) && have(Shriek).??(self.all(Byakhee).num) > 2 && !r.desecrated |=> 2000 -> "kiy retreat undesecrated"

                    u.goo && r.allies.goos.any |=> 5000 -> "goo to goo"

                    if (u.goo)
                        result ++= eval(MoveAction(u.faction, u.uclass, u.region, r))

                    true |=> (game.board.connected(r) ++ game.board.connected(r).flatMap(game.board.connected)).distinct.num -> "reachable regions"

                case _ =>
            }
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
