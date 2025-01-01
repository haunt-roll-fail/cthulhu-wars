package cws

import colmat._

object BotWW extends BotX(g => new GameEvaluationWW(g))

class GameEvaluationWW(game : Game) extends GameEvaluation(game, WW) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        def opposite = game.board.starting(self).but(game.starting(self)).head
        def pole(r : Region) = game.board.starting(self).contains(r)

        def canStrikeCC(r : Region) = (r.foes.has(Nyarlathotep) && CC.power > 0) || (CC.power > 1 && CC.allSB && r.near012.%(_.foes(Nyarlathotep).any).any)
        def canStrikeGC(r : Region) = (r.foes.has(Cthulhu) && GC.power > 0) || (GC.power > 1 && GC.allSB && r.near.%(_.foes(Cthulhu).any).any) || (GC.power > 0 && GC.allSB && GC.at(GC.deep).any)

        def checkAttack(r : Region, f : Faction, allies : List[UnitFigure], foes : List[UnitFigure], d : Int) {
            val enemyStr = f.strength(game, foes, self)
            val ownStr = self.strength(game, allies, f)

            val igh = others.%(_.has(Necrophagy))./(_.all(Ghoul).diff(foes).num).sum
            val ep = f.player

            var ac = allies(Acolyte).num
            var we = allies(Wendigo).num
            var gk = allies(GnophKeh).num
            val rha = allies.has(RhanTegoth)
            val ith = allies.has(Ithaqua)

            val rhas = rha.??(3)
            val iths = ith.??((f.doom + 1) / 2)

            f match {
                case GC =>
                    var ec = foes(Acolyte).num
                    var dp = foes(DeepOne).num
                    var sh = foes(Shoggoth).num
                    var ss = foes(Starspawn).num
                    var cth = foes.has(Cthulhu)

                    if (cth) {
                        if (ac > 0)
                            ac -= 1
                        else
                        if (we > 0)
                            we -= 1
                        else
                        if (gk > 0)
                            gk -= 1
                    }

                    var shield = ac + we + gk

                    val ownStr = we + gk * 3 + rhas + iths

                    var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(6)

                    val enough1 = shield * 5 > enemyStr * 4 && ownStr >= foes.num * 3
                    val enough2 = shield * 5 > enemyStr * 3 && ownStr >= 12

                    enough1 && (cth || foes.num > 1) |=> 11000 -> "fight gc 1"
                    enough2 && (cth || foes.num > 2) |=> 10000 -> "fight gc 2"

                case BG =>
                    var ec = foes(Acolyte).num
                    var gh = foes(Ghoul).num
                    var fu = foes(Fungi).num
                    var dy = foes(DarkYoung).num
                    var shu = foes.has(ShubNiggurath)

                    var shield = ac + we + gk

                    val ownStr = we + gk * 3 + rhas + iths

                    var enemyStr = ec * ep.has(Frenzy).??(1) + fu + dy * 2 + shu.??(ep.gates.num + ep.all(Acolyte).num + ep.all(DarkYoung).num * ep.has(RedSign).??(1))

                    val enough1 = shield * 5 > enemyStr * 4 && ownStr >= foes.num * 3
                    val enough2 = shield * 5 > enemyStr * 3 && ownStr >= 12

                    enough1 && (shu || dy - gh > 0 || foes.num - gh > 1) |=> 11000 -> "fight bg 1"
                    enough2 && (shu || dy - gh > 0 || foes.num - gh > 2) |=> 10000 -> "fight bg 2"

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
                        if (we > 0)
                            we -= 1
                        else
                        if (gk > 0)
                            gk -= 1
                    }

                    var inv = f.has(Invisibility).??(fp)

                    while (inv > 0) {
                        inv -= 1
                        if (gk > 0)
                            gk -= 1
                        else
                        if (we > 0)
                            we -= 1
                        else
                        if (ac > 0)
                            ac -= 1
                    }

                    var shield = ac + we + gk + (rha && power > 1).??(1)

                    if (!ith && gk == 0)
                        shield *= 2

                    val ownStr = we + gk * 3 + rhas + iths

                    var ihh = f.has(SeekAndDestroy).??(f.all(HuntingHorror).diff(foes).num)

                    var enemyStr = fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB)

                    val enough = shield * 5 > enemyStr * 4 && ownStr > foes.num * 3

                    f.has(Invisibility) && fp == foes.num |=> -1000000 -> "invis"
                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"
                    rha && nya && enough && (ownStr > 2 + (foes.num - 1) * 6 || (!f.active && ownStr > enemyStr && power > 1)) |=> 13000/d -> "rha attack nya"

                    ith && ec < foes.num && enough |=> 12000/d -> "attack cc"
                    ith && nya && enough |=> 19000/d -> "attack nya"
                    hh == foes.num && enough && ownStr > 3 |=> 11000/d -> "attack hh"
                    hh + fp == foes.num && enough && ownStr > 4 |=> 10000/d -> "attack hhfp"

                    0 -> "todo"

                case YS =>
                    var ec = foes(Acolyte).num
                    var un = foes(Undead).num
                    var by = foes(Byakhee).num
                    var kiy = foes.has(KingInYellow)
                    var has = foes.has(Hastur)

                    val shield = ac + we + gk

                    val ownStr = we + gk * 3 + rhas + iths

                    val enemyStr = (un > 0).??(un - 1) + (by > 0).??(by + 1) + has.??(game.ritualCost)

                    val enough1 = shield * 5 > enemyStr * 4 && ownStr >= foes.num * 3
                    val enough2 = shield * 5 > enemyStr * 3 && ownStr >= 12

                    enough1 && !has && (kiy || foes.num > 1) |=> 11000 -> "fight ys 1"
                    enough2 && !has && (kiy || foes.num > 1) |=> 10000 -> "fight ys 2"
                    enough1 && has && !ith |=> 9000 -> "fight ys 3"

                case SL =>
                    var ec = foes(Acolyte).num
                    var wz = foes(Wizard).num
                    var sm = foes(SerpentMan).num
                    var fs = foes(FormlessSpawn).num
                    var tsa = foes.has(Tsathoggua)

                    val shield = ac + we + gk

                    val ownStr = we + gk * 3 + rhas + iths

                    val enemyStr = wz + sm + fs * (ep.all(FormlessSpawn).num + ep.all(Tsathoggua).num) + tsa.??(max(2, power - 1))

                    val enough1 = shield * 5 > enemyStr * 4 && ownStr >= foes.num * 3
                    val enough2 = shield * 5 > enemyStr * 3 && ownStr >= 12

                    enough1 && (tsa || fs > 0 || foes.num > 1) |=> 11000 -> "fight sl 1"
                    enough2 && (tsa || fs > 0 || foes.num > 2) |=> 10000 -> "fight sl 2"

                case OW =>
                    0 -> "todo"

                case AN =>
                    allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                    AN.has(Extinction) && foes.monsters.num == 1 && foes(Yothan).any && ((rha && self.power > 1) || (ith && allies.num >= 3 && ownStr >= 6) || (allies.goos.none && ownStr >= 6)) |=> 1000 -> "attack lone extinct yothan"
            }
        }

        a match {
            case FirstPlayerAction(_, f) =>
                f == self && game.board.regions.%(_.allies.goos.any).%(_.foes.goos.any).any |=> 100 -> "play first goos together"
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

                abs(game.factions.indexOf(f) - game.factions.indexOf(self)).abs == 2 |=> 10 -> "stall opposite"
                f == CC && !CC.allSB |=> 1 -> "cc first"
                CC.allSB |=> 1000 -> "first, cc allsb"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            case SpellbookAction(_, sb, _) => sb match {
                case Cannibalism =>
                    true |=> 1000 -> "must have if have rha and ith"
                case Howl =>
                    true |=> 700 -> "too good"
                case ArcticWind =>
                    true |=> 800 -> "very good"
                    have(Ithaqua) |=> 1200 -> "must have if ith"
                case Herald =>
                    true |=> 900 -> "just good"
                    have(Ithaqua) && have(RhanTegoth) |=> 1100 -> "must have if both goos"
                case IceAge =>
                    true |=> 1600 -> "almost good"
                case Berserkergang =>
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

                self.pool.goos.none && cost == 5 |=> 2000 -> "very much"
                power - cost > 13 |=> 2000 -> "very much"
                power - cost > 10 && maxDoomGain > 4 |=> 1200 -> "much"
                power - cost > 8 && maxDoomGain > 5 |=> 1100 -> "minimuch"

            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"

            case MainDoneCancelAction(_) =>
                true |=> 0 -> "cancel"

            case MoveAction(_, GnophKeh, o, d) =>
                true |=> -100 -> "gnophkeh dont move"
                o.ownGate && o.foes.monsters.active.any && o.allies.monsters.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, Wendigo, o, d) =>
                o.allies(RhanTegoth).any |=> -200 -> "wendigo dont leave rha"
                o.ownGate && o.foes.monsters.active.any && o.allies.monsters.num == 1 && o.allies.goos.none |=> -500 -> "dont abandon gate"
                o.ownGate |=> -20 -> "leave own gate"

            case MoveAction(_, RhanTegoth, o, d) =>
                self.has(ScreamingDead) && !oncePerRound.contains(ScreamingDead) |=> -1000 -> "dont walk just scream"

                val protecting1 = o.ownGate && o.allies.goos.num == 1 && o.foes.goos.active.any
                val protecting2 = o.ownGate && o.allies.goos.num == 1 && o.allies.monsters.none && o.foes.monsters.active.any
                val protecting3 = o.ownGate && o.allies.goos.num == 1 && YS.power > 1 && YS.has(Hastur)
                val protecting4 = o.ownGate && o.allies.goos.num == 1 && YS.power > 1 && o.near.%(_.foes(KingInYellow).any).any
                val protecting5 = o.ownGate && o.allies.goos.num == 1 && BG.power > 0 && BG.has(ShubNiggurath) && o.allies.cultists.num == 1

                val protecting = protecting1 || protecting2 || protecting3 || protecting4 || protecting5

                d.enemyGate && !(protecting) && d.controllers.num == 1 && d.controllers.monsters.none && d.foes.goos.none && d.owner.blind(self) && power > 2 |=> 950 -> "go get a gate"
                d.enemyGate && !(protecting) && d.controllers.num == 2 && d.controllers.monsters.none && d.foes.goos.none && d.owner.blind(self) && power > 3 |=> 900 -> "go get a gate"
                true && !(protecting) && d.enemyGate && d.owner.at(d).goos.none && power > 2 |=> 450 -> "maybe capture"
                true && !(protecting) && d.foes.%(_.vulnerableG).%(u => !u.faction.has(Devolve) || u.faction.pool(DeepOne).num >= u.faction.at(d).cultists.num).any && power > 2 |=> 180 -> "maybe capture"

                !o.ownGate && d.ownGate && d.allies.goos.none && d.foes.active.goos.any |=> 1350 -> "protect gate"
                o.ownGate && o.foes.active.goos.none && (have(Ithaqua) || o.foes.monsters.active.none) && d.ownGate && d.allies.goos.none && d.foes.active.goos.any |=> 1300 -> "protect gate"

                !o.ownGate && d.ownGate && others.%(_.power > 1).any |=> 350 -> "protect gate"

                o.ownGate && o.foes.any && o.allies.monsters.none |=> -2100 -> "protect gate"

                power == 1 && o.allies(Ithaqua).any |=> -5000 -> "stay with ithaqua"

                active.none && d.enemyGate && d.foes.goos.none |=> 150 -> "enemy gate imp"
                active.none && d.near.%(n => n.enemyGate && n.foes.goos.none).any |=> 125 -> "enemy gate near imp"

                d.ownGate && power == 1 |=> 100 -> "own gate"
                d.freeGate && self.gates.num < self.all.cultists.num + self.pool.cultists.num |=> 75 -> "free gate"
                d.enemyGate && d.controllers.num == 1 |=> 60 -> "enemy gate"
                d.enemyGate |=> 50 -> "enemy gate"
                d.allies.cultists.any |=> 25 -> "hug cultist"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 |=> -50000 -> "beware unholy ground"

            case MoveAction(_, Ithaqua, o, d) =>
                true |=> 100 -> "walk"

                o.allies.cultists.any && need(OppositeGate) && game.board.distance(d, opposite) < game.board.distance(o, opposite) |=> 900 -> "go build opposite gate"
                d.allies.num > o.allies.num |=> 600 -> "gather"
                d.near.%(_.allies.num > o.allies.num).any |=> 300 -> "gather 2"
                d.enemyGate && d.foes.num < o.allies.num && d.foes.goos.none |=> 400 -> "poke at gate"
                d.enemyGate && d.foes.num < o.allies.num * 2 && d.foes.goos.none |=> 200 -> "poke at gate"
                d.freeGate && o.allies.cultists.num > o.ownGate.??(1) && d.foes.goos.active.none |=> 6000 -> "free gate"
                d.freeGate && o.allies.cultists.num > o.ownGate.??(1) && d.foes.goos.active.any |=> 1500 -> "free gate with goo"
                o.foes.goos.any && active.%(f => f.strength(game, f.at(o), self) > o.allies.num * 2).any |=> 3000 -> "too hot"

                o.ownGate && d.allies.num == 1 && d.allies.goos.any && d.foes.goos.none |=> -10000 -> "rha will manage"

                o.ownGate && o.foes.active.goos.none && d.ownGate && d.allies.goos.none && d.foes.active.goos.any |=> 1300 -> "protect gate"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 |=> -50000 -> "beware unholy ground"

            case ArcticWindAction(_, o, Acolyte, r) =>
                true |=> 100 -> "move a"
                o.ownGate && o.allies.cultists.num == 1 && (o.allies.goos.any || o.foes.active.goos.none) |=> -1000 -> "leave gate keeper"

            case ArcticWindAction(_, o, Wendigo, r) =>
                true |=> 200 -> "move w"

            case ArcticWindAction(_, o, GnophKeh, r) =>
                true |=> 300 -> "move gk"

            case ArcticWindAction(_, o, RhanTegoth, r) =>
                o.ownGate |=> -2000 -> "stay on gate"
                o.noGate |=> 1000 -> "move no gate"

            case ArcticWindDoneAction(_) =>
                true |=> 0 -> "none"

            case MoveAction(_, Acolyte, o, d) =>
                // Adjusted to stop moving cultist if they will be captured at opposite pole
                need(OppositeGate) && opposite.capturers.%(_.active).none && self.all.cultists.num == 6 && game.board.distance(d, opposite) < game.board.distance(o, opposite) && game.board.distance(d, opposite) == 0 |=> 900 -> "go build opposite gate"
                need(OppositeGate) && opposite.capturers.%(_.active).none && self.all.cultists.num == 6 && game.board.distance(d, opposite) < game.board.distance(o, opposite) && game.board.distance(d, opposite) == 1 |=> 800 -> "go build opposite gate"
                need(OppositeGate) && opposite.capturers.%(_.active).none && self.all.cultists.num == 6 && game.board.distance(d, opposite) < game.board.distance(o, opposite) && game.board.distance(d, opposite) == 2 |=> 700 -> "go build opposite gate"
                need(OppositeGate) && opposite.capturers.%(_.active).none && self.all.cultists.num == 6 && game.board.distance(d, opposite) < game.board.distance(o, opposite) && game.board.distance(d, opposite) == 3 |=> 600 -> "go build opposite gate"

                val u = self.at(o, Acolyte).%!(_.has(Moved)).head
                !u.gateKeeper && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.allies.goos.any && d.foes.goos.active.none |=> 5500 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 1400 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.allies.cultists.none && self.gates.num < self.all.%(_.canControlGate).num && self.player.iceage == Some(d) && d.capturers.%(_.power > 1).none |=> 1400 -> "ic free ice gate"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                checkAttack(r, f, allies, foes, 1)

            case CaptureAction(_, r, f) =>
                r.enemyGate && f == r.owner && r.controllers.num == 1 && r.allies.cultists.none && r.foes.%(_.canControlGate).num > 1 |=> -700 -> "give gate away"
                r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 3000 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 1 && f.power > 0 |=> 4000 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 1 && f.has(CursedSlumber) && f.power > 0 && f.player.gates.%(_.glyph == Slumber).none |=> 7000 -> "capture and prevent slumber"
                r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 3000 -> "capture and nearly open gate"
                true |=> 2000 -> "capture"
                r.enemyGate |=> 100 -> "enemy gate"
                power == 1 && oncePerRound.contains(ScreamingDead) |=> 6000 -> "capture now"

            case BuildGateAction(_, r) =>
                need(OppositeGate) && opposite == r && r.capturers.none && !(YS.power > 1 && r.allies.goos.none && r.near.%(_.foes(KingInYellow).any).any) |=> 2000 -> "build gate for sb"

                r.allies(Ithaqua).any && power > 3 |=> -5000 -> "move ith"
                r.allies(Ithaqua).any && power == 3 |=> 1200 -> "ith tied"

                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && r.allies.goos.none |=> -700 -> "cthulhu ith dreams"
                power >= 3 + maxEnemyPower && maxEnemyPower < 10 && r.capturers.none && !canStrikeCC(r) && !canStrikeGC(r) |=> 150 -> "building gates is ok"
                self.gates.num <= 1 && !have(Ithaqua) && r.capturers.none && !canStrikeCC(r) && !canStrikeGC(r) |=> 500 -> "building gates is good"
                self.gates.num <= 1 && r.allies(RhanTegoth).any && power > 5 |=> 400 -> "building gates is ok"
                self.gates.num <= 1 && r.allies(Ithaqua).any && power > 5 |=> 400 -> "building gates is ok"

            case RecruitAction(_, Acolyte, r) =>
                need(OppositeGate) && opposite == r && r.allies.cultists.none && power > 3 |=> 2000 -> "build gate for sb"

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

            case SummonAction(_, Wendigo, r) =>
                r.allies(Ithaqua).any |=> 450 -> "summon to ith"
                r.allies(RhanTegoth).any |=> 50 -> "summon to rha"

                pole(r) && !have(Ithaqua) && r.allies.cultists.num > 1 |=> 200 -> "army for ith"
                pole(r) && !have(Ithaqua) && r.allies.cultists.num > 2 |=> 210 -> "army for ith"
                pole(r) && !have(Ithaqua) && r.allies.monsters.num > 0 |=> 220 -> "army for ith"
                pole(r) && !have(Ithaqua) && r.allies.monsters.num > 1 |=> 230 -> "army for ith"
                pole(r) && !have(Ithaqua) && r.allies.monsters.num > 2 |=> 240 -> "army for ith"
                pole(r) && !have(Ithaqua) && r.allies.monsters.num > 3 |=> 250 -> "army for ith"

                r.controllers.num == 1 && r.foes.monsters.any && r.foes.goos.none && !have(Ithaqua) |=> 60 -> "prevent losing gate"
                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none && !have(Ithaqua) |=> 600 -> "prevent losing gate"
                r.controllers.num == 1 && r.allies.num == 1 && r.foes.goos.none && r.foes.monsters.active.any && !have(Ithaqua) |=> 4200 -> "prevent losing gate"
                r.allies.goos.num == 2 && r.foes.goos.any |=> 9000 -> "summon for defense"

            case SummonAction(_, GnophKeh, r) =>
                val cost = self.summonCost(game, GnophKeh, r)
                r.allies(Ithaqua).any |=> 100 -> "summon to ith"
                r.allies(RhanTegoth).any |=> 50 -> "summon to rha"
                !have(Ithaqua) |=> -200 -> "no ithaqua no gnophkeh"
                cost == 4 && r.allies(Ithaqua).any && power >= others./(_.power).max |=> 250 -> "only when leading on power"
                cost == 3 && r.allies(Ithaqua).any |=> 500 -> "only to ith"
                cost == 2 && r.allies(Ithaqua).any |=> 1000 -> "yes"
                cost == 1 && r.allies(Ithaqua).any |=> 2000 -> "very yes"

            case AwakenAction(_, RhanTegoth, r, _) =>
                true |=> -1000 -> "awaken rha"

                power > 6 && need(OppositeGate) && r == opposite && r.allies.cultists.any |=> 4000 -> "awaken to cultist"
                power > 6 && need(OppositeGate) && r == opposite && self.pool.cultists.any |=> 3000 -> "awaken to recruit"

                power > 6 && !need(OppositeGate) && r.allies.num >= 4 |=> 1500 -> "awaken to allies"
                power > 5 && !need(OppositeGate) && r.ownGate && r.allies.cultists.num == 1 |=> 3000 -> "awaken to protect gate"
                power > 7 && !need(OppositeGate) && r.enemyGate |=> 2000 -> "awaken at enemy gate"
                power > 5 && !need(OppositeGate) && r.ownGate && r.foes.active.goos.any |=> 3500 -> "awaken to save gate"

            case AwakenAction(_, Ithaqua, r, _) =>
                power > 9 |=> 4000 -> "yes awaken"
                power >= 9 && r.allies.monsters.num >= 3 |=> 3400 -> "maybe awaken"
                power >= 9 && self.pool(Wendigo).none |=> 3400 -> "maybe awaken"

                r.allies.monsters.none && self.pool(Wendigo).any |=> -5000 -> "no monsters"

                r.allies(RhanTegoth).any && r.allies.notGoos.num < 2 && canStrikeCC(r) |=> -6000 -> "nya can crush"
                r.allies(RhanTegoth).none && r.allies.notGoos.num < 4 && canStrikeCC(r) |=> -6000 -> "nya can crush"

                r.allies(RhanTegoth).any && r.allies.notGoos.num < 2 && canStrikeGC(r) |=> -6000 -> "cth can crush"
                r.allies(RhanTegoth).none && r.allies.notGoos.num < 4 && canStrikeGC(r) |=> -6000 -> "cth can crush"

            case HibernateMainAction(_, n) =>
                power == n |=> 1000 -> "optimal power"
                power == n + 1 |=> 900 -> "if nothing better 1"
                power == n + 2 |=> 40 -> "if nothing better 2"
                power == n + 3 |=> 30 -> "if nothing better 3"
                power == n + 4 |=> 20 -> "if nothing better 4"
                power == n + 5 |=> 10 -> "if nothing better 5"
                others.%(ofinale).any |=> -2000 -> (others.%(ofinale).mkString("/") + " ofinale")

            case AnytimeGainElderSignsAction(_, n, _) =>
                n == 3 |=> 10000000 -> "max es"
                numSB == 5 && self.realDoom + n >= 30 |=> 10000000 -> "last sb over 30"
                true |=> -1000 -> "delay"

            case IceAgeAction(_, r) =>
                def checkPos(r : Region, f : Int => Int) {
                    r.ownGate && r.allies.goos.none |=> f(1100) -> "protect own gate"
                    r.ownGate && r.foes.goos.active.any && r.allies.goos.none |=> f(1200) -> "protect own gate from goo"
                    r.ownGate && r.foes.goos.active.any && r.allies.goos.any && active.%(f => f.strength(game, f.at(r), self) > r.allies.num * 2).any |=> f(1300) -> "protect own gate from goo attack goo"

                    r.ownGate && r.allies.goos.none && YS.power > 1 && r.near.%(_.foes(KingInYellow).any).any |=> f(1000) -> "protect own gate from kiy"




                    r.enemyGate && r.owner == GC && game.starting(GC) == r && GC.power == 10 && GC.needs(AwakenCthulhu) && !GC.has(Cthulhu) |=> f(2000) -> "prevent cth 10"
                    r.enemyGate && r.owner == GC && game.starting(GC) == r && GC.power == 4 && !GC.needs(AwakenCthulhu) && !GC.has(Cthulhu) |=> f(1200) -> "prevent cth 4"

                    r.enemyGate && r.owner == CC && CC.power == 10 && CC.gates.num == 1 && !CC.has(Nyarlathotep) |=> f(2000) -> "prevent nya"

                    r.enemyGate && r.owner == BG && BG.power == 8 && BG.gates.num == 1 && !BG.has(ShubNiggurath) |=> f(1600) -> "prevent nya"

                    r.enemyGate && r.owner == YS && YS.power == 10 && r.foes(KingInYellow).any && !YS.has(Hastur) |=> f(2100) -> "prevent has"

                    SL.power == 8 && r.foes(FormlessSpawn).any && r.foes(FormlessSpawn).num == SL.all(FormlessSpawn).num && !SL.has(Tsathoggua) |=> f(1600) -> "prevent tsa"

                    r.noGate && r.foes.cultists./(_.faction).%(f => f.power == 3 && f.all.goos.none).any |=> f(600) -> "prevent gate"

                    YS.power > 0 && ofinale(YS) && !r.desecrated && r.foes(KingInYellow).any |=> f(7000) -> "prevent ys desecrate"

                    GC.power > 0 && ofinale(GC) && game.starting(GC) == r && !GC.has(Cthulhu) |=> f(5000) -> "prevent gc reawaken"

                    CC.power > 0 && ofinale(CC) && r.foes(Nyarlathotep).any && r.foes.goos.num > 1 |=> f(6000) -> "prevent gc reawaken"
                }

                active.none |=> -5000 -> "active none"

                self.player.iceage match {
                    case None =>
                        checkPos(r, x => x)
                    case Some(o) =>
                        checkPos(o, x => -(x + 1))
                        checkPos(r, x => x)
                }

            case CannibalismAction(_, r, Acolyte) =>
                r.allies.goos.any |=> 1000 -> "allied goo"
                have(Ithaqua) && r.foes.goos.active.none |=> 800 -> "no active enemy goos"
                r.freeGate && r.foes.active.none |=> 1200 -> "free gate"
                r.freeGate && r.foes.goos.active.none && have(Ithaqua) |=> 1100 -> "free gate"
                r.freeGate && r.allies.cultists.none && self.player.iceage == Some(r) && r.capturers.%(_.power > 1).none |=> 1400 -> "free ice gate"

            case CannibalismAction(_, r, Wendigo) =>
                true |=> 500 -> "ok"
                r.allies.none && r.foes.active(Cthulhu).any |=> -1000 -> "dont feed cth"

            case CannibalismDoneAction(_) =>
                true |=> 100 -> "done"


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

            case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                result = eval(SummonAction(self, uc, r))

            case MainDoneAction(_) =>
                (oncePerRound.contains(HWINTBN) || oncePerRound.contains(ScreamingDead)) && active.%(_.allSB).none |=> 5000 -> "double action done"
                true |=> 500 -> "main done"


            case _ if game.battle != null =>
                // battle

            case _ =>
                true |=> 1000000 -> "todo"
        }

        // BATTLE
        if (game.battle != null) {
            val battle = game.battle

            def elim(battle : Battle, u : UnitFigure) {
                val opponent = battle.side(self).opponent
                u.is(Acolyte) |=> 800 -> "elim acolyte"
                u.is(RhanTegoth) && self.power > 1 |=> 500 -> "sacrifice rha"
                u.is(Wendigo) |=> 400 -> "elim wendigo"
                u.is(GnophKeh) |=> 200 -> "elim gnophkeh"
                u.is(RhanTegoth) && self.power == 0  |=> 100 -> "elim rha"
                u.is(Ithaqua) |=> -1 -> "elim ith"
            }

            def retreat(battle : Battle, u : UnitFigure) {
                u.gateKeeper && battle.side(self).units./(battle.canAssignPains).sum > 2 |=> -1000 -> "retr gate keeper"
                u.is(Acolyte) |=> 800 -> "retr acolyte"
                u.is(GnophKeh) |=> 400 -> "retr gnophkeh"
                u.is(Wendigo) |=> 200 -> "retr wendigo"
                u.is(RhanTegoth) |=> -10000 -> "retr rha"
                u.is(Ithaqua) |=> -100000 -> "retr ith"
            }

            if (battle.attacker != self && battle.defender != self) {
            }
            else {
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

                    case DemandSacrificeProvideESAction(_) =>
                        true |=> 0 -> "whatever"

                    case UnholyGroundEliminateAction(_, _, r, ur) =>
                        val u = game.unit(ur)
                        u.uclass == RhanTegoth |=> 1000 -> "rha yes"
                        u.uclass == Ithaqua |=> -1000 -> "ith no"

                    case AssignKillAction(_, _, _, u) =>
                        elim(battle, u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(battle, u)

                    case EliminateNoWayAction(_, u) =>
                        elim(battle, u)

                    case EternalPayAction(_, _, Kill) =>
                        true |=> 1000 -> "save from kill"

                    case EternalPayAction(_, _, Pain) =>
                        true |=> -1000 -> "dont save from pain"

                    case EternalIgnoreAction(_) =>
                        true |=> 0 -> "other"

                    case HowlPreBattleAction(_) =>
                        first && enemies.num == 1 |=> -1000 -> "dont howl only foe"
                        true |=> -100 -> "dont generally"

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.monsters.any |=> 1000 -> "send cultist to be protected by monsters"
                        u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                        u.cultist && r.foes.none && r.ownGate |=> 300 -> "send cultist where no foes"
                        u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                        u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                        u.cultist && r.foes.goos.none && r.freeGate && have(Ithaqua) |=> 3500 -> "send cultist to semifree gate"
                        u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                        u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                        u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                        u.is(Wendigo) && r.allies(Ithaqua).any |=> 3000 -> "wendigo go rha"
                        u.is(Wendigo) && r.allies(Wendigo).num >= 4 |=> 2400 -> "groupw"
                        u.is(Wendigo) && r.allies(Wendigo).num == 3 |=> 2300 -> "groupw"
                        u.is(Wendigo) && r.allies(Wendigo).num == 2 |=> 2200 -> "groupw"
                        u.is(Wendigo) && r.allies(Wendigo).num == 1 |=> 2100 -> "groupw"

                        u.goo && r.allies.num >= 9 |=> 3100 -> "regrouphug"
                        u.goo && r.allies.num == 8 |=> 3000 -> "regrouphug"
                        u.goo && r.allies.num == 7 |=> 2900 -> "regrouphug"
                        u.goo && r.allies.num == 6 |=> 2800 -> "regrouphug"
                        u.goo && r.allies.num == 5 |=> 2700 -> "regrouphug"
                        u.goo && r.allies.num == 4 |=> 2600 -> "regrouphug"
                        u.goo && r.allies.num == 3 |=> 2500 -> "regrouphug"
                        u.goo && r.allies.num == 2 |=> 2400 -> "regrouphug"
                        u.goo && r.allies.num == 1 |=> 2300 -> "regrouphug"

                        u.monster && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                        u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                        u.monster && r.foes.%(_.vulnerableM).any && !r.foes.goos.any && r.allies.monsters.none |=> 1000 -> "send monster to capture"
                        u.goo && r.foes.%(_.vulnerableG).any |=> 1000 -> "send goo to capture"

                        u.monster && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                        u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                        u.monster && r.ownGate |=> 400 -> "send monster to own gate"
                        u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                        u.monster && r.freeGate |=> 300 -> "send monster to free gate"
                        u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                        u.monster && r.enemyGate |=> 300 -> "send monster to enemy gate"
                        u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                        u.monster && r.foes(Tsathoggua).any |=> -450 -> "dont send monster to tsa"

                        if (u.goo)
                            result ++= eval(MoveAction(u.faction, u.uclass, u.region, r))

                        true |=> (game.board.connected(r) ++ game.board.connected(r).flatMap(game.board.connected)).distinct.num -> "reachable regions"

                    case _ =>
                        true |=> 1000 -> "todo"
                }
            }
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -abs(v.weight))
    }

}
