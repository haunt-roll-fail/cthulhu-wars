package cws

import hrf.colmat._

object BotAN extends BotX(g => new GameEvaluationAN(g))

class GameEvaluationAN(game : Game) extends GameEvaluation(game, AN) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        def checkAttack(r : Region, f : Faction, allies : List[UnitFigure], foes : List[UnitFigure], d : Int) {
            val igh = others.%(_.has(Necrophagy))./(_.all(Ghoul).diff(foes).num).sum
            r.ownGate && allies.num < 2 + igh |=> -1000 -> "ghouls will knock off the gate"

            var ac = allies(Acolyte).num
            var um = allies(UnMan).num
            var ra = allies(Reanimated).num
            var yo = allies(Yothan).num

            //var eght = foes(Ghast).num
            var egug = foes(Gug).num
            var esht = foes(Shantak).num
            var esv = foes(StarVampire).num

            f match {
                case GC =>
                    var ec = foes(Acolyte).num
                    var dp = foes(DeepOne).num
                    var sh = foes(Shoggoth).num
                    var ss = foes(Starspawn).num
                    var cth = foes.has(Cthulhu)

                    var enemyStr = (f.has(Absorb) && sh > 0).?(ec * 3 + dp * 3).|(dp) + sh * 2 + ss * 3 + cth.??(6) + egug * 3 + esht * 2 + esv
                    var shield = ac + um + ra + yo - 1

                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                    cth && f.power < 4 && !f.needs(KillDevour1) && !f.needs(KillDevour2) |=> 11000/d -> "attack cth no respawn"
                    cth && f.power == 0 && f.needs(KillDevour1) && f.needs(KillDevour2) |=> 11000/d -> "attack cth no power"
                    cth && f.power == 0 && f.needs(OceanGates) |=> 11000/d -> "attack cth no gates"

                case BG =>
                    0 -> "todo"

                case CC =>
                    var ec = foes(Acolyte).num
                    var ng = foes(Nightgaunt).num
                    var fp = foes(FlyingPolyp).num
                    var hh = foes(HuntingHorror).num
                    var nya = foes.has(Nyarlathotep)

                    var abd = f.has(Abduct).??(ng)

                    // Note that Yothans are immune to abduct.
                    while (abd > 0) {
                        abd -= 1
                        if (ac > 0)
                            ac -= 1
                        else
                        if (um > 0)
                            um -= 1
                        else
                        if (ra > 0)
                            ra -= 1
                    }

                    var inv = f.has(Invisibility).??(fp)

                    // Note that Yothans are immune to invisibility (as in CC can't make them not participate)
                    while (inv > 0) {
                        inv -= 1
                        if (ra > 0)
                            ra -= 1
                        else
                        if (um > 0)
                            um -= 1
                        else
                        if (ac > 0)
                            ac -= 1
                    }

                    val shield = ac + um + ra + yo

                    val ownStr = ra * 2 + yo * 7

                    var ihh = f.has(SeekAndDestroy).??(f.all(HuntingHorror).diff(foes).num)

                    var enemyStr = fp + (hh + ihh) * 2 + nya.??(f.numSB + self.numSB) + egug * 3 + esht * 2 + esv

                    val enough = shield * 5 > enemyStr * 4

                    f.has(Invisibility) && fp == foes.num |=> -1000000 -> "invis"
                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                    nya && f.has(Emissary) && yo > 0 && shield < 3 |=> -500000 -> "dont risk yothans vs unkillable nya"

                    hh == foes.num && enough && ownStr > 3 |=> 11000/d -> "attack hh"
                    hh + fp == foes.num && enough && ownStr > 4 |=> 10000/d -> "attack hhfp"

                case SL =>
                    var ec = foes(Acolyte).num
                    var wz = foes(Wizard).num
                    var sm = foes(SerpentMan).num
                    var fs = foes(FormlessSpawn).num
                    var tsa = foes.has(Tsathoggua)

                    val shield = ac + um + ra + yo

                    val ownStr = ra * 2 + yo * 7

                    val enemyStr = wz + sm + fs * (f.all(FormlessSpawn).num + f.all(Tsathoggua).num) + tsa.??(max(2, power - 1)) + egug * 3 + esht * 2 + esv

                    val enough1 = shield * 5 > enemyStr * 4 && ownStr >= foes.num * 3
                    val enough2 = shield * 5 > enemyStr * 3 && ownStr >= 12

                    enough1 && (tsa || fs > 0 || foes.num > 1) |=> 11000 -> "fight sl 1"
                    enough2 && (tsa || fs > 0 || foes.num > 2) |=> 10000 -> "fight sl 2"

                case YS =>
                    0 -> "todo"

                case WW =>
                    0 -> "todo"

                case OW =>
                    val ec = foes(Acolyte).num
                    val mu = foes(Mutant).num
                    val ab = foes(Abomination).num
                    val sp = foes(SpawnOW).num
                    val yog = foes.has(YogSothoth)

                    val ownStr = ra * 2 + yo * 7

                    val enemyStr = mu + ab * 2 + sp * 3 + game.factions.but(f).map(_.goos.num).sum * 2 + egug * 3 + esht * 2 + esv

                    var shield = ac + um + ra + yo

                    enemyStr > shield * 5 |=> -500000 -> "not enough shield"

                    self.gates.contains(r) && ab + sp > 0 && ownStr >= enemyStr/2 |=> 11000 -> "protect gate from beyond one"

            }
        }

        def isIsolatedBrainless(p: Player, u: UnitFigure): Boolean = {
            if (p.has(Brainless)) {
                if (u.uclass != Reanimated) return false
                val r = u.region
                val monsters = p.at(r, Monster).but(u)
                !(p.at(r, Cultist).any || p.at(r, GOO).any || p.at(r, Terror).any || monsters.exists(_.uclass != Reanimated))
            }
            else {
                false
            }
        }

        def getCathedralCost(r : Region) : Int = {
            var adjacentCathedral = false
            game.board.connected(r).foreach { cr =>
                if (game.cathedrals.contains(cr)) {
                    adjacentCathedral = true
                }
            }

            if (adjacentCathedral) { 3 } else { 1 }
        }

        a match {
            case StartingRegionAction(_, r) =>
                val isWW = r.glyph == GlyphWW
                val isOO = r.glyph == GlyphOO
                val isAA = r.glyph == GlyphAA
                val isNonGlyph = (r.glyph != GlyphWW && r.glyph != GlyphOO && r.glyph != GlyphAA)

                val nearWW = r.near.%(_.glyph == GlyphWW).any
                val nearOO = r.near.%(_.glyph == GlyphOO).any
                val nearAA = r.near.%(_.glyph == GlyphAA).any
                val nearNonGlyph = r.near.exists(nr => nr.glyph != GlyphWW && nr.glyph != GlyphOO && nr.glyph != GlyphAA)

                val allGlyphsClose = (isNonGlyph && nearWW && nearOO && nearAA) || (isWW && nearOO && nearAA && nearNonGlyph) ||
                                     (isOO && nearWW && nearAA && nearNonGlyph) || (isAA && nearWW && nearOO && nearNonGlyph)

                allGlyphsClose |=> 1100 -> "all glyphs close"

                val mostGlyphsClose = !allGlyphsClose &&
                                      ((isWW && ((nearNonGlyph && nearOO) || (nearNonGlyph && nearAA) || (nearOO && nearAA))) ||
                                       (isOO && ((nearWW && nearNonGlyph) || (nearWW && nearAA) || (nearNonGlyph && nearAA))) ||
                                       (isAA && ((nearWW && nearOO) || (nearWW && nearNonGlyph) || (nearOO && nearNonGlyph))) ||
                                       (isNonGlyph && ((nearWW && nearOO) || (nearWW && nearAA) || (nearOO && nearAA))))

                mostGlyphsClose |=> 1000 -> "most glyphs close"

                r.near.%(_.foes.any).num > 1 |=> -1000 -> "near multiple enemies"

                r.near.%(_.of(YS).any).any |=> -10000 -> "ys non-empty near"

            case FirstPlayerAction(_, f) =>
                f == self && game.board.regions.%(_.allies.goos.any).%(_.foes.goos.any).any |=> 100 -> "play first goos together"
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

                (game.factions.indexOf(f) - game.factions.indexOf(self)).abs == 2 |=> 10 -> "stall opposite"
                f == CC && !CC.allSB |=> 1 -> "cc first"
                CC.allSB |=> 1000 -> "first, cc allsb"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            case SpellbookAction(self, sb, _) => sb match {
                case Festival =>
                    true |=> 700 -> "cheap defense"
                case Brainless =>
                    true |=> 600 -> "defense and combat power"
                case WorshipServices =>
                    true |=> 200 -> "more power"
                    game.cathedrals.%(_.enemyGate).any |=> 600 -> "cathedral at enemy gate"
                case Extinction =>
                    true |=> 500 -> "protection"
                    others.exists(_.goos.any) |=> 900 -> "enemy has goos"
                    self.pool(Yothan).num == 0 |=> -1000 -> "all yothans out"
                    self.pool(Yothan).num == 1 |=> -400 -> "two yothans out"
                    self.pool(Yothan).num == 2 |=> -200 -> "one yothan out"
                case UnholyGround =>
                    true |=> 400 -> "protect against goos"
                    others.exists(_.goos.any) |=> 800 -> "enemy has goos"
                    self.count(Yothan) == 1 |=> -100 -> "one yothan"
                    self.count(Yothan) == 2 |=> -200 -> "two yothans"
                    self.count(Yothan) == 3 |=> -400 -> "three yothans"
                    game.cathedrals.%(_.ownGate).num > 1 |=> 1000 -> "cathedrals at own gates"
                case Consecration =>
                    true |=> 200 -> "for rituals"
                    game.cathedrals.num == 4 |=> 400 -> "all cathedrals"
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
                !AN.has(Consecration) || game.cathedrals.num == 0 |=> -600 -> "no cathedrals or no consecration"
                AN.has(Consecration) && game.cathedrals.num == 4 |=> 400 -> "consecration and all cathedrals"

                true |=> -250 -> "don't ritual unless have reasons"

            case LoyaltyCardAction(_, _, _) =>
                true |=> -10000 -> "don't obtain loyalty cards (for now)"

            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"
                active.none |=> 500000 -> "move done"

            case MoveAction(_, Reanimated, o, d) =>
                val u = self.at(o, Reanimated).%(!_.has(Moved)).head

                o.ownGate && o.allies.goos.none && o.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(o).goos.none).%(_.at(o).monsters.any).any |=> -2000 -> "dont abandon gate"
                d.ownGate && d.capturers.any && d.capturers.%(_.power > 0).any && d.capturers.%(_.power > 0).%(_.at(d).goos.any).none |=> 1100 -> "protect gate"

                o.ownGate && u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.num == 1 |=> -600 -> "dont leave lone cultist on gate"
                u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.any |=> -20 -> "dont leave cultists unprotected"

                o.foes.goos.any |=> -500 -> "stay with enemy goos"

                // Don't go for capture with reanimated.

                d.ownGate && canSummon(Reanimated) && !game.hasMoved(self) |=> -1200 -> "why move if can summon"

                d.ownGate && d.allies.monsters.none |=> 50 -> "move to undefended own gate"
                d.enemyGate && d.allies.any |=> 14 -> "join allies at enemy gate"
                o.enemyGate |=> -20 -> "stay with enemy gate"

                active.none && power > 1 && d.allies.goos.any && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) > d.allies(Acolyte).num + d.allies(UnMan).num |=> 1100000 -> "shield from kiy"

                d.foes.goos.any && d.allies.any && game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) == 0 |=> 450 -> "threaten goo with unholy ground"

                d.allies.none || (d.allies.cultists.none && (d.allies.monsters.forall(_.uclass == Reanimated))) |=> -100000 -> "dont move to isolation"

                true |=> -10 -> "stay"

            case MoveAction(_, UnMan, o, d) =>
                val u = self.at(o, UnMan).%(!_.has(Moved)).head

                o.ownGate && o.allies.goos.none && o.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(o).goos.none).%(_.at(o).monsters.any).any |=> -2000 -> "dont abandon gate"
                d.ownGate && d.capturers.any && d.capturers.%(_.power > 0).any && d.capturers.%(_.power > 0).%(_.at(d).goos.any).none |=> 1100 -> "protect gate"

                o.ownGate && u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.num == 1 |=> -600 -> "dont leave lone cultist on gate"
                u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.any |=> -20 -> "dont leave cultists unprotected"

                o.foes.goos.any |=> -500 -> "stay with enemy goos"

                val canCaptureYS = self.all.cultists./(_.region).%(_.capturers.contains(YS)).none
                val canCaptureWW = !WW.has(Ithaqua)
                d.allies.none && d.foes.cultists.%(_.vulnerableM).map(_.faction).%(f => !f.active && (f != YS || canCaptureYS) && (f != WW || canCaptureWW)).any |=> 250 -> "go for capture"

                d.ownGate && canSummon(UnMan) && !game.hasMoved(self) |=> -1200 -> "why move if can summon"

                d.ownGate && d.allies.monsters.none |=> 50 -> "move to undefended own gate"
                d.enemyGate && d.allies.any |=> 13 -> "join allies at enemy gate"
                d.enemyGate |=> 11 -> "move to enemy gate"
                SL.power > 0 && d.foes(Tsathoggua).any |=> -20 -> "not to tsa"
                o.enemyGate |=> -20 -> "stay with enemy gate"

                active.none && power > 1 && d.allies.goos.any && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) > d.allies(Acolyte).num + d.allies(UnMan).num |=> 1100000 -> "shield from kiy"

                val destIsWW = d.glyph == GlyphWW
                val destIsOO = d.glyph == GlyphOO
                val destIsAA = d.glyph == GlyphAA
                val destIsNonGlyph = (d.glyph != GlyphWW && d.glyph != GlyphOO && d.glyph != GlyphAA)

                val destHasNeededGlyph = (destIsWW && need(CathedralWW)) || (destIsOO && need(CathedralOO)) ||
                                         (destIsAA && need(CathedralAA)) || (destIsNonGlyph && need(CathedralNG))

                o.allies.monsters.num > 1 && d.allies.cultists.any && !d.allies.monsters.any |=> 20 -> "protect cultist"
                d.allies.cultists.any && !d.allies.monsters.any |=> 15 -> "protect cultist"

                true |=> -10 -> "stay"

            case MoveAction(_, Yothan, o, d) =>
                val u = self.at(o, Yothan).%(!_.has(Moved)).head

                o.ownGate && o.allies.goos.none && o.allies.monsters.num == 1 && others.%(_.power > 0).%(_.at(o).goos.none).%(_.at(o).monsters.any).any |=> -2000 -> "dont abandon gate"
                d.ownGate && d.capturers.any && d.capturers.%(_.power > 0).any && d.capturers.%(_.power > 0).%(_.at(d).goos.any).none |=> 1100 -> "protect gate"

                o.ownGate && u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.num == 1 |=> -600 -> "dont leave lone cultist on gate"
                u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.any |=> -20 -> "dont leave cultists unprotected"

                o.foes.goos.any |=> -500 -> "stay with enemy goos"

                // Don't go for capture with yothans (which would leave them exposed).

                d.ownGate && canSummon(Yothan) && !game.hasMoved(self) |=> -1200 -> "why move if can summon"

                d.ownGate && d.allies.monsters.none |=> 50 -> "move to undefended own gate"
                d.enemyGate && d.allies.any |=> 15 -> "join allies at enemy gate"
                o.enemyGate |=> -20 -> "stay with enemy gate"

                active.none && power > 1 && d.allies.goos.any && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) > d.allies(Acolyte).num + d.allies(UnMan).num |=> 1100000 -> "shield from kiy"

                d.foes.goos.any && d.allies.any && game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) == 0 |=> 450 -> "threaten goo with unholy ground"

                o.allies.any && d.allies.none |=> -200 -> "dont expose yothan"
                o.allies.none && d.allies.any |=> 40 -> "protect yothan"

                true |=> -10 -> "stay"

            case MoveAction(_, Acolyte, o, d) =>
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
                // SL has these, but not CC. Should AN? Should CC?
                //o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d.near.all(_.of(YS).none) |=> 900 -> "crowded cultists 6 explore all no-ys around"
                //o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d == EarthMap4v35.Antarctica |=> 800 -> "crowded cultists 6 explore - antarctica"
                //o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d == EarthMap4v35.NorthAmerica |=> 750 -> "crowded cultists 6 explore - north america"
                //o.allies.cultists.num == 6 && !self.all.monsters.none && d.empty && d == EarthMap4v35.Arabia |=> 700 -> "crowded cultists 6 explore - arabia"

                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 400 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && (active.none || d.capturers.%(f => f.power > 0 || f.has(Passion)).none) |=> 300 -> "ic temporary free gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.ownGate |=> 60 -> "flee from capture to own gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.allies.monsters.any |=> 59 -> "flee from capture to monster"
                o.allies.cultists.num == 1 && o.capturers.%(_.power > 0).any && d.capturers.none && d.empty |=> 58 -> "flee from capture"

                // Not sure about this.
                //others.%(f => f.power > 0 || f.has(Passion)).%(f => o.of(f).goos.none && o.of(f).monsters.none).none |=> -300 -> "why move"
                //game.cathedrals.num < 4 && others.%(f => f.power > 0 || f.has(Passion)).%(f => o.of(f).goos.none && o.of(f).monsters.none).none |=> -300 -> "why move"

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

                o.allies.monsters.any && !d.allies.monsters.any |=> -40 -> "don't forego protection wo reason"

                val destIsWW = d.glyph == GlyphWW
                val destIsOO = d.glyph == GlyphOO
                val destIsAA = d.glyph == GlyphAA
                val destIsNonGlyph = (d.glyph != GlyphWW && d.glyph != GlyphOO && d.glyph != GlyphAA)

                val origIsWW = o.glyph == GlyphWW
                val origIsOO = o.glyph == GlyphOO
                val origIsAA = o.glyph == GlyphAA
                val origIsNonGlyph = (o.glyph != GlyphWW && o.glyph != GlyphOO && o.glyph != GlyphAA)

                val cultistGlyphs = self.all.cultists.map(_.region.glyph).toSet
                val cultistAtWW = cultistGlyphs.contains(GlyphWW)
                val cultistAtOO = cultistGlyphs.contains(GlyphOO)
                val cultistAtAA = cultistGlyphs.contains(GlyphAA)
                val cultistAtNonGlyph = cultistGlyphs.exists(g => g != GlyphWW && g != GlyphOO && g != GlyphAA)

                val origHasNeededGlyph = origIsWW && need(CathedralWW) || origIsOO && need(CathedralOO) ||
                                         origIsAA && need(CathedralAA) || origIsNonGlyph && need(CathedralNG)

                val destHasNeededGlyph = (destIsWW && need(CathedralWW) && !cultistAtWW) ||
                                         (destIsOO && need(CathedralOO) && !cultistAtOO) ||
                                         (destIsAA && need(CathedralAA) && !cultistAtAA) ||
                                         (destIsNonGlyph && need(CathedralNG) && !cultistAtNonGlyph)

                val destIsNearNeededGlyph = (need(CathedralWW) && d.near.%(_.glyph == GlyphWW).any && !cultistAtWW) ||
                                            (need(CathedralOO) && d.near.%(_.glyph == GlyphOO).any && !cultistAtOO) ||
                                            (need(CathedralAA) && d.near.%(_.glyph == GlyphAA).any && !cultistAtAA) ||
                                            (need(CathedralNG) && d.near.% { r => r.glyph != GlyphWW && r.glyph != GlyphOO && r.glyph != GlyphAA }.any && !cultistAtNonGlyph)

                var wouldLeaveNeededGlyph = o.allies.cultists.num == 1 && origHasNeededGlyph

                destHasNeededGlyph && !d.capturers.any |=> 210 -> "need glyph and no capturers"
                destHasNeededGlyph |=> 40 -> "need glyph"
                wouldLeaveNeededGlyph |=> -40 -> "would leave glyph"
                d.ocean && GC.needs(OceanGates) |=> -20 -> "avoid ocean gc"
                destIsNearNeededGlyph |=> 30 -> "glyph-adjacent"

                destHasNeededGlyph && !wouldLeaveNeededGlyph && d.enemyGate && !d.foes.goos.any && d.allies.monsters.any |=> 200 -> "needed glyph at enemy gate"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val enemyStr = f.strength(game, foes, self)
                val ownStr = self.strength(game, allies, f)

                checkAttack(r, f, allies, foes, 1)

                foes.goos.any && game.cathedrals.contains(r) && r.str(AN) > 0 && have(UnholyGround) |=> 100000 -> "unholy ground enemy goo"

                r.allies.num == 1 && r.allies(Yothan).any && enemyStr > 0 |=> -1000 -> "dont attack with lone yothan"

                foes.goos.num == 1 && foes.monsters.none && foes.cultists.none && r.allies(Yothan).any && (r.allies.%(_.uclass.utype == Monster).any || r.allies.cultists.any) |=> 60000 -> "prot yothan vs lone goo"
                foes.goos.num == 1 && (foes.monsters.num == 1 || foes.cultists.num == 1) && r.allies(Yothan).any && (r.allies.%(_.uclass.utype == Monster).any || r.allies.cultists.any) |=> 40000 -> "prot yothan vs goo with 1 unit"

                f.gates.contains(r) && (foes.monsters.any || foes.goos.any) && r.allies(Yothan).any && (r.allies.%(_.uclass.utype == Monster).any || r.allies.cultists.any) && ownStr >= enemyStr |=> 14000 -> "yothan w shield vs protected gate"

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
                true |=> 10 -> "base"

                active.none && self.gates.num < 4 && (self.allSB || self.doom + self.gates.num < 29) |=> (10 * 100000 / 9) -> "safe build gate"

                WW.exists && game.board.starting(WW).contains(r) |=> -10000000 -> "starting ww"
                GC.exists && game.board.starting(GC).contains(r) |=> -10000000 -> "starting gc"

                YS.has(Hastur) && YS.power > 1 |=> -1000 -> "hastur in play"
                YS.has(KingInYellow) && YS.power > 1 && game.board.connected(YS.player.goo(KingInYellow).region).contains(r) |=> -1000 -> "kiy is near"

                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 |=> -700 -> "cthulhu ygs dreams"

                power > 5 && r.near.%(n => active.%(_.at(n).any).any).none |=> -600 -> "building can wait"
                (power >= 3 + maxEnemyPower || self.gates.num <= 1) && r.capturers.none |=> 500 -> "building gates is good"

            case RecruitAction(_, Acolyte, r) =>
                active.none && r.freeGate |=> (3 * 100000 / 1) -> "safe recruit and get gate"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && r.allies.cultists.none |=> (1 * 100000 / 1) -> "safe recruit"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && r.allies.cultists.any |=> (8 * 100000 / 9) -> "safe recruit"
                active.none && r.ownGate && r.allies.cultists.num == 1 |=> (7 * 100000 / 8) -> "safe recruit"

                r.capturers.%(_.power > 0).any |=> -2000 -> "dont recruit to be captured"

                r.freeGate |=> 1700 -> "free gate"

                self.pool.cultists.num >= power |=> 300 -> "recover lost cultists"

                r.ownGate && others.all(_.power < power) |=> -250 -> "dont recruit if max power"
                r.ownGate && r.allies.goos.any |=> 200 -> "a cultist needs a big friend"
                
                r.allies.monsters.any |=> 50 -> "recruit to monsters"

            case SummonAction(_, UnMan, r) =>
                !have(Festival) |=> -10 -> "lacks fesival"

                !have(Festival) && r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                !have(Festival) && r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                !have(Festival) && (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 200 -> "protect gate"

                have(Festival) && r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1910 -> "prevent losing gate festival"
                have(Festival) && r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1810 -> "prevent capture festival"
                have(Festival) && (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 210 -> "protect gate festival"

                r.allies(UnMan).any |=> -100 -> "already have unman"

            case SummonAction(_, Reanimated, r) =>
                !have(Brainless) |=> -15 -> "lacks brainless"

                have(Brainless) && r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1910 -> "prevent losing gate brainless"
                have(Brainless) && r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1810 -> "prevent capture brainless"
                have(Brainless) && (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 210 -> "protect gate brainless"

                r.allies(Reanimated).any |=> -100 -> "already have reanimated"

                r.foes.goos.any && r.allies.any && game.cathedrals.contains(r) && AN.has(UnholyGround) && r.str(AN) == 0 |=> 100 -> "threaten goo with unholy ground"
                r.foes.goos.any && r.allies.any && game.cathedrals.contains(r) && AN.has(UnholyGround) && !AN.has(Extinction) && r.str(AN) == 0 |=> 200 -> "threaten goo with unholy ground no extinction"

            case SummonAction(_, Yothan, r) =>
                !have(Extinction) |=> -20 -> "lacks extinction"

                have(Extinction) && r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1910 -> "prevent losing gate extinction"
                have(Extinction) && r.capturers.%(_.power > 0).any && r.capturers.%(_.power > 0).%(_.at(r).goos.any).none |=> 1810 -> "prevent capture extinction"
                have(Extinction) && (others.%(_.power > 1).any || r.foes.monsters.active.any) && r.foes.goos.none && r.allies.monsters.none && r.allies.goos.none |=> 210 -> "protect gate extinction"

                have(Extinction) && r.allies.monsters.any |=> 100 -> "has shield"
                have(Extinction) && others.exists(_.goos.any) |=> 100 -> "enemy has goos"
                r.allies(Yothan).any |=> -100 -> "already have yothan"

            case BuildCathedralAction(_, r) =>
                val expensiveCathedral = getCathedralCost(r) == 3
                val nonGlyph = (r.glyph != GlyphWW && r.glyph != GlyphOO && r.glyph != GlyphAA)
                val needGlyph = (r.glyph == GlyphWW && need(CathedralWW)) || (r.glyph == GlyphOO && need(CathedralOO)) ||
                                (r.glyph == GlyphAA && need(CathedralAA)) || (nonGlyph && need(CathedralNG))

                r.glyph == GlyphWW && !need(CathedralWW) && (need(CathedralOO) || need(CathedralAA) || need(CathedralNG)) |=> -100000 -> "already have ww glyph"
                r.glyph == GlyphOO && !need(CathedralOO) && (need(CathedralWW) || need(CathedralAA) || need(CathedralNG)) |=> -100000 -> "already have oo glyph"
                r.glyph == GlyphAA && !need(CathedralAA) && (need(CathedralWW) || need(CathedralOO) || need(CathedralNG)) |=> -100000 -> "already have aa glyph"
                nonGlyph && !need(CathedralNG) && (need(CathedralWW) || need(CathedralOO) || need(CathedralAA))           |=> -100000 -> "already have non glyph"
                !need(CathedralWW) && !need(CathedralOO) && !need(CathedralAA) && !need(CathedralNG) |=> 400 -> "have all cathedral spellbooks"

                needGlyph && !expensiveCathedral |=> 480 -> "cheap new glyph"
                needGlyph && expensiveCathedral |=> 90 -> "expensive new glyph"

                game.starting(self) == r && game.cathedrals.num == 0 && numSB == 0 |=> -480 -> "not start region first"

                game.starting(self) != r && game.cathedrals.num == 0 && needGlyph |=> 510 -> "first cathedral before new gate"

                // Need to be more than 500, to prioritize over building a general gate somewhere.
                needGlyph && r.enemyGate |=> 1000 -> "new glyph at enemy gate"
                have(WorshipServices) && needGlyph && r.enemyGate |=> 1100 -> "new glyph at enemy gate worship services"

            case GiveWorstMonsterMainAction(_) =>
                true |=> -10 -> "base"

                others.forall(_.power <= 2) |=> 210 -> "others low power"
                game.cathedrals.num > 0 |=> 200 -> "1+ cathedrals"

            case GiveBestMonsterMainAction(_) =>
                true |=> -20 -> "base"
                numSB == 5 |=> 20 -> "last spellbook"

            case FestivalUnManSummonAction(_, f) =>
                true |=> 100 -> "base"

                f.doom == others.minBy(_.doom).doom |=> 200 -> "lowest doom"
                f.power == 0 |=> 100 -> "zero power"
                f.has(Tsathoggua) |=> -300 -> "dont aid sleeper"

                f.power > 0 && game.board.regions.%(r => r.allies.cultists.any && r.capturers.contains(f)).any |=> -1000 -> "dont give power to capture"

            case DematerializationDoomAction(_) =>
                true |=> 100000 -> "just do"

            case DematerializationFromRegionAction(_, o) =>
                true |=> -10 -> "base"

                // Flags for origin region (o).
                val origIsWW = o.glyph == GlyphWW
                val origIsOO = o.glyph == GlyphOO
                val origIsAA = o.glyph == GlyphAA
                val origIsNonGlyph = (o.glyph != GlyphWW && o.glyph != GlyphOO && o.glyph != GlyphAA)
                val origIsOcean = o.glyph == Ocean

                val cultistGlyphs = self.all.cultists.map(_.region.glyph).toSet
                val cultistAtWW = cultistGlyphs.contains(GlyphWW)
                val cultistAtOO = cultistGlyphs.contains(GlyphOO)
                val cultistAtAA = cultistGlyphs.contains(GlyphAA)
                val cultistAtNonGlyph = cultistGlyphs.exists(g => g != GlyphWW && g != GlyphOO && g != GlyphAA)

                val origHasNeededGlyph = origIsWW && need(CathedralWW) || origIsOO && need(CathedralOO) ||
                                         origIsAA && need(CathedralAA) || origIsNonGlyph && need(CathedralNG)

                var wouldLeaveNeededGlyph = o.allies.cultists.num == 1 && origHasNeededGlyph

                val ownGateInOrig = o.ownGate || o.freeGate
                val noOwnGateInOrig = !o.ownGate && !o.freeGate
                val cultistsInOrig = o.allies.cultists.num > 0
                val unemployedCultistsInOrig = !((o.ownGate || o.freeGate) && o.allies.cultists.num == 1)
                val availableCultistInOrig = cultistsInOrig && unemployedCultistsInOrig
                val yothansInOrig = o.allies(Yothan).any
                val excessYothansInOrig = o.allies(Yothan).num > 1
                val monstersInOrig = o.allies.%(_.uclass.utype == Monster).any
                val excessMonstersInOrig = o.allies.%(_.uclass.utype == Monster).num > 1
                val availableMonsterInOrig = (o.allies.cultists.num == 0 && noOwnGateInOrig && monstersInOrig) ||
                                             (o.allies.cultists.num > 0 && noOwnGateInOrig && excessMonstersInOrig) ||
                                             (ownGateInOrig && excessMonstersInOrig)
                val onlyReanimatedMonstersInOrig = o.allies(Reanimated).any && o.allies.monsters.forall(_.uclass == Reanimated)
                val availableYothanInOrig =  (o.allies.cultists.num == 0 && noOwnGateInOrig && yothansInOrig) ||
                                             (o.allies.cultists.num > 0 && noOwnGateInOrig && excessYothansInOrig) ||
                                             (ownGateInOrig && excessYothansInOrig)
                val brainlessIsolatedReanimatedInOrig = o.allies.monsters.exists(u => isIsolatedBrainless(game.of(self), u))

                // Flags for potential destination regions.
                var needCultistOnlyForGlyph = false
                var needCultistWithEscortForGlyph = false
                var needCultistOnlyForGate = false
                var needCultistWithEscortForGate = false
                var needYothanBackupToRaidGate = false
                var needMonsterBackupToRaidGate = false
                var needNonReanimatedBackupToRaidGate = false
                var needYothanToRaidGate = false
                var needMonsterToRaidGate = false

                game.board.regions.foreach { r =>
                    val destIsWW = r.glyph == GlyphWW
                    val destIsOO = r.glyph == GlyphOO
                    val destIsAA = r.glyph == GlyphAA
                    val destIsNonGlyph = (r.glyph != GlyphWW && r.glyph != GlyphOO && r.glyph != GlyphAA)
                    val destIsOcean = r.glyph == Ocean

                    val destHasNeededGlyph = (destIsWW && need(CathedralWW) && !cultistAtWW) ||
                                             (destIsOO && need(CathedralOO) && !cultistAtOO) ||
                                             (destIsAA && need(CathedralAA) && !cultistAtAA) ||
                                             (destIsNonGlyph && need(CathedralNG) && !cultistAtNonGlyph)

                    val destHasOwnCultist = r.allies.cultists.any
                    val destHasOwnMonsterOrTerror = r.allies.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).any
                    val destHasOwnMonster = r.allies.%(u => u.uclass.utype == Monster).any
                    val destHasYothans = r.allies.%(u => u.uclass == Yothan).any
                    val destHasEnemyMonster = r.foes.%(_.uclass.utype == Monster).any
                    val destHasEnemyGOO = r.foes.%(_.uclass.utype == GOO).any
                    val destHasEnemyMonsterOrGOO = destHasEnemyMonster || destHasEnemyGOO
                    val destCouldGiveGCOceanGate = r.ocean && GC.needs(OceanGates)
                    val destHasLoneEnemyGoo = others.exists { f =>
                        val ref = r.of(f)
                        ref.goos.num == 1 && ref.monsters.none && ref.cultists.none
                    }
                    val destHasEnemyGooWithLittleShield = others.exists { f =>
                        val ref = r.of(f)
                        ref.goos.num == 1 && (ref.monsters.num == 1 || ref.cultists.num == 1)
                    }
                    val destHasEnemyGoosWithMuchShield = others.exists { f =>
                        val ref = r.of(f)
                        ref.goos.any && (ref.monsters.num + ref.cultists.num > 2)
                    }

                    if ((destHasNeededGlyph && destHasOwnMonsterOrTerror && !destHasOwnCultist && !destHasEnemyMonster && !destHasEnemyGOO && !destCouldGiveGCOceanGate) ||
                        (destHasNeededGlyph && destHasOwnMonsterOrTerror && !destHasOwnCultist && destHasEnemyMonster && !destHasEnemyGOO && !destCouldGiveGCOceanGate)) {
                        needCultistOnlyForGlyph = true
                    }

                    if ((destHasNeededGlyph && !destHasOwnMonsterOrTerror && !destHasOwnCultist && !destHasEnemyMonster && !destHasEnemyGOO && !destCouldGiveGCOceanGate) ||
                        (destHasNeededGlyph && !destHasOwnMonsterOrTerror && !destHasOwnCultist && destHasEnemyMonster && !destHasEnemyGOO && !destCouldGiveGCOceanGate)) {
                        needCultistWithEscortForGlyph = true
                    }

                    if (r.freeGate && destHasOwnMonsterOrTerror && !destHasEnemyMonster && !destHasEnemyGOO && !destHasOwnCultist) {
                        needCultistOnlyForGate = true
                    }

                    if (r.freeGate && !destHasOwnMonsterOrTerror && destHasEnemyMonster && !destHasEnemyGOO && !destHasOwnCultist) {
                        needCultistWithEscortForGate = true
                    }

                    if (r.enemyGate && !destHasYothans && destHasOwnMonster && !destHasEnemyGoosWithMuchShield) {
                        needYothanBackupToRaidGate = true
                    }

                    if (r.enemyGate && destHasYothans && !destHasOwnMonster && !destHasEnemyGoosWithMuchShield) {
                        needMonsterBackupToRaidGate = true
                    }

                    if (r.enemyGate && !destHasOwnMonsterOrTerror && !destHasEnemyGoosWithMuchShield) {
                        needYothanToRaidGate = true
                    }

                    if (r.enemyGate && !destHasEnemyMonster && !destHasEnemyGOO && !destHasOwnMonsterOrTerror) {
                        needMonsterToRaidGate = true
                    }
                }

                var demCase = 0

                // Send acolyte to needed glyph.
                if ((availableCultistInOrig && needCultistOnlyForGlyph && !wouldLeaveNeededGlyph) ||
                    (availableCultistInOrig && needCultistWithEscortForGlyph && availableMonsterInOrig && !wouldLeaveNeededGlyph)) {
                    demCase = 1
                    game.demCaseMap = game.demCaseMap + (o -> demCase)
                    true |=> 15000 -> "go get glyph"
                }

                // Send acolyte to abandoned gate.
                // Should only do this if expected to be first player.
                // else if (demCase != 1 &&
                //         (availableCultistInOrig && needCultistOnlyForGate && !wouldLeaveNeededGlyph) ||
                //         (availableCultistInOrig && needCultistWithEscortForGate && availableMonsterInOrig && !wouldLeaveNeededGlyph)) {
                //     demCase = 2
                //     game.demCaseMap = game.demCaseMap + (o -> demCase)
                //     true |=> 14000 -> "go get free gate"
                // }

                // Reinforce with yothan to raid enemy gate.
                else if (!(1 to 2 contains demCase) &&
                          (needYothanBackupToRaidGate && !o.enemyGate && availableYothanInOrig && !wouldLeaveNeededGlyph)) {
                    demCase = 3
                    game.demCaseMap = game.demCaseMap + (o -> demCase)
                    true |=> 12000 -> "reinforce with yothan to raid gate"
                }

                // Reinforce with monster to raid enemy gate.
                else if (!(1 to 3 contains demCase) &&
                          (needMonsterBackupToRaidGate && !o.enemyGate && availableMonsterInOrig && !wouldLeaveNeededGlyph)) {
                    demCase = 4
                    game.demCaseMap = game.demCaseMap + (o -> demCase)
                    true |=> 12000 -> "reinforce with monster to raid gate"
                }

                // Raid enemy gate.
                else if (!(1 to 4 contains demCase) &&
                          (needYothanToRaidGate && !o.enemyGate && availableYothanInOrig && availableMonsterInOrig && !wouldLeaveNeededGlyph) ||
                          (needMonsterToRaidGate && !o.enemyGate && availableMonsterInOrig && !onlyReanimatedMonstersInOrig && !wouldLeaveNeededGlyph)) {
                    demCase = 5
                    game.demCaseMap = game.demCaseMap + (o -> demCase)
                    true |=> 12000 -> "go raid gate"
                }

                // Retrieve isolated brainless reanimated.
                else if (!(1 to 5 contains demCase) &&
                          brainlessIsolatedReanimatedInOrig && self.gates.num > 0) {
                    demCase = 6
                    game.demCaseMap = game.demCaseMap + (o -> demCase)
                    true |=> 11000 -> "retrieve brainless"
                }

                // Retrieve wayward yothans.
                else if (!(1 to 6 contains demCase) &&
                          yothansInOrig && o.noGate && self.gates.num > 0) {
                    demCase = 7
                    game.demCaseMap = game.demCaseMap + (o -> demCase)
                    true |=> 10000 -> "retrieve yothans"
                }

                // Save unprotected cultists from enemy goo?

                // Send monster to capture?

            case DematerializationToRegionAction(_, o, d) =>
                true |=> -10 -> "base"

                val demCase = game.demCaseMap.getOrElse(o, 0)

                // Flags for origin region (o).
                val ownGateInOrig = o.ownGate || o.freeGate
                val noOwnGateInOrig = !o.ownGate && !o.freeGate
                val cultistsInOrig = o.allies.cultists.num > 0
                val unemployedCultistsInOrig = !((o.ownGate || o.freeGate) && o.allies.cultists.num == 1)
                val availableCultistInOrig = cultistsInOrig && unemployedCultistsInOrig
                val yothansInOrig = o.allies(Yothan).any
                val excessYothansInOrig = o.allies(Yothan).num > 1
                val monstersInOrig = o.allies.%(_.uclass.utype == Monster).any
                val excessMonstersInOrig = o.allies.%(_.uclass.utype == Monster).num > 1
                val availableMonsterInOrig = (o.allies.cultists.num == 0 && noOwnGateInOrig && monstersInOrig) ||
                                             (o.allies.cultists.num > 0 && noOwnGateInOrig && excessMonstersInOrig) ||
                                             (ownGateInOrig && excessMonstersInOrig)
                val onlyReanimatedMonstersInOrig = o.allies(Reanimated).any && o.allies.monsters.forall(_.uclass == Reanimated)
                val availableYothanInOrig =  (o.allies.cultists.num == 0 && noOwnGateInOrig && yothansInOrig) ||
                                             (o.allies.cultists.num > 0 && noOwnGateInOrig && excessYothansInOrig) ||
                                             (ownGateInOrig && excessYothansInOrig)
                val brainlessIsolatedReanimatedInOrig = o.allies.monsters.exists(u => isIsolatedBrainless(game.of(self), u))

                // Flags for destination region (d).
                val destIsWW = d.glyph == GlyphWW
                val destIsOO = d.glyph == GlyphOO
                val destIsAA = d.glyph == GlyphAA
                val destIsNonGlyph = (d.glyph != GlyphWW && d.glyph != GlyphOO && d.glyph != GlyphAA)

                val cultistGlyphs = self.all.cultists.map(_.region.glyph).toSet
                val cultistAtWW = cultistGlyphs.contains(GlyphWW)
                val cultistAtOO = cultistGlyphs.contains(GlyphOO)
                val cultistAtAA = cultistGlyphs.contains(GlyphAA)
                val cultistAtNonGlyph = cultistGlyphs.exists(g => g != GlyphWW && g != GlyphOO && g != GlyphAA)

                val destHasNeededGlyph = (destIsWW && need(CathedralWW) && !cultistAtWW) ||
                                            (destIsOO && need(CathedralOO) && !cultistAtOO) ||
                                            (destIsAA && need(CathedralAA) && !cultistAtAA) ||
                                            (destIsNonGlyph && need(CathedralNG) && !cultistAtNonGlyph)

                val destHasOwnCultist = d.allies.cultists.any
                val destHasOwnMonsterOrTerror = d.allies.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).any
                val destHasOwnMonster = d.allies.%(u => u.uclass.utype == Monster).any
                val destHasYothans = d.allies.%(u => u.uclass == Yothan).any
                val destHasEnemyMonster = d.foes.%(_.uclass.utype == Monster).any
                val destHasEnemyGOO = d.foes.%(_.uclass.utype == GOO).any
                val destHasEnemyMonsterOrGOO = destHasEnemyMonster || destHasEnemyGOO
                val destCouldGiveGCOceanGate = d.ocean && GC.needs(OceanGates)
                val destHasLoneEnemyGoo = others.exists { f =>
                    val ref = d.of(f)
                    ref.goos.num == 1 && ref.monsters.none && ref.cultists.none
                }
                val destHasEnemyGooWithLittleShield = others.exists { f =>
                    val ref = d.of(f)
                    ref.goos.num == 1 && (ref.monsters.num == 1 || ref.cultists.num == 1)
                }
                val destHasEnemyGoosWithMuchShield = others.exists { f =>
                    val ref = d.of(f)
                    ref.goos.any && (ref.monsters.num + ref.cultists.num > 2)
                }

                // Send acolyte to needed glyph.
                if (demCase == 1) {
                    destHasNeededGlyph |=> 200 -> "needed glyph"
                    destHasNeededGlyph && destHasOwnMonsterOrTerror |=> 250 -> "has own monster"
                    destHasNeededGlyph && d.freeGate |=> 300 -> "free gate"
                    destHasNeededGlyph && d.freeGate && !destHasEnemyMonsterOrGOO |=> 400 -> "free unguarded gate"
                    destHasEnemyMonster |=> -200 -> "has enemy monster"
                    destHasOwnCultist |=> -400 -> "already has own cultist"
                    destHasEnemyGOO |=> -10000 -> "has enemy goo"
                }

                // Send acolyte to abandoned gate.
                // if (demCase == 2) {
                //     d.freeGate |=> 200 -> "free gate"
                //     d.freeGate && destHasOwnMonsterOrTerror |=> 250 -> "has own monster"
                //     d.freeGate && !destHasEnemyMonsterOrGOO |=> 400 -> "free unguarded gate"
                //     destHasEnemyMonster |=> -100 -> "has enemy monster"
                //     destHasOwnCultist |=> -400 -> "already has own cultist"
                //     destHasEnemyGOO |=> -10000 -> "has enemy goo"
                // }

                // Reinforce with yothan to raid enemy gate.
                if (demCase == 3) {
                    d.enemyGate && destHasOwnMonster && !destHasYothans |=> 200 -> "enemy gate"
                    d.enemyGate && destHasOwnMonster && !destHasYothans && !destHasEnemyMonsterOrGOO |=> 400 -> "unguarded enemy gate"
                    d.enemyGate && destHasOwnMonster && !destHasYothans && destHasOwnCultist |=> 300 -> "has own cultist"
                    d.enemyGate && destHasOwnMonster && !destHasYothans && destHasLoneEnemyGoo |=> 400 -> "lone enemy goo"
                }

                // Reinforce with monster to raid enemy gate.
                if (demCase == 4) {
                    d.enemyGate && !destHasOwnMonster && destHasYothans |=> 200 -> "enemy gate"
                    d.enemyGate && !destHasOwnMonster && destHasYothans && !destHasEnemyMonsterOrGOO |=> 400 -> "unguarded enemy gate"
                    d.enemyGate && !destHasOwnMonster && destHasYothans && destHasOwnCultist |=> 300 -> "has own cultist"
                    d.enemyGate && !destHasOwnMonster && destHasYothans && destHasLoneEnemyGoo |=> 400 -> "lone enemy goo"
                }

                // Raid enemy gate.
                if (demCase == 5) {
                    d.enemyGate |=> 200 -> "enemy gate"
                    d.enemyGate && !destHasEnemyMonsterOrGOO |=> 400 -> "unguarded enemy gate"
                    d.enemyGate && destHasOwnCultist |=> 300 -> "has own cultist"
                    d.enemyGate && destHasLoneEnemyGoo |=> 400 -> "lone enemy goo"
                }

                // Retrieve isolated brainless reanimated.
                if (demCase == 6) {
                    d.ownGate |=> 200 -> "own gate"
                    d.ownGate && !destHasOwnMonsterOrTerror |=> 400 -> "unprotected gate"
                    d.ownGate && destHasOwnMonsterOrTerror |=> -100 -> "protected gate"
                    d.ownGate && destHasEnemyMonsterOrGOO |=> 150 -> "enemies at gate"
                }

                // Retrieve wayward yothans.
                if (demCase == 7) {
                    d.ownGate |=> 200 -> "own gate"
                    d.ownGate && !destHasOwnMonsterOrTerror |=> 400 -> "unprotected gate"
                    d.ownGate && destHasOwnMonsterOrTerror |=> -100 -> "protected gate"
                    d.ownGate && destHasEnemyMonsterOrGOO |=> 150 -> "enemies at gate"
                }

            case DematerializationMoveUnitAction(_, o, d, uc) =>
                true |=> -10 -> "base"

                val demCase = game.demCaseMap.getOrElse(o, 0)

                // Flags for origin region (o).
                val ownGateInOrig = o.ownGate || o.freeGate
                val noOwnGateInOrig = !o.ownGate && !o.freeGate
                val cultistsInOrig = o.allies.cultists.num > 0
                val unemployedCultistsInOrig = !((o.ownGate || o.freeGate) && o.allies.cultists.num == 1)
                val availableCultistInOrig = cultistsInOrig && unemployedCultistsInOrig
                val yothansInOrig = o.allies(Yothan).any
                val excessYothansInOrig = o.allies(Yothan).num > 1
                val monstersInOrig = o.allies.%(_.uclass.utype == Monster).any
                val excessMonstersInOrig = o.allies.%(_.uclass.utype == Monster).num > 1
                val availableMonsterInOrig = (o.allies.cultists.num == 0 && noOwnGateInOrig && monstersInOrig) ||
                                             (o.allies.cultists.num > 0 && noOwnGateInOrig && excessMonstersInOrig) ||
                                             (ownGateInOrig && excessMonstersInOrig)
                val onlyReanimatedMonstersInOrig = o.allies(Reanimated).any && o.allies.monsters.forall(_.uclass == Reanimated)
                val availableYothanInOrig =  (o.allies.cultists.num == 0 && noOwnGateInOrig && yothansInOrig) ||
                                             (o.allies.cultists.num > 0 && noOwnGateInOrig && excessYothansInOrig) ||
                                             (ownGateInOrig && excessYothansInOrig)
                val brainlessIsolatedReanimatedInOrig = o.allies.monsters.exists(u => isIsolatedBrainless(game.of(self), u))

                // Flags for destination region (d).
                val destHasOwnCultist = d.allies.cultists.any
                val destHasOwnMonsterOrTerror = d.allies.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).any
                val destHasOwnMonster = d.allies.%(u => u.uclass.utype == Monster).any
                val destHasYothans = d.allies.%(u => u.uclass == Yothan).any
                val destHasEnemyMonster = d.foes.%(_.uclass.utype == Monster).any
                val destHasEnemyGOO = d.foes.%(_.uclass.utype == GOO).any
                val destHasEnemyMonsterOrGOO = destHasEnemyMonster || destHasEnemyGOO
                val destCouldGiveGCOceanGate = d.ocean && GC.needs(OceanGates)
                val destHasLoneEnemyGoo = others.exists { f =>
                    val ref = d.of(f)
                    ref.goos.num == 1 && ref.monsters.none && ref.cultists.none
                }
                val destHasEnemyGooWithLittleShield = others.exists { f =>
                    val ref = d.of(f)
                    ref.goos.num == 1 && (ref.monsters.num == 1 || ref.cultists.num == 1)
                }
                val destHasEnemyGoosWithMuchShield = others.exists { f =>
                    val ref = d.of(f)
                    ref.goos.any && (ref.monsters.num + ref.cultists.num > 2)
                }

                // Send acolyte to needed glyph.
                if (demCase == 1) {
                    !destHasOwnCultist && uc == Acolyte |=> 300 -> "send acolyte"
                    !destHasOwnMonsterOrTerror && destHasOwnCultist && uc.utype == Monster |=> 200 -> "send monster escort"
                }

                // Send acolyte to abandoned gate.
                // if (demCase == 2) {
                //     !destHasOwnCultist && uc == Acolyte |=> 300 -> "send acolyte"
                //     !destHasOwnMonsterOrTerror && destHasOwnCultist && uc.utype == Monster |=> 200 -> "send monster escort"
                // }

                // Reinforce with yothan to raid enemy gate.
                // Will currently send one yothan to reinforce monster(s). Plus an acolyte if possible.
                if (demCase == 3) {
                    !destHasYothans && availableYothanInOrig && uc == Yothan |=> 400 -> "send yothan to reinforce"
                    !destHasOwnCultist && availableCultistInOrig && !destHasEnemyGOO && uc == Acolyte |=> 200 -> "send acolyte"
                }

                // Reinforce with monster to raid enemy gate.
                // Will currently send all available monsters to reinforce yothan(s). Plus an acolyte if possible.
                if (demCase == 4) {
                    availableMonsterInOrig && uc.utype == Monster |=> 300 -> "send monster to reinforce"
                    !destHasOwnCultist && availableCultistInOrig && !destHasEnemyGOO && uc == Acolyte |=> 200 -> "send acolyte"
                }

                // Raid enemy gate.
                // Will currently send all available monsters and yothans. Plus an acolyte if possible.
                if (demCase == 5) {
                    availableMonsterInOrig && !destHasEnemyMonster && !destHasEnemyGOO && uc.utype == Monster && !(uc == Reanimated && !destHasOwnMonsterOrTerror) |=> 500 -> "send monster to unguarded gate"
                    availableYothanInOrig && (destHasEnemyMonster || destHasEnemyGOO) && uc == Yothan |=> 400 -> "send yothan to guarded gate"
                    availableMonsterInOrig && (destHasEnemyMonster || destHasEnemyGOO) && uc.utype == Monster && !(uc == Reanimated && !destHasOwnMonsterOrTerror) |=> 300 -> "send monster to guarded gate"
                    !destHasOwnCultist && availableCultistInOrig && !destHasEnemyGOO && uc == Acolyte |=> 200 -> "send acolyte"
                }

                // Retrieve isolated Brainless Reanimated.
                // Shouldn't be any other units here, so could just use true, I guess.
                if (demCase == 6) {
                    uc == Reanimated |=> 400 -> "send reanimated"
                }

                // Retrieve wayward Yothans.
                // Will currently send all the yothans. If there are any other units here, they will be sent too.
                if (demCase == 7) {
                    uc == Yothan |=> 400 -> "send yothan"
                    true |=> 200 -> "send other"
                }

            case DematerializationDoneAction(_) =>
                true |=> 0 -> "base"

            case AvatarReplacementAction(_, _, r, o, uc) =>
                val u = self.at(r, uc).head
                u.cultist && o.capturers.%(_.power > 0).none && o.freeGate |=> 300 -> "free gate"
                u.cultist && o.capturers.%(_.power > 0).any |=> -100 -> "don't send cultist to be captured"
                u.cultist && o.capturers.none |=> 150 -> "no capturers"
                u.cultist && o.capturers.any && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                u.monster && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monster && u.friends.cultists.num > 1 && u.friends.monsters.none && r.foes.monsters./(_.faction).%(_ != BG).%(_.power > 0).any |=> -200 -> "dont sent temp defender"
                u.monster && o.allies.cultists.any && o.allies.monsters.none |=> 50 -> "protect cultist"

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

            case DreadCurseAssignAction(_, _, _, _, _, _, s, uc) =>
                s == Kill && uc == Reanimated |=> 800 -> "kill reanimated"
                s == Kill && uc == UnMan |=> 600 -> "kill unman"
                s == Kill && uc == Acolyte |=> 400 -> "kill acolyte"
                s == Kill && uc == Yothan |=> -10000 -> "kill yothan"

                s == Pain && uc == Acolyte |=> 800 -> "pain acolyte"
                s == Pain && uc == UnMan |=> 400 -> "pain unman"
                s == Pain && uc == Reanimated |=> 200 -> "pain reanimated"
                s == Pain && uc == Yothan |=> -10000 -> "pain yothan"

            case MainDoneAction(_) =>
                (oncePerRound.contains(HWINTBN) || oncePerRound.contains(ScreamingDead)) && active.%(_.allSB).none |=> 5000 -> "double action done"
                true |=> 500 -> "main done"

            case _ if game.battle == null =>
                true |=> 1000 -> "todo"

            case _ =>
        }

        // BATTLE
        if (game.battle != null) {
            val battle = game.battle

            def elim(battle : Battle, u : UnitFigure) {
                u.is(Reanimated) |=> 800 -> "elim reanimated"
                u.is(UnMan) |=> 600 -> "elim unman"
                u.is(Acolyte) && u.faction == self |=> 400 -> "elim acolyte"
                u.is(Yothan) |=> -10000 -> "elim yothan"
            }

            def retreat(battle : Battle, u : UnitFigure) {
                u.gateKeeper && u.faction == self && battle.side(self).units./(battle.canAssignPains).sum > 2 |=> -1000 -> "retr gate keeper"
                u.is(Acolyte) && u.faction == self |=> 800 -> "retr acolyte"
                u.is(UnMan) |=> 400 -> "retr unman"
                u.is(Reanimated) |=> 200 -> "retr reanimated"
                u.is(Yothan) |=> -10000 -> "retr yothan"

                u.faction != self |=> u.uclass.cost * 100 -> "cost * 100"
                u.is(Acolyte) && u.faction != self |=> 50 -> "pain enemy acolyte"
            }

            if (game.battle.attacker != self && game.battle.defender != self) {
                a match {
                    case _ =>
                        true |=> 1000 -> "todo"
                }
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
                        enemies.goos.any && battle.strength(self) >= enemies.num * 6 |=> 2000 -> "chance to kill tsa"

                    case AssignKillAction(_, _, _, u) =>
                        elim(battle, u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(battle, u)

                    case EliminateNoWayAction(_, u) =>
                        elim(battle, u)

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.monsters.any && (r.allies.cultists.none && (r.allies.monsters.forall(_.uclass == Reanimated)))  |=> 1100 -> "send cultist to brainless reanimated"
                        u.cultist && r.allies.monsters.any |=> 1000 -> "send cultist to be protected by monsters"
                        u.cultist && r.allies.cultists.any |=> -3000 -> "dont group cultists"
                        u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                        u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to unprotected free gate"
                        u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                        u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                        u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                        u.is(UnMan) && r.allies(Yothan).any |=> 800 -> "unman go yothan"
                        u.is(UnMan) && (r.allies.cultists.none && (r.allies.monsters.forall(_.uclass == Reanimated))) |=> 350 -> "unman go brainless reanimated"

                        u.is(Yothan) && (r.allies.cultists.none && (r.allies.monsters.forall(_.uclass == Reanimated))) |=> 350 -> "yothan go brainless reanimated"

                        u.monster && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"

                        u.monster && r.foes.%(_.vulnerableM).any && !r.foes.goos.any && r.allies.monsters.none |=> 1000 -> "send monster to capture"

                        u.monster && r.ownGate |=> 400 -> "send monster to own gate"

                        u.monster && r.freeGate |=> 300 -> "send monster to free gate"

                        u.monster && r.enemyGate |=> 300 -> "send monster to enemy gate"

                        // Less worries with a terror, but not a good idea anyway.
                        u.monster && r.foes(Tsathoggua).any |=> -450 -> "dont send monster to tsa"

                        true |=> (game.board.connected(r) ++ game.board.connected(r).flatMap(game.board.connected)).distinct.num -> "reachable regions"

                    case UnholyGroundAction(_, f, r) =>
                        r.noGate |=> 1800 -> "no gate"
                        r.enemyGate |=> 1400 -> "enemy gate"
                        r.ownGate |=> 1200 -> "own gate"
                        r.foes.goos.any |=> -1800 -> "enemy goo" // should disregard if in battle area, and if there's only one goo there (that will be eliminated)

                    case UnholyGroundIgnoreAction(self) =>
                        true |=> -2000 -> "dont"

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
