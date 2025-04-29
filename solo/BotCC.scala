package cws

import hrf.colmat._

object BotCC extends BotX(g => new GameEvaluationCC(g))

class GameEvaluationCC(game : Game) extends GameEvaluation(game, CC) {
    def costA(g : Game, a : Action) : Int = a match {
        case MoveAction(self, _, _, r) => 1 + g.tax(r, self)
        case AttackAction(self, r, _) => 1 + g.tax(r, self)
        case CaptureAction(self, r, _) => 1 + g.tax(r, self)
        case BuildGateAction(self, r) => 3 + g.tax(r, self)
        case RecruitAction(self, uc, r) => self.recruitCost(g, uc, r) + g.tax(r, self)
        case SummonAction(self, uc, r) => self.summonCost(g, uc, r) + g.tax(r, self)
        case AwakenAction(self, uc, r, cost) => cost + g.tax(r, self)
        case Pay4PowerMainAction(_) => 4
        case Pay6PowerMainAction(_) => 6
        case Pay10PowerMainAction(_) => 10
        case _ => 0
    }

    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { Stats.triggerR(self, e._2, bool); if (bool) result +:= Evaluation(e._1, e._2) }
        }

        // Crawling Chaos
        val has1000f = can(ThousandForms) && have(Nyarlathotep)
        has1000f && a != ThousandFormsMainAction(self) && costA(game, a) > 0 && power - costA(game, a) < 1 |=> -4000 -> "dont spend last power if 1000F unused"
        has1000f && a != ThousandFormsMainAction(self) && costA(game, a) > 0 && power - costA(game, a) < 1 && active.none |=> -1000000 -> "dont spend last power if 1000F unused"

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
                case ThousandForms =>
                    have(Nyarlathotep) && power > 0 |=> 1000 -> "must have if have nyarlathotep"
                case Emissary =>
                    have(Nyarlathotep) |=> 900 -> "good if have nyarlathotep"
                    true |=> 450 -> "too good"
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
                    true |=> -10000 -> "just dont"
                case Madness =>
                    true |=> 750 -> "madness"
                case _ =>
                    true |=> -1000 -> "unknown"
            }

            case RitualAction(_, cost, _) =>
                val drain = need(AwakenNyarlathotep).?(10).|(0) + need(Pay4Power).?(4).|(0) + need(Pay4Power).?(6).|(0) +
                    need(CaptureCultist).?(10).|(0) + need(Gates3Power12).?(12).|(0) + need(Gates4Power15).?(15).|(0)

                instantDeathNow |=> 10000 -> "instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "dont ritual if ID next and not all SB"
                instantDeathNext && !allSB && others.all(!_.allSB) && realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30, and all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom <= 29 && realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"

                drain + cost > power |=> -5000 -> "not enough power for all"
                !allSB && realDoom + maxDoomGain < 30 |=> -5000 -> "not all sb and wont get 28"

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
                active.none |=> 500000 -> "move done"

            case MoveAction(_, Nyarlathotep, o, d) =>
                val hasturThreat = YS.has(Hastur) && YS.player.goo(Hastur).region.ownGate && YS.power == 0
                val kiyThreat = YS.has(KingInYellow) && YS.player.goo(KingInYellow).region.ownGate && YS.power == 0
                val ihh = have(SeekAndDestroy).?(self.all(HuntingHorror).%(_.region != d).num).|(0)

                d.enemyGate && others./(_.aprxDoom).max > 15 && (d.owner.aprxDoom + 5 < others./(_.aprxDoom).max) && d.foes.goos.none |=> -1500 -> "bash leader instead"
                d.enemyGate && others./(_.aprxDoom).max > 23 && (d.owner.aprxDoom + 1 < others./(_.aprxDoom).max) && d.foes.goos.none |=> -1500 -> "bash leader instead"

                val kiyScreamCapture = YS.power > 1 && YS.has(KingInYellow) && YS.has(ScreamingDead)

                if (kiyScreamCapture && !self.allSB) {
                    val r = YS.player.goo(KingInYellow).region
                    o.allies.cultists.any && o.allies.cultists.num < 4 && r.near.%(_ == o).any && self.strength(game, o.allies.monsters, YS) <= r.foes(Undead).num + 1 |=> -1000 -> "kiy scream capture"
                    d.allies.cultists.any && o.allies.cultists.num < 4 && r.near.%(_ == d).any && d.ownStr <= r.foes(Undead).num + 1 && d.ownGate |=> 900 -> "kiy scream capture"
                }

                power == 1 && o.ownGate && o.allies.num == 2 && active.%(_.at(o).goos.any).none && o.foes.monsters.%(_.faction.power > 0).any |=> -2000 -> "dont lose the gate"
                power == 1 && o.ownGate && o.allies.num == 2 && active.%(_.at(o).goos.any).none && o.near.%(_.foes.monsters.%(_.faction.power > 1).any).any |=> -1900 -> "dont lose the gate"
                power == 1 && o.ownGate && o.allies.num == 2 && active.%(_.at(o).goos.any).none && o.near.%(_.near.%(_.foes.monsters.%(_.faction.power > 2).any).any).any |=> -1800 -> "dont lose the gate"

                active.none && self.gates.num < 4 && power > 1 && have(Emissary) && d.enemyGate && (d.owner != YS || (!hasturThreat && !kiyThreat)) && d.controllers.num == 1 && d.of(d.owner).goos.none |=> (5 * 100000 / 3) -> "go get a gate"

                d.enemyGate && self.gates.num < 4 && (!d.owner.active || d.owner.power < power) && (d.owner != YS || !hasturThreat) && d.controllers.num == 1 && d.controllers.monsters.none && d.foes.goos.none && d.owner.blind(self) && power > 1 |=> 999 -> "go get a gate"
                d.enemyGate && self.gates.num < 4 && (!d.owner.active || d.owner.power < power) && (d.owner != YS || !hasturThreat) && d.controllers.num == 2 && d.controllers.monsters.none && d.foes.goos.none && d.owner.blind(self) && power > 2 |=> 990 -> "go get a gate"
                d.enemyGate && self.gates.num < 4 && (!d.owner.active || d.owner.power < power) && (d.owner != YS || !hasturThreat) && d.owner.power + 1 < power && d.foes.goos.none && have(Emissary) && power > 2 |=> 900 -> "go get a gate"
                d.enemyGate |=> -d.controllers.num -> "controllers"

                d.foes(Hastur).any |=> -3000 -> "no not hastur"
                o.foes(Hastur).any |=> 2800 -> "no not hastur"
                d.foes(Cthulhu).any && d.of(GC).num > d.allies.num |=> -2600 -> "no not cthulhu"
                o.foes(Cthulhu).any && o.of(GC).num > o.allies.num && GC.power > 0 |=> 2400 -> "no not cthulhu"
                o.ownGate && active.%(_.at(o).monsters.any).any && o.allies.monsters.none |=> -1500 -> "protect gate"

                d.foes(Hastur).any && power > 1 && YS.power == 0 && have(Abduct).?(d.allies(Nightgaunt).num).|(0) + have(Invisibility).?(d.allies(FlyingPolyp).num).|(0) >= d.of(YS).num - 1 |=> 3333 -> "assassinate hastur"

                d.ownGate && d.allies.cultists.num <= YS.power && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= (d.allies.num + ihh    ) * 4 && YS.power >= 0 |=> 2000 -> "nya defend from kiy"
                d.ownGate && d.allies.cultists.num <= BG.power && d.foes(ShubNiggurath).any                       && d.str(BG) <= (d.allies.num + ihh    ) * 4 && BG.power >= 0 |=> 1800 -> "nya defend from shu"
                d.ownGate && d.allies.cultists.num <= GC.power && d.foes(Cthulhu).any                             && d.str(GC) <= (d.allies.num + ihh - 1) * 4 && GC.power >= 0 |=> 1600 -> "nya defend from cth"
                d.ownGate && d.allies.cultists.num <= WW.power && (d.foes(RhanTegoth).any || d.foes(Ithaqua).any) && d.str(WW) <= (d.allies.num + ihh - 1) * 4 && WW.power >= 0 |=> 1600 -> "nya defend from rha"

                (self.allSB || !YS.active || !d.desecrated        ) && power > 1 && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= (d.allies.num + ihh    ) * 4 && YS.power >= 0 |=> 1000 -> "nya bully kiy"
                (self.allSB || !BG.active || d.allies.cultists.any) && power > 1 && d.foes(ShubNiggurath).any                       && d.str(BG) <= (d.allies.num + ihh    ) * 4 && BG.power >= 0 |=>  900 -> "nya bully shu"
                (self.allSB || !GC.active || d.allies.cultists.any) && power > 1 && d.foes(Cthulhu).any                             && d.str(GC) <= (d.allies.num + ihh - 1) * 4 && GC.power >= 0 |=>  800 -> "nya bully cth"
                (self.allSB || !WW.active || d.allies.cultists.any) && power > 1 && (d.foes(RhanTegoth).any || d.foes(Ithaqua).any) && d.str(WW) <= (d.allies.num + ihh - 1) * 4 && WW.power >= 0 |=>  800 -> "nya bully rha"

                val shield = d.allies.num + have(SeekAndDestroy).?(self.all(HuntingHorror).num).|(0) >= 2

                active.none && power > 1 && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= (d.allies.num + ihh) * 2 |=> 400000 -> "nya defend from kiy"
                active.none && power > 1 && d.foes(ShubNiggurath).any && d.foes(Hastur).none && d.str(BG) <= (d.allies.num + ihh) * 2 |=> 350000 -> "nya defend from shub"
                active.none && power > 1 && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= (d.allies.num + ihh) * 3 |=> 300000 -> "nya defend from kiy"
                active.none && power > 1 && d.foes(ShubNiggurath).any && d.foes(Hastur).none && d.str(BG) <= (d.allies.num + ihh) * 3 |=> 250000 -> "nya defend from shub"
                active.none && power > 1 && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) <= (d.allies.num + ihh) * 4 |=> 200000 -> "nya defend from kiy"
                active.none && power > 1 && d.foes(ShubNiggurath).any && d.foes(Hastur).none && d.str(BG) <= (d.allies.num + ihh) * 4 |=> 150000 -> "nya defend from shub"

                shield && d.foes(KingInYellow).any && d.foes(Hastur).none && !game.desecrated.contains(d) |=> 300 -> "nya chase kiy"
                shield && d.foes(ShubNiggurath).any && d.enemyGate && d.owner == BG |=> 290 -> "nya chase shub"
                shield && d.foes(Cthulhu).any && d.ocean.not |=> 280 -> "nya chase cthulhu"
                shield && d.foes(RhanTegoth).any && d.ocean.not |=> 270 -> "nya chase rhan"
                shield && d.foes(Ithaqua).any && d.ocean.not |=> 260 -> "nya chase ithaqua"

                need(CaptureCultist) && d.foes.%(_.vulnerableG).any && d.enemyGate && d.owner.power < power |=> 450 -> "need capture"

                d.enemyGate && (power > d.owner.power || !d.owner.active) && d.foes.goos.none |=> 110 -> "enemy gate"
                d.foes.%(_.vulnerableG).any && power > 1 |=> 90 -> "maybe capture"
                d.freeGate |=> 75 -> "free gate"
                active.any && d.ownGate |=> 50 -> "own gate"
                active.any && d.allies.cultists.any |=> 25 -> "hug cultist"

                game.tax(d, self) > 0 |=> -100 -> "iceaged"

                d.enemyGate && d.owner.gates.num >= others./(_.gates.num).max |=> 100 -> "maxgater"

                game.cathedrals.contains(o) && AN.has(UnholyGround) && o.str(AN) > 0 && (AN.player.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                game.cathedrals.contains(d) && AN.has(UnholyGround) && d.str(AN) > 0 && (AN.player.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

            case MoveAction(_, Acolyte, o, d) =>
                WW.exists && game.board.starting(WW).contains(d) && d.noGate |=> -10000000 -> "starting ww"

                val u = self.at(o, Acolyte).%(!_.has(Moved)).head

                active.none && self.gates.num < 4 && !u.gateKeeper && d.freeGate |=> (2 * 100000 / 1) -> "safe move and get gate"

                active.none && self.gates.num < 4 && !u.gateKeeper && d.noGate && power > 3 && (d.ocean.not || !GC.needs(OceanGates)) |=> (2 * 100000 / 4) -> "safe move and build gate"

                u.gateKeeper && (!u.capturable || u.enemies.goos.active.none) |=> -500 -> "dont move gatekeeper"
                self.pool.cultists.any && d.allies.any && !self.all.%(_.has(Moved)).any |=> -500 -> "why move if can recruit for same"
                o.allies.cultists.num == 6 && d.empty && d.near.all(_.empty) && d.near2.all(_.empty) |=> 999 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && d.empty && d.near.all(_.empty) && d.near2.all(_.of(YS).none) |=> 990 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && d.empty && d.near.all(_.empty) |=> 900 -> "crowded cultists 6 explore all empty around"
                o.allies.cultists.num == 6 && d.empty && d.near.all(r => r.empty || r.foes.active.num == r.foes.active.cultists.num) |=> 800 -> "crowded cultists 6 explore all empty or passive or cultists around"

                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.none |=> 400 -> "ic free gate"
                !u.gateKeeper && d.freeGate && d.foes.goos.none && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && (active.none || d.capturers.%(f => f.power > 0 || f.has(Passion)).none) |=> 350 -> "ic temporary free gate"
                !u.gateKeeper && d.freeGate && d.foes.goos.any  && self.gates.num < self.all.%(_.canControlGate).num && d.capturers.any && (active.none || d.capturers.%(f => f.power > 0 || f.has(Passion)).none) |=> 300 -> "ic temporary free gate with goo"
                o.allies.cultists.num == 1 && o.capturers.%(_.active).any && d.capturers.none && d.ownGate |=> 60 -> "flee from capture to own gate"
                o.allies.cultists.num == 1 && o.capturers.%(_.active).any && d.capturers.none && d.allies.monsters.any |=> 59 -> "flee from capture to monster"
                o.allies.cultists.num == 1 && o.capturers.%(_.active).any && d.capturers.none && d.empty |=> 58 -> "flee from capture"

                others.%(f => f.power > 0 || f.has(Passion)).%(f => o.of(f).goos.none && o.of(f).monsters.none).none |=> -300 -> "why move"

                d.foes.goos.any && d.allies.goos.none |=> -250 -> "dont move to enemy goos"
                o.capturers.%(_.active).any && d.capturers.none && o.allies.cultists.num == 1 |=> 220 -> "move from capture"
                o.ownGate && o.allies.cultists.num == 2 |=> -210 -> "move from own gate"
                o.ownGate && o.allies.cultists.num == 3 |=> -110 -> "move from own gate"
                d.ownGate && d.allies.cultists.num >= 2 |=> -210 -> "move to own gate"
                !o.ownGate && d.allies.goos.any |=> 200 -> "move to goo"
                d.ownGate && d.allies.cultists.num == 1 |=> 190 -> "move to own gate with single cultist"
                d.allies.monsters.any |=> 80 -> "move to monsters"
                d.ownGate |=> 70 -> "move to own gate"
                d.foes.any |=> -50 -> "dont move to foes"
                o.gate |=> -10 -> "move from gate"

            case MoveAction(_, uc, o, d) =>
                val u = self.at(o, uc).%(!_.has(Moved)).head

                // Works best in early game when it's "safe" to leave cultists alone.
                wwLoneCultistPolarGate(1) && (WW.exists.?(d == game.board.starting(WW).but(game.starting(WW)).head)|(false)) |=> 1500 -> "move monster to block ww getting 2nd gate at opp pole"

                o.ownGate && o.allies.goos.none && o.allies.monsters.num == 1 && active.%(_.at(o).goos.none).%(_.at(o).monsters.any).any |=> -2000 -> "dont abandon gate"

                d.ownGate && d.capturers.any && d.capturers.%(_.active).any && d.capturers.%(_.active).%(_.at(d).goos.any).none |=> 1100 -> "protect gate"

                uc == Nightgaunt && (o.noGate || o.enemyGate || o.foes.none) && have(Nyarlathotep) && power > 2 && o.foes.goos.none && d.foes.goos.any && d.foes(Hastur).none && (d.foes(Cthulhu).none || d.allies.any || power > 3) && (others.%(_.at(d).goos.any).%(!_.active).any) |=> 700 -> "maybe shield"

                uc == FlyingPolyp && (o.noGate || o.enemyGate || o.foes.none) && have(Nyarlathotep) && power > 2 && o.foes.goos.none && d.foes.goos.any && d.foes(Hastur).none && (d.foes(Cthulhu).none || d.allies.any || power > 3) && (others.%(_.at(d).goos.any).%(!_.active).any) |=> 700 -> "maybe shield"

                o.ownGate && u.friends.goos.none && u.friends.monsters.none && u.friends.cultists.num == 1 |=> -600 -> "dont leave lone cultist on gate"

                o.foes.goos.any |=> -500 -> "stay with enemy goos"

                val canCaptureYS = self.all.cultists./(_.region).%(_.capturers.contains(YS)).none

                d.allies.none && d.foes.cultists.%(_.vulnerableM).map(_.faction).%(f => f.blind(self) && (f != YS || canCaptureYS)).any && (need(CaptureCultist) || !have(Nyarlathotep)) |=> 250 -> "go for capture"

                d.ownGate && canSummon(uc) && !game.hasMoved(self) |=> -1200 -> "why move if can summon"

                o.allies.cultists.none && d.ownGate && d.allies.monsters.none |=> 50 -> "move"

                o.allies.cultists.none && d.enemyGate && d.allies.none |=> 2 -> "move"

                u.is(Nightgaunt) && have(Abduct) && d.foes(Hastur).any && power > 2 && YS.power == 0 && have(Abduct).?(d.allies(Nightgaunt).num).|(0) + have(Invisibility).?(d.allies(FlyingPolyp).num).|(0) < d.of(YS).num - 1 |=> 3332 -> "assassinate hastur"
                u.is(FlyingPolyp) && have(Invisibility) && d.foes(Hastur).any && power > 2 && YS.power == 0 && have(Abduct).?(d.allies(Nightgaunt).num).|(0) + have(Invisibility).?(d.allies(FlyingPolyp).num).|(0) < d.of(YS).num - 1 |=> 3331 -> "assassinate hastur"

                active.none && power > 1 && u.is(Nightgaunt) && d.allies.goos.any && d.foes(KingInYellow).any && d.foes(Hastur).none && d.str(YS) > d.allies(Acolyte).num + d.allies(Nightgaunt).num |=> 1100000 -> "shield from kiy"

                u.is(FlyingPolyp) |=> -1 -> "fp stay"
                u.is(HuntingHorror) |=> 1 -> "hh move"

            case AttackAction(_, r, f) =>
                val allies = self.at(r)
                val foes = f.at(r)

                val enemyStr = f.strength(game, foes, self)
                val ownStr = self.strength(game, allies, f)

                val igh = BG.has(Necrophagy).?(BG.all(Ghoul).diff(foes).num).|(0)

                var ac = allies(Acolyte).num
                var ng = allies(Nightgaunt).num
                var fp = allies(FlyingPolyp).num
                var hh = allies(HuntingHorror).num

                val nya = allies.has(Nyarlathotep)

                var ihh = have(SeekAndDestroy).??(self.all(HuntingHorror).diff(allies).num)

                def emissary = have(Emissary) && nya && foes.goos.none

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

                        r.ownGate && !nya && kiy && ((ownStr > enemyStr && ownStr > ec + un + by) || ownStr >= foes.num * 2) && !(have(Nyarlathotep) && self.allSB && power > 1) |=> 4444 -> "chase kiy away"

                        r.ownGate && !nya && has && un <= 1 && by == 0 && ownStr > un |=> 1222 -> "chase has away"

                        un == 1 && by == 0 && !kiy && !has && (ac == 0 || !YS.has(Zingaya)) && !game.desecrated.contains(r) |=> -1000 -> "dont fight lone undead on undesecrated"

                        has && have(Abduct).?(ng).|(0) + have(Invisibility).?(fp).|(0) >= ec + un + by && ownStr + ihh * 2 > 4 |=> 3334 -> "assassinate has"

                        !f.active && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any && f.has(Passion) && ec > 1 |=> -1000 -> "dont attack if passion allows reverse capture"
                        emissary && r.enemyGate && r.owner == f && r.owner.has(Passion) && ec > 1 |=> 900 -> "better skirmish ys than capture"

                    case SL =>
                        var ec = foes(Acolyte).num
                        var wz = foes(Wizard).num
                        var sm = foes(SerpentMan).num
                        var fs = foes(FormlessSpawn).num
                        var tsa = foes.has(Tsathoggua)

                        nya && !tsa && ownStr >= 6 && (fs * 3 + sm * 2 + wz) > 1 |=> 100 -> "ok sl attack"

                        0 -> "todo"

                    case WW =>
                        def ec = foes(Acolyte).num
                        def we = foes(Wendigo).num
                        def gk = foes(GnophKeh).num
                        def rha = foes.has(RhanTegoth)
                        def ith = foes.has(Ithaqua)

                        val goo = rha || ith

                        nya && !goo && foes.num > 1 |=> 400 -> "blind ww attack"

                        nya && foes.num <= allies.num + ihh && (foes.num > 1 || goo) |=> 1200 -> "blind more ww attack"

                        0 -> "todo"

                    case OW =>
                        var ac = foes(Acolyte).num
                        var mu = foes(Mutant).num
                        var ab = foes(Abomination).num
                        val sp = foes(SpawnOW).num
                        val ygs = allies.has(YogSothoth)

                        nya && !ygs && ownStr >= (mu + ab + sp) * 5 |=> 100 -> "ok ow attack"

                        0 -> "todo"

                    case AN =>
                        allies.goos.any && game.cathedrals.contains(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                        AN.has(Extinction) && foes.monsters.num == 1 && foes(Yothan).any && ((nya && allies.num >= 3 && ownStr >= 6) || (nya && self.has(Emissary)) || (allies.goos.none && ownStr >= 6)) |=> 1000 -> "attack lone extinct yothan"
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

                need(CaptureCultist) |=> 1700 -> "spellbook good"
                r.enemyGate && f == r.owner && r.controllers.num == 1 && r.allies.cultists.none && active.%(_.at(r).%(_.canControlGate).any).any |=> -700 -> "give gate away"
                !f.has(Passion) |=> 1600 -> "capture"
                f.has(Passion) && !f.active && self.all.cultists./(_.region).%(_.capturers.contains(YS)).any |=> -1500000 -> "dont capture if passion allows reverse capture"
                f.has(Passion) && (!f.active || !YS.has(Hastur) || !YS.has(ThirdEye)) |=> 1100 -> "capture with passion safe"
                f.has(Passion) |=> 950 -> "capture with passion"
                r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 450 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 400 -> "capture and nearly open gate"
                r.enemyGate |=> 100 -> "enemy gate"

            case BuildGateAction(_, r) =>
                active.none && self.gates.num < 4 |=> (10 * 100000 / 9) -> "safe build gate"

                YS.has(Hastur) && YS.power > 1 |=> -1000 -> "hastur in play"
                YS.has(KingInYellow) && YS.power > 1 && game.board.connected(YS.player.goo(KingInYellow).region).contains(r) |=> -1000 -> "kiy is near"
                BG.has(ShubNiggurath) && BG.power > 0 && r.allies.cultists.num == 1 && !r.allies.has(Nyarlathotep) |=> -800 -> "shub in play and lone cultist"
                GC.has(Dreams) && GC.power > 1 && r.allies.cultists.num == 1 && !r.allies.has(Nyarlathotep) |=> -700 -> "cthulhu has dreams"

                WW.exists && game.board.starting(WW).contains(r) |=> -10000000 -> "starting ww"
                GC.exists && game.board.starting(GC).contains(r) |=> -10000000 -> "starting gc"

                power > 5 && r.near.%(n => active.%(_.at(n).any).any).none |=> -600 -> "building can wait"
                (power >= 3 + maxEnemyPower || self.gates.num <= 1) && r.capturers.none |=> 500 -> "building gates is good"

            case RecruitAction(_, Acolyte, r) =>
                active.none && r.freeGate |=> (3 * 100000 / 1) -> "safe recruit and get gate"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && power <= self.pool(Acolyte).num && r.allies.cultists.none |=> (1 * 100000 / 1) -> "safe recruit"
                active.none && self.gates.num < 4 && r.noGate && power > 3 && power <= self.pool(Acolyte).num && r.allies.cultists.any |=> (8 * 100000 / 9) -> "safe recruit"
                active.none && r.ownGate && r.allies.cultists.num == 1 |=> (7 * 100000 / 8) -> "safe recruit"

                r.capturers.%(_.active).any |=> -2000 -> "dont recruit to be captured"
                r.freeGate |=> 1700 -> "free gate"
                self.pool.cultists.num >= power && !have(Nyarlathotep) |=> 300 -> "recover lost cultists"
                r.ownGate && others.all(_.power < power) |=> -250 -> "dont recruit if max power"
                r.ownGate && r.allies.cultists.num == 1 |=> 100 -> "a cultist needs a friend"

            case SummonAction(_, Nightgaunt, r) =>
                // Defense against WW setting up 2nd gate at opposite pole.  Summon this turn in order to move next turn
                r.distanceToWWOppPole <= 2 && wwLoneCultistPolarGate(2) && r.allies.monsters.none |=> 1500 ->"cheap summon to stop WW 2nd gate at opp pole"

                true |=> (-r.allies.monsters.num) -> "monsters count"

                power == 1 && r.allies.num == 1 && active.%(_.at(r).goos.any).none && r.foes.monsters.%(_.faction.power > 0).any |=> 2000 -> "dont lose the gate"
                power == 1 && r.allies.num == 1 && active.%(_.at(r).goos.any).none && r.near.%(_.foes.monsters.%(_.faction.power > 1).any).any |=> 1900 -> "dont lose the gate"
                power == 1 && r.allies.num == 1 && active.%(_.at(r).goos.any).none && r.near.%(_.near.%(_.foes.monsters.%(_.faction.power > 2).any).any).any |=> 1800 -> "dont lose the gate"

                power > 1 && r.allies.goos.any && r.foes.goos(Hastur).none && others.%(_.at(r).goos.any).%(!_.active).any |=> 850 -> "shield"

                r.allies(Nightgaunt).none |=> 20 -> "ng cheap"
                r.allies(Nightgaunt).any |=> 10 -> "ng cheap"
                r.foes(KingInYellow).any |=> -150 -> "ng bad against kiy"
                r.near.%(_.foes(KingInYellow).any).any |=> -110 -> "ng bad against kiy"
                r.near.%(n => n.noGate && n.of(YS).any && YS.power > 5).any |=> -100 -> "ng bad against kiy"
                YS.has(KingInYellow) && YS.power >= 2 && r.allies.monsters.num == r.allies.monsters(Nightgaunt).num && r.allies.goos.none |=> -90 -> "ng bad against kiy"
                YS.power >= 6 |=> -80 -> "ng bad against kiy"

                self.pool(Nightgaunt).num == 1 && self.pool(FlyingPolyp).num > 0 && power == 2 |=> -500 -> "summon fp instead"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.active).any && r.capturers.%(_.active).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.capturers.any && r.capturers.%(_.at(r).goos.any).none |=> 250 -> "prevent later capture"
                r.foes.cultists.%(_.vulnerableM).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsters.none |=> 150 -> "lone cultist"
                r.allies.monsters.none |=> 140 -> "no monsters"
                r.allies.cultists.num == 1 && r.allies.monsters.any |=> 130 -> "one cultist"
                r.allies.goos.any |=> 100 -> "summon to nya"

            case SummonAction(_, FlyingPolyp, r) =>
                true |=> (-r.allies.monsters.num) -> "monsters count"

                power > 2 && r.allies.goos.any && r.foes.goos(Hastur).none && others.%(_.at(r).goos.any).%(!_.active).any |=> 800 -> "shield"

                r.allies(FlyingPolyp).none |=> 15 -> "fp useful"
                r.allies(FlyingPolyp).any && r.foes.goos.none |=> -150 -> "one fp enough"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.active).any && r.capturers.%(_.active).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.foes(KingInYellow).any && r.foes(Hastur).none |=> 230 -> "fight kiy"
                r.foes.cultists.%(_.vulnerableM).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsters.none |=> 150 -> "lone cultist"
                r.allies.monsters.none |=> 140 -> "no monsters"
                r.allies.cultists.num == 1 && r.allies.monsters.any |=> 130 -> "one cultist"
                r.allies.goos.any |=> 100 -> "summon to nya"

            case SummonAction(_, HuntingHorror, r) =>
                true |=> (-r.allies.monsters.num) -> "monsters count"

                have(Nyarlathotep) && !have(ThousandForms) && need(Pay4Power) && power > 4 && power < 8 |=> -300 -> "better pay 4 power for 1000f"

                !have(Nyarlathotep) |=> 10 -> "hh expensive"
                have(Nyarlathotep) |=> 30 -> "hh to shield nya"

                r.foes(KingInYellow).any && power > 3 |=> -150 -> "ng bad againgt kiy"

                r.controllers.num == 1 && r.controllers.%(_.capturable).any && r.foes.goos.none |=> 1900 -> "prevent losing gate"
                r.capturers.%(_.active).any && r.capturers.%(_.active).%(_.at(r).goos.any).none |=> 1800 -> "prevent capture"
                r.foes.cultists.%(_.vulnerableG).any |=> 200 -> "summon to capture"
                r.allies.cultists.num == 1 && r.allies.monsters.none |=> 150 -> "lone cultist"
                r.allies.monsters.none |=> 140 -> "no monsters"
                r.allies.cultists.num == 1 && r.allies.monsters.any |=> 130 -> "one cultist"
                r.allies.goos.any |=> 100 -> "summon to nya"

            case AwakenAction(_, _, r, _) =>
                numSB >= 5 && need(AwakenNyarlathotep) |=> 5000 -> "need nyarlathotep"
                r.foes.has(Hastur) |=> -3000 -> "hastur is scary"
                numSB < 5 && numSB > 3 && need(AwakenNyarlathotep) |=> 2500 -> "need nyarlathotep"
                power > 10 |=> 2000 -> "yes awaken"
                power == 10 |=> 1800 -> "maybe awaken"
                r.foes(KingInYellow).any |=> 400 -> "awaken to battle kiy"
                r.foes.goos.any |=> 300 -> "awaken to battle"
                r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                r.foes.%(_.vulnerableG).any |=> 200 -> "awaken to capture"
                r.allies.%(_.vulnerableM).any |=> 150 -> "allies vulnerable"
                r.foes.monsters.any |=> 100 -> "awaken to defend"
                r.near.%(_.foes(KingInYellow).any).any |=> 50 -> "awaken near kiy"

            case AvatarReplacementAction(_, _, r, o, uc) =>
                val u = self.at(r, uc).head
                u.cultist && o.capturers.%(_.active).none && o.freeGate |=> 300 -> "free gate"
                u.cultist && r.allies.goos.any |=> -250 -> "stay with goo"
                u.cultist && o.allies.goos.any |=> 250 -> "go to goo"
                u.cultist && o.capturers.%(_.active).%(_ != BG).any |=> -100 -> "dont send cultist to be captured"
                u.cultist && o.capturers.none |=> 150 -> "no capturers"
                u.cultist && o.capturers.any && o.capturers.%(_.active).none |=> 100 -> "no capturers with power"
                u.monster && o.foes.%(_.capturable).any && power > 0 |=> 200 -> "send to capture"
                u.monster && u.friends.cultists.any && u.friends.monsters.none && r.foes.monsters./(_.faction).%(_ != BG).%(_.active).any |=> -200 -> "dont sent temp defender"
                u.monster && o.allies.cultists.any && o.allies.monsters.none |=> 50 -> "protect cultist"

            case RevealESAction(_, es, false, _) if game.of(self).es != es =>
                true |=> -10000 -> "better reveal all"

            case RevealESAction(_, _, _, _) =>
                allSB && realDoom >= 30 |=> 1000 -> "reveal and try to win"
                allSB && realDoom < 30 && realDoom < self.aprxDoom && realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                !allSB && realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 800 -> "reveal so 30 broken and nobody wins"
                true |=> -100 -> "dont reveal"
                canRitual |=> -2000 -> "ritual first"

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

            case ThousandFormsMainAction(_) =>
                power == 1 |=> 2000 -> "spend last power on 1000F"
                active.none |=> 1000000 -> "no enemy power"
                self.allSB && others.all(f => f.all.goos./(_.region).all(_.str(f) > 2)) |=> 2000 -> "before launching nya attack"

            case Pay10PowerMainAction(_) =>
                self.numSB == 4 && realDoom >= 30 |=> 5000 -> "last spellbooks and end"
                self.numSB == 4 && power > 20 |=> 3000 -> "last spellbooks and plenty power"
                self.numSB == 4 && have(Nyarlathotep) && power >= 12 |=> 800 -> "last spellbooks for nya"

                true |=> 10 -> "if wiped out"

            case Pay4PowerMainAction(_) =>
                self.numSB == 5 && power > 14 |=> 3000 -> "last spellbooks and plenty power"

                self.numSB == 4 && have(Nyarlathotep) && need(Pay6Power) && power >= 12 && others.all(_.aprxDoom < 25) |=> 900 -> "last spellbooks for nya but slow"

                self.numSB == 5 && realDoom >= 30 |=> 5000 -> "last spellbook and end"
                self.numSB == 5 && have(Nyarlathotep) && power >= 6 |=> 800 -> "last spellbook for nya"
                self.numSB == 4 |=> 400 -> "pre-last spellbook"
                power < 6 |=> -200 -> "not much power"
                power == 4 |=> -200 -> "last power"
                realDoom >= 20 |=> 600 -> "the end is near"
                realDoom >= 25 |=> 450 -> "the end is very near"
                realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"
                have(Nyarlathotep) && !have(ThousandForms) && power == 5 |=> 1100 -> "last power roll"

                true |=> 4 -> "if wiped out"

            case Pay6PowerMainAction(_) =>
                numSB == 5 && power > 16 |=> 3000 -> "last spellbooks and plenty power"
                numSB == 5 && realDoom >= 30 |=> 5000 -> "last spellbook and end"
                numSB == 5 && have(Nyarlathotep) && power >= 8 |=> 800 -> "last spellbook for nya"
                numSB == 4 |=> 200 -> "pre-last spellbook"
                power < 9 |=> -200 -> "not much power"
                power == 6 |=> -200 -> "last power"
                realDoom >= 20 |=> 590 -> "the end is near"
                realDoom >= 25 |=> 440 -> "the end is very near"
                realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

                true |=> 6 -> "if wiped out"


            case MainDoneAction(_) =>
                game.battled.any |=> -1000 -> "unlimited battle drains power"
                true |=> 500 -> "main done"

            case _ =>
        }

        // BATTLE
        if (game.battle != null) {
            val battle = game.battle

            def elim(battle : Battle, u : UnitFigure) {
                u.is(Nyarlathotep) && have(Emissary) && battle.units(battle.opponent(self)).goos.none |=> 1000 -> "emissary or what"
                u.is(Nightgaunt) |=> 400 -> "elim ng"
                u.is(Acolyte) |=> 800 -> "elim acolyte"
                u.is(FlyingPolyp) |=> 200 -> "elim fp"
                u.is(HuntingHorror) |=> 100 -> "elim hh"
                u.is(Nyarlathotep) |=> -1 -> "elim nya"
            }

            def retreat(battle : Battle, u : UnitFigure) {
                u.uclass == Acolyte |=> 600 -> "retr acolyte"
                u.gateKeeper && battle.region.allies.num - battle.side(self).opponent.rolls.%(_ == Pain).num >= 2 |=> -1000 -> "retr gate keeper"

                u.uclass == Nightgaunt |=> 100 -> "retr ng"
                u.uclass == FlyingPolyp |=> 200 -> "retr fp"
                u.uclass == HuntingHorror |=> 300 -> "retr hh"
                u.uclass == Nyarlathotep && u.region.foes.goos(Hastur).%(_.health == Alive).any |=> 2000 -> "retr nya from hastur"
                u.uclass == Nyarlathotep && u.region.foes.goos.%(!_.is(Hastur)).%(_.health == Alive).any |=> -1500 -> "nya stays with goo"
                u.uclass == Nyarlathotep && u.region.foes.goos.%(_.is(RhanTegoth)).%(_.faction.power > 0).any |=> -1500 -> "nya stays with rha"
                u.uclass == Nyarlathotep && u.region.ownGate && (u.region.allies.monsters.none || u.region.foes.goos.any) && u.region.foes.any |=> -500 -> "nya holds to gate"
                u.uclass == Nyarlathotep |=> 400 -> "nya moves anyway"
            }

            def rout(f : Faction, u : UnitClass, r : Region) {
                if (f == GC)
                    r.ocean.not |=> 10 -> "gc go ashore"

                u match {
                    case Acolyte =>
                        r.allies.monsters.any |=> 1000 -> "send cultist to be captured by monsters"
                        r.allies.goos.any |=> 1000 -> "send cultist to be captured by goos"
                        r.foes.none |=> 200 -> "send cultist where no foes"
                        f.at(r).%(!_.cultist).none |=> 200 -> "send where no friends"
                        f.at(r).%(_.monster).any |=> -1000 -> "dont send where friends"
                        f.at(r).%(_.goo).any |=> -3000 -> "dont send to goo"
                        r.freeGate |=> -2000 -> "dont send cultist to empty gate"
                        r.ownGate |=> -100 -> "dont send cultist to own gate"
                        r.gate |=> -100 -> "dont send cultist to gate"
                        r.empty |=> 50 -> "send cultist to empty area"

                    case KingInYellow =>
                        r.of(YS).goos.any |=> -2000 -> "dont send kiy to has"
                        r.allies.goos.any |=> 1500 -> "send kiy to nya"
                        r.ownGate |=> -1000 -> "dont send kiy to own gate"

                        r.of(YS).num == 0 |=> 800 -> "send where 0 friends"
                        r.of(YS).num == 1 |=> 700 -> "send where 1 friends"
                        r.of(YS).num == 2 |=> 600 -> "send where 2 friends"
                        r.of(YS).num == 3 |=> 500 -> "send where 3 friends"
                        r.of(YS).num == 4 |=> 400 -> "send where 4 friends"

                        r.gateOf(YS) |=> -300 -> "dont send kiy to own gate"
                        r.desecrated |=> 200 -> "send kiy where already desecrated"
                        r.glyph == GlyphWW && YS.needs(DesecrateWW) |=> -200 -> "dont sent for spellbook"
                        r.glyph == GlyphAA && YS.needs(DesecrateAA) |=> -200 -> "dont sent for spellbook"
                        r.glyph == GlyphOO && YS.needs(DesecrateOO) |=> -200 -> "dont sent for spellbook"
                        r.gate |=> -100 -> "dont send kiy to gate"

                        game.cathedrals.contains(r) && AN.has(UnholyGround) && r.str(AN) > 0 |=> 50000 -> "send kiy to unholy ground"

                    case Hastur =>
                        r.of(YS).goos.any |=> -2000 -> "dont send has to kiy"
                        r.ownGate |=> -1500 -> "dont send has to own gate"
                        r.allies.goos.any |=> -1000 -> "dont send has to nya"

                        r.of(YS).num == 0 |=> 800 -> "send where 0 friends"
                        r.of(YS).num == 1 |=> 700 -> "send where 1 friends"
                        r.of(YS).num == 2 |=> 600 -> "send where 2 friends"
                        r.of(YS).num == 3 |=> 500 -> "send where 3 friends"
                        r.of(YS).num == 4 |=> 400 -> "send where 4 friends"

                        r.gateOf(YS) |=> -300 -> "dont send has to own gate"
                        r.desecrated && r.of(YS).none |=> -1000 -> "dont send for feast"
                        r.glyph == GlyphWW && YS.needs(DesecrateWW) |=> -200 -> "dont sent for spellbook"
                        r.glyph == GlyphAA && YS.needs(DesecrateAA) |=> -200 -> "dont sent for spellbook"
                        r.glyph == GlyphOO && YS.needs(DesecrateOO) |=> -200 -> "dont sent for spellbook"
                        r.gate |=> -100 -> "dont send has to gate"

                        game.cathedrals.contains(r) && AN.has(UnholyGround) && r.str(AN) > 0 |=> 50000 -> "send has to unholy ground"

                    case u if u == Undead || u == Byakhee =>
                        r.of(YS).goos(KingInYellow).any |=> -2000 -> "dont send ys to kiy"
                        r.of(YS).goos(Hastur).any |=> -200 -> "dont send ys to has"
                        r.ownGate |=> -1500 -> "dont send ys to own gate"
                        r.desecrated && r.of(YS).none |=> -1000 -> "dont send for feast"
                        r.glyph == GlyphWW && YS.needs(DesecrateWW) |=> -200 -> "dont sent for spellbook"
                        r.glyph == GlyphAA && YS.needs(DesecrateAA) |=> -200 -> "dont sent for spellbook"
                        r.glyph == GlyphOO && YS.needs(DesecrateOO) |=> -200 -> "dont sent for spellbook"
                        r.gate && r.allies.any |=> -100 -> "dont send ys to gate"
                        r.of(YS).any |=> -r.of(YS).num*100 -> "disperse ys"

                    case _ =>
                        r.allies.any |=> -2000 -> "dont send non cultists to self"
                        r.ownGate |=> -2000 -> "dont send non cultists to own gate"
                        r.freeGate |=> -3000 -> "dont send non cultists to free gate"
                        r.gateOf(f) |=> -2000 -> "dont send non cultists to their gate"
                        r.enemyGate && r.owner != f && r.owner.gates.num == 6 |=> 600 -> "send non cultists to enemy gate 6"
                        r.enemyGate && r.owner != f && r.owner.gates.num == 5 |=> 500 -> "send non cultists to enemy gate 5"
                        r.enemyGate && r.owner != f && r.owner.gates.num == 4 |=> 400 -> "send non cultists to enemy gate 4"
                        r.enemyGate && r.owner != f && r.owner.gates.num == 3 |=> 300 -> "send non cultists to enemy gate 3"
                        r.enemyGate && r.owner != f && r.owner.gates.num == 2 |=> 200 -> "send non cultists to enemy gate 2"
                        r.enemyGate && r.owner != f && r.owner.gates.num == 1 |=> 100 -> "send non cultists to enemy gate 1"
                        r.freeGate |=> -100 -> "dont send non cultists to free gate"
                        r.empty |=> 200 -> "send non cultists to empty"

                        f != AN && u.utype == GOO && game.cathedrals.contains(r) && AN.has(UnholyGround) && r.str(AN) > 0 |=> 50000 -> "send goo to unholy ground"

                }
            }

            if (game.battle.attacker != self && game.battle.defender != self) {
                a match {
                    case RetreatOrderAction(_, a, b) =>
                        a.aprxDoom < b.aprxDoom |=> 100 -> "retreat less doom first"

                    case RetreatUnitAction(_, u, r) =>
                        rout(u.faction, u.uclass, r)

                    case _ =>
                }
            }
            else {
                val opponent = battle.opponent(self)
                val allies = battle.units(self)
                val enemies = battle.units(opponent)
                val first = battle.attacker == self

                def ac = allies(Acolyte).num
                def ng = allies(Nightgaunt).num
                def fp = allies(FlyingPolyp).num
                def hh = allies(HuntingHorror).num
                def ihh = have(SeekAndDestroy).?(self.all(HuntingHorror).diff(allies).num).|(0)
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

                def emissary = have(Emissary) && nya && enemies.goos.none

                a match {
                    case DevourAction(_, u) =>
                        elim(battle, u)

                    case AbductPreBattleAction(_) =>
                        opponent match {
                            case GC =>
                                cth && first && ng == 1 && ac + fp + hh == 0 |=> 1000 -> "abduct to avoid devour"
                                cth && first && ng == 1 && ac + fp + hh > 0 |=> -900 -> "wait for devour"
                                nya && (cth || !have(Emissary)) |=> -500 -> "stay as shield"
                                ng > ec + dp && sh + ss > 0 |=> 400 -> "eat good unit"
                                true |=> -100 -> "dont bother"
                            case BG =>
                                nya && ec + gh + fu == 0 && dy == 1 |=> -700 -> "dont if just one dy"
                                dy > 0 && gh == 0 && ng > ec + fu |=> 600 -> "eat dark young"
                                nya && (shu || !have(Emissary)) |=> -500 -> "stay as shield"
                                !nya && gh == 0 |=> 400 -> "eat cultist or unit"
                                nya && gh + fu == 0 |=> 300 -> "eat good cultist or unit"
                                true |=> -100 -> "dont bother"
                            case YS =>
                                nya && (kiy || !have(Emissary)) && !has |=> -500 -> "stay as shield"
                                ng == 1 && un == 1 |=> -400 -> "lone undead"
                                ng > ec + un && by > 0 |=> 300 -> "eat byakhee"
                                has |=> 200 -> "try strip hastur"
                                true |=> -100 -> "dont bother"
                            case SL =>
                                0 -> "todo"
                            case WW =>
                                0 -> "todo"
                            case OW =>
                                0 -> "todo"
                            case AN =>
                                0 -> "todo"
                        }

                    case InvisibilityAction(_, ifp, u) =>
                        val invises = allies(FlyingPolyp).%(!_.has(Invised)).num
                        if (!first && !nya && battle.enemyStr >= battle.ownStr * 2) {
                            u.uclass == HuntingHorror |=> 120000 -> "hide self"
                            ifp == u |=> 110000 -> "hide self"
                        }

                        opponent match {
                            case GC =>
                                nya && (cth || !have(Emissary)) && u.faction == self |=> -100000 -> "dont hide shields"
                                nya && !cth && have(Emissary) && u.uclass == HuntingHorror |=> 90000 -> "hide hh when emissary"
                                nya && !cth && have(Emissary) && u == ifp |=> 85000 -> "hide self when emissary"
                                cth && first && ifp == u && ac + ng + hh + ihh == 0 && fp == 1 |=> 90000 -> "hide self to avoid devour"
                                !first && cth && !nya && ec + dp > 1 && u.uclass == HuntingHorror |=> 80000 -> "hide hh from cthulhu"
                                !first && cth && !nya && ec + dp > 1 && u == ifp |=> 70000 -> "hide self from cthulhu"
                                first && u.uclass == Shoggoth && ec + dp > 0 && opponent.has(Absorb) && fp >= sh |=> 60000 -> "prevent absorb"
                                !first && u.uclass == Shoggoth && u.has(Absorbed) && u.count(Absorbed) == enemies./(_.count(Absorbed)).max |=> 50000 -> "hide max absorb"
                                u.uclass == Starspawn |=> 40000 -> "hide starspawn"
                                u.uclass == Shoggoth |=> 30000 -> "hide shoggoth"
                                u.uclass == DeepOne |=> 20000 -> "hide deep one"
                                nya && cth && u.uclass == Acolyte && u.faction == opponent |=> 15000 -> "hide enemy cultist"
                                u.uclass == Acolyte && u.faction == self |=> 10000 -> "hide cultist just in case"

                                true |=> -100 -> "dont bother"
                            case BG =>
                                nya && (shu || !have(Emissary)) && u.faction == self |=> -100000 -> "dont hide shields"
                                nya && !shu && have(Emissary) && u.uclass == HuntingHorror |=> 90000 -> "hide hh when emissary"
                                nya && !shu && have(Emissary) && u == ifp |=> 85000 -> "hide self when emissary"
                                !first && shu && !nya && ec + gh + fu > 1 && u.uclass == HuntingHorror |=> 80000 -> "hide hh from shub"
                                !first && shu && !nya && ec + gh + fu > 1 && u == ifp |=> 70000 -> "hide self from shub"
                                u.uclass == DarkYoung && (shu || !nya) |=> 50000 -> "hide dark young"
                                u.uclass == Fungi && (shu || !nya) |=> 40000 -> "hide fungi"
                                u.uclass == Ghoul && (shu || !nya) |=> 30000 -> "hide ghoul"
                                u.uclass == Acolyte && u.faction.has(Frenzy) && (shu || !nya) |=> 20000 -> "hide frenzy"
                                nya && shu && u.uclass == Acolyte && u.faction == opponent |=> 15000 -> "hide enemy cultist"
                                u.uclass == Acolyte && u.faction == self |=> 10000 -> "hide cultist just in case"

                                true |=> -100 -> "dont bother"
                            case YS =>
                                nya && !has && (kiy || !have(Emissary)) && u.faction == self |=> -100000 -> "dont hide shields"
                                nya && !has && !kiy && have(Emissary) && u.uclass == HuntingHorror |=> 90000 -> "hide hh when emissary"
                                nya && !has && !kiy && have(Emissary) && u == ifp |=> 85000 -> "hide self when emissary"
                                !first && has && !nya && ec + un + by > 0 && u.uclass == HuntingHorror |=> 80000 -> "hide hh from hastur"
                                !first && has && !nya && ec + un + by > 0 && u == ifp |=> 70000 -> "hide self from hastur"
                                u.uclass == Byakhee && invises >= by |=> 60000 -> "hide all byakhee"
                                u.uclass == Undead && un > 1 |=> 50000 -> "hide undead but last"
                                u.uclass == Byakhee && invises < by |=> 40000 -> "hide some byakhee"
                                u.uclass == Acolyte && u.faction == opponent && (kiy || has) |=> 30000 -> "hide passion cultist"
                                u.uclass == Acolyte && u.faction == self |=> 10000 -> "hide cultist just in case"
                                u.uclass == Undead && un == 1 && (kiy || has) |=> 5000 -> "hide last undead"

                                true |=> -100 -> "dont bother"

                            case SL =>
                                true |=> 0 -> "todo"

                            case WW =>
                                0 -> "todo"
                                true |=> -100 -> "dont bother"

                            case OW =>
                                true |=> 0 -> "todo"

                            case AN =>
                                true |=> 0 -> "todo"
                        }

                    case SeekAndDestroyAction(_, _, r) =>
                        opponent match {
                            case GC =>
                                first && cth && ac + ng + fp + hh == 0 && ihh == 1 |=> -3000 -> "dont seek devour"
                                emissary && dp + sh + ss > 0 |=> -3000 -> "dont seek emissary"

                            case BG =>
                                emissary && fr + fu + dy > 0 |=> -3000 -> "dont seek emissary"

                            case YS =>
                                emissary && (un > 1 || by > 0) |=> -3000 -> "dont seek emissary"

                            case SL =>
                                0 -> "todo"

                            case WW =>
                                0 -> "todo"

                            case OW =>
                                0 -> "todo"

                            case AN =>
                                0 -> "todo"
                        }

                        true |=> 2000 -> "seek seek destroy destroy"
                        r.ownGate && r.allies.goos.none && r.allies.monsters.num == 1 && r.foes.monsters.%(_.faction.power > 0).any |=> -2500 -> "dont lose gate"
                        true |=> r.allies.cultists.num -> "more cultists"
                        nya && !emissary && allies.num == 1 && battle.enemyStr > 0 |=> 3000 -> "shield nya"
                        nya && !emissary && allies.num == 2 && battle.enemyStr > 6 |=> 3500 -> "shield nya"

                    case DemandSacrificeKillsArePainsAction(_) =>
                        battle.strength(self) < battle.strength(battle.opponent(self)) |=> 1000 -> "less str"
                        battle.strength(self) > battle.strength(battle.opponent(self)) |=> -1000 -> "more str"

                    case HarbingerPowerAction(_, _, n) =>
                        n == 2 |=> 200 -> "harb 2 power"
                        n == 3 |=> 300 -> "harb 3 power"
                        n == 4 |=> 400 -> "harb 4 power"
                        n == 5 |=> 500 -> "harb 5 power"
                        has1000f && power == 0 |=> 1000 -> "harb for 1000f"

                    case HarbingerESAction(_, _, n) =>
                        !have(Nyarlathotep) |=> -1000 -> "no nya"
                        allSB |=> 350 -> "es all spellbooks"
                        allSB && realDoom >= 28 |=> 2000 -> "all spellbooks and sure break 30"
                        realDoom > others./(_.aprxDoom).max + 5 |=> -300 -> "doom lead"
                        power > 4 |=> 350 -> "enough power"

                    case AssignKillAction(_, _, _, u) =>
                        elim(battle, u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(battle, u)

                    case EliminateNoWayAction(_, u) =>
                        elim(battle, u)

                    case RetreatOrderAction(_, a, b) =>
                        a == self |=> 1000 -> "retreat self first"

                    case RetreatUnitAction(_, u, r) if u.faction != self =>
                        rout(u.faction, u.uclass, r)

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

                        u.monster && r.foes.%(_.vulnerableM).any && !r.foes.goos.any && r.allies.goos.none && r.allies.monsters.none |=> 1000 -> "send monster to capture"
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
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
