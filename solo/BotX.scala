package cws

import hrf.colmat._

case class Evaluation(weight : Int, desc : String)
case class ActionEval(action : Action, evaluations : List[Evaluation])

class BotX[F <: Faction](ge : Game => GameEvaluation[F]) {
    def sortByAbs(a : List[Int]) : List[Int] =
        a.sortBy(v => -v.abs)

    def compareEL(aaa : List[Int], bbb : List[Int]) : Int =
        (aaa, bbb) match {
            case (a :: aa, b :: bb) => (a == b).?(compareEL(aa, bb)).|((a > b).?(1).|(-1))
            case (0 :: _, Nil) => 0
            case (Nil, 0 :: _) => 0
            case (a :: _, Nil) => (a > 0).?(1).|(-1)
            case (Nil, b :: _) => (0 > b).?(1).|(-1)
            case (Nil, Nil) => 0
        }

    def compare(a : ActionEval, b : ActionEval) = compareEL(sortByAbs(a.evaluations./(_.weight)), sortByAbs(b.evaluations./(_.weight))) > 0

    def ask(game : Game, actions : List[Action], error : Double) : Action =
        askE(game, Explode.explode(game, actions), error)

    def askE(game : Game, actions : List[Action], error : Double) : Action = {
        if (actions.num == 1)
            return actions.head

        val ev = ge(game)
        val eas = actions./(a => ActionEval(a, ev.eval(a)))

        val o = eas.sortWith(compare)

        if (ev.self == CC && o.num > 1) {
            val top = o.head.action
            val descs = o./~(_.evaluations./(_.desc)).distinct

            descs.foreach { d =>
                val t = o./(ae => ActionEval(ae.action, ae.evaluations.%(_.desc != d))).sortWith(compare).head.action
                Stats.triggerI(ev.self, d, t != top)
            }
        }

        var v = o
        while (error > 0 && random() < error) {
            v = v.drop(1)
            if (v.none)
                v = o
        }

        v.head.action
    }

    def eval(game : Game, actions : List[Action]) : List[ActionEval] = {
        val ev = ge(game)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

object NoPlayer extends Player(SL)($)

abstract class GameEvaluation[F <: Faction](val game : Game, val self : F) {
    val others = game.factions.%(_ != self)

    implicit class SelfFactionClassify(val f : F) {
        def realDoom = self.doom + self.player.es./(_.value).sum
    }

    implicit class FactionClassify(val f : Faction) {
        def player = game.players.get(f).getOrElse(NoPlayer)
        def exists = game.players.contains(f)
        def power = player.power
        def active = power > 0 && !player.hibernating
        def gates = player.gates
        def doom = player.doom
        def es = player.es.num
        def aprxDoom = doom + (es * 1.67).round.toInt
        def maxDoom = doom + min(6, es) * 3 + max(0, es - 6) * 2
        def at(r : Region) = player.at(r)
        def at(r : Region, uc : UnitClass) = player.at(r, uc)
        def pool = player.inPool()
        def has(uc : UnitClass) = all.has(uc)
        def has(sb : Spellbook) = player.has(sb)
        def can(sb : Spellbook) = player.can(sb)
        def goo(uc : UnitClass) = all(uc).single.get
        def goos = all.%(_.uclass.utype == GOO)
        def cultists = all.cultists.num
        def units(uc : UnitClass) = all(uc)
        def count(uc : UnitClass) = all(uc).num
        def allSB = player.hasAllSB
        def numSB = player.spellbooks.num
        def all = player.all()
        def needs(r : Requirement) = player.needs(r)
        def blind(current : Faction) = willActBeforeFaction(current, f)
    }

    implicit class FactionListClassify(val l : List[Faction]) {
        def active = l.%(_.active)
    }

    val power = self.power
    def realDoom = self.doom + self.player.es./(_.value).sum
    def need(rq : Requirement) = self.needs(rq)
    def have(sb : Spellbook) = self.has(sb)
    def can(sb : Spellbook) = self.can(sb)
    def have(uc : UnitClass) = self.has(uc)
    def units(uc : UnitClass) = self.units(uc)
    def allSB = self.allSB
    def numSB = self.numSB
    def oncePerRound = self.player.oncePerRound

    implicit class RegionClassify(val r : Region) {
        def empty = allies.none && foes.none
        def allies = self.at(r)
        def foes = others./~(_.at(r))
        def of(f : Faction) = f.at(r)
        def str(f : Faction) = f.strength(game, of(f), self)
        def ownStr = str(self)
        def gate = game.gates.contains(r)
        def noGate = !gate
        def ownGate = self.gates.contains(r)
        def enemyGate = others.%(_.gates.contains(r)).any
        def freeGate = gate && !ownGate && !enemyGate
        def controllers = (ownGate || enemyGate).?(owner.at(r).%(_.canControlGate)).|(Nil)
        def gateOf(f : Faction) = f.gates.contains(r)
        def owner = game.factions.%(_.gates.contains(r)).single.get
        def capturers = others.%(f => allies.goos.none && ((of(f).monsters.any && allies.monsters.none) || of(f).goos.any))
        def desecrated = game.desecrated.contains(r)
        def near = game.board.connected(r)
        def near2 = game.board.connected(r).flatMap(n => game.board.connected(n)).%(_ != r).%(!near.contains(_))
        def near012 = game.board.connected(r).flatMap(n => game.board.connected(n)).distinct
        def ocean = r.glyph == Ocean
        /** Check if unaccompanied cultist for given faction at risk of capture in this region*/
        def riskyForCultists(f: Faction) = (allies ++ foes).%!(_.cultist).%!(_.faction == f).any
        /** Distance to specified faction unit type */
        def distanceTo(f: Faction, u: UnitType) = f.all.%(_.uclass.utype == u)./(uf => game.board.distance(r, uf.region)).minOr(Int.MaxValue)
        /* Distance from this region to Pole region opposite WW start location*/
        def distanceToWWOppPole: Int = (WW.exists).?(game.board.distance(r, game.board.starting(WW).but(game.starting(WW)).head))|Int.MaxValue
    }

    implicit class UnitListClassify(val us : List[UnitFigure]) {
        def active = us.%(_.active)
        def cultists = us.%(_.uclass.utype == Cultist)
        def actualMonsters = us.%(_.uclass.utype == Monster)
        def monsters = us.filter(u => u.uclass.utype == Monster || u.uclass.utype == Terror) // Should fit the intent in most cases.
        def goos = us.%(_.uclass.utype == GOO)
        def notGoos = us.%(_.uclass.utype != GOO)
        def apply(uc : UnitClass) = us.%(_.uclass == uc)
        def has(uc : UnitClass) = us.%(_.uclass == uc).any
    }

    implicit class UnitClassify(val u : UnitFigure) {
        def active = u.faction.active
        def is(uc : UnitClass) = u.uclass == uc
        def ally = u.faction == self
        def foe = u.faction != self
        def cultist = u.uclass.utype == Cultist
        def actualMonster = u.uclass.utype == Monster
        def monster = u.uclass.utype == Monster || u.uclass.utype == Terror // Should fit the intent in most cases.
        def goo = u.uclass.utype == GOO
        def friends = u.faction.at(u.region).%(_ != u)
        def enemies = game.factions.%(_ != u.faction)./~(_.at(u.region))
        def canControlGate = (cultist || (u.uclass == DarkYoung && u.faction.has(RedSign))) && u.health != Pained
        def ownGate = u.region.ownGate
        def enemyGate = u.region.enemyGate
        def gateController = u.region.gate && u.region.controllers.contains(u)
        def gateKeeper = gateController && friends.%(_.canControlGate).none
        def defender = ownGate && (monster || goo) && friends.monsters.none
        def protector = (monster || goo) && friends.cultists.any && friends.monsters.none
        def preventsCaptureM = monster && friends.cultists.any && friends.monsters.none && friends.goos.none && enemies.monsters.any
        def preventsCaptureG = goo && friends.cultists.any && friends.goos.none && enemies.goos.any
        def prevents = preventsCaptureM || preventsCaptureG
        def preventsActiveCaptureM = monster && friends.cultists.any && friends.monsters.none && friends.goos.none && enemies.monsters.active.any
        def pretender = cultist && !capturable && enemyGate
        def shield = friends.goos.any
        def capturable = cultist && capturers.active.any
        def capturers = game.factions.%(_ != u.faction).%(f => friends.goos.none && (f.at(u.region).goos.any || (friends.monsters.none && f.at(u.region).monsters.any)))
        def vulnerableM = cultist && friends.goos.none && friends.monsters.none
        def vulnerableG = cultist && friends.goos.none
    }

    implicit def unitref2unit(r : UnitRef) : UnitFigure = game.unit(r)
    implicit def unitref2unitcl(r : UnitRef) : UnitClassify = UnitClassify(game.unit(r))

    implicit class BattleClassify(val b : Battle) {
        def enemy = b.opponent(self)
        def allies = b.units(self)
        def foes = b.units(enemy)
        def ownStr = b.strength(self)
        def enemyStr = b.strength(enemy)
    }

    def maxEnemyPower = others./(_.power).max

    def active = others.%(_.active)

    def canSummon(u : UnitClass) = self.gates.%(r => power >= self.summonCost(game, u, r)).any && self.pool(u).any
    def canRitual = !game.acted && power >= game.ritualCost

    def otherOceanGates = others./(_.gates.%(_.glyph == Ocean).any).any

    def instantDeathNow = game.ritualTrack(game.ritualMarker) == 999 || game.factions.%(_.doom >= 30).any
    def instantDeathNext = game.ritualTrack(game.ritualMarker) != 999 && game.ritualTrack(game.ritualMarker + 1) == 999
    def maxDoomGain = self.gates.num + self.all.goos.num * 3
    def aprxDoomGain = self.gates.num + self.all.goos.num * 1.666

    def willActBeforeFaction(current : Faction, f : Faction) : Boolean = {
        if (power == 0)
            return false

        if (current == self)
            return power > 1 && f.power == 0

        if (current == f)
            return !f.allSB

        val factions = (game.order ++ game.order).dropWhile(_ != current)

        return factions.indexOf(f) > factions.indexOf(self)
    }

    /** Check if  WW could position lone cultist with no protector at opposite pole within specified turns */
    def wwLoneCultistPolarGate(turns: Int): Boolean = {
        if (WW.exists && WW.needs(OppositeGate) && WW.active) {
            val oppPole = game.board.starting(WW).but(game.starting(WW)).head
            val cd = oppPole.distanceTo(WW, Cultist)
            val pd = min(oppPole.distanceTo(WW, Monster), oppPole.distanceTo(WW, GOO)) // Should consider Terror here as well?
            !oppPole.riskyForCultists(WW) && cd <= turns && WW.power >= cd && pd > cd
       }
        else
            false
    }

    def ofinale(f : Faction) = (3 * f.doom + 6 * f.gates.num + 5 * (f.es + (f match {
        case GC =>
            var p = f.power

            if (f.has(Cthulhu))
                p += 4

            p / 4

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

        case SL =>
            var p = f.power

            if (f.has(Tsathoggua))
                p += 8

            if (p < 8)
                0
            else
            if (p < 12)
                1
            else
            if (p < 16)
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
            var p = f.power

            if (f.has(YogSothoth))
                p += 6

            if (p < 6)
                0
            else
                1

        case AN =>
            var p = f.power

            if (p < 6)
                0
            else
                1

    }))) >= 30 * 3

    def eval(a : Action) : List[Evaluation]
}
