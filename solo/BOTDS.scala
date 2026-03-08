package cws

import hrf.colmat._

object BotDS extends BotX(implicit g => new GameEvaluationDS)

class GameEvaluationDS(implicit game : Game) extends GameEvaluation(DS)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        // How isolated a region is from all enemy units (higher = more remote)
        def distanceFromEnemies(r : Region) : Int =
            others./~(_.units.onMap./(_.region)).distinct./(e => game.board.distance(r, e)).minOr(999)

        // True if any active enemy GOO is adjacent to or in this region
        def enemyGooThreat(r : Region) : Boolean =
            (r.foes.goos ++ r.near./~(_.foes.goos)).%(_.faction.power > 0).any

        // True if DS has only the lone keeper acolyte in r (no monsters, no GOOs)
        def loneKeeperIn(r : Region) : Boolean =
            DS.at(r).%(_.canControlGate).num == 1 && DS.at(r).monsterly.none && DS.at(r).goos.none

        // How many factions (including DS) are present in r
        def factionsPresent(r : Region) : Int =
            game.factions.%(_.at(r).any).num

        // Turn 1: DS should use all 6 Psychosis before building gates or summoning
        val turn1PsychosisFirst = game.turn == 1 && self.pool.cultists.any

        a match {

            // ---- FIRST PLAYER / ORDER ----
            case FirstPlayerAction(_, f) =>
                f == self && allSB                          |=> 100  -> "play first all SB"
                f == self && others.%(ofinale).any          |=> 200  -> "play first someone near win"
                f == self                                   |=> -50  -> "stall"
                (game.factions.indexOf(f) - game.factions.indexOf(self)).abs == 2 |=> 10 -> "stall opposite"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            // ---- SPELLBOOK SELECTION ----
            // 1. Chaos Gate — cheap gates + Psychosis platform
            // 2. Consummation — reuse ChaosGateSB (and UndirectedEnergy) for more actions
            // 3. AnimateMatter, Traitors — gate disruption
            // 4. FiendishGrowth, UndirectedEnergy — situational, less often decisive
            case SpellbookAction(_, sb, _) => sb match {
                case ChaosGateSB      => true |=> 1000 -> "chaos gate first"
                case Consummation     => true |=> 800  -> "consummation reuse chaos gate"
                case UndirectedEnergy => true |=> 800  -> "undirected energy power engine"
                case AnimateMatter    =>
                    // Only worth selecting if a safe enemy gate destination exists — else prefer Traitors for ES
                    val safeEnemyGateReachable = DS.chaosGateRegions.%(cgr =>
                        cgr.near.%(n => n.enemyGate && !DS.chaosGateRegions.has(n) && !n.foes.goos.active.any && !enemyGooThreat(n)).any
                    ).any
                    safeEnemyGateReachable  |=> 500 -> "animate matter safe enemy gate"
                    !safeEnemyGateReachable |=> 100 -> "animate matter no safe target"
                case Traitors         => true |=> 350  -> "traitors convert chaos gate"
                case FiendishGrowth   => true |=> 100  -> "fiendish growth often last"
                case _                => true |=> 50   -> "other"
            }

            // ---- RITUAL ----
            // DS with all 3 avatars has allSB — at that point every ritual is good because
            // Elder Signs are hard to earn otherwise (mainly Traitors). 2 gates = 6 power for
            // 2 doom + 3 ES is excellent; even 1 gate pays off in the ES engine.
            case RitualAction(_, cost, _) =>
                instantDeathNow                                                               |=> 10000 -> "instant death now"
                instantDeathNext && allSB && others.all(!_.allSB)                            |=> 10000 -> "ritual if ID next and all SB"
                instantDeathNext && !allSB && others.%(_.allSB).any                          |=> -1000 -> "dont ritual if ID next and not all SB"
                allSB && realDoom + maxDoomGain >= 30                                        |=> 1100  -> "can break 30 all SB"
                !allSB && self.doom + self.gates.num >= 30                                   |=> -5000 -> "will break 30 but not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom + maxDoomGain >= 29    |=> 700   -> "come near 30"
                // Once allSB, every ritual yields ES — ritual becomes a reliable ES engine
                allSB                                                                        |=> 1200  -> "all SB ritual earns ES"
                allSB && cost * 2 <= power                                                   |=> 400   -> "all SB cheap ritual bonus"
                numSB >= 5 && cost * 2 <= power                                              |=> 800   -> "5 SB cheap ritual"
                numSB >= 2 && aprxDoomGain / cost > 1                                        |=> 600   -> "sweet deal"
                numSB >= 3 && aprxDoomGain / cost > 0.75                                     |=> 400   -> "ok deal"
                !allSB                                                                        |=> -1000 -> "spellbooks first"
                true                                                                          |=> -250  -> "dont ritual unless reasons"

            // ---- PASS ----
            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            // ---- POWER/DOOM OFFER: use in turn 1-2; enemies with expensive GOOs will take power ----
            // Always do it turn 1 — getting ChaosGateSB from the requirement this doom phase is critical.
            // Without it, DS has no ChaosGateSB during turn-2 action phase and awakens avatars instead.
            // The CC Thousand Forms concern is outweighed by the 1-turn earlier chaos gate access.
            case PowerDoomOfferAction(_) =>
                game.turn == 1 |=> 3000 -> "power doom offer turn 1 unlock chaos gate sb"
                game.turn == 2 |=> 1500 -> "power doom offer turn 2 ok"
                game.turn >= 3 |=> -500 -> "power doom offer late less value"

            // ---- DOOM DONE ----
            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            // ---- MOVE DONE ----
            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"

            // ---- PSYCHOSIS: place acolyte in empty region, prefer remote ----
            // Base 900 beats EndTurnAction (500). Strong bonus when chaos gate is available but blocked
            // because no non-gate acolyte exists: +2500 pushes total to 3400, beating larva summon (~3200)
            // so DS spreads first and enables ChaosGateSBAction on the very next evaluation.
            case PsychosisPlaceAction(_, r) =>
                val hasFreeAcolyteForChaosGate = areas.nex.%(re => game.gates.has(re).not).%(re => self.at(re).%(_.canControlGate).any).any
                true                                                                                                             |=> 900  -> "psychosis place"
                true                                                                                                             |=> distanceFromEnemies(r) -> "remote"
                r.near.%(_.foes.any).any                                                                                         |=> -300 -> "enemy nearby"
                r.foes.any                                                                                                       |=> -500 -> "enemy present"
                // Retake an abandoned/free gate immediately via Psychosis — beats larva summon and most other actions
                r.freeGate                                                                                                       |=> 4500 -> "psychosis retake abandoned gate"
                // Extra urgency: spreading here is the only way to unlock chaos gate placement
                have(ChaosGateSB) && DS.chaosGateRegions.num < 3 && !hasFreeAcolyteForChaosGate                                 |=> 2500 -> "spread unlocks chaos gate"
                // In turn 1, bias hard toward spreading out over all 6 spots before anything else
                turn1PsychosisFirst                                                                                              |=> 3000 -> "turn1 psychosis first"

            // ---- CHAOS GATE PLACEMENT: prefer remote; bonus for adjacent to enemy gate ----
            case ChaosGateSBPlaceAction(_, r) =>
                true                                                           |=> 100 -> "place chaos gate"
                true                                                           |=> distanceFromEnemies(r) -> "remote chaos gate"
                r.near.%(_.foes.any).any                                       |=> -200 -> "enemy nearby"
                // Aggressive: adjacent to an enemy gate = can use AnimateMatter to destroy it
                r.near.%(n => n.enemyGate && !DS.chaosGateRegions.has(n)).any  |=> 800 -> "adjacent to enemy gate for animate"
                // Don't place into immediate GOO threat with lone keeper
                enemyGooThreat(r) && loneKeeperIn(r)                           |=> -600 -> "goo threat on lone keeper"

            // ---- ANIMATE MATTER SOURCE: prefer the gate best positioned to reach a good destination ----
            // Destination quality is already judged by AnimateMatterMoveAction scoring;
            // source selection just picks the gate with the best options to move from.
            case AnimateMatterFromAction(_, from) =>
                // Primary: this gate is adjacent to an enemy gate we can destroy
                from.near.%(n => n.enemyGate && !DS.chaosGateRegions.has(n)).any |=> 2000 -> "from gate adjacent to enemy gate"
                // Defensive: move a gate that has a lone keeper under GOO threat
                enemyGooThreat(from) && loneKeeperIn(from)                        |=> 1500 -> "escape goo threat from here"

            // ---- ANIMATE MATTER: almost always used to destroy an enemy gate ----
            case AnimateMatterMoveAction(_, from, to) =>
                // Strong penalty for moving to a non-gate region — almost never correct
                !to.gate                                                         |=> -3000 -> "no reason to move to non-gate"
                // Core use: destroy an enemy-controlled gate
                to.enemyGate                                                    |=> 2000  -> "destroy enemy gate"
                to.enemyGate && to.owner.power == 0                             |=> 1000  -> "destroy gate enemy no power bonus"
                // Defensive exception: escape GOO threat on lone keeper
                enemyGooThreat(from) && loneKeeperIn(from)                     |=> 1500  -> "escape goo threat"
                // Penalise moving into a threatened destination — active GOO kills the keeper outright
                to.foes.goos.active.any                                         |=> -2500 -> "active goo at destination keeper dies"
                enemyGooThreat(to) && !to.foes.goos.active.any                 |=> -1500 -> "goo threat at destination risky"

            // ---- CHAOS GATE SB ACTION: place a chaos gate (costs 1 power, much cheaper than regular gate) ----
            // Scores are set high to guarantee at least 1 chaos gate per action phase.
            // 0 gates: beats Thesis awaken (5100) and larva setup chains (5200) to ensure it always happens.
            // After Traitors: DS traded a gate for an Elder Sign — urgently rebuild.
            case ChaosGateSBAction(_) =>
                DS.chaosGateRegions.num == 0 |=> 7000 -> "place first chaos gate urgent"
                DS.chaosGateRegions.num == 1 |=> 4000 -> "place second chaos gate"
                DS.chaosGateRegions.num == 2 |=> 1500 -> "place third chaos gate"
                // Bonus when Consummation can immediately unflip for a second use this turn
                can(Consummation)                      |=> 600  -> "consummation combo available bonus"
                // After Traitors, DS traded a chaos gate for an Elder Sign — push to rebuild immediately
                self.oncePerTurn.has(Traitors)         |=> 1500 -> "rebuild after traitors"
                turn1PsychosisFirst                    |=> -5000 -> "turn1 psychosis first not chaos gate"

            // ---- CONSUMMATION ACTION: only worth using if there's something good to unflip ----
            // Primary target: ChaosGateSB (place a second chaos gate this turn)
            // Secondary: UndirectedEnergy, but only when Thesis is in a contested area (2+ factions)
            // AnimateMatter/Traitors/FiendishGrowth: situational — covered by their own action scores
            case ConsummationAction(_) =>
                val canUnflip = self.oncePerTurn
                val hasCGTarget = canUnflip.has(ChaosGateSB) && DS.chaosGateRegions.num < 3 &&
                    areas.nex.%(r => game.gates.has(r).not).%(r => self.at(r).%(_.canControlGate).any).any
                val hasEnergyTarget = canUnflip.has(UndirectedEnergy) &&
                    self.all(AvatarThesis).any && factionsPresent(self.all(AvatarThesis).head.region) >= 2
                hasCGTarget                              |=> 900 -> "consummation reuse chaos gate"
                hasEnergyTarget && !hasCGTarget          |=> 500 -> "consummation reuse undirected energy contested"
                !hasCGTarget && !hasEnergyTarget         |=> -200 -> "nothing good to unflip"

            // ---- CONSUMMATION UNFLIP: which spellbook to restore ----
            // UndirectedEnergy drops to the situational tier if Thesis isn't in a contested area
            case ConsummationUnflipAction(_, sb) => sb match {
                case ChaosGateSB      => true |=> 1200 -> "unflip chaos gate primary"
                case UndirectedEnergy =>
                    val thesisContested = self.all(AvatarThesis).any &&
                        factionsPresent(self.all(AvatarThesis).head.region) >= 2
                    thesisContested  |=> 1000 -> "unflip undirected energy thesis contested"
                    !thesisContested |=> 150  -> "unflip undirected energy thesis not contested"
                case AnimateMatter    => true |=> 300  -> "unflip animate matter rare"
                case Traitors         => true |=> 200  -> "unflip traitors rare"
                case FiendishGrowth   => true |=> 100  -> "unflip fiendish growth"
                case _                => true |=> 50   -> "unflip other"
            }

            // ---- COSMIC RULER: prefer sacrificing LarvaThesis (re-awakens for 0); protect AvatarSynthesis ----
            case CosmicRulerSacrificeAction(_, saved, sacrificed) =>
                saved.is(AvatarSynthesis)       |=> 3000 -> "save synthesis cosmic ruler"
                sacrificed.is(LarvaThesis)      |=> 2000 -> "sacrifice larva thesis cheap re-awaken"
                sacrificed.is(LarvaAntithesis)  |=> 800  -> "sacrifice larva antithesis ok"
                sacrificed.is(LarvaSynthesis)   |=> 600  -> "sacrifice larva synthesis ok"
                sacrificed.is(AvatarThesis)     |=> 200  -> "sacrifice thesis ok to protect synthesis"
                sacrificed.is(AvatarAntithesis) |=> -500 -> "avoid sacrificing antithesis"
                sacrificed.is(AvatarSynthesis)  |=> -5000 -> "never sacrifice synthesis"

            // ---- COSMIC RULER DECLINE: score 0 so sacrifice options beat it when the trade is worthwhile ----
            case CosmicRulerDeclineAction(_) =>
                true |=> 0 -> "decline cosmic ruler kill phase"

            case CosmicRulerDeclineNoWayAction(_, _) =>
                true |=> 0 -> "decline cosmic ruler no way"

            // ---- TRAITORS SOURCE GATE: which chaos gate to use Traitors from ----
            // Lone keeper: DS gains an Elder Sign and the acolyte returns to pool for future Psychosis
            // GOO threat: Traitors is a clean defensive exit — convert the threatened gate rather than lose it
            // Animate Matter combo: prefer a gate adjacent to another DS chaos gate so AnimateMatter can
            // immediately reclaim it on the next action (Traitors donates, AnimateMatter destroys)
            case TraitorsChaosGateAction(_, r) =>
                loneKeeperIn(r)      |=> 1500 -> "traitors lone keeper gains elder sign"
                enemyGooThreat(r)    |=> 1000 -> "traitors defensive exit from goo threat"
                // Combo: another DS chaos gate is adjacent — can immediately reclaim with AnimateMatter
                can(AnimateMatter) && DS.chaosGateRegions.%(c => c != r && game.board.connected(c).has(r)).any |=> 800 -> "traitors animate matter combo setup"

            // ---- TRAITORS: which faction to give the gate to ----
            // Avoid giving a gate to factions near winning — extra power income accelerates their ritual.
            // Exception: if AnimateMatter can immediately reclaim the donated gate, the doom risk evaporates.
            case TraitorsFactionAction(_, r, target) =>
                val animateMatterCombo = can(AnimateMatter) &&
                    DS.chaosGateRegions.%(c => c != r && game.board.connected(c).has(r)).any
                target.power == 0                                     |=> 2000 -> "traitors enemy no power"
                target.power == 1                                     |=> 500  -> "traitors enemy low power"
                loneKeeperIn(r)                                       |=> 800  -> "will gain elder sign"
                enemyGooThreat(r)                                     |=> 1200 -> "defensive traitors goo threat"
                target.power >= 5                                     |=> -800 -> "traitors gives too much power"
                target.aprxDoom >= 25 && !animateMatterCombo          |=> -1500 -> "traitors near-win enemy dangerous"
                target.aprxDoom >= 20 && !animateMatterCombo          |=> -500  -> "traitors high doom caution"
                target.aprxDoom >= 20 && animateMatterCombo           |=> 600   -> "traitors animate matter reclaim cancels doom risk"

            // ---- AVATAR THESIS AWAKENING COST: prefer low track ----
            // Thesis is the most expendable GOO (re-awakens for 0 via Cosmic Ruler), so never overpay.
            // Against combat-heavy factions (GC/CC/WW) keep it cheap; against lighter lineups track 3-4 is ok.
            case DSAvatarThesisCostAction(_, r, track) =>
                val combatFactions = others.%(f => f == GC || f == BG || f == CC || f == WW || f == SL).any
                track == 0                          |=> 2000 -> "track 0 best free"
                track == 1                          |=> 1500 -> "track 1 good"
                track == 2                          |=> 1000 -> "track 2 ok"
                track == 3 && !combatFactions       |=> 600  -> "track 3 ok no combat factions"
                track == 3 && combatFactions        |=> 200  -> "track 3 cautious combat factions present"
                track == 4 && !combatFactions       |=> 300  -> "track 4 acceptable no combat factions"
                track == 4 && combatFactions        |=> -300 -> "track 4 avoid combat factions present"
                track >= 5                          |=> -1000 -> "track too high never"
                // Prefer awakening where thesis can capture an enemy gate next action
                val nearCapture = r.near.%(n => n.enemyGate && n.foes.goos.none && n.foes.%(_.canControlGate).num == 1)
                nearCapture.any                                                        |=> 1500 -> "thesis near capture target"
                nearCapture.any && can(UndirectedEnergy) && factionsPresent(r) >= 2   |=> 600  -> "undirected energy synergy at awakening region"

            // ---- AVATAR THESIS POWER DISTRIBUTION ----
            // Prefer giving +2 to the faction in last place (lowest doom) — not a threat.
            // Choose all-1 when enemies are low on power and can't do anything useful with a +2 spike.
            case DSAvatarThesisOnePowerAction(_, target) =>
                target.aprxDoom == others./(_.aprxDoom).min  |=> 800  -> "give 2 to last place"
                target.aprxDoom == others./(_.aprxDoom).max  |=> -500 -> "dont boost doom leader"

            case DSAvatarThesisAllPowerAction(_) =>
                // Prefer all-1 when everyone is low-power (nothing dangerous to enable with +2)
                others./(_.power).max <= 3                   |=> 600  -> "all low power all-1 safe"
                others./(_.power).max <= others./(_.power).min + 2 |=> 300 -> "powers even all-1 ok"

            // ---- AVATAR ANTITHESIS CHOICE ----
            // Give ES to last place (lowest doom) — keeps threats in check without accelerating leaders.
            // Choose all-doom when it pushes the endgame forward (someone already near winning).
            case DSAvatarAntithesisOneESAction(_, target) =>
                target.aprxDoom == others./(_.aprxDoom).min  |=> 800  -> "es to last place"
                target.aprxDoom == others./(_.aprxDoom).max  |=> -500 -> "dont es doom leader"

            case DSAvatarAntithesisAllDoomAction(_) =>
                others./(_.aprxDoom).max >= 20 |=> 800  -> "all doom accelerates endgame"
                others./(_.aprxDoom).max < 15  |=> -200 -> "all doom too early"

            // ---- AZATHOTH BIDDING: DS never bids — only enemies offer power/doom ----
            // These actions are asked to enemy factions, not DS, so DS should never reach this code.
            // Included as safety stubs only.

            // ---- UNDIRECTED ENERGY: fires at AvatarThesis region, nets (factions-1) power ----
            // Viable power-maximizing alternative to ChaosGate combo when thesis is in contested region
            case UndirectedEnergyAction(_) =>
                val r = self.all(AvatarThesis).head.region
                val n = factionsPresent(r)
                n >= 3                 |=> 2000 -> "3 factions undirected energy"
                n == 2                 |=> 1200 -> "2 factions undirected energy gain 2 power"
                n <= 1                 |=> -500 -> "lone region undirected energy bad"
                enemyGooThreat(r)      |=> -800 -> "goo threat at thesis"

            // ---- FIENDISH GROWTH: fires at AvatarAntithesis region, places n units ----
            case FiendishGrowthAction(_) =>
                val r = self.all(AvatarAntithesis).head.region
                val n = factionsPresent(r)
                n >= 3                 |=> 2000 -> "3 factions fiendish growth"
                n == 2                 |=> 800  -> "2 factions fiendish growth"
                n <= 1                 |=> -500 -> "lone region fiendish growth bad"
                enemyGooThreat(r)      |=> -800 -> "goo threat at antithesis"

            // ---- FIENDISH GROWTH: which unit to place ----
            // Region is fixed (AvatarAntithesis's area). Three contexts:
            //   own gate  — defensive: larvae bulk up defense, acolyte fine as extra body
            //   enemy gate — offensive: larvae fight, acolyte critical to control gate after battle/capture
            //   contested area — larvae for combat, acolyte situational
            case FiendishGrowthPlaceUnitAction(_, r, uc, _) =>
                val atOwnGate   = r.ownGate
                val atEnemyGate = r.enemyGate
                val battleImminent = r.foes.any || r.near.%(_.foes.goos.active.any).any
                // Larvae: always strong — they have combat value and protect Antithesis
                (uc == LarvaThesis || uc == LarvaAntithesis || uc == LarvaSynthesis) |=> 1200 -> "larva has combat value"
                // Extra bonus for a larva type still missing from map (progresses OneLarvaEach)
                uc == LarvaThesis     && self.all(LarvaThesis).none     |=> 500 -> "larva thesis missing from map"
                uc == LarvaAntithesis && self.all(LarvaAntithesis).none |=> 500 -> "larva antithesis missing from map"
                uc == LarvaSynthesis  && self.all(LarvaSynthesis).none  |=> 500 -> "larva synthesis missing from map"
                // Acolyte at enemy gate: critical — controls the gate after a battle or capture
                uc == Acolyte && atEnemyGate                      |=> 900  -> "acolyte controls gate after battle"
                // Acolyte at own gate or with imminent battle: useful as an extra body
                uc == Acolyte && (atOwnGate || battleImminent)    |=> 400  -> "acolyte defends gate or shields antithesis"
                // Acolyte with no gate context and no battle: not worth the slot
                uc == Acolyte && !atEnemyGate && !atOwnGate && !battleImminent |=> -300 -> "acolyte no use here"
                // Unawakened avatars: never waste FiendishGrowth slots on them
                uc == AvatarThesis    |=> -1000 -> "dont use fiendish growth slot on avatar"
                uc == AvatarAntithesis|=> -1000 -> "dont use fiendish growth slot on avatar"
                uc == AvatarSynthesis |=> -2000 -> "never fiendish growth synthesis"

            // ---- STANDARD MOVE: acolyte ----
            case MoveAction(_, u, o, d, _) if u.uclass == Acolyte =>
                u.onGate                                                                    |=> -10  -> "on gate"
                // Retaking an abandoned gate (including former DS chaos gates): huge priority.
                // +2 doom +2 power/round next gather — beats larva summon (~3200) and Psychosis spread.
                d.freeGate && d.allies.cultists.none && d.capturers.none                   |=> 5000 -> "move to free gate"
                d.freeGate && d.allies.goos.any && d.foes.goos.active.none                 |=> 5500 -> "move to free gate with goo"
                d.near.%(n => n.enemyGate && n.foes.%(_.canControlGate).num == 1 && n.foes.goos.none).any |=> 300 -> "near capture target"
                // Without any gates DS gets no power income and can't summon — moving is rarely worth it
                self.gates.none                                                             |=> -2000 -> "no gates dont just move"

            // ---- STANDARD MOVE: Avatar Thesis (acts like King in Yellow) ----
            // Only reward moving toward a GATE capture — chasing lone non-gate cultists wastes power
            case MoveAction(_, u, o, d, _) if u.uclass == AvatarThesis =>
                d.enemyGate && d.foes.%(_.canControlGate).any && d.foes.goos.none           |=> 800  -> "thesis capture"
                d.enemyGate && d.foes.%(_.canControlGate).num == 1 && d.foes.goos.none      |=> 1200 -> "thesis take gate"
                d.ownGate                                                                    |=> 200  -> "thesis at own gate"
                // Flee: active enemy GOO in origin and enemy significantly outpowers DS there
                o.foes.goos.active.any && active.%(f => f.strength(f.at(o), self) > o.allies.num * 2).any |=> 2000 -> "thesis flee active goo"
                d.foes.goos.active.none && d.allies.any                                     |=> 300  -> "thesis flee to safer region"

            // ---- STANDARD MOVE: Avatar Antithesis ----
            case MoveAction(_, u, o, d, _) if u.uclass == AvatarAntithesis =>
                d.allies.num >= 2                                                            |=> 500  -> "antithesis with allies"
                d.ownGate                                                                    |=> 200  -> "antithesis at gate"
                d.foes.goos.any                                                              |=> -2000 -> "antithesis dont move into goo"
                // Flee: antithesis is expensive to re-awaken, get it out of danger
                o.foes.goos.active.any && active.%(f => f.strength(f.at(o), self) > o.allies.num * 2).any |=> 2500 -> "antithesis flee active goo"
                d.foes.goos.active.none && d.allies.any                                     |=> 400  -> "antithesis flee to safer region"

            // ---- STANDARD MOVE: Avatar Synthesis — keep it heavily defended ----
            case MoveAction(_, u, o, d, _) if u.uclass == AvatarSynthesis =>
                d.allies.num >= 3               |=> 1500 -> "synthesis in big group"
                d.allies.num >= 2               |=> 800  -> "synthesis with allies"
                d.allies.num == 0               |=> -1500 -> "synthesis alone dangerous"
                d.foes.goos.any                 |=> -3000 -> "synthesis dont move into goo"
                enemyGooThreat(d)               |=> -1000 -> "synthesis avoid goo threat"
                d.ownGate                       |=> 300  -> "synthesis at gate"
                // Flee: synthesis is the most critical GOO — highest priority to escape
                o.foes.goos.active.any && active.%(f => f.strength(f.at(o), self) > o.allies.num * 2).any |=> 3500 -> "synthesis flee active goo"
                d.foes.goos.active.none && d.allies.num >= 2                                |=> 500  -> "synthesis flee to safe group"

            // ---- STANDARD MOVE: Larvae — expendable, no need to flee GOOs ----
            // Larvae are cheap to re-summon so we never waste a move action just to flee.
            // Only move a larva if it has a genuinely useful destination.
            case MoveAction(_, u, o, d, _) if u.uclass == LarvaThesis || u.uclass == LarvaAntithesis || u.uclass == LarvaSynthesis =>
                d.allies.goos.any               |=> 300  -> "larva near goo"
                d.ownGate                       |=> 200  -> "larva at gate"
                // LarvaThesis specifically: position adjacent to an unguarded enemy gate
                // so AvatarThesis can awaken there and capture next action
                u.uclass == LarvaThesis && self.all(AvatarThesis).none &&
                    d.near.%(n => n.enemyGate && n.foes.goos.none && n.foes.%(_.canControlGate).num == 1).any |=> 1000 -> "larva thesis adjacent to capture target setup"

            // ---- BUILD GATE ----
            case BuildGateAction(_, r) =>
                true                                   |=> distanceFromEnemies(r) -> "build gate remote"
                // Without a gate DS gets no power income and can't summon larvae — urgently needed
                self.gates.none                        |=> 3000  -> "desperately need first gate"
                self.gates.num == 1                    |=> 1200  -> "need second gate"
                self.gates.num == 2                    |=> 600   -> "need third gate"
                r.allies.goos.any                      |=> 300   -> "build gate with goo"
                r.capturers.%(_.power > 0).any         |=> -1000 -> "build gate risky"
                // Chaos gates cost 1 power vs 3 for regular gates — strongly prefer them whenever available
                // Use self.has (not self.can) so penalty applies even after ChaosGateSB is flipped this turn
                self.has(ChaosGateSB) && DS.chaosGateRegions.num < 3 |=> -4000 -> "chaos gate available strongly prefer it"
                // Don't build a gate in turn 1 while pool still has cultists to Psychosis out
                turn1PsychosisFirst                    |=> -5000 -> "turn1 psychosis first not gate"

            // ---- RECRUIT ----
            case RecruitAction(_, Acolyte, r) =>
                r.freeGate                             |=> 5000 -> "recruit to free gate"
                r.freeGate && r.allies.goos.any        |=> 5700 -> "recruit to free gate with goo"
                r.capturers.%(_.power > 0).any         |=> -2000 -> "dont recruit to be captured"

            // ---- SUMMON LARVAE ----
            // Priority: get one of each type first (satisfies OneLarvaEach + enables all awakenings)
            case SummonAction(_, LarvaThesis, r) =>
                need(OneLarvaEach)                                                          |=> 1500 -> "need larva for sb"
                // Strong bonus when this type is completely missing from map
                self.all(LarvaThesis).none                                                  |=> 1500 -> "no thesis on map"
                // Penalize summoning more if another type is still missing
                self.all(LarvaThesis).any && self.all(LarvaAntithesis).none                 |=> -2000 -> "have thesis need antithesis first"
                self.all(LarvaThesis).any && self.all(LarvaSynthesis).none                  |=> -2000 -> "have thesis need synthesis first"
                self.gates.any                                                              |=> 800  -> "have gate can summon"
                r.allies.goos.any                                                           |=> 300  -> "summon near goo"
                r.ownGate                                                                   |=> 200  -> "summon at gate"
                // LarvaThesis on map = free re-awaken option after Cosmic Ruler sacrifice
                self.all(AvatarThesis).any                                                  |=> 400  -> "larva thesis on map re-awaken option"
                // Core DS strategy setup: summon LarvaThesis adjacent to an unguarded enemy gate so Thesis
                // can awaken there and immediately capture it (enemy took a turn to awaken their own GOO)
                self.all(AvatarThesis).none && r.near.%(n => n.enemyGate && n.foes.goos.none && n.foes.%(_.canControlGate).num == 1).any |=> 1000 -> "larva thesis adjacent to capture target setup"
                // Don't summon a second LarvaThesis unless the existing one is under GOO threat
                // (spare is only needed if the on-map copy could be eliminated)
                self.all(LarvaThesis).any && self.all(LarvaThesis).%(u => !enemyGooThreat(u.region)).any |=> -2000 -> "one safe larva thesis is enough"
                turn1PsychosisFirst                                                         |=> -5000 -> "turn1 psychosis first not larva"

            case SummonAction(_, LarvaAntithesis, r) =>
                need(OneLarvaEach)                                                          |=> 1500 -> "need larva for sb"
                self.all(LarvaAntithesis).none                                              |=> 1500 -> "no antithesis on map"
                self.all(LarvaAntithesis).any && self.all(LarvaThesis).none                 |=> -2000 -> "have antithesis need thesis first"
                self.all(LarvaAntithesis).any && self.all(LarvaSynthesis).none              |=> -2000 -> "have antithesis need synthesis first"
                r.allies.goos.any                                                           |=> 300  -> "summon near goo"
                r.ownGate                                                                   |=> 200  -> "summon at gate"

            case SummonAction(_, LarvaSynthesis, r) =>
                need(OneLarvaEach)                                                          |=> 1500 -> "need larva for sb"
                self.all(LarvaSynthesis).none                                               |=> 1500 -> "no synthesis on map"
                self.all(LarvaSynthesis).any && self.all(LarvaThesis).none                  |=> -2000 -> "have synthesis need thesis first"
                self.all(LarvaSynthesis).any && self.all(LarvaAntithesis).none              |=> -2000 -> "have synthesis need antithesis first"
                r.allies.goos.any                                                           |=> 300  -> "summon near goo"
                r.ownGate                                                                   |=> 200  -> "summon at gate"

            // ---- AWAKEN GOOs ----
            case AwakenAction(_, AvatarThesis, r, _) =>
                // Core DS turn-2 strategy: awaken adjacent to an unprotected enemy gate, capture it next action,
                // and optionally use UndirectedEnergy in the same location to net power.
                val nearCaptureTarget = r.near.%(n => n.enemyGate && n.foes.goos.none && n.foes.%(_.canControlGate).num == 1)
                need(AwakenAvatarThesis)                                            |=> 3000 -> "need thesis"
                r.allies(LarvaThesis).any                                           |=> 200  -> "larva thesis present"
                nearCaptureTarget.any                                               |=> 1500 -> "near capture target"
                // Extra bonus when UndirectedEnergy is available to exploit the same contested area
                nearCaptureTarget.any && can(UndirectedEnergy) && factionsPresent(r) >= 2 |=> 600 -> "undirected energy synergy at awakening region"

            case AwakenAction(_, AvatarAntithesis, r, _) =>
                need(AwakenAvatarAntithesis) && !need(AwakenAvatarThesis) |=> 3000 -> "need antithesis"
                r.allies(LarvaAntithesis).any                             |=> 200  -> "larva antithesis present"
                // Hold off on Antithesis if chaos gates haven't been established — ChaosGateSBAction (7000)
                // should already beat this, but guard in case generation is blocked by missing non-gate acolyte
                have(ChaosGateSB) && DS.chaosGateRegions.num == 0        |=> -1500 -> "chaos gate before antithesis"
                // Prefer protected regions — Antithesis is expensive to re-awaken
                r.allies.num >= 2             |=> 600  -> "antithesis with allies"
                r.ownGate                     |=> 300  -> "antithesis at own gate"
                r.foes.goos.active.any        |=> -1500 -> "antithesis dont awaken into active goo"
                enemyGooThreat(r)             |=> -800  -> "antithesis avoid goo threat region"

            case AwakenAction(_, AvatarSynthesis, r, _) =>
                need(AwakenAvatarSynthesis) && !need(AwakenAvatarAntithesis) |=> 3000 -> "need synthesis"
                power >= 14                                                   |=> 500  -> "high power synthesis"
                power < 8                                                     |=> -2000 -> "too low power for synthesis"
                r.allies(LarvaSynthesis).any                                  |=> 200  -> "larva synthesis present"
                // Synthesis is the MVP — strongly prefer heavily defended safe regions
                r.allies.num >= 3             |=> 1200 -> "synthesis big group"
                r.allies.num >= 2             |=> 600  -> "synthesis with allies"
                r.ownGate                     |=> 300  -> "synthesis at own gate"
                r.allies.num == 0             |=> -1000 -> "synthesis alone risky"
                r.foes.goos.active.any        |=> -2500 -> "synthesis never awaken into active goo"
                enemyGooThreat(r)             |=> -1500 -> "synthesis avoid goo threat region"

            // ---- ATTACK ----
            case AttackAction(_, r, f, _) if f.neutral =>
                true |=> -100000 -> "dont attack neutrals"

            case AttackAction(_, r, f, _) =>
                val allies = self.at(r)
                val foes   = f.at(r)
                val ownStr = self.strength(allies, f)
                val foeStr = f.strength(foes, self)
                // Thesis capture: fight lone cultist on gate when winning
                allies.goos.any && foes.%(_.canControlGate).any && foes.goos.none && ownStr > foeStr |=> 1500 -> "thesis capture attack"
                foes.goos.any && ownStr < foeStr                                                       |=> -2000 -> "dont fight goo outpowered"
                // Sacrifice a lone acolyte (no DS GOOs) to return it to pool for Psychosis
                // Only worthwhile when pool is empty so Psychosis has nothing to place
                self.pool.cultists.none && can(Psychosis) &&
                    allies.%(_.canControlGate).num == 1 && allies.goos.none &&
                    !allies.head.gateKeeper && foes.any                        |=> 600 -> "sacrifice acolyte for psychosis pool"

            // ---- CAPTURE ----
            // DS captures to gain gates, not to harass. Non-gate captures waste 2 power (move + capture)
            // for no strategic return, while DS should instead be building chaos gates.
            case CaptureAction(_, r, f, _) =>
                r.enemyGate && f == r.owner && r.controllers.num == 1              |=> 3000 -> "capture and open gate"
                r.enemyGate && f == r.owner && r.controllers.num == 1 && f.power > 0 |=> 4000 -> "capture open gate enemy has power"
                true                                                               |=> 2000 -> "capture"
                r.enemyGate.not                                                    |=> -3000 -> "capture without gate not strategic"

            // ---- GATE CONTROL ----
            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "never abandon"

            case ControlGateAction(_, r, u, _) =>
                r.allies.%(_.onGate).foreach { c =>
                    c.uclass == u.uclass |=> -1000000 -> "remain calm"
                }
                true |=> 1000000 -> "always control"

            case EndTurnAction(_) =>
                true |=> 500 -> "main done"

            case NextPlayerAction(_) =>
                true |=> 0 -> "cancel"

            // ---- THOUSAND FORMS NEGOTIATION: same probabilistic decay as all other factions ----
            // Higher offers score progressively lower max values; never overpay past total demand
            case ThousandFormsAskAction(f, r, offers, _, _, _, p) =>
                r < p + offers./(_.n).sum |=> -6*6*6*6*6*6 -> "dont overpay"
                p == -1 && power >= f.power + r && !f.allSB |=> (4*4*4*4*4*4 * Math.random()).round.toInt -> "refuse pay"
                p == 0 |=> (3*3*3*3*3*3 * Math.random()).round.toInt -> "pay 0"
                p == 1 |=> (2*3*3*3*3*3 * Math.random()).round.toInt -> "pay 1"
                p == 2 |=> (2*2*3*3*3*3 * Math.random()).round.toInt -> "pay 2"
                p == 3 |=> (2*2*2*3*3*3 * Math.random()).round.toInt -> "pay 3"
                p == 4 |=> (2*2*2*2*3*3 * Math.random()).round.toInt -> "pay 4"
                p == 5 |=> (2*2*2*2*2*3 * Math.random()).round.toInt -> "pay 5"

            // ---- GHROTH NEGOTIATION: DS always wants to offer exactly 1 acolyte ----
            // Losing an acolyte fills the pool, enabling future Psychosis placements (free spread + slow power spending)
            case GhrothAskAction(_, x, offers, _, _, _, n) =>
                x < n + offers./(_.n).sum |=> -6*6*6*6*6*6  -> "dont overpay ghroth"
                n == 1                    |=> 5000           -> "offer 1 acolyte fills pool for psychosis"
                n == 2                    |=> 3000           -> "offer 2 acolytes also fine"
                n == 0                    |=> -1000          -> "offering nothing wastes ghroth benefit"
                n == -1                   |=> -2000          -> "refusing still risks full penalty"
                n >= 3                    |=> -3000          -> "offering 3+ too costly"

            // ---- GHROTH TARGET: which unit DS sacrifices ----
            // Prefer an acolyte (returns to pool for Psychosis); protect gate-keepers and larvae
            case GhrothTargetAction(_, u, _, _) =>
                u.is(Acolyte)        |=> 3000  -> "acolyte to pool for psychosis"
                u.gateKeeper         |=> -3000 -> "dont lose gate keeper"
                u.is(LarvaThesis)    |=> -5000 -> "dont sacrifice larva thesis"
                u.is(LarvaAntithesis)|=> -5000 -> "dont sacrifice larva antithesis"
                u.is(LarvaSynthesis) |=> -5000 -> "dont sacrifice larva synthesis"
                u.is(AvatarThesis)   |=> -8000 -> "dont sacrifice avatar thesis"
                u.is(AvatarAntithesis)|=> -8000 -> "dont sacrifice avatar antithesis"
                u.is(AvatarSynthesis) |=> -9000 -> "never sacrifice avatar synthesis"

            case _ if game.battle.none =>
                // fall through to battle section

            case _ =>
                true |=> 1000000 -> "todo"
        }

        // ---- BATTLE ----
        if (game.battle.any) {
            if (game.battle./~(_.sides).has(self).not) {
                a match {
                    case _ => true |=> 1000 -> "todo"
                }
            }
            else {
                implicit val battle = game.battle.get

                val allies  = self.forces
                val enemies = self.opponent.forces

                def elim(u : UnitFigure) {
                    u.is(Acolyte)          |=> 800  -> "elim acolyte"
                    u.is(LarvaThesis)      |=> 600  -> "elim larva thesis"
                    u.is(LarvaAntithesis)  |=> 590  -> "elim larva antithesis"
                    u.is(LarvaSynthesis)   |=> 580  -> "elim larva synthesis"
                    // Thesis re-awakens for 0 power — more expendable than other GOOs
                    u.is(AvatarThesis)     |=> 100  -> "thesis expendable re-awakens free"
                    u.is(AvatarAntithesis) |=> -200 -> "keep antithesis"
                    u.is(AvatarSynthesis)  |=> -500 -> "keep synthesis"
                }

                def retreat(u : UnitFigure) {
                    u.gateKeeper           |=> -1000  -> "dont retreat gate keeper"
                    u.is(Acolyte)          |=> 800    -> "retreat acolyte"
                    u.is(LarvaThesis)      |=> 300    -> "retreat larva"
                    u.is(AvatarThesis)     |=> -5000  -> "dont retreat thesis"
                    u.is(AvatarAntithesis) |=> -6000  -> "dont retreat antithesis"
                    u.is(AvatarSynthesis)  |=> -8000  -> "dont retreat synthesis"
                }

                a match {
                    case AssignKillAction(_, _, _, u) => elim(u)
                    case AssignPainAction(_, _, _, u) => retreat(u)
                    case EliminateNoWayAction(_, u)   => elim(u)

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.goos.any      |=> 2000 -> "retreat cultist to goo"
                        u.cultist && r.allies.monsterly.any |=> 1000 -> "retreat cultist to monsters"
                        u.cultist && r.ownGate              |=> 300  -> "retreat cultist to own gate"
                        u.cultist && r.foes.none && !r.gate |=> 200  -> "retreat to safety"
                        u.cultist && r.freeGate             |=> 1500 -> "retreat to free gate"
                        u.goo && r.allies.num >= 3          |=> 1000 -> "regroup goo"
                        u.goo && r.ownGate                  |=> 400  -> "goo to own gate"
                        true |=> (r.connected ++ r.connected./~(_.connected)).distinct.num -> "reachable"

                    case _ =>
                        true |=> 1000000 -> "todo"
                }
            }
        }

        result.none |=> 0 -> "none"
        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
