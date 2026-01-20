package cws

import hrf.colmat._

object Explode {
    def explode(game : Game, actions : $[Action]) : $[Action] = {
        var result : $[Action] = $

        def process(actions : $[Action]) {
            val aaa = actions.unwrap
            var aa = aaa.%(_.isMore).some.|(aaa)

            aa = aa.%!(_.isCancel).%!(_.isInfo)

            aa = aa.distinct

            aa.%(_.isSoft).foreach { a =>
                val (_, c) = game.perform(a)
                c match {
                    case Ask(_, actions) => process(actions)
                    case Then(then) => process($(then))
                }
            }

            aa.%!(_.isSoft).foreach { a =>
                result :+= a
            }
        }

        process(actions)

        result
    }

    def isOffense(friend : Faction)(a : Action)(implicit game : Game) = a match {
        case MoveAction(self, _, _, dest, cost) if self != friend && friend.gates.has(dest) => true
        case AttackAction(_, _, f, _) if f == friend => true
        case CaptureAction(_, _, f, _) if f == friend => true
        case AvatarAction(self, _, _, f) if self != friend && f == friend => true
        case ThousandFormsAskAction(f, _, _, _, _, _, power) if f == friend && power > 0 => true
        case DreamsAction(_, _, f) if f == friend => true
        case UnsubmergeAction(self, r) if self != friend && friend.gates.contains(r) => true
        case HWINTBNAction(self, _, r) if self != friend && friend.gates.contains(r) => true
        case ShriekAction(self, r) if self != friend && friend.gates.contains(r) => true

        case BuildGateAction(self, r) if game.turn == 1 => false
        case BuildGateAction(GC, r) if game.starting(GC) == r => false
        case BuildGateAction(WW, r) if game.board.starting(WW).contains(r) => false
        case BuildGateAction(CC, r) if CC.all(Nyarlathotep).none => false
        case BuildGateAction(YS, r) if YS.all(Hastur).none => false
        // Implement something for AN here? It's used for survival mode.
        case BuildGateAction(_, r) => true

        case _ => false
    }

}
