package cws

import hrf.colmat._

object Explode {

    def isMore(a : Action) = a match {
        case a : More => true
        case _ => false
    }

    def isCancel(a : Action) = a match {
        case a : Cancel => true
        case _ => false
    }

    def isSoft(a : Action) = a match {
        case a : Soft => true
        case _ => false
    }

    def isRecorded(a : Action) = a match {
        case a : More => false
        case a : Cancel => false
        case a : Soft => false
        case _ => true
    }

    def explode(game : Game, actions : List[Action]) : List[Action] = {
        var result : List[Action] = Nil

        def process(actions : List[Action]) {
            var aa = actions.%(isMore).some.|(actions)

            aa = aa.%!(isCancel)

            aa = aa.distinct

            aa.%(isSoft).foreach { a =>
                val (_, c) = game.perform(a)
                c match {
                    case Ask(_, actions) => process(actions)
                    case Force(a) => process(List(a))
                }
            }

            aa.%!(isSoft).foreach { a =>
                result :+= a
            }
        }

        process(actions)

        result
    }

    def isOffense(game : Game, friend : Faction)(a : Action) = a match {
        case MoveAction(self : Faction, _, _, dest) if self != friend && game.of(friend).gates.contains(dest) => true
        case AttackAction(_, _, f) if f == friend => true
        case CaptureAction(_, _, f, _) if f == friend => true
        case AvatarAction(self, _, _, f) if self != friend && f == friend => true
        case ThousandFormsAskAction(f, _, _, _, _, _, power) if f == friend && power > 0 => true
        case DreamsAction(_, _, f) if f == friend => true
        case UnsubmergeAction(self, r) if self != friend && game.of(friend).gates.contains(r) => true
        case HWINTBNAction(self, _, r) if self != friend && game.of(friend).gates.contains(r) => true
        case ShriekAction(self, r) if self != friend && game.of(friend).gates.contains(r) => true

        case BuildGateAction(self, r) if game.turn == 1 => false
        case BuildGateAction(GC, r) if game.starting(GC) == r => false
        case BuildGateAction(WW, r) if game.board.starting(WW).contains(r) => false
        case BuildGateAction(CC, r) if game.of(CC).all(Nyarlathotep).none => false
        case BuildGateAction(YS, r) if game.of(YS).all(Hastur).none => false
        // Implement something for AN here? It's used for survival mode.
        case BuildGateAction(_, r) => true

        case _ => false
    }

}
