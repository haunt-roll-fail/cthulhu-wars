package cws

import hrf.colmat._

import html._


trait GameImplicits {
    type GC = GC.type
    type CC = CC.type
    type BG = BG.type
    type YS = YS.type
    type SL = SL.type
    type WW = WW.type
    type OW = OW.type
    type AN = AN.type

    implicit def factionToState(f : Faction)(implicit game : Game) : Player = f match {
        case f : NeutralFaction => game.neutrals(f)
        case f : Faction => game.players.get(f).|(game.noPlayer)
    }

    def options(implicit game : Game) = game.options
    def factions(implicit game : Game) = game.factions
    def factionlike(implicit game : Game) = game.factionlike
    def areas(implicit game : Game) = game.board.regions


    def log(m : Any*)(implicit game : Game) = game.appendLog(m.$)

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def neutral = f.is[NeutralFaction]
        def real = f.is[NeutralFaction].not
        def enemies = game.factions.but(f)
        def factionGOOs = f.goos.factionGOOs
        def log(m : Any*) = game.appendLog(f +: m.$)
        def any = game.players.contains(f)
    }

    implicit class FactionListEx(l : $[Faction])(implicit game : Game) {
        def real = l.notOf[NeutralFaction]
    }

    implicit class UnitFigureEx(u : UnitFigure) {
        def goo = u.uclass.utype == GOO
        def factionGOO = u.uclass.utype == GOO && u.uclass.is[IGOO].not
        def independentGOO = u.uclass.utype == GOO && u.uclass.is[IGOO]
        def monster = u.uclass.utype == Monster
        def monsterly = u.uclass.utype == Monster || u.uclass.utype == Terror
        def terror = u.uclass.utype == Terror
        def cultist = u.uclass.utype == Cultist
        def inPlay = u.region.glyph.inPlay
        def onMap = u.region.glyph.onMap
    }

    implicit class RegionEx(r : Region) {
        def inPlay = r.glyph.inPlay
        def onMap = r.glyph.onMap
        def connected(implicit game : Game) = game.board.connected(r)
    }

    implicit class RegionListEx(l : $[Region]) {
        def inPlay = l.%(_.glyph.inPlay)
        def onMap = l.%(_.glyph.onMap)
        def nex(implicit game : Game) = game.nexed.some./(x => l.%(x.has)).|(l)
    }

    implicit class UnitFigureGameEx(u : UnitFigure)(implicit game : Game) {
        def canMove = u.uclass.canMove(u)
        def canBeMoved = u.uclass.canBeMoved(u)
        def canCapture = u.uclass.canCapture(u)
        def canBattle = u.uclass.canBattle(u)
        def canControlGate = u.uclass.canControlGate(u) && u.health != Pained
    }

    implicit class UnitFigureListEx(l : $[UnitFigure]) {
        def apply(uc : UnitClass) = l.%(_.uclass == uc)
        def not(uc : UnitClass) = l.%(_.uclass != uc)
        def got(uc : UnitClass) = l.exists(_.uclass == uc)
        def one(uc : UnitClass) = l.%(_.uclass == uc).sortBy(_.onGate).first
        def one(ut : UnitType) = l.%(_.uclass.utype == ut).sortBy(_.onGate).first
        def goos = l.%(_.uclass.utype == GOO)
        def factionGOOs = l.%(u => u.uclass.utype == GOO && u.uclass.is[IGOO].not)
        def independentGOOs = l.%(u => u.uclass.utype == GOO && u.uclass.is[IGOO])
        def cultists = l.%(_.uclass.utype == Cultist)
        def acolytes = l.%(_.uclass == Acolyte)
        def vulnerable = l.%(u => u.uclass.utype == Cultist || u.uclass.utype == Monster)
        def monsters = l.%(_.uclass.utype == Monster)
        def monsterly = l.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror)
        def terrors = l.%(_.uclass.utype == Terror)
        def notGOOs = l.%(_.uclass.utype != GOO)
        def notMonsters = l.%(_.uclass.utype != Monster)
        def notTerrors = l.%(_.uclass.utype != Terror)
        def notCultists = l.%(_.uclass.utype != Cultist)
        def onMap = l.%(_.region.glyph.onMap)
        def inPlay = l.%(_.region.glyph.inPlay)
    }

    implicit class UnitFigureListGameEx(l : $[UnitFigure])(implicit game : Game) {
        def sortA = l.sortWith(game.compareUnitsActive)
        def sortP = l.sortWith(game.compareUnitsPassive)
        def preferablyNotOnGate =
            l.sortWith(game.compareUnitsPassive)
                .useIf(l => l.sameBy(_.region) && l.sameBy(_.uclass) && (l.sameBy(_.onGate) || l./(_.faction).distinct.only.clings))(_.take(1))
        def nex = game.nexed.some./(x => l.%(u => x.has(u.region))).|(l)
        def tag(s : UnitState) = l.%(_.tag(s))
        def not(s : UnitState) = l.%!(_.tag(s))
    }

    implicit class ActionListEx(l : $[Action]) {
        def unwrap = l./(_.unwrap)
    }

    implicit def stringToDesc(s : String) : Game => String = (g : Game) => s
    implicit def regionToDesc(r : Region) : Game => String = (g : Game) => r.toString
    implicit def factionToDesc(f : Faction) : Game => String = (g : Game) => f.full
    implicit def spellbookToDesc(b : Spellbook) : Game => String = (g : Game) => b.elem
    implicit def optionToDesc(n : |[String]) : Game => String = (g : Game) => n.|(null)
    implicit def unitRefShortToDesc(ur : UnitRefShort) : Game => String = (g : Game) => g.unit(ur.r).short
    implicit def unitRefFullToDesc(ur : UnitRefFull) : Game => String = (g : Game) => g.unit(ur.r).full

    implicit def actionToForce(a : ForcedAction) : Continue = Force(a)
    implicit def askWrapperToAsk(w : AskWrapper) : Continue = w.ask

    implicit def unitRefToFigure(ur : UnitRef)(implicit game : Game) : UnitFigure = game.unit(ur)
    implicit def unitRefToFigureEx(ur : UnitRef)(implicit game : Game) : UnitFigureEx = UnitFigureEx(ur)
    implicit def figureToUnitRef(u : UnitFigure)(implicit game : Game) : UnitRef = u.ref
}
