package cws

import hrf.colmat._

object Html {
    implicit class HtmlString(val s : String) extends AnyVal {
        def styled(l : String*) = "<span class='" + l.mkString(" ")  + "'>" + s + "</span>"
        def power = (s + " Power").styled("power")
        def hl = s.styled("highlight")
    }

    implicit class HtmlInt(val n : Int) extends AnyVal {
        def styled(c : String) = n.toString.styled(c)
        def power = n.toString.power
        def doom = ("" + n + " Doom").styled("doom")
        def es = (n == 1).?("an " + "Elder Sign".styled("es")).|(("" + n + " Elder Signs").styled("es"))
        def str = ("["+n+"]").toString.styled("str")
    }

    implicit class HtmlInt2(val n : (Int, Int)) extends AnyVal {
        def range = n match {
            case (a, b) if a == b => a.toString
            case (a, b) => a.toString + "-" + b.toString
        }
        def power = range.power
    }

    implicit class ListUnitFigure(val list : List[UnitFigure]) extends AnyVal {
        def unique = {
            val distinct = list./(u => (u.uclass, u.state, u.health) -> u).toMap.values.toList
            list.%(distinct.contains)
        }
    }
}
