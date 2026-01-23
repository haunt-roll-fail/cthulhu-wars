package cws

import hrf.colmat._


package object html {
    implicit class HtmlString(val s : String) extends AnyVal {
        def spn : String = "<span>" + s + "</span>"
        def inline : String = "<span class='inline-block'>" + s + "</span>"
        def styled(l : String*) : String = "<span class='" + l.mkString(" ")  + "'>" + s + "</span>"
        def styled(f : Faction) : String = s.styled(f.style)
        def power = (s + " Power").styled("power")
        def hl = s.styled("highlight")
        def hh = s.styled("halfhigh")
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
}
