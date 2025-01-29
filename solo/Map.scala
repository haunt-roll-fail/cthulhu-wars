package cws

import hrf.colmat._

object EarthMap4v35 extends Board {
    val name = "Earth Map (4 players 3/5 variant)"

    val ArcticOcean = Region("Arctic Ocean", Ocean)
    val Scandinavia = Region("Scandinavia", GlyphWW)
    val Europe = Region("Europe", GlyphWW)
    val NorthAsia = Region("North Asia", GlyphWW)
    val SouthAsia = Region("South Asia", GlyphWW)
    val Arabia = Region("Arabia", GlyphWW)
    val EastAfrica = Region("East Africa", GlyphAA)
    val WestAfrica = Region("West Africa", GlyphAA)
    val NorthAtlantic = Region("North Atlantic", Ocean)
    val SouthAtlantic = Region("South Atlantic", Ocean)
    val Antarctica = Region("Antarctica", NoGlyph)
    val SouthPacific = Region("South Pacific", Ocean)
    val SouthAmerica = Region("South America", GlyphOO)
    val NorthAmerica = Region("North America", GlyphOO)
    val NorthPacific = Region("North Pacific", Ocean)
    val IndianOcean = Region("Indian Ocean", Ocean)
    val Australia = Region("Australia", GlyphAA)

    val regions = List(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmerica, SouthAmerica, Australia, Antarctica)
    val nonFactionRegions = List(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, SouthAmerica, Australia)
    val west = List(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = List(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => List(NorthAmerica, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => List(NorthPacific, NorthAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => List(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => List(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => List(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => List(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Scandinavia => List(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => List(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => List(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => List(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => List(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => List(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => List(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmerica => List(ArcticOcean, NorthAtlantic, SouthAmerica, NorthPacific)
        case SouthAmerica => List(NorthAmerica, NorthAtlantic, SouthAtlantic, SouthPacific, NorthPacific)
        case Australia => List(SouthPacific, IndianOcean)
        case Antarctica => List(SouthPacific, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case GC => List(SouthPacific)
        case CC => List(SouthAsia)
        case BG => List(WestAfrica)
        case YS => List(Europe)
        case SL => List(NorthAmerica)
        case WW => List(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }
}
