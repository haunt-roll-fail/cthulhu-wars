package cws

import hrf.colmat._

object EarthMap3 extends Board {
    val id = "earth33"
    val name = "Earth Map (3 players)"

    val ArcticOcean = Region("Arctic Ocean", Ocean)
    val Europe = Region("Europe", GlyphWW)
    val Asia = Region("Asia", GlyphWW)
    val Africa = Region("Africa", GlyphAA)
    val NorthAtlantic = Region("North Atlantic", Ocean)
    val SouthAtlantic = Region("South Atlantic", Ocean)
    val Antarctica = Region("Antarctica", NoGlyph)
    val SouthPacific = Region("South Pacific", Ocean)
    val SouthAmerica = Region("South America", GlyphOO)
    val NorthAmerica = Region("North America", GlyphOO)
    val NorthPacific = Region("North Pacific", Ocean)
    val IndianOcean = Region("Indian Ocean", Ocean)
    val Australia = Region("Australia", GlyphAA)

    val regions = List(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Europe, Asia, Africa, NorthAmerica, SouthAmerica, Australia, Antarctica)
    val nonFactionRegions = List(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthAmerica, Australia)
    val west = List(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = List(Europe, Asia, Africa, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => List(NorthAmerica, NorthAtlantic, Europe, Asia, NorthPacific)
        case NorthAtlantic => List(NorthPacific, NorthAmerica, ArcticOcean, Europe, Asia, Africa, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => List(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, Africa, IndianOcean)
        case NorthPacific => List(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, Asia)
        case IndianOcean => List(Antarctica, SouthAtlantic, Africa, Asia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => List(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Europe => List(ArcticOcean, NorthAtlantic, Asia)
        case Asia => List(ArcticOcean, NorthAtlantic, NorthPacific, Europe, Africa)
        case Africa => List(NorthAtlantic, Asia, SouthAtlantic, IndianOcean)
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
        case CC => List(Asia)
        case BG => List(Africa)
        case YS => List(Europe)
        case SL => List(NorthAmerica)
        case WW => List(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r: Region): (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Europe => (1110, 255)
        case Asia => (1540, 255)
        case Africa => (1145, 525)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (950, 820)
        case SouthPacific => (540, 830)
        case SouthAmerica => (590, 680)
        case NorthAmerica => (380, 290)
        case NorthPacific => (105, 355)
        case IndianOcean => (1568, 660)
        case Australia => (215, 707)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap4v35 extends Board {
    val id = "earth35"
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

    def gateXYO(r: Region): (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (950, 820)
        case SouthPacific => (540, 830)
        case SouthAmerica => (590, 680)
        case NorthAmerica => (380, 290)
        case NorthPacific => (105, 355)
        case IndianOcean => (1568, 660)
        case Australia => (215, 707)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap4v53 extends Board {
    val id = "earth53"
    val name = "Earth Map (4 players 5/3 variant)"

    val ArcticOcean = Region("Arctic Ocean", Ocean)
    val Europe = Region("Europe", GlyphWW)
    val Asia = Region("Asia", GlyphWW)
    val Africa = Region("Africa", GlyphAA)
    val NorthAtlantic = Region("North Atlantic", Ocean)
    val SouthAtlantic = Region("South Atlantic", Ocean)
    val Antarctica = Region("Antarctica", NoGlyph)
    val SouthPacific = Region("South Pacific", Ocean)
    val SouthAmericaWest = Region("South America West", GlyphOO)
    val SouthAmericaEast = Region("South America East", GlyphOO)
    val NorthAmericaWest = Region("North America West", GlyphOO)
    val NorthAmericaEast = Region("North America East", GlyphOO)
    val CentralAmerica = Region("Central America", GlyphOO)
    val NorthPacific = Region("North Pacific", Ocean)
    val IndianOcean = Region("Indian Ocean", Ocean)
    val Australia = Region("Australia", GlyphAA)
    val NewZealand = Region("New Zealand", GlyphAA)

    val regions = List(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Europe, Asia, Africa, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val nonFactionRegions = List(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand)
    val west = List(ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = List(Europe, Asia, Africa, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => List(NorthAmericaWest, NorthAmericaEast, NorthAtlantic, Europe, Asia, NorthPacific)
        case NorthAtlantic => List(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Europe, Asia, Africa, SouthAtlantic, SouthAmericaEast)
        case SouthAtlantic => List(Antarctica, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, Africa, IndianOcean)
        case NorthPacific => List(ArcticOcean, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, Asia)
        case IndianOcean => List(Antarctica, SouthAtlantic, Africa, Asia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => List(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Europe => List(ArcticOcean, NorthAtlantic, Asia)
        case Asia => List(ArcticOcean, NorthAtlantic, NorthPacific, Europe, Africa)
        case Africa => List(NorthAtlantic, Asia, SouthAtlantic, IndianOcean)
        case NorthAmericaWest => List(ArcticOcean, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => List(ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => List(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => List(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => List(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => List(IndianOcean, NewZealand)
        case NewZealand => List(IndianOcean, SouthPacific, Australia)
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
        case CC => List(Asia)
        case BG => List(Africa)
        case YS => List(Europe)
        case SL => List(NorthAmericaWest)
        case WW => List(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r: Region): (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Europe => (1110, 255)
        case Asia => (1540, 255)
        case Africa => (1145, 525)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (950, 820)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670,615)
        case CentralAmerica => (305,410)
        case NorthAmericaWest => (405, 225)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1568, 660)
        case Australia => (125, 685)
        case NewZealand => (265,710)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap5 extends Board {
    val id = "earth55"
    val name = "Earth Map (5 players)"

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
    val SouthAmericaWest = Region("South America West", GlyphOO)
    val SouthAmericaEast = Region("South America East", GlyphOO)
    val NorthAmericaWest = Region("North America West", GlyphOO)
    val NorthAmericaEast = Region("North America East", GlyphOO)
    val CentralAmerica = Region("Central America", GlyphOO)
    val NorthPacific = Region("North Pacific", Ocean)
    val IndianOcean = Region("Indian Ocean", Ocean)
    val Australia = Region("Australia", GlyphAA)
    val NewZealand = Region("New Zealand", GlyphAA)

    val regions = List(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val nonFactionRegions = List(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand)
    val west = List(ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = List(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => List(NorthAmericaWest, NorthAmericaEast, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => List(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmericaEast)
        case SouthAtlantic => List(Antarctica, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => List(ArcticOcean, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => List(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => List(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Scandinavia => List(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => List(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => List(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => List(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => List(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => List(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => List(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmericaWest => List(ArcticOcean, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => List(ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => List(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => List(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => List(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => List(IndianOcean, NewZealand)
        case NewZealand => List(IndianOcean, SouthPacific, Australia)
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
        case SL => List(NorthAmericaWest)
        case WW => List(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r: Region): (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (950, 820)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670,615)
        case CentralAmerica => (305,410)
        case NorthAmericaWest => (405, 225)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1568, 660)
        case Australia => (125, 685)
        case NewZealand => (265,710)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap6 extends Board {
    val id = "earth66"
    val name = "Earth Map (6 players)"

    val BeringSea = Region("Bering Sea", Ocean)
    val ArcticOcean = Region("Arctic Ocean", Ocean)
    val Scandinavia = Region("Scandinavia", GlyphWW)
    val Europe = Region("Europe", GlyphWW)
    val NorthAsia = Region("North Asia", GlyphWW)
    val SouthAsia = Region("South Asia", GlyphWW)
    val Arabia = Region("Arabia", GlyphWW)
    val EastAfrica = Region("East Africa", GlyphAA)
    val WestAfrica = Region("West Africa", GlyphAA)
    val NorthAtlantic = Region("North Atlantic", Ocean)
    val MediterraneanSea = Region("Mediterranean Sea", Ocean)
    val SouthAtlantic = Region("South Atlantic", Ocean)
    val Antarctica = Region("Antarctica", NoGlyph)
    val MountainsOfMadness = Region("Mountains of Madness", NoGlyph)
    val SouthPacific = Region("South Pacific", Ocean)
    val SouthAmericaWest = Region("South America West", GlyphOO)
    val SouthAmericaEast = Region("South America East", GlyphOO)
    val NorthAmericaWest = Region("North America West", GlyphOO)
    val NorthAmericaEast = Region("North America East", GlyphOO)
    val CentralAmerica = Region("Central America", GlyphOO)
    val NorthPacific = Region("North Pacific", Ocean)
    val IndianOcean = Region("Indian Ocean", Ocean)
    val Australia = Region("Australia", GlyphAA)
    val NewZealand = Region("New Zealand", GlyphAA)

    val regions = List(BeringSea, ArcticOcean, NorthAtlantic, MediterraneanSea, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica, MountainsOfMadness)
    val nonFactionRegions = List(BeringSea, NorthAtlantic, MediterraneanSea, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val west = List(BeringSea, ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, MediterraneanSea, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = List(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean, MountainsOfMadness)

    def connected(region : Region) = region match {
        case BeringSea => List(ArcticOcean, NorthAmericaEast, NorthAmericaWest, NorthPacific)
        case ArcticOcean => List(BeringSea, NorthAmericaEast, NorthAtlantic, Scandinavia, NorthAsia)
        case NorthAtlantic => List(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Scandinavia, Europe, MediterraneanSea, WestAfrica, SouthAtlantic, SouthAmericaEast)
        case MediterraneanSea => List(NorthAtlantic, Europe, Arabia, WestAfrica)
        case SouthAtlantic => List(Antarctica, MountainsOfMadness, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => List(BeringSea, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => List(MountainsOfMadness, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => List(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Scandinavia => List(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => List(NorthAtlantic, Scandinavia, NorthAsia, Arabia, MediterraneanSea)
        case NorthAsia => List(ArcticOcean, BeringSea, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => List(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => List(MediterraneanSea, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => List(NorthAtlantic, MediterraneanSea, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => List(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmericaWest => List(BeringSea, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => List(BeringSea, ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => List(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => List(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => List(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => List(IndianOcean, NewZealand)
        case NewZealand => List(IndianOcean, SouthPacific, Australia)
        case Antarctica => List(SouthPacific, SouthAtlantic, MountainsOfMadness)
        case MountainsOfMadness => List(Antarctica, SouthAtlantic, IndianOcean)
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
        case SL => List(NorthAmericaWest)
        case WW => List(ArcticOcean, MountainsOfMadness)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r: Region): (Int, Int) = r match {
        case BeringSea => (345, 40)
        case ArcticOcean => (1055, 60)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (735, 315)
        case MediterraneanSea => (1130, 360)
        case SouthAtlantic => (855, 665)
        case Antarctica => (850, 835)
        case MountainsOfMadness => (1235,865)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670,615)
        case CentralAmerica => (305,410)
        case NorthAmericaWest => (405, 225)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1568, 660)
        case Australia => (125, 685)
        case NewZealand => (265,710)
        case _ => throw new Error("Unknown region " + r)
    }
}
