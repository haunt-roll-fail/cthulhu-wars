package cws

import hrf.colmat._

object EarthMap3 extends Board {
    val id = "earth33"
    val name = "Earth Map (3 players)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Europe = Area("Europe", GlyphWW)
    val Asia = Area("Asia", GlyphWW)
    val Africa = Area("Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmerica = Area("South America", GlyphOO)
    val NorthAmerica = Area("North America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Europe, Asia, Africa, NorthAmerica, SouthAmerica, Australia, Antarctica)
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthAmerica, Australia)
    val west = $(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = $(Europe, Asia, Africa, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmerica, NorthAtlantic, Europe, Asia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmerica, ArcticOcean, Europe, Asia, Africa, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, Africa, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, Asia)
        case IndianOcean => $(Antarctica, SouthAtlantic, Africa, Asia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Europe => $(ArcticOcean, NorthAtlantic, Asia)
        case Asia => $(ArcticOcean, NorthAtlantic, NorthPacific, IndianOcean, Europe, Africa)
        case Africa => $(NorthAtlantic, Asia, SouthAtlantic, IndianOcean)
        case NorthAmerica => $(ArcticOcean, NorthAtlantic, SouthAmerica, NorthPacific)
        case SouthAmerica => $(NorthAmerica, NorthAtlantic, SouthAtlantic, SouthPacific, NorthPacific)
        case Australia => $(SouthPacific, IndianOcean)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
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
        case GC => $(SouthPacific)
        case CC => $(Asia)
        case BG => $(Africa)
        case YS => $(Europe)
        case SL => $(NorthAmerica)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Europe => (1240,245)
        case Asia => (1605, 350)
        case Africa => (1110, 480)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmerica => (590, 680)
        case NorthAmerica => (380, 290)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (215, 707)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap4v35 extends Board {
    val id = "earth35"
    val name = "Earth Map (4 players 3/5 variant)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Scandinavia = Area("Scandinavia", GlyphWW)
    val Europe = Area("Europe", GlyphWW)
    val NorthAsia = Area("North Asia", GlyphWW)
    val SouthAsia = Area("South Asia", GlyphWW)
    val Arabia = Area("Arabia", GlyphWW)
    val EastAfrica = Area("East Africa", GlyphAA)
    val WestAfrica = Area("West Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmerica = Area("South America", GlyphOO)
    val NorthAmerica = Area("North America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmerica, SouthAmerica, Australia, Antarctica)
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, SouthAmerica, Australia)
    val west = $(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = $(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmerica, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => $(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Scandinavia => $(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => $(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => $(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => $(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => $(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => $(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => $(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmerica => $(ArcticOcean, NorthAtlantic, SouthAmerica, NorthPacific)
        case SouthAmerica => $(NorthAmerica, NorthAtlantic, SouthAtlantic, SouthPacific, NorthPacific)
        case Australia => $(SouthPacific, IndianOcean)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
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
        case GC => $(SouthPacific)
        case CC => $(SouthAsia)
        case BG => $(WestAfrica)
        case YS => $(Europe)
        case SL => $(NorthAmerica)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
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
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmerica => (590, 680)
        case NorthAmerica => (380, 290)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (215, 707)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap4v53 extends Board {
    val id = "earth53"
    val name = "Earth Map (4 players 5/3 variant)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Europe = Area("Europe", GlyphWW)
    val Asia = Area("Asia", GlyphWW)
    val Africa = Area("Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmericaWest = Area("South America West", GlyphOO)
    val SouthAmericaEast = Area("South America East", GlyphOO)
    val NorthAmericaWest = Area("North America West", GlyphOO)
    val NorthAmericaEast = Area("North America East", GlyphOO)
    val CentralAmerica = Area("Central America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)
    val NewZealand = Area("New Zealand", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Europe, Asia, Africa, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand)
    val west = $(ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = $(Europe, Asia, Africa, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmericaWest, NorthAmericaEast, NorthAtlantic, Europe, Asia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Europe, Asia, Africa, SouthAtlantic, SouthAmericaEast)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, Africa, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, Asia)
        case IndianOcean => $(Antarctica, SouthAtlantic, Africa, Asia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Europe => $(ArcticOcean, NorthAtlantic, Asia)
        case Asia => $(ArcticOcean, NorthAtlantic, NorthPacific, IndianOcean, Europe, Africa)
        case Africa => $(NorthAtlantic, Asia, SouthAtlantic, IndianOcean)
        case NorthAmericaWest => $(ArcticOcean, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => $(ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => $(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => $(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => $(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => $(IndianOcean, NewZealand)
        case NewZealand => $(IndianOcean, SouthPacific, Australia)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
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
        case GC => $(SouthPacific)
        case CC => $(Asia)
        case BG => $(Africa)
        case YS => $(Europe)
        case SL => $(NorthAmericaWest)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Europe => (1240,245)
        case Asia => (1605, 350)
        case Africa => (1110, 480)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670, 615)
        case CentralAmerica => (305, 410)
        case NorthAmericaWest => (330, 180)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (125, 685)
        case NewZealand => (265, 710)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap5 extends Board {
    val id = "earth55"
    val name = "Earth Map (5 players)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Scandinavia = Area("Scandinavia", GlyphWW)
    val Europe = Area("Europe", GlyphWW)
    val NorthAsia = Area("North Asia", GlyphWW)
    val SouthAsia = Area("South Asia", GlyphWW)
    val Arabia = Area("Arabia", GlyphWW)
    val EastAfrica = Area("East Africa", GlyphAA)
    val WestAfrica = Area("West Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmericaWest = Area("South America West", GlyphOO)
    val SouthAmericaEast = Area("South America East", GlyphOO)
    val NorthAmericaWest = Area("North America West", GlyphOO)
    val NorthAmericaEast = Area("North America East", GlyphOO)
    val CentralAmerica = Area("Central America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)
    val NewZealand = Area("New Zealand", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand)
    val west = $(ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = $(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmericaWest, NorthAmericaEast, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmericaEast)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => $(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Scandinavia => $(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => $(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => $(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => $(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => $(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => $(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => $(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmericaWest => $(ArcticOcean, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => $(ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => $(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => $(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => $(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => $(IndianOcean, NewZealand)
        case NewZealand => $(IndianOcean, SouthPacific, Australia)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
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
        case GC => $(SouthPacific)
        case CC => $(SouthAsia)
        case BG => $(WestAfrica)
        case YS => $(Europe)
        case SL => $(NorthAmericaWest)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
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
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670, 615)
        case CentralAmerica => (305, 410)
        case NorthAmericaWest => (330, 180)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (125, 685)
        case NewZealand => (265, 710)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap6 extends Board {
    val id = "earth66"
    val name = "Earth Map (6 players)"

    val BeringSea = Area("Bering Sea", Ocean)
    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Scandinavia = Area("Scandinavia", GlyphWW)
    val Europe = Area("Europe", GlyphWW)
    val NorthAsia = Area("North Asia", GlyphWW)
    val SouthAsia = Area("South Asia", GlyphWW)
    val Arabia = Area("Arabia", GlyphWW)
    val EastAfrica = Area("East Africa", GlyphAA)
    val WestAfrica = Area("West Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val MediterraneanSea = Area("Mediterranean Sea", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val MountainsOfMadness = Area("Mountains of Madness", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmericaWest = Area("South America West", GlyphOO)
    val SouthAmericaEast = Area("South America East", GlyphOO)
    val NorthAmericaWest = Area("North America West", GlyphOO)
    val NorthAmericaEast = Area("North America East", GlyphOO)
    val CentralAmerica = Area("Central America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)
    val NewZealand = Area("New Zealand", GlyphAA)

    val regions = $(BeringSea, ArcticOcean, NorthAtlantic, MediterraneanSea, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica, MountainsOfMadness)
    val nonFactionRegions = $(BeringSea, NorthAtlantic, MediterraneanSea, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val west = $(BeringSea, ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, MediterraneanSea, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = $(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean, MountainsOfMadness)

    def connected(region : Region) = region match {
        case BeringSea => $(ArcticOcean, NorthAmericaEast, NorthAmericaWest, NorthPacific)
        case ArcticOcean => $(BeringSea, NorthAmericaEast, NorthAtlantic, Scandinavia, NorthAsia)
        case NorthAtlantic => $(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Scandinavia, Europe, MediterraneanSea, WestAfrica, SouthAtlantic, SouthAmericaEast)
        case MediterraneanSea => $(NorthAtlantic, Europe, Arabia, WestAfrica)
        case SouthAtlantic => $(Antarctica, MountainsOfMadness, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => $(BeringSea, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => $(MountainsOfMadness, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Scandinavia => $(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => $(NorthAtlantic, Scandinavia, NorthAsia, Arabia, MediterraneanSea)
        case NorthAsia => $(ArcticOcean, BeringSea, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => $(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => $(MediterraneanSea, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => $(NorthAtlantic, MediterraneanSea, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => $(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmericaWest => $(BeringSea, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => $(BeringSea, ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => $(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => $(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => $(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => $(IndianOcean, NewZealand)
        case NewZealand => $(IndianOcean, SouthPacific, Australia)
        case Antarctica => $(SouthPacific, SouthAtlantic, MountainsOfMadness)
        case MountainsOfMadness => $(Antarctica, SouthAtlantic, IndianOcean)
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
        case GC => $(SouthPacific)
        case CC => $(SouthAsia)
        case BG => $(WestAfrica)
        case YS => $(Europe)
        case SL => $(NorthAmericaWest)
        case WW => $(ArcticOcean, MountainsOfMadness)
        case OW => regions
        case AN => nonFactionRegions
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case BeringSea => (85, 55)
        case ArcticOcean => (1075, 60)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (665, 385)
        case MediterraneanSea => (1130, 360)
        case SouthAtlantic => (855, 665)
        case Antarctica => (870, 845)
        case MountainsOfMadness => (1240, 855)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670, 615)
        case CentralAmerica => (305, 410)
        case NorthAmericaWest => (330, 180)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (125, 685)
        case NewZealand => (265, 710)
        case _ => throw new Error("Unknown region " + r)
    }
}
