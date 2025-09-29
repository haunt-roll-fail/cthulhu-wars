package cws

import scala.scalajs._
import scala.scalajs.js.annotation._

import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import Html._

import util.canvas._

class InfoOverlay(overlay : html.Element) {
    var showing : |[String] = None
    var soonHide : Int = 0

    overlay.parentElement.parentElement.style.display = "none"

    overlay.parentElement.parentElement.addEventListener("click", (_ : dom.Event) => hide(), true)

    private var list : $[dom.HTMLTableElement] = $
    private var old : |[dom.HTMLTableElement] = None

    def show(tableHtml : String) {
        soonHide = 0

        if (showing.has(tableHtml))
            return

        hide()

        showing = |(tableHtml)

        overlay.parentElement.parentElement.style.display = ""

        list = 20.to(100).reverse.%(_ % 2 == 0).$./ { z =>
            val p = dom.document.createElement("div").asInstanceOf[html.Div]
            p.style.position = "absolute"
            p.style.fontSize = "" + z + "%"
            p.style.opacity = "0"
            p.style.height = "100%"
            p.style.overflowY = "hidden"
            p.innerHTML = tableHtml

            overlay.appendChild(p)

            p.children(0).as[dom.HTMLTableElement].get
        }

        old = None
    }

    def hide() {
        soonHide = 0

        showing = None

        overlay.parentElement.parentElement.style.display = "none"

        while (overlay.hasChildNodes())
            overlay.removeChild(overlay.lastChild)

        list = $
    }

    private def readjust() {
        if (soonHide == 1)
            hide()

        if (soonHide > 1)
            soonHide -= 1

        if (list.any) {
            val target = overlay.clientHeight

            val best = list.%(_.clientHeight <= target).starting.|(list.minBy(_.clientHeight))

            if (old.has(best).not) {
                old = |(best)

                list.foreach { p =>
                    if (p == best) {
                        p.parentElement.style.opacity = "1"
                        p.parentElement.style.pointerEvents = ""
                        p.parentElement.style.overflowY = "auto"
                        p.parentElement.style.height = (p.clientHeight <= target).??("100%")
                    }
                    else {
                        p.parentElement.style.opacity = "0"
                        p.parentElement.style.pointerEvents = "none"
                        p.parentElement.style.overflowY = "hidden"
                        p.parentElement.style.height = "100%"
                    }
                }
            }
        }

        dom.window.requestAnimationFrame { _ =>
            readjust()
        }
    }

    readjust()
}

object Overlays {
    val overlay = new InfoOverlay(dom.document.getElementById("overlay").asInstanceOf[html.Element])
    var temp = false

    def imageSource(id : String) = hrf.web.getElem(id).as[dom.html.Image]./(_.src).|!("unknown image source " + id)

    implicit class ElementString(val s : String) extends AnyVal {
        def & = "<span style=inline-block>" + s + "</span>"
    }

    @JSExportTopLevel("onExternalClick")
    def onExternalClick(s : Any*) {
        temp = false
        val info = s.$.@@ {
            case $("GC") => faction(GC, "info:gc-background", Immortal, "Ongoing", "Once Cthulhu has Awakened, he costs only 4 Power each subsequent time he is Awakened. Whenever you Awaken any Great Old One, gain <span class=es>1 Elder Sign.</span>",
                $(), $(
                (Acolyte,   6, "1", "0", s"""<div class=p>Spellbook: ${reference(GC, Dreams)}</div>"""),
                (DeepOne,   4, "1", "1", s"""<div class=p>Spellbook: ${reference(GC, Devolve)}</div>"""),
                (Shoggoth,  2, "2", "2", s"""<div class=p>Spellbook: ${reference(GC, Absorb)}</div>"""),
                (Starspawn, 2, "3", "3", s"""<div class=p>Spellbook: ${reference(GC, Regenerate)}</div>"""),
                (Cthulhu,   1, "10/4", "6", s"""
                    <div class=p>${cost(s"How to Awaken ${Cthulhu.name}:")}</div>
                    <div class=p>${cost("1)")} There must be a Gate in Great Cthulhu's starting Area (Can be abandoned or enemy-Controlled).</div>
                    <div class=p>${cost("2)")} If this is the first Awakening, pay <span class=cost-color>10 Power</span>. Otherwise pay <span class=cost-color>4 Power</span>.</div>
                    <div class=p>${cost("3)")} ${Cthulhu.name} appears in its starting Area (Remember to gain <span class=es>1 Elder Sign</span>).</div>
                    <div class=p>${ref(Devour)} ${cost("(Pre-Battle):")} The enemy player chooses and Eliminates one of his Monsters or Cultists in the Battle.</div>
                    <div class=p>Spellbooks: ${reference(GC, Submerge)}, ${reference(GC, YhaNthlei)}</div>"""
                ),
            ))

            case $("GC", FirstDoomPhase.text) => requirement("Receive this Spellbook in the first Doom Phase.<br/>Receive <span class=es>1 Elder Sign.</span>")
            case $("GC", KillDevour1.text) => requirement("Kill and/or Devour an enemy Unit in a Battle.<br/><br/>* You may earn both Spellbooks in a single Battle, if you Kill and/or Devour 3 or more Units.")
            case $("GC", KillDevour2.text) => requirement("Kill and/or Devour 2 enemy Units in a Battle.<br/><br/>* You may earn both Spellbooks in a single Battle, if you Kill and/or Devour 3 or more Units.")
            case $("GC", AwakenCthulhu.text) => requirement("Awaken Cthulhu.")
            case $("GC", OceanGates.text) => requirement("Control 3 Gates in Ocean/sea Areas<br/>OR<br/>4 Gates exist in Ocean/sea Areas.")
            case $("GC", FiveSpellbooks.text) => requirement("This must be the last Faction Spellbook you receive.<br/>It must be taken during the Doom Phase.<br/>Receive <span class=es>1 Elder Sign.</span>")

            case $("GC", Dreams.name) => spellbook(Dreams.name, "Action: Cost 2", "Choose an Area containing an enemy’s Acolyte Cultist. Your enemy must Eliminate one of his Acolyte Cultists from that Area and replace it with one from your Pool.")
            case $("GC", Devolve.name) => spellbook(Devolve.name, "Ongoing", "After any player's Action, replace one or more of your Acolyte Cultists anywhere on the Map with Deep Ones from your Pool.")
            case $("GC", Absorb.name) => spellbook(Absorb.name, "Pre-Battle", "If a Shoggoth is present, Eliminate one or more of your Monsters or Cultists in the Battle. For each Unit so removed, add 3 dice to the Shoggoth's Combat for that Battle.")
            case $("GC", Regenerate.name) => spellbook(Regenerate.name, "Post-Battle", "Assign up to 2 Kill or Pain Battle results to the same Starspawn. If 2 Kills are applied, the Starspawn is Killed. On any other combination of Kill or Pain results, the Starspawn is only Pained.")
            case $("GC", Submerge.name) => spellbook(Submerge.name, "Action: Cost 1", "If Cthulhu is in an ocean or sea Area, remove him from the Map and place him on your Faction Card, along with any or all of your Units in the Area. Later, as a 0-cost Action, you may place Cthulhu, plus all accompanying Units, into any Area.")
            case $("GC", YhaNthlei.name) => spellbook(YhaNthlei.name, "Gather Power Phase", "During Gather Power, if Cthulhu is in play, gain 1 Power for each enemy-controlled Gate in an ocean or sea Area.")


            case $("CC") => faction(CC, "info:cc-background", Flight, "Ongoing", "All your units can fly (even Cultists). When moved, they can travel 2 Areas. They can fly over Areas containing enemy Units.",
                $(Madness), $(
                (Acolyte,       6, "1", "0", ""),
                (Nightgaunt,    3, "1", "0", s"""<div class=p>Spellbook: ${reference(CC, Abduct)}</div>"""),
                (FlyingPolyp,   3, "2", "1", s"""<div class=p>Spellbook: ${reference(CC, Invisibility)}</div>"""),
                (HuntingHorror, 2, "3", "2", s"""<div class=p>Spellbook: ${reference(CC, SeekAndDestroy)}</div>"""),
                (Nyarlathotep,  1, "10", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${Nyarlathotep.name}:")}</div>
                    <div class=p>${cost("1)")} You must have a Controlled Gate.</div>
                    <div class=p>${cost("2)")} Pay ${power(10)}. Nyarlathotep appears at the controlled Gate.</div>
                    <div class=p>${combat} Equals the total of your own Faction Spellbooks plus the Faction Spellbooks of your opponent in the Battle.</div>
                    <div class=p>${ref(Harbinger)} ${cost("(Post-Battle):")} If Nyarlathotep is in a Battle in which one or more enemy Great Old Ones are Pained or Killed, you receive Power equal to half the cost to Awaken those Great Old Ones. Per enemy Great Old One, you may choose to receive 2 Elder Signs instead of Power. Harbringer takes effect even if Nyarlathotep is Killed or Pained in the Battle himself.</div>
                    <div class=p>Spellbooks: ${reference(CC, Emissary)}, ${reference(CC, ThousandForms)}</div>"""
                ),
            ))

            case $("CC", Pay4Power.text) => requirement("As your Action, pay 4 Power.*<br/><br/>* You may earn both these Spellbooks in a single Round by paying 10 Power.")
            case $("CC", Pay6Power.text) => requirement("As your Action, pay 6 Power.*<br/><br/>* You may earn both these Spellbooks in a single Round by paying 10 Power.")
            case $("CC", Gates3Power12.text) => requirement("Control three Gates OR have 12 Power.")
            case $("CC", Gates4Power15.text) => requirement("Control four Gates OR have 15 Power.")
            case $("CC", CaptureCultist.text) => requirement("Capture an enemy Cultist.")
            case $("CC", AwakenNyarlathotep.text) => requirement("Awaken Nyarlathotep.")

            case $("CC", Abduct.name) => spellbook(Abduct.name, "Pre-Battle", "Eliminate one or more Nightgaunts from the Battle. For each one Eliminated, your enemy must Eliminate one of his own Monsters or Cultists from the Battle.")
            case $("CC", Invisibility.name) => spellbook(Invisibility.name, "Pre-Battle", "Select one Monster or Cultist (from either Faction) for each Flying Polyp present and \"exempt\" it. The selected Unit does not participate in the rest of the Battle.")
            case $("CC", SeekAndDestroy.name) => spellbook(SeekAndDestroy.name, "Pre-Battle", "Immediately move any or all Hunting Horrors from any Area to the Battle Area.")
            case $("CC", Emissary.name) => spellbook(Emissary.name, "Post-Battle", "Unless an enemy Great Old One is involved in the Battle, a Kill applied to Nyarlathotep becomes a Pain. If Nyarlathotep cannot be Pained due to being surrounded, he is not Eliminated.")
            case $("CC", ThousandForms.name) => spellbook(ThousandForms.name, "Action: Cost 0", s"If Nyarlathotep is in play, roll 1d6. Your foes lose that much Power between them; they have 1 minute to decide how much each loses. If they cannot agree, you get the rolled number as Power added to your total. This spellbook cannot be used again this Action Phase.")
            case $("CC", Madness.name) => spellbook(Madness.name, "Post-Battle", "After all Pain results have been assigned, you, rather than the Units' owners, choose the Area(s) to which all Pained Units will go. You may apply these results in any order (rather than the normal 'attacker first, then defender'), but you must still follow all other rules. Do this even for Battles in which you did not participate.")


            case $("BG") => faction(BG, "info:bg-background", Fertility, "Ongoing", "You may Summon Monsters as an Unlimited Action.",
                $(BloodSacrifice), $(
                (Acolyte,       6, "1", "0/1", s"""<div class=p>${combat} 1 with ${reference(BG, Frenzy)}</div>"""),
                (Ghoul,         2, "1/0", "0", s"""
                    <div class=p>${cost("Cost:")} 0 with ${reference(BG, ThousandYoung)}.</div>
                    <div class=p>Spellbook: ${reference(BG, Necrophagy)}</div>"""
                ),
                (Fungi,         4, "2/1", "1", s"""
                    <div class=p>${cost("Cost:")} 1 with ${reference(BG, ThousandYoung)}.</div>
                    <div class=p>Spellbook: ${reference(BG, Ghroth)}</div>"""
                ),
                (DarkYoung,     3, "3/2", "2", s"""
                    <div class=p>${cost("Cost:")} 2 with ${reference(BG, ThousandYoung)}.</div>
                    <div class=p>Spellbook: ${reference(BG, RedSign)}</div>"""
                ),
                (ShubNiggurath, 1, "8", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${ShubNiggurath.name}:")}</div>
                    <div class=p>${cost("1)")} You must have a Controlled Gate, and at least 2 Cultists on the Map &mdash; they can be in any Area(s).</div>
                    <div class=p>${cost("2)")} Pay ${power(8)}.</div>
                    <div class=p>${cost("3)")} Remove your 2 Cultists, then place Shub-Niggurath at your Controlled Gate.</div>
                    <div class=p>${combat} Equals to the sum of your Controlled Gates and in-play Cultists. If you have The Red Sign, add another +1 for each Dark Young you have in play.</div>
                    <div class=p>${ref(Avatar)} ${cost("(Action: Cost 1):")} Choose an Area and a Faction. Swap the location of Shub-Niggurath with that of a Monster or Cultist in the chosen Area. The owner of the chosen Faction chooses which Unit to relocate.</div>"""
                ),
            ))

            case $("BG", Spread4.text) => requirement("Have units in 4 Areas.")
            case $("BG", Spread6.text) => requirement("Have units in 6 Areas.")
            case $("BG", Spread8.text) => requirement("Have units in 8 Areas.")
            case $("BG", SpreadSocial.text) => requirement("Share Areas with all enemies. Gain 1 Power per enemy player.")
            case $("BG", Eliminate2Cultists.text) => requirement("As your Action, for a Round, Eliminate 2 of your Cultists from any Area(s) on the Map.")
            case $("BG", AwakenShubNiggurath.text) => requirement("Awaken Shub-Niggurath.")

            case $("BG", ThousandYoung.name) => spellbook(ThousandYoung.name, "Ongoing", "If Shub-Niggurath is in play, Ghouls, Fungi, and Dark Young cost 1 fewer Power each to Summon.")
            case $("BG", Frenzy.name) => spellbook(Frenzy.name, "Battle", "Your Cultists now have 1 Combat.")
            case $("BG", Necrophagy.name) => spellbook(Necrophagy.name, "Post-Battle", "Move any or all Ghouls (who did not partecipate in the Battle) from any Area to the Battle Area, even if your Faction was not involved in the Battle. For each Ghoul so moved, both sides in the Battle suffer an additional Pain result.")
            case $("BG", Ghroth.name) => spellbook(Ghroth.name, "Action: Cost 2", "Roll a die. If the roll is less than or equal to the number of Areas containing Fungi, your enemies must collectively Eliminate Cultists equal to the die roll. They have 1 minute to decide how to distribute these Eliminations. If time runs out, you choose for them. If the roll is greater than the number of Areas with Fungi, place 1 Acolyte from any Faction's pool anywhere on the map.")
            case $("BG", RedSign.name) => spellbook(RedSign.name, "Ongoing", "Dark Young can Create and Control Gates. Each Dark Young adds 1 to Shub-Niggurath's Combat and each provides 1 Power during the Gather Power Phase. They do not act as Cultists with respect to any other purpose.")
            case $("BG", BloodSacrifice.name) => spellbook(BloodSacrifice.name, "Doom Phase", "If Shub-Niggurath is in play during the Doom Phase, you can choose to Eliminate one of your Cultists (from anywhere on the map). If you do, gain <span class=es>1 Elder Sign.</span>")


            case $("YS") => faction(YS, "info:ys-background", Feast, "Gather Power Phase", "During Gather Power, you gain 1 Power for each Area containing both a Desecration Token and one or more of your units.",
                $(), $(
                (Acolyte,   6, "1",  "0", s"""<div class=p>Spellbook: ${reference(YS, Passion)}</div>"""),
                (Undead,    6, "1", "1-", s"""
                    <div class=p>${combat} Roll 1 die less than the total Undead in the area.</div>
                    <div class=p>Spellbook: ${reference(YS, Zingaya)}</div>"""),
                (Byakhee,   4, "2", "1+", s"""
                    <div class=p>${combat} Roll 1 die more than the total Byakhee in the area.</div>
                    <div class=p>Spellbook: ${reference(YS, Shriek)}</div>"""),
                (KingInYellow, 1, "4", "0", s"""
                    <div class=p>${cost(s"How to Awaken the ${KingInYellow.name}:")}</div>
                    <div class=p>${cost("1)")} You must have a Unit in an Area lacking a Gate.</div>
                    <div class=p>${cost("2)")} Pay ${power(4)}. ${KingInYellow.name} appears in that Area.</div>
                    <div class=p>${ref(Desecrate)} ${cost("(Action: Cost 2):")} If the King is in an Area with no Desecration Token, roll 1 die and compare to your total units in the Area (including the King). On a roll equal or less than your unit total, place a Desecration Token in the Area. If you succeed or fail, place a Monster or Cultist with a cost of 2 or less in the Area.</div>
                    <div class=p>Spellbook: ${reference(YS, ScreamingDead)}</div>"""
                ),
                (Hastur, 1, "10", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${Hastur.name}:")}</div>
                    <div class=p>${cost("1)")} You must have a Controlled Gate and the King in Yellow in the same area.</div>
                    <div class=p>${cost("2)")} Pay ${power(10)}. ${Hastur.name} appears in the King's Area.</div>
                    <div class=p>${combat} Equals the current Cost of a Ritual of Annihilation.</div>
                    <div class=p>${ref(Vengeance)} ${cost("(Post-Battle):")} If Hastur is involved in a Battle, choose which Combat results are applied to which enemy Unit.</div>
                    <div class=p>Spellbooks: ${reference(YS, ThirdEye)}, ${reference(YS, HWINTBN)}</div>"""
                ),
            ))

            case $("YS", Provide3Doom.text) => requirement("As your Action for a round, select another player.<br/>That player gains three Doom points.")
            case $("YS", AwakenKing.text) => requirement("Awaken the King in Yellow.")
            case $("YS", DesecrateAA.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-aa")} class=inline-glyph />")
            case $("YS", DesecrateOO.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-oo")} class=inline-glyph />")
            case $("YS", DesecrateWW.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-ww")} class=inline-glyph />")
            case $("YS", AwakenHastur.text) => requirement("Awaken Hastur. Also receive <span class=es>1 Elder Sign</span>.")

            case $("YS", Passion.name) => spellbook(Passion.name, "Ongoing", "When one or more of your Cultists are Eliminated by an enemy (Killed, Captured, etc.), gain 1 Power total.")
            case $("YS", Zingaya.name) => spellbook(Zingaya.name, "Action: Cost 1", "If Undead are in an Area with enemy Acolyte Cultists, your enemy must Eliminate an Acolyte Cultist. Then, place an Undead in the Area.")
            case $("YS", Shriek.name) => spellbook(Shriek.name, "Action: Cost 1", "Move any or all Byakhee from their current Area(s) to any one Area on the Map.")
            case $("YS", ScreamingDead.name) => spellbook(ScreamingDead.name, "Action: Cost 1", "Move the King in Yellow to an adjacent Area. Any Undead in the same Area can move with him for free. You may then take a second, different Action. You may NOT take He Who is Not to be Named as your second Action.")
            case $("YS", ThirdEye.name) => spellbook(ThirdEye.name, "Ongoing", "If Hastur is in play, the cost of Desecration is reduced to 1. If the Desecration succeeds, you also get 1 Elder Sign.")
            case $("YS", HWINTBN.name) => spellbook(HWINTBN.name, "Action: Cost 1", "Move Hastur to any Area containing a Cultist of any Faction. You may then take a second, different Action. You may NOT take The Screaming Dead as your second Action.")


            case $("SL") => faction(SL, "info:sl-background", DeathFromBelow, "Doom Phase", "Place your lowest-cost Monster from your Pool into any Area containing at least 1 of your Units.",
                $(Burrow, CursedSlumber), $(
                (Acolyte,       6, "1", "0", ""),
                (Wizard,        2, "1", "0", s"""<div class=p>Spellbook: ${reference(SL, EnergyNexus)}</div>"""),
                (SerpentMan,    3, "2", "1", s"""<div class=p>Spellbook: ${reference(SL, AncientSorcery)}</div>"""),
                (FormlessSpawn, 4, "3", "?", s"""<div class=p>${combat} Equals count of Formless Spawns on the map, +1 if Tsathoggua is also on the map.</div>"""),
                (Tsathoggua,    1, "8", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${Tsathoggua.name}:")}</div>
                    <div class=p>${cost("1)")} You must have a Formless Spawn on the map.</div>
                    <div class=p>${cost("2)")} Pay ${power(8)}. Place Tsathoggua in the Area with the Formless Spawn.</div>
                    <div class=p>${combat} Equals the opponent's current Power or 2, whichever is greater.</div>
                    <div class=p>${ref(Lethargy)} ${cost("(Action: Cost 0):")} If Tsathoggua is in play, do nothing. This counts as an Action.</div>
                    <div class=p>Spellbooks: ${reference(SL, DemandSacrifice)}, ${reference(SL, CaptureMonster)}</div>"""
                ),
            ))

            case $("SL", Pay3SomeoneGains3.text) => requirement("As your action spend 3 Power. Select another player.<br/>He gains 3 Power.")
            case $("SL", Pay3EverybodyGains1.text) => requirement("As your action spend 3 Power.<br/>Each other player gains 1 Power.")
            case $("SL", Pay3EverybodyLoses1.text) => requirement("As your action spend 3 Power.<br/>Each other player loses 1 Power.")
            case $("SL", Roll6DiceInBattle.text) => requirement("Roll 6 or more combat dice in a single Battle.")
            case $("SL", PerformRitual.text) => requirement("Perform a Ritual of Annihilation.")
            case $("SL", AwakenTsathoggua.text) => requirement("Awaken Tsathoggua.")

            case $("SL", Burrow.name) => spellbook(Burrow.name, "Ongoing", "After a Move Action in which you spend 2 or more Power moving Units, regain 1 Power.")
            case $("SL", EnergyNexus.name) => spellbook(EnergyNexus.name, "Ongoing", "Just before a Battle in an Area containing a Wizard, you may take one Action that originates in the Area for its normal Power cost. The Battle proceeds once that Action is finished, starting with Pre-Battle Spellbooks and abilities.")
            case $("SL", AncientSorcery.name) => spellbook(AncientSorcery.name, "Action: Cost 1", "Remove a Serpent Man from the Map and place him on an enemy's Faction Card. You now have access to that Faction's Unique Ability until the end of the next Doom Phase. At that point, gain 1 Power and replace the Serpent Man anywhere on the Map. If a Faction's Unique Ability mentions a Great Old One, it is also considered to include Tsathoggua.")
            case $("SL", CaptureMonster.name) => spellbook(CaptureMonster.name, "Action: Cost 1", "Tsathoggua can Capture Enemy Monsters in the same manner as Cultists are Captured. They are sacrificed for 1 Power in the next Gather Power Phase.")
            case $("SL", DemandSacrifice.name) => spellbook(DemandSacrifice.name, "Pre-Battle", "If Tsathoggua is in play, your enemy chooses ONE of the following options before a Battle with you:<br/>1) You gain <span class=es>1 Elder Sign</span>.<br/>OR<br/>2) All of their Kill results against your Units in this Battle count as Pains instead.")
            case $("SL", CursedSlumber.name) => spellbook(CursedSlumber.name, "Action: Cost 1", "Remove your Controlled Gate and its Cultist from the map and place it on your Faction Card. This Gate and Cultist still provide Power and Doom points, but are immune to enemy abilities. As a Cost 1 Action, return the Gate and Cultist to any Area lacking a Gate. You may only have one Gate on your Faction Card at a time.")


            case $("WW") => faction(WW, "info:ww-background", Hibernate, "Action: Cost 0", "Add +1 Power to your total for each enemy Great Old One in play (but not more than your current Power). You can take no further Actions during this Action Phase. At the start of the next Gather Power Phase, do NOT lose your Power, but add it to your total.",
                $(IceAge, Herald), $(
                (Acolyte,   6, "1", "0", s"""<div class=p>Spellbook: ${reference(WW, Cannibalism)}</div>"""),
                (Wendigo,   4, "1", "1", s"""<div class=p>Spellbooks: ${reference(WW, Cannibalism)}, ${reference(WW, Howl)}</div>"""),
                (GnophKeh,  4, "?", "3", s"""
                    <div class=p>${cost("Cost:")} Equals the number of Gnoph-Kehs in your Unit pool.</div>
                    <div class=p>${reference(WW, Berserkergang)}</div>"""
                ),
                (RhanTegoth, 1, "6", "3", s"""
                    <div class=p>${cost(s"How to Awaken ${RhanTegoth.name}:")}</div>
                    <div class=p>${cost("1)")} Pay ${power(6)}.</div>
                    <div class=p>${cost("2)")} ${RhanTegoth.name} appears in an Area containing the Windwalker Glyph.</div>
                    <div class=p>${ref(Eternal)} ${cost("(Post-Battle):")} If ${RhanTegoth.name} receives a Pain or a Kill, you may pay 1 Power to cancel its effect on him. He can only receive one combat result per Battle.</div>"""
                ),
                (Ithaqua, 1, "6", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${Ithaqua.name}:")}</div>
                    <div class=p>${cost("1)")} ${RhanTegoth.name} has been awakened (he need not be in play).</div>
                    <div class=p>${cost("2)")} A Gate must exist in an Area marked with your Glyph. You need not control the Gate.</div>
                    <div class=p>${cost("3)")} Pay ${power(6)} and replace the Gate with ${Ithaqua.name}.</div>
                    <div class=p>${combat} Equal to half of your opponent's Doom, rounded up.</div>
                    <div class=p>${ref(Ferox)} ${cost("(Ongoing):")} While ${Ithaqua.name} is in play, your Cultists cannot be captured by enemy Monsters or Terrors. They are still vulnerable to enemy Great Old Ones.</div>
                    <div class=p>Spellbook: ${reference(WW, ArcticWind)}</div>"""
                ),
            ))

            case $("WW", FirstPlayer.text) => requirement("You are the First Player.")
            case $("WW", OppositeGate.text) => requirement("A Gate exists in the Area marked with the Windwalker Glyph and in which you did not start.")
            case $("WW", AnytimeGainElderSigns.text) => requirement("Take this spellbook at any time. Gain <span class=es>1 Elder Sign</span> for each enemy player with 6 Spellbooks on their Faction Card, to a maximum of <span class=es>3 Elder Signs</span>.")
            case $("WW", AnotherFactionAllSpellbooks.text) => requirement("Another Faction has 6 Spellbooks on their Faction Card.")
            case $("WW", AwakenIthaqua.text) => requirement("Awaken Ithaqua.")
            case $("WW", AwakenRhanTegoth.text) => requirement("Awaken Rhan Tegoth.")

            case $("WW", Cannibalism.name) => spellbook(Cannibalism.name, "Post-Battle", "After all Battle results have been applied, if one or more enemy Units were killed, you may place a Wendigo or Acolyte from your Pool into the Battle Area. You may do this even if you were not involved in the Battle.")
            case $("WW", Howl.name) => spellbook(Howl.name, "Pre-Battle", "Before Battle, if any Wendigos are present, you may force the enemy to Retreat one Unit (of their choice) out of the Area to an adjacent Area. This is not a Pain - the Unit may be moved to an Area containing your Units.")
            case $("WW", Berserkergang.name) => spellbook(Berserkergang.name, "Post-Battle", "For each Gnoph-Keh assigned a Kill in Battle, the enemy must Eliminate 1 Monster or Cultist.")
            case $("WW", ArcticWind.name) => spellbook(ArcticWind.name, "Ongoing", "When Ithaqua uses Move Action, any or all your Units in the same Area can Move with him for no additional cost.")
            case $("WW", IceAge.name) => spellbook(IceAge.name, "Action: Cost 1", "Place or move your Ice Age token to any Area. When an enemy Faction takes any Action ending in the Ice Age Area, they must pay +1 Power.")
            case $("WW", Herald.name) => spellbook(Herald.name, "Doom Phase", "Pay 5 Power for Windwalker's Ritual of Annihilation, regardless of the number indicated on the Ritual track.")


            case $("OW") => faction(OW, "info:ow-background", BeyondOne, "Action: Cost 1", "Select one of your your Units with a Cost of 3+ in an Area that contains a Gate and no enemy Great Old Ones. Move that Unit, the Gate, and any Controlling Unit to any Area on the map that does not already have a Gate.",
                $(TheyBreakThrough, ChannelPower, DragonAscending, DragonDescending), $(
                (Acolyte,     6, "1", "0", s"""<div class=p>Spellbook: ${reference(OW, MillionFavoredOnes)}</div>"""),
                (Mutant,      4, "2", "1", s"""<div class=p>Spellbook: ${reference(OW, MillionFavoredOnes)}</div>"""),
                (Abomination, 3, "3", "2", s"""<div class=p>Spellbooks: ${reference(OW, MillionFavoredOnes)}, ${reference(OW, DreadCurse)}</div>"""),
                (SpawnOW,     2, "4", "3", s"""<div class=p>Spellbooks: ${reference(OW, MillionFavoredOnes)}, ${reference(OW, DreadCurse)}</div>"""),
                (YogSothoth,  1, "6", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${YogSothoth.name}:")}</div>
                    <div class=p>${cost("1)")} You must have a Spawn of Yog-Sothoth on the Map.</div>
                    <div class=p>${cost("2)")} Pay ${power(6)}. Replace the Spawn with Yog-Sothoth.</div>
                    <div class=p>${combat} Equal to twice the number of enemy-Controlled Faction Great Old Ones in play.</div>
                    <div class=p>${ref(KeyAndGate)} ${cost("(Ongoing):")} Yog-Sothoth counts as a Gate for every purpose, except for The Beyond One ability. Yog-Sothoth is not Controlled by any Cultist, and can exist in the same Area as another Gate.</div>"""
                ),
            ))

            case $("OW", EightGates.text) => requirement("There are 8 Gates on the Map.")
            case $("OW", TenGates.text) => requirement("There are 10 Gates on the Map.")
            case $("OW", TwelveGates.text) => requirement("There are 12 Gates on the Map.")
            case $("OW", UnitsAtEnemyGates.text) => requirement("You have Units in at least 2 Areas containing<br/>enemy-Controlled Gates.")
            case $("OW", LoseUnitInBattle.text) => requirement("Lose 1 of your own Units in Battle.")
            case $("OW", GooMeetsGoo.text) => requirement("Your Great Old One is in the same Area with<br/>an enemy Great Old One.")
            case $("OW", AwakenYogSothoth.text) => requirement("Awaken Yog-Sothoth.")

            case $("OW", TheyBreakThrough.name) => spellbook(TheyBreakThrough.name, "Ongoing", "You can Summon Monsters at enemy-Controlled and Abandoned Gates. You do not need to have any Units present in the Area.")
            case $("OW", MillionFavoredOnes.name) => spellbook(MillionFavoredOnes.name, "Post-Battle", "After Pains and Kills are resolved, replace any or all surviving Acolytes to Mutants, Mutants with Abominations, and Abominations with Spawns of Yog-Sothoth. You can replace a Spawn of Yog-Sothoth with as many Mutants as are in your Pool.")
            case $("OW", ChannelPower.name) => spellbook(ChannelPower.name, "Battle", "After rolling Battle dice, you may pay 1 Power to reroll all dice which were not Kills or Pains. You may do this more than once.")
            case $("OW", DreadCurse.name) => spellbook(DreadCurse.name, "Action: Cost 2", "Select an Area and roll 1 Battle die per Abomination and Spawn of Yog-Sothoth in play. Apply the results as Kills and Pains to enemy Factions in the Area. You choose which Faction receives which results, but they choose which of their Units receives each result. No Battle-type abilities apply. You choose to which Area each Unit is Pained, ignoring normal Pain rules.")
            case $("OW", DragonAscending.name) => spellbook(DragonAscending.name, "Once Only", "Once during the game (at any time), set your Power to be equal to the current Power of one chosen enemy Faction.")
            case $("OW", DragonDescending.name) => spellbook(DragonDescending.name, "Once Only", "Once during the game when you perform a Ritual of Annihilation, you receive twice the normal Doom points.")


            case $("AN") => faction(AN, "info:an-background", Dematerialization, "Doom Phase", "Relocate any or all of your own Units from one Area to a single other Area, anywhere on the Map.",
                $, $(
                (Acolyte,    6,   "1",   "0", s""""""),
                (UnMan,      3, "3/0",   "0", s"""<div class=p><span class=cost-color>Cost:</span> 0 with ${reference(AN, Festival)}</div>"""),
                (Reanimated, 3, "4/1",   "2", s"""<div class=p><span class=cost-color>Cost:</span> 1 with ${reference(AN, Brainless)}</div>"""),
                (Yothan,     3, "6/3",   "7", s"""<div class=p><span class=cost-color>Cost:</span> 3 with ${reference(AN, Extinction)}</div>"""),
                (Cathedral,  4, "3/1", "n/a", s"""
                    <div class=p>You may use the Create Gate Action to Create Cathedrals instead of Gates.</div>
                    <div class=p>${cost("Cost:")} 1 if built not adjacent to another Cathedral</div>
                    <div class=p>Spellbooks: ${reference(AN, WorshipServices)}, ${reference(AN, Consecration)}, ${reference(AN, UnholyGround)}</div>
                    <div class=p>${cost("Special:")} If all 4 Cathedrals are in play, you may Awaken an Independent Great Old One without your own Great Old One (when Awakening Cthugha this way, just pay 6 Power).</div>"""
                )
            ))

            case $("AN", CathedralAA.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-aa")} class=inline-glyph />")
            case $("AN", CathedralOO.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-oo")} class=inline-glyph />")
            case $("AN", CathedralWW.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-ww")} class=inline-glyph />")
            case $("AN", CathedralNG.text) => requirement(s"A Cathedral is in an Area without<br/>any of these Glyphs: <img src=${imageSource("sign-aa")} class=inline-glyph /><img src=${imageSource("sign-oo")} class=inline-glyph /><img src=${imageSource("sign-ww")} class=inline-glyph />")
            case $("AN", GiveWorstMonster.text) => requirement("As your Action, each enemy Summons their lowest cost Monster at their Controlled Gate for free.")
            case $("AN", GiveBestMonster.text) => requirement("As your Action, each enemy Summons their highest cost Monster at their Controlled Gate for free.")

            case $("AN", Brainless.name) => spellbook(Brainless.name, "Ongoing", "Reanimated now cost 1 Power to Summon. They may only Move, Capture, or declare Battle if they share an Area with one or more of your non-Reanimated Units.")
            case $("AN", Festival.name) => spellbook(Festival.name, "Ongoing", "Un-Men now cost 0 Power to Summon. When you Summon an Un-Man, also select an enemy to gain 1 Power.")
            case $("AN", Extinction.name) => spellbook(Extinction.name, "Ongoing", "Yothans now cost 3 Power to Summon. When a Yothan is Killed or Eliminated, remove it permanently from the game.")
            case $("AN", UnholyGround.name) => spellbook(UnholyGround.name, "Post Battle", "If there is a Cathedral in the Battle Area, you may choose to remove a Cathedral from anywhere. If you do, an enemy Great Old One in the Battle must be eliminated by its owner.")
            case $("AN", Consecration.name) => spellbook(Consecration.name, "Doom Phase", "When you perform a Ritual of Annihilation, gain <span style=es>1 Elder Sign</span> if at least one Cathedral is in play. If all four Cathedrals are in play, gain <span style=es>2 Elder Signs</span> instead.")
            case $("AN", WorshipServices.name) => spellbook(WorshipServices.name, "Gather Power Phase", "Gain 1 Power for each Cathedral that shares an Area with an enemy Gate. Those enemies each gain 1 Power.")


            case $(_, MaoCeremony.name) => spellbook(MaoCeremony.name, "Ongoing", "At the end of Gather Power, after Power has been added (i.e., before Determine First Player), you may choose to sacrifice 1 or more of your own Cultists, adding 1 Power apiece to your total.")
            case $(_, Recriminations.name) => spellbook(Recriminations.name, "Action: Cost 1", "Remove any spellbook (including this one) from your Faction Card and replace it with another available spellbook.")
            case $(_, Shriveling.name) => spellbook(Shriveling.name, "Pre-Battle", "Select an enemy Monster or Cultist in the Battle. That Unit is Eliminated, and the owner receives Power equal to the Unit's cost.")
            case $(_, StarsAreRight.name) => spellbook(StarsAreRight.name, "Ongoing", "During the Doom Phase, if you turn in Elder Signs for Doom points, you also immediately receive Power equal to their face value.")
            case $(_, UmrAtTawil.name) => spellbook(UmrAtTawil.name, "Ongoing", "Gates now cost you 2 Power to Build.")
            case $(_, Undimensioned.name) => spellbook(Undimensioned.name, "Action: Cost 2", "Rearrange your Units among their Areas as you see fit. You may completely empty an Area, but you may not move to any new Areas.")


            case $("Ghast") => loyaltyCard(GhastCard.name, GhastCard.quantity, GhastCard.cost, GhastCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place all 4 Ghasts at your controlled Gate(s).", "Hordeling", "Ongoing", "When you spend 2 Power to Summon Ghasts, all Ghasts in your pool are immediately placed on the map at any Gate(s) you control.")
            case $("Gug") => loyaltyCard(GugCard.name, GugCard.quantity, GugCard.cost, GugCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Gug at your controlled Gate.", "Clumsy", "Ongoing", "A Gug cannot Capture a Cultist.")
            case $("Shantak") => loyaltyCard(ShantakCard.name, ShantakCard.quantity, ShantakCard.cost, ShantakCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Shantak at your controlled Gate.", "Horror Steed", "Ongoing", "When Moving a Shantak, it can reach any Area on the map. In addition, the Shantak may carry one of your Cultists with it for free.")
            case $("Star Vampire") => loyaltyCard(StarVampireCard.name, StarVampireCard.quantity, StarVampireCard.cost, StarVampireCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Star Vampire at your controlled Gate.", "Vampirism", "Battle", "Roll the Star Vampire's combat dice separately. Each Pain they roll drains 1 Power from the enemy Faction. Each Kill they roll drains 1 Doom point from the enemy Faction. The drained point(s) are transferred to you immediately. If the enemy Faction lacks Power or Doom points, you get nothing. The Pains and Kills rolled still count towards your Combat Results.")

            case $("High Priest") => loyaltyCard(HighPriestCard.name, HighPriestCard.quantity, HighPriestCard.cost, HighPriestCard.combat, "The High Priest is a new type of Cultist, it is Recruited like an Acolyte. Each High Priest generates 1 Power during the Gather Power Phase, can Create and Control a Gate, and can be Captured.", "Unspeakable Oath", "Ongoing", "At the end of any player's Action (even if it is not your turn), Sacrifice your High Priest (return him to your Pool) and gain 2 Power. This may also be done during the Gather Power and Doom Phases.")


            case _ =>
                println("onExternalClick " + s.$.mkString(" | "))
                ""
        }

        if (info == "")
            overlay.hide()
        else
            overlay.show(info)
    }

    @JSExportTopLevel("onExternalOver")
    def onExternalOver(s : Any*) {
        println("onExternalOver " + s)
        if (overlay.showing.none || overlay.soonHide > 0) {
            onExternalClick(s : _*)
            if (overlay.showing.any)
                temp = true
        }
    }

    @JSExportTopLevel("onExternalOut")
    def onExternalOut(s : Any*) {
        println("onExternalOut " + s)
        if (overlay.showing.any) {
            if (temp) {
                overlay.soonHide = 15
                temp = false
            }
        }
    }

    def combat = s"<span class=combat-color>Combat:</span>"

    def cost(s : String) = s"<span class=cost-color>${s}</span>"

    def power(n : Int) = cost(s"${n} Power")

    def loyaltyCard(name : String, quantity : Int, cost : Int, combat : Int, obtainText : String, ability: String, phase: String, abilityText : String) = s"""
        <table class="loyalty-card-table" style="">
            <thead>
                <tr>
                    <th style=width:10%>
                    </th>
                    <th style=width:80%>
                    </th>
                    <th style=width:10%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="h2 abaddon nt">${name}<sup><span class="deh3 nt">(${quantity.toString})</span></sup></span></div>
                        <img class="img" src="${imageSource("info:" + "n-" + name.toLowerCase.replace(" ", "-"))}">
                        <div>&nbsp;</div>
                        <div>
                            <span class="cost-color black-border">Cost: ${cost.toString}</span>
                        </div>
                        <div>
                            <span class="combat-color black-border">Combat: ${combat.toString}</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border">
                            <span class="nt">${obtainText}</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border">
                            <span class="ability-color">${ability} </span><span class="cost-color">(${phase})</span><span class="nt">: ${abilityText}</span>
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    def spellbook(name : String, phase : String, text : String) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:20%>
                    </th>
                    <th style=width:60%>
                    </th>
                    <th style=width:20%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="ability-color inline-block">${name}</span> <span class="cost-color inline-block">(${phase})</span></div>
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    def requirement(text : String) = s"""
        <table class="requirement-table" style="">
            <thead>
                <tr>
                    <th style=width:20%>
                    </th>
                    <th style=width:60%>
                    </th>
                    <th style=width:20%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    def ref(spellbook : Spellbook) = s"""<span class=ability-color>${spellbook.name}</span>"""

    def reference(f : Faction, spellbook : Spellbook) = s"""<span class="ability-color pointer" onclick="onExternalClick('${f.short}', '${spellbook.name}')">${spellbook.name}</span>"""

    def faction(f : Faction, background : String, unique : Spellbook, uniquePhase : String, uniqueText : String, miscSpellbooks : $[Spellbook], units : $[(UnitClass, Int, String, String, String)]) = s"""
        <table class="faction-table" style="background-image:url(${imageSource(background)})">
            <thead>
                <tr>
                    <th style=width:9%>
                    </th>
                    <th style=width:13%>
                    </th>
                    <th style=width:4%>
                    </th>
                    <th style=width:7%>
                    </th>
                    <th style=width:7%>
                    </th>
                    <th style=width:60%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td colspan=6>
                        <div style="padding-left: 3ex; padding-right: 3ex; padding-bottom: 1ex;">
                            <div class="h1 abaddon">${f.name}</div>
                            <div class="border-outer">
                                <div class="border-inner">
                                    <span class=ability-color>${unique.name}</span>
                                    <span class=cost-color>(${uniquePhase}):</span>
                                    <span>${uniqueText}</span>
                                </div>
                            </div>
                        </div>
                    </td>
                </tr>
                ${
                    if (miscSpellbooks.any) { s"""
                        <tr>
                            <td colspan=6>
                                <div style="padding-left: 3ex; padding-right: 3ex; padding-bottom: 1ex;">
                                    Spellbooks:
                                    ${
                                        miscSpellbooks./{ sb =>
                                            s"""${reference(f, sb)}"""
                                        }.join(", ")
                                    }
                                </div>
                            </td>
                        </tr>"""
                    }
                    else
                        ""
                }
                <tr>
                    <td colspan=2>
                        <div class=h3>Unit<sup><span class="deh3">(Total)</span></sup></div>
                    </td>
                    <td>
                    </td>
                    <td>
                        <div class=h3><span class=cost-color>Cost</span></div>
                    </td>
                    <td>
                        <div class=h3><span class=combat-color>Combat</span></div>
                    </td>
                    <td>
                        <div class=h3>Notes</div>
                    </td>
                </tr>
                <tr>
                    <td colspan=6>
                        <div class="separator">
                        </div>
                    </td>
                </tr>
                ${
                    units./{ case (uc, n, c, b, t) => s"""
                        <tr>
                            <td>
                                <img class="img" src=${imageSource("info:" + f.short.toLowerCase + "-" + uc.name.toLowerCase.replace(" ", "-"))}>
                            </td>
                            <td>
                                <div class="unit-desc">
                                    <div class="p"><span class=unit-name>${uc.name}${(n > 1).??(s"""<sup>(${n})</sup>""")}</div>
                                    <div class="p"><span class=unit-type>${uc.utype.name.replace("GOO", "Great Old One")}</div>
                                </div>
                            </td>
                            <td>
                            </td>
                            <td>
                                <span class=cost-color>${c}</span>
                            </td>
                            <td>
                                <span class=combat-color>${b}</span>
                            </td>
                            <td>
                                <div class="notes">
                                    ${t}
                                </div>
                            </td>
                        </tr>"""
                    }.join("""
                        <tr>
                            <td colspan=6>
                                <div class="separator">
                                </div>
                            </td>
                        </tr>""")
                }
            </tbody>
        </table>"""

}
