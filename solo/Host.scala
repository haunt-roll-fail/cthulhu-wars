package cws

import colmat._

import scala.collection.parallel.CollectionConverters._

object Host {
    def writeLog(s : String) {
    }

    def askFaction(g : Game, c : Continue) : Action = {
        c match {
            case Force(action) =>
                action

            case DelayedContinue(_, c) =>
                askFaction(g, c)

            case RollD6(question, roll) =>
                roll((1::2::3::4::5::6).maxBy(_ => random()))

            case RollBattle(_, n, roll) =>
                roll(List.fill(n)(BattleRoll.roll()))

            case DrawES(_, 0, 0, 0, draw) =>
                draw(0, true)

            case DrawES(_, es1, es2, es3, draw) =>
                draw((List.fill(es1)(1) ++ List.fill(es2)(2) ++ List.fill(es3)(3)).maxBy(_ => random()), false)

            case Ask(_, List(action)) =>
                action

            case Ask(faction, actions) =>
                faction match {
                    case GC => BotGC.ask(g, actions, 0.03)
                    case CC => BotCC.ask(g, actions, 0.03)
                    case BG => Bot3(BG).ask(g, actions, 0.03)
                    case YS => BotYS.ask(g, actions, 0.03)
                    case SL => BotSL.ask(g, actions, 0.03)
                    case WW => BotWW.ask(g, actions, 0.03)
                    case OW => BotOW.ask(g, actions, 0.03)
                    case AN => BotAN.ask(g, actions, 0.03)
                }
        }
    }

    def main(args:Array[String]) {
        val allFactions = List(GC, CC, BG, YS, SL, WW, OW, AN)
        val allComb = allFactions.combinations(4).toList
        val sixComb = allFactions.but(OW).combinations(4).toList
        val factions = List(GC, BG, CC, OW)
        val repeat = 0.to(15).map(_ => factions)

        def allSeatings(factions : List[Faction]) = factions.permutations.toList.%(s => s.contains(GC).?(s(0) == GC).|(s(0) != WW))
        def randomSeating(factions : List[Faction]) = allSeatings(factions).sortBy(s => random()).head

        var results : List[List[Faction]] = Nil

        val base = allComb

        1.to(100).foreach { i =>
            results ++= base.par.map { ff =>
                var log : List[String] = Nil
                def writeLog(s : String) {
                    log = s :: log
                }

                try {
                    val game = new Game(EarthMap4v35, RitualTrack.for4, randomSeating(ff), true, Nil)
                    val (l, cc) = game.perform(StartAction)
                    var c = cc
                    l.foreach(writeLog)
                    var n = 0
                    var k = 1L
                    var f = false
                    while (!c.isInstanceOf[GameOver]) {
                        n += 1
                        val a = askFaction(game, c)

                        val (l, cc) = game.perform(a)
                        c = cc
                        l.foreach(writeLog)


                        k *= (c match {
                            case Ask(_, actions) if actions.num > 1 => min(3, actions.num)
                            case _ => 1
                        })

                        if (game.turn > 1 && !f) {
                            f = true
                        }

                        if (n > 6000)
                            throw null
                    }
                    val w = c.asInstanceOf[GameOver].winners
                    println(w.any.?(w./(_.name).mkString(", ")).|("Humanity") + " won (" + n + ")")
                    w
                }
                catch {
                    case e : Throwable if !false =>
                        println(e)

                        import java.nio.file.{Paths, Files}
                        import java.nio.charset.StandardCharsets

                        Files.write(Paths.get("game-error-" + System.currentTimeMillis + ".txt"), (e.getMessage + "\n" + e.getStackTrace.mkString("\n") + log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")).getBytes(StandardCharsets.UTF_8))
                    Nil
                }
            }

            val wins = results.groupBy(w => w).mapValues(_.size)

            println()

            wins.keys.toList.sortBy(k => wins(k)).reverse.foreach { k =>
                println(k.any.?(k./(_.name).mkString(", ")).|("Humanity") + ": " + wins(k) + " " + "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
            }

            println()

            allFactions.map { f =>
                val ww = wins.filterKeys(_.contains(f))
                val solo = ww.filterKeys(_.size == 1).values.sum
                val tie = ww.filterKeys(_.size > 1).values.sum
                (solo + tie) -> (f.name + ": " + solo + "+" + tie + " " + "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
            }.sortBy(_._1).map(_._2).reverse.foreach(println)

            println("Humanity" + ": " + wins.filterKeys(_.size == 0).values.sum + " " + "%6.0f".format(wins.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            println("Total: " + results.num)
            println()
        }

        import scala.collection.JavaConverters._

        println()
        println("RULES")
        val rules = Stats.rules.asScala.toList.sortBy(r => min(r._2._1, r._2._2))
        rules.foreach { case (k, (on, off)) => println(k + " : " + on + " / " + off) }

        println()
        println("IMPORTANT")
        val important = Stats.important.asScala.toList.sortBy(r => min(r._2._1, r._2._2))
        important.foreach { case (k, (on, off)) => println(k + " : " + on + " / " + off) }

    }
}