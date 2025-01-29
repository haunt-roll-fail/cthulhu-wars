package cws

import hrf.colmat._

import java.util.concurrent._

object Stats {
    val rules = new ConcurrentHashMap[String, (Int, Int)]()
    val important = new ConcurrentHashMap[String, (Int, Int)]()

    def triggerR(f : Faction, rule : String, on : Boolean) {
        if (on)
            rules.compute(f.short + ":" + rule, (_, n) => if (n == null) (1, 0) else (n._1 + 1, n._2))
        else
            rules.compute(f.short + ":" + rule, (_, n) => if (n == null) (0, 1) else (n._1, n._2 + 1))

        import scala.collection.convert.decorateAsScala._
    }

    def triggerI(f : Faction, rule : String, on : Boolean) {
        if (on)
            important.compute(f.short + ":" + rule, (_, n) => if (n == null) (1, 0) else (n._1 + 1, n._2))
        else
            important.compute(f.short + ":" + rule, (_, n) => if (n == null) (0, 1) else (n._1, n._2 + 1))
    }
}
