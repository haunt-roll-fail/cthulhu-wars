package cws

import hrf.colmat._

import java.util.concurrent._

object Stats {
    val rules = new ConcurrentHashMap[String, (Int, Int)]()
    val important = new ConcurrentHashMap[String, (Int, Int)]()

    def triggerR(f : Faction, rule : String, on : Boolean) {
    }

    def triggerI(f : Faction, rule : String, on : Boolean) {
    }
}
