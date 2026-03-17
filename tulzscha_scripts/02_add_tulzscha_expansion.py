#!/usr/bin/env python3
"""Script 2: Add Tulzscha logic to IGOOsExpansion in IGOOs.scala"""
import sys, os

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
path = os.path.join(sdir, "IGOOs.scala")

with open(path, "r") as f:
    content = f.read()

# 1. eliminate handler — insert before "case _ =>" at end of eliminate()
old = '''            case Nyogtha =>
                f.units = f.units.%!(_.uclass == Nyogtha)
                f.upgrades :-= FromBelow
                f.upgrades :-= NightmareWeb
                f.loyaltyCards :-= NyogthaCard
                game.loyaltyCards :+= NyogthaCard

            case _ =>'''
new = old.replace(
    "            case _ =>",
    """
            case Tulzscha =>
                f.units :-= u
                f.upgrades :-= CeremonyOfAnnihilation
                f.loyaltyCards :-= TulzschaCard
                game.loyaltyCards :+= TulzschaCard

            case _ =>"""
)
if old not in content:
    print("ERROR: Could not find Nyogtha eliminate block")
    print("Nearby content:"); idx = content.find("case Nyogtha if"); print(repr(content[max(0,idx-50):idx+400]))
    sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added Tulzscha eliminate handler")

# 2. triggers() — add checkTulzschaUndyingFlame
old = '''    override def triggers()(implicit game : Game) {
        checkAbhothSpellbook()
        checkInterdimensional()
    }'''
new = '''    def checkTulzschaUndyingFlame()(implicit game : Game) {
        factions.foreach { f =>
            if (f.has(Tulzscha) && game.isAfterGatherPower) {
                val others = factions.but(f)
                if (others.exists(_.doom > f.doom)) {
                    f.doom += 1
                    f.log(Tulzscha.styled(f), "gained 1 Doom from", "Undying Flame".styled(f))
                }
                if (others.exists(_.es > f.es)) {
                    f.takeES(1)
                    f.log(Tulzscha.styled(f), "gained 1 Elder Sign from", "Undying Flame".styled(f))
                }
                if (others.exists(_.power > f.power)) {
                    f.power += 1
                    f.log(Tulzscha.styled(f), "gained 1 Power from", "Undying Flame".styled(f))
                }
            }
        }
    }

    override def triggers()(implicit game : Game) {
        checkAbhothSpellbook()
        checkInterdimensional()
        checkTulzschaUndyingFlame()
    }'''
if old not in content:
    print("ERROR: Could not find triggers() block"); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added checkTulzschaUndyingFlame trigger")

# 3. Awaken handler — grant CeremonyOfAnnihilation on awaken
old = '''                case Daoloth =>
                    self.upgrades :+= CosmicUnity

                case Nyogtha =>'''
new = '''                case Daoloth =>
                    self.upgrades :+= CosmicUnity

                case Tulzscha =>
                    self.upgrades :+= CeremonyOfAnnihilation

                case Nyogtha =>'''
if old not in content:
    print("ERROR: Could not find Daoloth/Nyogtha awaken cases"); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added Tulzscha awaken case")

# 4. Add TulzschaGivePower action classes after NightmareWebAction
old = 'case class NightmareWebAction(self : Faction, r : Region) extends BaseFactionAction(g => "Awaken " + Nyogtha.styled(self) + g.forNPowerWithTax(r, self, 2) + " in", implicit g => r + self.iced(r))'
new = old + '''

case class TulzschaGivePowerMainAction(self : Faction) extends OptionFactionAction("Give each enemy 2 Power (Tulzscha SBR)".styled(self)) with MainQuestion with Soft
case class TulzschaGivePowerAction(self : Faction) extends BaseFactionAction(g => "Give each enemy 2 Power for " + Tulzscha.styled(self) + " Spellbook Requirement")'''
if old not in content:
    print("ERROR: Could not find NightmareWebAction class"); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added TulzschaGivePower action classes")

# 5. perform() handlers before "// ..."
old = '''        // ...
        case _ => UnknownContinue
    }
}'''
new = '''        // TULZSCHA
        case TulzschaGivePowerMainAction(self) =>
            Ask(self).option(TulzschaGivePowerAction(self)).cancel

        case TulzschaGivePowerAction(self) =>
            self.enemies.foreach { f =>
                f.power += 2
                log(f, "gained 2 Power from", Tulzscha.styled(self), "Spellbook Requirement")
            }

            if (self.upgrades.has(CeremonyOfAnnihilation).not) {
                self.upgrades :+= CeremonyOfAnnihilation
                self.log("gained", CeremonyOfAnnihilation.styled(self), "for", Tulzscha.styled(self))
            }

            EndAction(self)

        // ...
        case _ => UnknownContinue
    }
}'''
if old not in content:
    print("ERROR: Could not find end of perform() block"); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added Tulzscha perform() handlers")

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated.")
