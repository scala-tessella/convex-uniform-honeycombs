package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species
import SymbolCatalog.{canonicalKey, k1Of}
import SymbolRealization.derivedSymbolsOf

/** THE k = 1 SYMBOL GATE. For every shell-passing species, the minimal
  * symbols DERIVED from the certified honeycombs (vertex stabilizer from the certified development ball,
  * σ₀ read off the certified pattern through descriptor-rigidity pullback) must equal, key for key, the
  * minimal symbols of the combinatorial census. With the census total pinned at 28 and the derived side built
  * from the certified 28, key-set equality certifies both directions at once: every census symbol is realized
  * by a certified honeycomb, and every certified honeycomb's minimal symbol is in the census. The canary tier
  * gates three species shapes (forced regular, forced quasiregular, and a doubled multi-coset species); the
  * full 26-species gate is the complete theorem-grade check.
  */
class SymbolGateSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val cubic                  = bySupport("{cube:8}").head
  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse
  private lazy val elongTri               = bySupport("{cube:4 p3:6}").head

  private def gate(i: Int): Unit =
    val flags   = MonoShell.Flags()
    val derived = derivedSymbolsOf(i, flags).map(canonicalKey).toSet
    val census  = k1Of(i, sigma0Cap = 100000)._1.map(canonicalKey).toSet
    withClue(s"${SpeciesCorona.label(i)}: "):
      derived shouldBe census
      flags.items.distinct shouldBe empty

  "the k = 1 oracle gate" should "hold on the canary species (regular, quasiregular, doubled)" in:
    gate(cubic)
    gate(octetC)
    gate(elongTri)

  it should "hold key-for-key on ALL 26 shell-passing species, totalling the 28" in:
    val flags = MonoShell.Flags()
    val sat   = MonoShell.results._1.filter(_._2.sat).map(_._1)
    sat.size shouldBe 26
    var total = 0
    for i <- sat do
      val derived = derivedSymbolsOf(i, flags).map(canonicalKey).toSet
      val census  = k1Of(i, sigma0Cap = 100000)._1.map(canonicalKey).toSet
      withClue(s"${SpeciesCorona.label(i)}: ")(derived shouldBe census)
      total += derived.size
    total shouldBe 28
    flags.items.distinct shouldBe empty
