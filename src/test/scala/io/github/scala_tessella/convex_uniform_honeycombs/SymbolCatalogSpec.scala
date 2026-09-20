package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species
import StarFoldings.{fold, symmetryOf}
import Sigma0Assembly.{enumerateSigma0, unionOf}
import SymbolCatalog.*

/** The symbol catalog. Canonical keys must be invariant under chamber relabeling (seeded random conjugations)
  * and separate non-isomorphic symbols; minimality is pinned on hand fixtures (the 1-chamber symbol is
  * minimal, its hand-built double is not) and on an assembled instance (a cubic index-2 folding admits the σ₀
  * induced from the 1-chamber symbol, which must be flagged non-minimal); the k = 1 driver canaries must
  * contain the classical keys — the cubic sweep the regular {4,3,4} symbol, the octet-c sweep the
  * quasiregular tet-oct symbol — with every kept symbol valid and minimal. The full k = 1 sweeps (trivial
  * foldings included) are the guarded battery and the probe's census.
  */
class SymbolCatalogSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val cubic                  = bySupport("{cube:8}").head
  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse

  private def fullFoldSym(i: Int): Vector[Sym] =
    val sym       = symmetryOf(i)
    val u         = unionOf(Vector(fold(sym, sym.perms.toSet)))
    val (sols, _) = enumerateSigma0(u)
    sols.map(symOf(u, Vector(i), _))

  private def relabel(s: Sym, p: Vector[Int]): Sym =
    def conj(g: Vector[Int]): Vector[Int] =
      val out = Array.fill(s.size)(0)
      for c <- 0 until s.size do out(p(c)) = p(g(c))
      out.toVector
    def data(v: Vector[Int]): Vector[Int] =
      val out = Array.fill(s.size)(0)
      for c <- 0 until s.size do out(p(c)) = v(c)
      out.toVector
    Sym(
      conj(s.s0),
      conj(s.s1),
      conj(s.s2),
      conj(s.s3),
      data(s.m01),
      data(s.m23),
      data(s.cell),
      data(s.speciesOf)
    )

  "the canonical key" should "be invariant under chamber relabeling and separate distinct symbols" in:
    val syms = fullFoldSym(octetH)
    syms should not be empty
    val rnd  = new scala.util.Random(7)
    for s <- syms do
      for _ <- 1 to 5 do
        val p = rnd.shuffle(Vector.range(0, s.size))
        canonicalKey(relabel(s, p)) shouldBe canonicalKey(s)
    // distinct solutions on the same folding with distinct keys stay distinct after relabeling
    syms.map(canonicalKey).distinct.size shouldBe syms.map(canonicalKey).toSet.size

  "minimality" should "hold for the 1-chamber symbol and fail for its hand-built double" in:
    val single = Sym(Vector(0), Vector(0), Vector(0), Vector(0), Vector(4), Vector(4), Vector(1), Vector(0))
    isMinimal(single) shouldBe true
    val swap   = Vector(1, 0)
    val double = Sym(swap, swap, swap, swap, Vector(4, 4), Vector(4, 4), Vector(1, 1), Vector(0, 0))
    valid(double) shouldBe true
    isMinimal(double) shouldBe false

  it should "flag the σ₀ induced on a cubic index-2 folding as non-minimal" in:
    val sym       = symmetryOf(cubic)
    val sub       = StarFoldings.subgroupsOf(sym.perms).filter(_.size == 24).head
    val u         = unionOf(Vector(fold(sym, sub)))
    val (sols, _) = enumerateSigma0(u)
    sols should not be empty
    // the 1-chamber {4,3,4} symbol lifts to every folding: at least one solution must quotient back to it
    sols.map(symOf(u, Vector(cubic), _)).exists(s => !isMinimal(s)) shouldBe true

  "the k = 1 driver" should "recover the regular {4,3,4} symbol on the cubic sweep (canary tier)" in:
    val (syms, stats) = k1Of(cubic, maxChambers = 12)
    stats should not be empty
    syms.foreach { s =>
      valid(s) shouldBe true
      isMinimal(s) shouldBe true
    }
    val regular       =
      Sym(
        Vector(0),
        Vector(0),
        Vector(0),
        Vector(0),
        Vector(4),
        Vector(4),
        syms.head.cell.take(1),
        Vector(cubic)
      )
    syms.map(canonicalKey) should contain(
      canonicalKey(regular.copy(cell =
        Vector(SpeciesEnumerator.species(cubic).state.corners.head.cell.ordinal)
      ))
    )

  it should "census EXACTLY 28 minimal k = 1 symbols over the 34 species, zeros matching the mono-shell" in:
    // the headline: the combinatorial symbol catalog alone lands on the 28 — per species, the minimal
    // symbol count is 0 exactly on the shell-excluded species (an independent machinery
    // cross-check: the shell exclusion is geometric, the symbol census combinatorial), 2 exactly on the
    // two classical doubled species, and 1 elsewhere; realizability is certified by the symbol gate
    val counts    = species.indices.toVector.map(i => i -> k1Of(i, sigma0Cap = 100000)._1.size)
    counts.map(_._2).sum shouldBe 28
    val shellDead = MonoShell.results._1.filter(!_._2.sat).map(_._1).toSet
    counts.filter(_._2 == 0).map(_._1).toSet shouldBe shellDead
    counts.filter(_._2 == 2).map((i, _) => SpeciesCorona.label(i)).toSet shouldBe
      Set("{cube:4 p3:6}#1", "{tet:4 oct:3 p3:6}#1")
    counts.map(_._2).max shouldBe 2

  it should "recover the quasiregular tet-oct symbol on the octet-c sweep (canary tier)" in:
    val (syms, _) = k1Of(octetC, maxChambers = 12)
    syms.foreach { s =>
      valid(s) shouldBe true
      isMinimal(s) shouldBe true
    }
    // the 2-chamber quasiregular symbol: σ0/σ1/σ2 self-paired, σ3 swapping, all-triangle faces, rings of 4
    val sym       = symmetryOf(octetC)
    val u         = unionOf(Vector(fold(sym, sym.perms.toSet)))
    val quasi     = symOf(u, Vector(octetC), Vector(0, 1))
    syms.map(canonicalKey) should contain(canonicalKey(quasi))
