package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species
import StarFoldings.{fold, subgroupsOf, symmetryOf}
import Sigma0Assembly.*

/** The σ₀ assembly. The centerpiece is an INDEPENDENT brute-force oracle
  * — every involution-with-fixed-points on the chamber union, filtered by directly-written axiom formulas
  * (matching equalities, both commutations, face-closure divisibility on every full cycle, connectivity) —
  * which the propagating enumerator must reproduce exactly on small foldings of several shapes. Classical
  * pins: the cubic star's full folding admits EXACTLY σ₀ = id (the regular {4,3,4} symbol closes); octet-c's
  * full folding exactly σ₀ = id (the quasiregular tet-oct symbol — cell-type matching alone forbids the
  * swap); and a two-orbit union of those two full foldings admits NO σ₀ at all (cell types forbid every cross
  * pair, so connectivity kills the assembly) — the connectivity teeth.
  */
class Sigma0AssemblySpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val cubic                  = bySupport("{cube:8}").head
  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse

  private lazy val symCubic  = symmetryOf(cubic)
  private lazy val symOctetC = symmetryOf(octetC)
  private lazy val symOctetH = symmetryOf(octetH)

  private def fullFold(sym: StarFoldings.StarSymmetry) = fold(sym, sym.perms.toSet)

  // ---------- the independent oracle ----------

  /** All involutions on 0 until n, fixed points allowed. */
  private def involutions(n: Int): Vector[Vector[Int]] =
    def gen(pending: Vector[Int], acc: Map[Int, Int]): Vector[Map[Int, Int]] =
      pending match
        case c +: rest =>
          gen(rest, acc + (c -> c)) ++
            rest.flatMap(d => gen(rest.filterNot(_ == d), acc + (c -> d) + (d -> c)))
        case _         => Vector(acc)
    gen(Vector.range(0, n), Map.empty).map(m => Vector.tabulate(n)(m))

  /** The axioms, written directly (no propagation, no incrementality). */
  private def axiomsOk(u: ChamberUnion, s0: Vector[Int]): Boolean =
    val n                     = u.size
    val matching              = (0 until n).forall { c =>
      val d = s0(c)
      u.m01(c) == u.m01(d) && u.m23(c) == u.m23(d) && u.cell(c) == u.cell(d)
    }
    val commute               = (0 until n).forall { c =>
      s0(u.s2(c)) == u.s2(s0(c)) && s0(u.s3(c)) == u.s3(s0(c))
    }
    def cycleLen(c: Int): Int =
      var a = u.s1(s0(c))
      var k = 1
      while a != c do
        a = u.s1(s0(a))
        k += 1
      k
    val faces                 = (0 until n).forall(c => u.m01(c) % cycleLen(c) == 0)
    val conn                  =
      val seen  = collection.mutable.Set(0)
      var front = List(0)
      while front.nonEmpty do
        front = front.flatMap(c => List(s0(c), u.s1(c), u.s2(c), u.s3(c)).filter(seen.add))
      seen.size == n
    matching && commute && faces && conn

  private def bruteSigma0(u: ChamberUnion): Set[Vector[Int]] =
    involutions(u.size).filter(axiomsOk(u, _)).toSet

  // ---------- the tests ----------

  "the σ₀ enumerator" should "equal the brute-force oracle on small foldings of several shapes" in:
    // quotient complexes of 1, 2, 4, 6 and 8 chambers, one- and two-orbit unions
    val cubicSmall = subgroupsOf(symCubic.perms).filter(h => 48 / h.size <= 8).map(fold(symCubic, _))
    val cases      = Vector(
      Vector(fullFold(symCubic)),
      Vector(fullFold(symOctetC)),
      Vector(fullFold(symOctetH)),
      Vector(fullFold(symCubic), fullFold(symOctetC)), // two orbits, no cross-compatible chambers
      Vector(fullFold(symOctetC), fullFold(symOctetH)) // two orbits, cross σ₀ possible (the Barlow substrate)
    ) ++ cubicSmall.map(Vector(_))
    for parts <- cases do
      val u             = unionOf(parts)
      val (found, capd) = enumerateSigma0(u)
      withClue(s"union of sizes ${parts.map(_.size).mkString("+")}: "):
        capd shouldBe false
        found.toSet shouldBe bruteSigma0(u)

  it should "close the regular {4,3,4} symbol: the cubic full folding admits exactly σ₀ = id" in:
    val (found, _) = enumerateSigma0(unionOf(Vector(fullFold(symCubic))))
    found shouldBe Vector(Vector(0))

  it should "close the quasiregular tet-oct symbol: octet-c's full folding admits exactly σ₀ = id" in:
    val (found, _) = enumerateSigma0(unionOf(Vector(fullFold(symOctetC))))
    found shouldBe Vector(Vector(0, 1))

  it should "find at least one symbol on octet-h's full folding (the gyrated honeycomb exists)" in:
    val u          = unionOf(Vector(fullFold(symOctetH)))
    val (found, _) = enumerateSigma0(u)
    found should not be empty
    found.foreach(s0 => axiomsOk(u, s0) shouldBe true)

  it should "reject the disconnected two-orbit union (connectivity teeth)" in:
    val u          = unionOf(Vector(fullFold(symCubic), fullFold(symOctetC)))
    val (found, _) = enumerateSigma0(u)
    found shouldBe empty
