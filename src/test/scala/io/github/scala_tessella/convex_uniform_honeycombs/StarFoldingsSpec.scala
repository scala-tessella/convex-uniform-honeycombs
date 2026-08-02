package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species
import StarFoldings.*

/** The folding layer. The chamber action must be a faithful group of
  * flag-complex automorphisms for every species; the subgroup enumeration must agree with brute force on the
  * small stabilizers; every fold must be a well-defined chamber complex with constant m-data on orbits; and
  * the classical pins must land — the full folding of the cubic star is the 1-chamber regular {4,3,4} symbol
  * (m01 = 4, m23 = 4, all σ self-paired) and of the octet-c star the 2-chamber quasiregular tet-oct symbol
  * (σ1, σ2 self-paired, σ3 swapping the tet- and oct-side chambers, all faces triangles, rings of size 4).
  */
class StarFoldingsSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val cubic                  = bySupport("{cube:8}").head
  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse

  private def idPerm(n: Int): Perm = Vector.tabulate(n)(identity)

  private lazy val symCubic  = symmetryOf(cubic)
  private lazy val subsCubic = subgroupsOf(symCubic.perms)

  "the chamber action" should "be a faithful automorphism group of every species' flag complex" in:
    for i <- species.indices do
      val sym = symmetryOf(i)
      val n   = sym.cx.chambers.size
      withClue(s"species ${SpeciesCorona.label(i)}: "):
        // faithful: one distinct permutation per stabilizer matrix, identity included
        sym.perms.distinct.size shouldBe sym.perms.size
        sym.perms should contain(idPerm(n))
        // a group: closed under composition
        val set = sym.perms.toSet
        for a <- sym.perms; b <- sym.perms do set should contain(b.map(a))
        // automorphisms: every symmetry commutes with the three involutions
        for p <- sym.perms; c <- 0 until n do
          p(sym.cx.s1(c)) shouldBe sym.cx.s1(p(c))
          p(sym.cx.s2(c)) shouldBe sym.cx.s2(p(c))
          p(sym.cx.s3(c)) shouldBe sym.cx.s3(p(c))

  "the subgroup enumeration" should "agree with brute force on the small stabilizers" in:
    // brute force: every subset of the group closed under composition (finite => subgroup)
    for i <- species.indices.filter(j => symmetryOf(j).perms.size <= 8).take(6) do
      val sym   = symmetryOf(i)
      val g     = sym.perms
      val brute = (0 until (1 << g.size))
        .map(mask => g.indices.filter(j => (mask & (1 << j)) != 0).map(g).toSet)
        .filter(h =>
          h.nonEmpty && h.forall(a => h.forall(b => h.contains(b.map(a))))
        )
        .toSet
      withClue(s"species ${SpeciesCorona.label(i)} (|G| = ${g.size}): "):
        subgroupsOf(g).toSet shouldBe brute

  it should "satisfy Lagrange and conjugation-closure on the cubic star's order-48 group" in:
    val sym                             = symCubic
    val subs                            = subsCubic
    subs.map(_.size).distinct.sorted.foreach(48 % _ shouldBe 0)
    subs should contain(Set(idPerm(sym.cx.chambers.size)))
    subs.map(_.size).max shouldBe 48
    // conjugation-closure: g H g⁻¹ is again an enumerated subgroup (a completeness check of the BFS)
    def compose(a: Perm, b: Perm): Perm = b.map(a)
    def invert(p: Perm): Perm           =
      val inv = Array.fill(p.size)(0)
      for x <- p.indices do inv(p(x)) = x
      inv.toVector
    val set                             = subs.toSet
    for h <- subs; g <- sym.perms do
      set should contain(h.map(x => compose(compose(g, x), invert(g))))

  "the folds" should "be well-defined chamber complexes with constant m-data, for every cubic subgroup" in:
    val sym = symCubic
    for sub <- subsCubic do
      val f = fold(sym, sub)
      withClue(s"|H| = ${sub.size}: "):
        f.size shouldBe sym.cx.chambers.size / sub.size
        for s <- Vector(f.s1, f.s2, f.s3); c <- 0 until f.size do s(s(c)) shouldBe c
        // well-defined: the induced maps agree with the projection on EVERY original chamber
        for c <- sym.cx.chambers.indices do
          f.orbitOf(sym.cx.s1(c)) shouldBe f.s1(f.orbitOf(c))
          f.orbitOf(sym.cx.s2(c)) shouldBe f.s2(f.orbitOf(c))
          f.orbitOf(sym.cx.s3(c)) shouldBe f.s3(f.orbitOf(c))
          f.m01(f.orbitOf(c)) shouldBe sym.cx.faceSize(c)
          f.m23(f.orbitOf(c)) shouldBe sym.ringSize(sym.cx.endVertex(c))

  it should "fold the cubic star fully to the 1-chamber regular {4,3,4} symbol" in:
    val sym = symCubic
    val f   = fold(sym, sym.perms.toSet) // the full subgroup is the whole group
    f.size shouldBe 1
    (f.s1(0), f.s2(0), f.s3(0)) shouldBe (0, 0, 0)
    f.m01(0) shouldBe 4
    f.m23(0) shouldBe 4

  it should "fold the octet-c star fully to the 2-chamber quasiregular tet-oct symbol" in:
    val sym = symmetryOf(octetC)
    sym.perms.size shouldBe 48
    val f   = fold(sym, sym.perms.toSet) // the full subgroup is the whole group
    f.size shouldBe 2
    // σ1 and σ2 stay within a cell type; σ3 crosses between the tet- and oct-side chambers
    f.s1 shouldBe Vector(0, 1)
    f.s2 shouldBe Vector(0, 1)
    f.s3 shouldBe Vector(1, 0)
    f.m01 shouldBe Vector(3, 3)
    f.m23 shouldBe Vector(4, 4)
