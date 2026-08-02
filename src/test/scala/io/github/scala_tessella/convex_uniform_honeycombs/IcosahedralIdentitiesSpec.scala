package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import IcosahedralIdentities.*

/** The exactness upgrades: the exact ℚ(√5) models are combinatorially right, the certified
  * radicands match the classical values AND the certified intervals, the A5 embedding proves (I1), and the exact
  * ledger proves the 360° edge identity (I2).
  */
class IcosahedralIdentitiesSpec extends AnyFlatSpec with Matchers:

  "the exact icosahedron" should "have 12 vertices, 30 edges, 20 faces" in:
    icosaVertices.size shouldBe 12
    icosaEdges.size shouldBe 30
    icosaFaces.size shouldBe 20

  it should "have dihedral cos² = 5/9 with negative cosine" in:
    icosaDihedral shouldBe (-1, Q5.rat(5, 9))

  "the embedded A5" should "be an antiprism: two pentagon rings, 10 icosa faces as laterals" in:
    a5.ringsArePentagons shouldBe true
    a5.lateralFaces.size shouldBe 10
    a5.lateralEdgesAreIcosaEdges shouldBe true

  it should "prove (I1): its lateral 3·3 dihedral IS the icosahedral dihedral" in:
    a5LateralDihedral shouldBe icosaDihedral

  "the certified radicands" should "match the classical closed forms" in:
    radicandsCertified shouldBe true
    dodecDihedral shouldBe (-1, r1)
    a5BaseLateralDihedral shouldBe (-1, r2)
    icosidodecDihedral shouldBe (-1, r3)

  it should "lie inside the certified intervals" in:
    import CertifiedDihedrals.*
    def value(sign: Int, r2v: Q5): Double = sign * math.sqrt(r2v.toDouble)
    def deg(c: Double): Double            = math.toDegrees(math.acos(c))
    val cases                             = List(
      (List(5, 5, 5), (5, 5), dodecDihedral),
      (antiprismConfig(5), (3, 5), a5BaseLateralDihedral),
      (List(3, 5, 3, 5), (3, 5), icosidodecDihedral),
      (antiprismConfig(5), (3, 3), a5LateralDihedral),
      (List(3, 3, 3, 3, 3), (3, 3), icosaDihedral)
    )
    cases.foreach { (cfg, pair, exact) =>
      val iv = edgeTypesOf(cfg)(pair)
      iv.contains(deg(value(exact._1, exact._2))) shouldBe true
    }

  "the edge identity (I2)" should "verify exactly: dodec + A5(3·5) + icosidodec = 360°" in:
    edgeIdentityExact shouldBe true
    sumInterval.lo should be > 358.0
    sumInterval.hi should be < 362.0
    edgeIdentity shouldBe true
    info(f"certified interval for the sum: [${sumInterval.lo}%.9f, ${sumInterval.hi}%.9f]")
