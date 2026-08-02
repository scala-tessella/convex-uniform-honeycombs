package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import HoneycombAlphabet.CellType.*
import SpeciesCorona.*

/** The corona analysis — the face-cycle fixpoint keeps all 34 (with a teeth check on
  * the path machinery), the live sub-catalogue is 58 of 69, the self-hosting lemma is witnessed, and the
  * adjacency graph has the pinned structure (9 self-only species = the classical unique stars, components 23
  * + 2 + 9x1).
  */
class SpeciesCoronaSpec extends AnyFlatSpec with Matchers:

  private val ca = analysis
  private val sp = SpeciesEnumerator.species

  "the face-cycle fixpoint" should "keep all 34 species (no round kills)" in:
    ca.survivors shouldBe sp.indices.toVector
    ca.killedByRound shouldBe empty

  it should "run on a path test with genuine teeth" in:
    // exact-length reachability: rel a->b only; length 1 reaches, length 2 dead-ends
    val fa: Form = Vector((0, 3, 3))
    val fb: Form = Vector((1, 3, 3))
    pathExists(Map(fa -> Set(fb)), fa, fb, 1) shouldBe true
    pathExists(Map(fa -> Set(fb)), fa, fb, 2) shouldBe false
    // bouncing on a symmetric pair closes even lengths only: the parity that makes triangles the killers
    val sym      = Map(fa -> Set(fb), fb -> Set(fa))
    pathExists(sym, fa, fa, 2) shouldBe true
    pathExists(sym, fa, fa, 3) shouldBe false

  "the live sub-catalogue" should "be exactly 58 of the 69 edge figures" in:
    ca.figuresUsed.size shouldBe 58
    val dead = HoneycombAlphabet.catalogue.filterNot(f => ca.figuresUsed(f.key)).map(_.show)
    dead.size shouldBe 11
    dead should contain("[rco(4·4) tco(4·8) p8(8·4)]")
    dead should contain("[truncTet(6·6) truncTet(6·6) truncOct(6·6) truncOct(6·6)]")

  "hosting" should "witness the self-hosting lemma (pair-level corona is vacuous)" in:
    sp.indices.foreach { i =>
      sp(i).figures.foreach((f, _) => ca.hosting(f) should contain(i))
    }

  "the adjacency graph" should "be symmetric with distinct labels" in:
    sp.indices.foreach { i =>
      ca.adjacency(i).foreach(j => ca.adjacency(j) should contain(i))
    }
    sp.indices.map(label).distinct.size shouldBe sp.size

  it should "isolate exactly the nine classical unique stars (k >= 2 excludes them)" in:
    val selfOnly = sp.indices.filter(i => ca.adjacency(i).forall(_ == i)).map(i => sp(i).counts).toSet
    selfOnly shouldBe Set(
      Map(TruncOct           -> 4), // bitruncated cubic
      Map(TruncCuboctahedron -> 2, P8            -> 2), // omnitruncated cubic
      Map(Cube               -> 1, TruncOct      -> 1, TruncCuboctahedron  -> 2), // cantitruncated cubic
      Map(TruncTet           -> 1, TruncCube     -> 1, TruncCuboctahedron  -> 2), // runcicantic cubic
      Map(Cube               -> 1, TruncCube     -> 1, Rhombicuboctahedron -> 1, P8 -> 2), // runcitruncated cubic
      Map(Oct                -> 1, TruncCube     -> 4), // truncated cubic
      Map(TruncTet           -> 2, Cuboctahedron -> 1, TruncOct            -> 2), // cantic cubic
      Map(Cube               -> 2, P8            -> 4), // truncated square prismatic
      Map(Oct                -> 2, Cuboctahedron -> 4) // rectified cubic
    )

  it should "split into one 23-component, one 2-component and nine singletons" in:
    val seen  = collection.mutable.Set.empty[Int]
    val sizes = sp.indices.flatMap { i =>
      if seen(i) then None
      else
        var comp     = Set(i)
        var frontier = Set(i)
        while frontier.nonEmpty do
          frontier = frontier.flatMap(ca.adjacency(_)).diff(comp)
          comp ++= frontier
        seen ++= comp
        Some(comp.size)
    }
    sizes.sorted shouldBe Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 23)

  it should "join the two Barlow species (the stackings' 2-uniformity substrate)" in:
    val octet = sp.indices.filter(i => sp(i).counts == Map(Tet -> 8, Oct -> 6))
    ca.adjacency(octet(0)) should contain(octet(1))
