package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import TransitivePatterns.*

/** The transitive enumeration lands exactly on 28 honeycombs over the 26 species, with
  * the two classical doublings; 16 species are resolved by the forcing theorem alone; no pattern cap was hit,
  * fingerprints are radius-stable, and the snub species exhibits the developability gap (patterns passing the
  * local conditions but failing collision-free development) that the acceptance criterion closes.
  */
class TransitivePatternsSpec extends AnyFlatSpec with Matchers:

  private val (reports, flags) = results
  private def label(i: Int)    = SpeciesCorona.label(i)
  private val byLabel          = reports.map(r => label(r.idx) -> r).toMap

  "the enumeration" should "raise no flags" in:
    // pattern caps ARE hit by design (forced species stop at 1; sampled skeletons at 40);
    // the audit closes every capped skeleton by germ forcing or exhaustion
    flags shouldBe empty

  it should "cover the 26 shell-surviving species with at least one honeycomb each" in:
    reports.size shouldBe 26
    reports.filter(_.honeycombs < 1) shouldBe empty

  it should "count EXACTLY 28 uniform honeycombs" in:
    reports.map(_.honeycombs).sum shouldBe 28

  it should "double exactly the two classical gyro/elongated pairs" in:
    val doubled = reports.filter(_.honeycombs == 2).map(r => label(r.idx)).toSet
    doubled shouldBe Set(
      "{cube:4 p3:6}#1",     // elongated + gyroelongated triangular prismatic
      "{tet:4 oct:3 p3:6}#1" // elongated + gyroelongated alternated cubic
    )

  "the forcing theorem" should "resolve 16 species rigorously (single coset everywhere)" in:
    val forced = reports.filter(_.forced)
    forced.size shouldBe 16
    forced.foreach(_.honeycombs shouldBe 1)

  "the fingerprints" should "be stable between radius 2.05 and 3.05 for every species" in:
    reports.filter(!_.stableAtR3) shouldBe empty

  "the developability gap" should "appear exactly at the snub lift and be closed by development" in:
    // one full skeleton of the snub-trihexagonal lift passes R1+R2 but fails collision-free development:
    // without the development check the count would be 29
    val snub = byLabel("{p3:8 p6:2}#2")
    snub.patternsRejected should be > 0
    snub.honeycombs shouldBe 1
    reports.filterNot(_.idx == snub.idx).map(_.patternsRejected).sum shouldBe 0
