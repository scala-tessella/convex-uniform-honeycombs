package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CompletenessAudit.*

/** The completeness audit closes every gap — all 28 classes carry periodization
  * certificates, every accepted pattern coheres with its class representative, and every skeleton is closed
  * by germ forcing or exhaustion. With the Alphabet Theorem, the species table, the shell filter and the
  * pattern enumeration this is the completeness theorem for the 28.
  */
class CompletenessAuditSpec extends AnyFlatSpec with Matchers:

  private val (audits, flags) = results
  private def label(i: Int)   = SpeciesCorona.label(i)

  "the audit" should "raise no flags" in:
    flags shouldBe empty

  it should "certify all 28 classes across the 26 species" in:
    audits.size shouldBe 26
    audits.map(_.classes).sum shouldBe 28
    audits.foreach(a => withClue(label(a.idx))(a.certified shouldBe a.classes))

  it should "verify class coherence everywhere" in:
    audits.filterNot(_.coherent) shouldBe empty

  it should "close every skeleton by germ forcing or exhaustion" in:
    audits.foreach(a => withClue(label(a.idx))(a.forcingSkeletons shouldBe a.skeletonsWithPatterns))

  it should "conclude: every species audit passes — the completeness theorem holds" in:
    audits.forall(_.ok) shouldBe true

  "the doubled species" should "carry exactly two certified classes each" in:
    val doubled = audits.filter(_.classes == 2).map(a => label(a.idx)).toSet
    doubled shouldBe Set("{cube:4 p3:6}#1", "{tet:4 oct:3 p3:6}#1")

  "the snub lift" should "have all three skeletons closed (the capped one by exhaustion)" in:
    val snub = audits.find(a => label(a.idx) == "{p3:8 p6:2}#2").get
    snub.skeletonsWithPatterns shouldBe 3
    snub.forcingSkeletons shouldBe 3
