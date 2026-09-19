package io.github.scala_tessella.convex_uniform_honeycombs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Regenerates the sixteen plain-text certificates the paper cites, into `certs/`.
  *
  * Opt-in (`sbt -Dcerts test`) for one reason only: it writes files. Every claim these certificates record
  * is asserted by the specs of the fast tier, which run on every build without touching the filesystem —
  * the certificates are the human-readable rendering of the same computation, not a second source of truth.
  *
  * Nothing is read back from `certs/`: a certificate is an OUTPUT of this repository, never an input, so a
  * stale or hand-edited file cannot make any spec pass.
  */
class CertificatesSpec extends AnyFlatSpec with Matchers:

  private val dir = Path.of("certs")

  private val emitters: Vector[(String, Path => Unit)] = Vector(
    "edge-figures.txt"            -> EdgeFiguresCertificate.write,
    "outsider-exclusion.txt"      -> OutsiderExclusionCertificate.write,
    "icosahedral-identities.txt"  -> IcosahedralIdentitiesCertificate.write,
    "tail-exclusion.txt"          -> TailExclusionCertificate.write,
    "prism-grid.txt"              -> PrismGridCertificate.write,
    "exotic-lifts.txt"            -> ExoticLiftsCertificate.write,
    "species-supports.txt"        -> SpeciesSupportsCertificate.write,
    "species-table.txt"           -> SpeciesTableCertificate.write,
    "species-corona.txt"          -> SpeciesCoronaCertificate.write,
    "mono-shell.txt"              -> MonoShellCertificate.write,
    "transitive-patterns.txt"     -> TransitivePatternsCertificate.write,
    "completeness-audit.txt"      -> CompletenessAuditCertificate.write,
    "exact-certificates.txt"      -> ExactCertificatesCertificate.write,
    "the-28-identified.txt"       -> The28IdentifiedCertificate.write,
    "symbols-k1.txt"              -> SymbolCensusCertificate.write,
    "symbol-gate.txt"             -> SymbolGateCertificate.write
  )

  "the certificate set" should "regenerate in full, every file non-empty" in:
    assume(OptIn.enabled("certs"))
    // the f-interpolators of the writers format through the default locale: pin it, so a certificate
    // reproduces byte for byte on every machine (a decimal comma is not a different certificate)
    java.util.Locale.setDefault(java.util.Locale.ROOT)
    Files.createDirectories(dir)
    for (name, emit) <- emitters do
      val target = dir.resolve(name)
      Files.deleteIfExists(target)
      emit(dir)
      withClue(s"$name: "):
        Files.exists(target) shouldBe true
        Files.readString(target).linesIterator.size should be > 5
    emitters.map(_._1).distinct.size shouldBe emitters.size
