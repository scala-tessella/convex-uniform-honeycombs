package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import ExactCertificates.*

/** Certificate `exact-certificates.txt`. The exact Q(sqrt 2, sqrt 3) re-verification: exact star models, exact periodization certificates, and the
  * exact separation of the two doubled species.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object ExactCertificatesCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val r   = results
    val sb  = new StringBuilder
    sb ++= "EXACT CERTIFICATES — the certificates of the completeness\n"
    sb ++= "theorem upgraded to exact arithmetic in Q(sqrt2, sqrt3), the two-tier field of the programme.\n"
    sb ++= "All data in the INTERNAL BASIS of three star directions (nothing is ever normalized):\n"
    sb ++= "Gram matrices recognized as a + b*sqrt2 + c*sqrt3 + d*sqrt6 and certified inside the\n"
    sb ++= "assembly's interval enclosures; coordinates, stabilizers, gluings, translations, balls\n"
    sb ++= "derived and verified by exact field identities.\n\n"
    sb ++= "== (a) the 34 exact star models ==\n"
    sb ++= f"${"species"}%-34s ${"n"}%3s ${"exact"}%5s ${"max iv width"}%13s\n"
    r.stars.foreach { s =>
      val w = if s.maxIvWidth.isNaN then "-" else f"${s.maxIvWidth}%.2e"
      sb ++= f"${SpeciesCorona.label(s.idx)}%-34s ${s.n}%3d ${s.ok}%5s ${w}%13s"
      sb ++= (if s.msg.nonEmpty then s"  ${s.msg}\n" else "\n")
    }
    sb ++= f"\nexact stars: ${r.stars.count(_.ok)}/${r.stars.size}\n\n"
    sb ++= "== (b) the 28 exact periodization certificates ==\n"
    sb ++= "glu = every pattern gluing G3-orthogonal + back-vertex + exact ring agreement;\n"
    sb ++= "R1/R2 = reverse pairs and face-cycle words exact stabilizer elements (faces: zero\n"
    sb ++= "translation, exactly); trans = three words with rotational part exactly the identity;\n"
    sb ++= "indep = det(tau1,tau2,tau3) != 0; ball = exact development, collision-free; per =\n"
    sb ++= "Lambda-periodicity exact; lat = generator point parts fix Lambda (integer coords);\n"
    sb ++= "cov = R_per >= ub(covBound) + ub(max|tau|) + 3/2 with exact rational upper bounds\n\n"
    sb ++= f"${"species"}%-34s ${"cl"}%2s ${"glu"}%5s ${"R1"}%5s ${"R2"}%5s ${"trans"}%5s " +
      f"${"indep"}%5s ${"ball"}%5s ${"per"}%5s ${"lat"}%5s ${"cov"}%5s\n"
    r.classes.foreach { c =>
      sb ++= f"${SpeciesCorona.label(c.idx)}%-34s ${c.classIdx}%2d ${c.gluOk}%5s ${c.r1Ok}%5s " +
        f"${c.r2Ok}%5s ${c.transOk}%5s ${c.indepOk}%5s ${c.ballVerts}%5d ${c.periodic}%5s " +
        f"${c.latInv}%5s ${c.coverage}%5s\n"
    }
    sb ++= f"\nexact certificates: ${r.classes.count(_.ok)}/${r.classes.size}\n\n"
    sb ++= "== (c) exact class separation (the two doubled species) ==\n"
    r.separations.foreach { s =>
      sb ++= s"${SpeciesCorona.label(s.idx)}: ${s.classes} classes, exact fingerprints distinct: " +
        s"${s.distinct}\n"
    }
    sb ++= s"\nflags: ${r.flags.size}\n"
    r.flags.foreach(f => sb ++= s"  $f\n")
    sb ++= s"\nALL EXACT: ${r.allOk}\n\n"
    sb ++= "==> every equality asserted by the periodization certificates of the 28 is an exact\n"
    sb ++= "identity in Q(sqrt2, sqrt3); no floating-point tolerance decision remains on the\n"
    sb ++= "completeness theorem's critical path.\n"
    java.nio.file.Files.writeString(dir.resolve("exact-certificates.txt"), sb.toString)
    println(s"stars ${r.stars.count(_.ok)}/${r.stars.size}, classes ${r.classes.count(_.ok)}/" +
      s"${r.classes.size}, separations ${r.separations.count(_.distinct)}/${r.separations.size}, " +
      s"flags ${r.flags.size}, ALL EXACT: ${r.allOk} -> $dir/exact-certificates.txt")
