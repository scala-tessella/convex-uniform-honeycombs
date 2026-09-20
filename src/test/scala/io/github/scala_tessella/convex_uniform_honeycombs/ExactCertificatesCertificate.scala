package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import ExactCertificates.*

/** Certificate `exact-certificates.txt`. The exact Q(sqrt 2, sqrt 3) re-verification: exact star models,
  * exact periodization certificates, and the exact separation of the two doubled species.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object ExactCertificatesCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val r        = results
    val coh      = coherenceResults
    val sb       = new StringBuilder
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
    sb ++= "cov = R_per >= ub(covBound) + ub(max|tau|) + 3/2 with exact rational upper bounds;\n"
    sb ++= "gen = every generator is an exact symmetry of the ball within R_per (the entry at the\n"
    sb ++= "image position carries the image star up to Stab(S)) — checked, not derived from the\n"
    sb ++= "collision-free development, which expands only the first word reaching a position;\n"
    sb ++= "box = every entry within R_per is the exact Lambda-translate of the entry at its box\n"
    sb ++= "representative (rounded lattice coordinates, verified in the field); closed = every\n"
    sb ++= "generator image of an entry within the slack radius is an entry (the development\n"
    sb ++= "terminated before its depth cap)\n\n"
    sb ++= f"${"species"}%-34s ${"cl"}%2s ${"glu"}%5s ${"R1"}%5s ${"R2"}%5s ${"trans"}%5s " +
      f"${"indep"}%5s ${"ball"}%5s ${"per"}%5s ${"lat"}%5s ${"cov"}%5s ${"gen"}%5s ${"box"}%6s ${"closed"}%6s\n"
    r.classes.foreach { c =>
      sb ++= f"${SpeciesCorona.label(c.idx)}%-34s ${c.classIdx}%2d ${c.gluOk}%5s ${c.r1Ok}%5s " +
        f"${c.r2Ok}%5s ${c.transOk}%5s ${c.indepOk}%5s ${c.ballVerts}%5d ${c.periodic}%5s " +
        f"${c.latInv}%5s ${c.coverage}%5s ${c.genEquiv}%5s ${c.boxPeriodic}%6s ${c.closed}%6s\n"
    }
    sb ++= f"\nexact certificates: ${r.classes.count(_.ok)}/${r.classes.size}\n\n"
    sb ++= "== (c) exact class coherence (every accepted pattern within the caps) ==\n"
    sb ++= "cert = the pattern's own exact periodization certificate, its ball reaching the\n"
    sb ++= "representative's basis and determination ball; align = an element of Stab(S) carrying the\n"
    sb ++= "representative's exact ball onto the pattern's at the determination radius; transport = the\n"
    sb ++= "aligned representative basis acts by symmetries on the pattern's certified ball\n\n"
    sb ++= f"${"species"}%-34s ${"patterns"}%8s ${"cert"}%5s ${"align"}%5s ${"transport"}%9s\n"
    coh.coherences.groupBy(_.idx).toVector.sortBy(_._1).foreach { (idx, cs) =>
      sb ++= f"${SpeciesCorona.label(idx)}%-34s ${cs.size}%8d ${cs.count(_.certified)}%5d " +
        f"${cs.count(_.aligned)}%5d ${cs.count(_.transported)}%9d\n"
    }
    sb ++= f"\nexact coherence: ${coh.coherences.count(_.ok)}/${coh.coherences.size} accepted patterns\n\n"
    sb ++= "== (d) exact germ forcing (every skeleton) ==\n"
    val open     = r.germs.filterNot(_.forced)
    sb ++= s"skeletons forced exactly: ${r.germs.count(_.forced)}/${r.germs.size}; open (closed by\n"
    sb ++= "exhaustion, replayed exactly in (f)): " +
      (if open.isEmpty then "none"
       else open.map(g => s"${SpeciesCorona.label(g.idx)} skeleton ${g.skeleton}").mkString(", ")) + "\n\n"
    sb ++= "== (f) the exhaustions replayed exactly ==\n"
    sb ++= "every (R1)+(R2)-consistent pattern of an open skeleton enumerated uncapped; accepted =\n"
    sb ++= "collision-free to radius 3.05; cohered = exactly cohered with its class representative\n\n"
    sb ++=
      f"${"species"}%-34s ${"skeleton"}%8s ${"patterns"}%8s ${"accepted"}%8s ${"cohered"}%7s ${"capped"}%6s\n"
    coh.exhaustions.foreach { e =>
      sb ++= f"${SpeciesCorona.label(e.idx)}%-34s ${e.skeleton}%8d ${e.patterns}%8d ${e.accepted}%8d " +
        f"${e.cohered}%7d ${e.capped}%6s\n"
    }
    sb ++= f"\nexhaustions closed exactly: ${coh.exhaustions.count(_.ok)}/${coh.exhaustions.size}\n\n"
    sb ++= "== (e) exact class separation (the two doubled species) ==\n"
    r.separations.foreach { s =>
      sb ++= s"${SpeciesCorona.label(s.idx)}: ${s.classes} classes, exact fingerprints distinct: " +
        s"${s.distinct}\n"
    }
    val allFlags = (r.flags ++ coh.flags).distinct
    sb ++= s"\nflags: ${allFlags.size}\n"
    allFlags.foreach(f => sb ++= s"  $f\n")
    sb ++= s"\nALL EXACT: ${r.allOk && coh.allOk}\n\n"
    sb ++= "==> every equality asserted by the periodization certificates of the 28, by the coherence of\n"
    sb ++= "every accepted pattern with its class (within the caps and beyond them) and by the germ\n"
    sb ++= "forcing of every closed skeleton is an exact identity in Q(sqrt2, sqrt3): every positive\n"
    sb ++= "certificate on the completeness theorem's critical path is exact. What stays numeric: the\n"
    sb ++= "enumerations' negative decisions (interval misses in the species assembly, tolerance equality\n"
    sb ++= "tests in the shell filter, the gluing atlas and the R1/R2 search).\n"
    java.nio.file.Files.writeString(dir.resolve("exact-certificates.txt"), sb.toString)
    println(s"stars ${r.stars.count(_.ok)}/${r.stars.size}, classes ${r.classes.count(_.ok)}/" +
      s"${r.classes.size}, separations ${r.separations.count(_.distinct)}/${r.separations.size}, " +
      s"flags ${allFlags.size}, ALL EXACT: ${r.allOk && coh.allOk} -> $dir/exact-certificates.txt")
