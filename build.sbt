// convex-uniform-honeycombs — the machine-checked verification artifact for the paper
//   "The 28 convex uniform honeycombs: a completeness theorem".
// It contains the paper's claim specs and the k = 1 modules that carry its own results — the Alphabet
// Theorem's exclusion campaigns, the exact Q(sqrt2, sqrt3) certificates and the classical identification.
// Every shared engine — the cell alphabet, certified dihedrals, the species assembly, the shell filter, the
// pattern/development engine, the audit certificates and the Delaney-Dress symbol side of a vertex star —
// is the pinned research-core library. The specs live in package
// io.github.scala_tessella.convex_uniform_honeycombs and import the library from
// io.github.scala_tessella.research_core.
//
// `sbt test` runs the whole paper: the alphabet, the species table, the shell filter, the pattern
// enumeration, the audit, the exact certificates, the identification and the symbol appendix, all
// re-derived from scratch in about a minute, exact and in-JVM, with no external tools and no fixtures.
// Certificate emission is the one opt-in (-Dcerts), because it writes files.

ThisBuild / scalaVersion  := "3.9.0"
ThisBuild / organization  := "io.github.scala-tessella"
ThisBuild / versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .settings(
    name           := "convex-uniform-honeycombs",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "io.github.scala-tessella" %% "research-core"   % "0.12.0",
      "org.scalatest"            %% "scalatest"       % "3.2.20"   % Test,
      "org.scalacheck"           %% "scalacheck"      % "1.20.0"   % Test,
      "org.scalatestplus"        %% "scalacheck-1-19" % "3.2.20.0" % Test
    )
    // Deliberately NOT forked: the opt-in run is selected by a system property (-Dcerts), which a forked
    // test JVM would not inherit.
  )
