import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.osgi.OsgiKeys._

object Build extends sbt.Build {

  lazy val root = Project("wandou-math-root", file("."))
    .aggregate(wandou_util, wandou_math, wandou_indicator)
    .settings(basicSettings: _*)
    .settings(noPublishing: _*)

  lazy val wandou_util = Project("wandou-util", file("wandou-util"))
    //.settings(defaultOsgiSettings: _*)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(releaseSettings: _*)
    .settings(libraryDependencies ++= (Dependencies.all ++ Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)))

  lazy val wandou_math = Project("wandou-math", file("wandou-math"))
    .dependsOn(wandou_util)
    //.settings(defaultOsgiSettings: _*)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(releaseSettings: _*)
    .settings(libraryDependencies ++= Dependencies.all)

  lazy val wandou_indicator = Project("wandou-indicator", file("wandou-indicator"))
    .dependsOn(wandou_util, wandou_math)
    //.settings(defaultOsgiSettings: _*)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(releaseSettings: _*)
    .settings(libraryDependencies ++= Dependencies.all)



  lazy val basicSettings = Seq(
    organization := "com.wandoulabs.math",
    version := "0.1.1-SNAPSHOT",
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    resolvers ++= Seq(
      "typesafe repo" at "http://repo.typesafe.com/typesafe/releases/",
      "spray" at "http://repo.spray.io",
      "spray nightly" at "http://nightlies.spray.io/"))


  lazy val releaseSettings = Seq(
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { (repo: MavenRepository) => false },
    pomExtra := pomXml)

  lazy val noPublishing = Seq(
    publish := (),
    publishLocal := (),
    // required until these tickets are closed https://github.com/sbt/sbt-pgp/issues/42,
    // https://github.com/sbt/sbt-pgp/issues/36
    publishTo := None)

  lazy val pomXml = (<url>https://github.com/wandoulabs/wandou-math</url>
      <licenses>
        <license>
          <name>Apache License 2.0</name>
          <url>http://www.apache.org/licenses/</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:wandoulabs/wandou-math.git</url>
        <connection>scm:git:git@github.com:wandoulabs/wandou-math.git</connection>
      </scm>
      <developers>
        <developer>
          <id>dcaoyuan</id>
          <name>Caoyuan DENG</name>
          <email>dcaoyuan@gmail.com</email>
        </developer>
      </developers>)

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test := formattingPreferences)

  import scalariform.formatter.preferences._
  def formattingPreferences =
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, false)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(IndentSpaces, 2)

}

object Dependencies {
  val AKKA_VERSION = "2.3.11"

  val scala_xml = "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  val akka_actor = "com.typesafe.akka" %% "akka-actor" % AKKA_VERSION
  val akka_contrib = "com.typesafe.akka" %% "akka-contrib" % AKKA_VERSION
  val akka_testkit = "com.typesafe.akka" %% "akka-testkit" % AKKA_VERSION % Test
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.4" % Test
  val scalaspecs = "org.specs2" %% "specs2-core" % "2.3.13" % Test

  val all = Seq(scala_xml, akka_actor, akka_contrib, scalatest, scalaspecs, akka_testkit)

}
