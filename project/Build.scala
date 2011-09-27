// https://github.com/harrah/xsbt/wiki/Full-Configuration-Example
import sbt._
import Keys._

object BuildSettings {
    val buildOrganization = "fosd"
    val buildVersion      = "0.3"
    val buildScalaVersion = "2.9.0-1"

    val buildSettings = Defaults.defaultSettings ++ Seq(
        organization := buildOrganization,
        version      := buildVersion,
        scalaVersion := buildScalaVersion,
        shellPrompt  := ShellPrompt.buildShellPrompt
    )
}

object ShellPrompt {
    object devnull extends ProcessLogger {
        def info(s: => String) { }
        def error(s: => String) { }
        def buffer[T](f: => T): T = f
    }
    def currBranch = (
        ("git status -sb" lines_! devnull headOption)
        getOrElse "-" stripPrefix "## "
    )

    val buildShellPrompt = {
        (state: State) => {
            val currProject = Project.extract (state).currentProject.id
                "%s:%s:%s> ".format (
                currProject, currBranch, BuildSettings.buildVersion
            )
        }
    }
}

object Resolvers {
    val sunrepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
    val sunrepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish" 
    val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"

    val oracleResolvers = Seq (sunrepo, sunrepoGF, oraclerepo)
}

object Dependencies {
    val junit = "junit" % "junit" % "4.8.2" % "test"
    val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test"
    val kiama = "com.googlecode" %% "kiama" % "1.1.0"
    val sat4j = "org.sat4j" % "org.sat4j.core" % "2.3.0"
    val scalacheck = "org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" % "test"
}

object TypeChef extends Build {
    import Resolvers._
    import Dependencies._
    import BuildSettings._

    val defaultDeps = Seq (
        junitInterface,
        scalacheck
    )

    lazy val typechef = Project(
        "TypeChef",
        file("."),
        settings = buildSettings
    ) aggregate (featureexpr, conditionallib, parserexp)

    lazy val featureexpr = Project(
        "FeatureExprLib",
        file("FeatureExprLib"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps ++ Seq(sat4j))
    )

    lazy val conditionallib = Project(
        "ConditionalLib",
        file("ConditionalLib"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps ++ Seq(kiama))
    ) dependsOn(featureexpr)

    lazy val parserexp = Project(
        "ParserFramework",
        file("ParserFramework"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps ++ Seq(kiama))
    ) dependsOn(featureexpr, conditionallib)

    lazy val jcpp = Project(
        "PartialPreprocessor",
        file("PartialPreprocessor"),
        settings = buildSettings
    ) dependsOn(featureexpr)

    lazy val cparser = Project(
        "CParser",
        file("CParser"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps ++ Seq(kiama))
    ) dependsOn(featureexpr, jcpp, parserexp, conditionallib)

    lazy val linuxanalysis = Project(
        "LinuxAnalysis",
        file("LinuxAnalysis"),
        settings = buildSettings
    ) dependsOn(featureexpr, jcpp, cparser, ctypechecker, conditionallib)

    lazy val ctypechecker = Project(
        "CTypeChecker",
        file("CTypeChecker"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps ++ Seq(kiama))
    ) dependsOn(cparser, conditionallib)

    lazy val javaparser = Project(
        "JavaParser",
        file("JavaParser"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps)
    ) dependsOn(featureexpr, parserexp, conditionallib)

    lazy val crewrite = Project(
        "CRewrite",
        file("CRewrite"),
        settings = buildSettings ++ Seq(libraryDependencies ++= defaultDeps ++ Seq(kiama))
    ) dependsOn(cparser, ctypechecker, conditionallib)
}

