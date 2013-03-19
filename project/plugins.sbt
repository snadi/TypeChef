resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.3")

addCompilerPlugin("org.scala-tools.sxr" % "sxr_2.9.0" % "0.2.7")

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

publishTo := Some(Resolver.file("file",  new File(Path.userHome+"/.ivy2/repository")))
