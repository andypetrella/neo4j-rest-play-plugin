scalacOptions += "-deprecation"

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

//for sbt-unique-version
resolvers += Resolver.url("sbt-plugin-releases", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0.3")

addSbtPlugin("com.eed3si9n" % "sbt-unique-version" % "latest.integration")
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")
