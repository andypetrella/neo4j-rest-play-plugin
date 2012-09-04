scalacOptions += "-deprecation"

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0.3")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")