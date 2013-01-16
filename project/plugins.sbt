scalacOptions += "-deprecation"

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

//add clean-cache and clean-local
addSbtPlugin("com.eed3si9n" % "sbt-dirty-money" % "0.0.1")

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.1-RC1")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.7")

//addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.6")