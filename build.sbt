name := "EbTreeSynchroSimulation"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"




libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.0" % "test"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.10" % "2.2.4"

libraryDependencies += "com.typesafe.akka" % "akka-testkit_2.10" % "2.2.4"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.2.4"

libraryDependencies += "org.scalafx" % "scalafx_2.10" % "2.2.60-R9"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.9"

lazy val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"


javaHome := {
  var s = System.getenv("JAVA_HOME")
  if (s==null) {
    // tbd. try to detect JDK location on multiple platforms
    //
    // OS X: "/Library/Java/JavaVirtualMachines/jdk1.xxx.jdk/Contents/Home" with greatest id (i.e. "7.0_10")
    //
    s= "/Library/Java/JavaVirtualMachines/jdk1.7.0_10.jdk/Contents/Home"
  }
  //
  val dir = new File(s)
  if (!dir.exists) {
    throw new RuntimeException( "No JDK found - try setting 'JAVA_HOME'." )
  }
  //
  Some(dir)  // 'sbt' 'javaHome' value is ': Option[java.io.File]'
}

unmanagedJars in Compile <+= javaHome map { jh /*: Option[File]*/ =>
  val dir: File = jh.getOrElse(null)    // unSome
//
val jfxJar = new File(dir, "/jre/lib/jfxrt.jar")
  if (!jfxJar.exists) {
    throw new RuntimeException( "JavaFX not detected (needs Java runtime 7u06 or later): "+ jfxJar.getPath )  // '.getPath' = full filename
  }
  Attributed.blank(jfxJar)
}