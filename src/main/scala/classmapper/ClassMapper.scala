package classmapper

import java.io.File
import java.net.URI
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class ClassMapper(mapperName:String) {
  val configName = s"$mapperName.conf"
  val mappingConfig = ConfigFactory.load(configName)

  val superclassKey = s"$mapperName.interface"
  val superclassName = mappingConfig.getString(superclassKey)

  val classmappingKey = s"$mapperName.classmapping"

  val counterKey = s"$mapperName.counter"
  val counter = mappingConfig.getInt(counterKey)

  def remap = {
    val actualClassNames = findMappableClassNames
    val storedMapping = configToMap(mappingConfig.getConfig(classmappingKey))
    val storedClassNames = storedMapping.keySet
    val newClassNames = actualClassNames.diff(storedClassNames)
    val newClassNamesMapping = newClassNames.zipWithIndex.map { case (clazz, index) => (clazz, index + counter)}.toMap
    val newCounter = counter + newClassNamesMapping.size
    val removedClassNames = storedClassNames.diff(actualClassNames)
    val oldMapping = storedMapping -- removedClassNames
    val mapping = oldMapping ++ newClassNamesMapping
    //mapping.foreach(println)
    val javaMapping: java.util.Map[String, _ <: AnyRef] = mapping.map { case (k, v) => (k, v: java.lang.Integer)}.asJava
    val newConfig = mappingConfig.withOnlyPath(mapperName)
      .withValue(counterKey, ConfigValueFactory.fromAnyRef(newCounter))
      .withValue(classmappingKey, ConfigValueFactory.fromMap(javaMapping))
    println(newConfig.root().render())
    replaceConfig(mappingConfig, newConfig, configName)
  }

  def configToMap(c:Config) = c.root().unwrapped().asScala.toMap.mapValues(v => v.toString.toInt)

  def findClassNames = {
    val url = getClass.getResource("/")
    val root = url.getFile
    findFiles(new File(root), ".class")
  }

  def findMappableClassNames:Set[String] = {
    val names = findClassNames
    val loader = getClass.getClassLoader
    val superclass = loader.loadClass(superclassName)

    def isRightClass(name: String) = {
      val clazz = loader.loadClass(name)
      clazz != superclass && superclass.isAssignableFrom(clazz)
    }
    names.collect { case name if isRightClass(name) => name}.toSet
  }

  def findFiles(directory:File, postfix:String):List[String] = {
    @tailrec
    def findFilesRec(uncheckedDirs:List[(String, File)], found:List[String]):List[String] = {
      if (uncheckedDirs.isEmpty) found
      else {
        val (packagePath, dir) = uncheckedDirs.head
        //val currentPackagePath = s"$packagePath${dir.getName}."
        val (directories, files) =  dir.listFiles.partition(_.isDirectory)
        val foundFiles = files.filter(_.getName.endsWith(postfix)).map(file => s"$packagePath${trimPostfix(file.getName, postfix.length)}")
        findFilesRec(uncheckedDirs.tail ++ directories.map(d => (s"$packagePath${d.getName}.", d)), found ++ foundFiles)
      }
    }
    def trimPostfix(name:String, postfixSize:Int) = {
      name.substring(0, name.length - postfixSize)
    }

    findFilesRec(List(("", directory)), List.empty)
  }

  def replaceConfig(oldConfig:Config, newConfig:Config, name:String) = {
    val configDir = getClass.getResource("/")
    val pathForNew = Paths.get(new URI(s"$configDir$name"))
    val format = new SimpleDateFormat("yyyyddMMmmssss")
    val date = format.format(new Date)
    val pathForOld = Paths.get(new URI(s"$configDir${name}$date"))
    Files.copy(pathForNew, pathForOld)
    Files.write(pathForNew, newConfig.root().render().getBytes)
  }
}