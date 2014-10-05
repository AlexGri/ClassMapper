package classmapper

import java.io.File
import java.net.{URL, URLClassLoader, URI}
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
case class ClassMapperSettings(configName:String, mappingConfig:Config, superclassName:String,
                               classmappingKey:String, storedMapping:Map[String, Int], counter:Int)
object ClassMapperSettings {
  def load(path:String, mapperName:String):ClassMapperSettings = load(new File(path), mapperName)
  def load(directory:File, mapperName:String):ClassMapperSettings = {
    val configName = s"$mapperName.conf"
    val cfg = ConfigFactory.parseFile(new File(directory, configName))
    val superclassKey = s"$mapperName.interface"
    val superclassName = cfg.getString(superclassKey)
    val classmappingKey = s"$mapperName.classmapping"
    val storedMapping = configToMap(cfg.getConfig(classmappingKey))
    ClassMapperSettings(configName, cfg, superclassName, classmappingKey, storedMapping, storedMapping.values.max + 1)
  }
  def configToMap(c:Config) = c.root().unwrapped().asScala.toMap.mapValues(v => v.toString.toInt)
}
class ClassMapper(mapperName:String) {
  lazy val settings = ClassMapperSettings.load("", "")

  def updateMapping(path:File, cms:ClassMapperSettings):Map[String, Int] = {
    val actualClassNames = findMappableClassNames(path, cms.superclassName)
    val storedClassNames = cms.storedMapping.keySet
    val newClassNames = actualClassNames.diff(storedClassNames)
    val newClassNamesMapping = newClassNames.zipWithIndex.map { case (clazz, index) => (clazz, index + cms.counter)}.toMap
    val removedClassNames = storedClassNames.diff(actualClassNames)
    val oldMapping = cms.storedMapping -- removedClassNames
    oldMapping ++ newClassNamesMapping
  }

  def remap(path:File, cms:ClassMapperSettings) = {
    val mapping = updateMapping(path, cms)
    //mapping.foreach(println)
    val javaMapping = mapping.mapValues(v=>v: java.lang.Integer).asJava
    val newConfig = cms.mappingConfig.withOnlyPath(mapperName)
      .withValue(cms.classmappingKey, ConfigValueFactory.fromMap(javaMapping))
    println(newConfig.root().render())
    replaceConfig(path, cms.configName, newConfig)
  }



  def findClassNames(path:File):Set[String] = {
    /*val url = getClass.getResource("/")
    val root = url.getFile*/
    findFiles(path, ".class")
  }

  def loaderFor(path:File):URLClassLoader = URLClassLoader.newInstance(Array(path.toURI.toURL))

  def findMappableClassNames(path:File, superclazzName:String):Set[String] = {
    val names = findClassNames(path)
    val loader = loaderFor(path)
    val superclass = loader.loadClass(superclazzName)

    def isRightClass(name: String) = {
      val clazz = loader.loadClass(name)
      clazz != superclass && superclass.isAssignableFrom(clazz)
    }
    names.collect { case name if isRightClass(name) => name}.toSet
  }

  def findFiles(directory:File, postfix:String):Set[String] = {
    @tailrec
    def findFilesRec(uncheckedDirs:List[(String, File)], found:Set[String]):Set[String] = {
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

    findFilesRec(List(("", directory)), Set.empty)
  }

  def replaceConfig(path:File, name:String, newConfig:Config) = {
    val pathForNew = Paths.get(new File(path, name).toURI)
    val format = new SimpleDateFormat("yyyyddMMmmssss")
    val date = format.format(new Date)
    val pathForOld = Paths.get(new File(path, s"$name$date").toURI)
    Files.copy(pathForNew, pathForOld)
    Files.write(pathForNew, newConfig.root().render().getBytes)
  }
}