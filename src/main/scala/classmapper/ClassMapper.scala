package classmapper

import java.io.File
import java.net.URLClassLoader
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date

import com.typesafe.config._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Success, Try}

case class ClassMapperSettings(configName:String, mappingConfig:Config, superclassName:String,
                               classmappingKey:String, storedMapping:Map[String, Int], counter:Int, dir:File)
object ClassMapperSettings {
  def load(path:String, mapperName:String):Try[ClassMapperSettings] = load(new File(path), mapperName)
  def load(directory:File, mapperName:String):Try[ClassMapperSettings] = {
    val configName = s"$mapperName.conf"
    val superclassKey = s"$mapperName.interface"
    val classmappingKey = s"$mapperName.classmapping"

    Try {
      val cfg = ConfigFactory.parseFile(new File(directory, configName)).resolve()
      val superclassName = cfg.getString(superclassKey)
      val mapping = ConfigFactory.parseString(cfg.getList(classmappingKey).get(0).render())
      val storedMapping = configToMap(mapping)
      ClassMapperSettings(configName, cfg, superclassName, classmappingKey, storedMapping, storedMapping.values.max + 1, directory)
    }
  }
  def configToMap(c:Config) = c.root().unwrapped().asScala.toMap.mapValues(v => v.toString.toInt)
}
class ClassMapper {
  val renderOpts = ConfigRenderOptions.defaults().setOriginComments(false)

  def updateMapping(cms:ClassMapperSettings):Map[String, Int] = {
    val actualClassNames = findMappableClassNames(cms.dir, cms.superclassName)
    val storedClassNames = cms.storedMapping.keySet
    val newClassNames = actualClassNames.diff(storedClassNames)
    val newClassNamesMapping = newClassNames.zipWithIndex.map { case (clazz, index) => (clazz, index + cms.counter)}.toMap
    val removedClassNames = storedClassNames.diff(actualClassNames)
    val oldMapping = cms.storedMapping -- removedClassNames
    oldMapping ++ newClassNamesMapping
  }

  def remap(cms:ClassMapperSettings) = {
    val mapping = updateMapping(cms)
    //mapping.foreach(println)
    val javaMapping = mapping.mapValues(v=>v: java.lang.Integer).asJava
    val newConfig = cms.mappingConfig.withOnlyPath(cms.configName)
      .withValue(cms.classmappingKey, ConfigValueFactory.fromMap(javaMapping))
    println(newConfig.root().render())
    replaceConfig(cms.dir, cms.configName, newConfig)
  }


  def loaderFor(path:File):URLClassLoader = URLClassLoader.newInstance(Array(path.toURI.toURL))

  def findMappableClassNames(path:File, superclazzName:String):Set[String] = {
    val names = findFiles(path, ".class")
    val loader = loaderFor(path)
    val superclass = Try(loader.loadClass(superclazzName))
    val isRightClass = superclass match {
      case Success(s) => (clazz:Class[_]) => clazz != s && s.isAssignableFrom(clazz)
      case _ => (clazz:Class[_]) => clazz.getInterfaces.toList.map(_.getName).contains(superclazzName)
    }

    names.map(loader.loadClass(_)).collect { case clazz if isRightClass(clazz) => clazz.getName}.toSet
  }

  def findFiles(directory:File, postfix:String):Set[String] = {
    @tailrec
    def findFilesRec(uncheckedDirs:List[(String, File)], found:Set[String]):Set[String] = {
      if (uncheckedDirs.isEmpty) found
      else {
        val (packagePath, dir) = uncheckedDirs.head
        //val currentPackagePath = s"$packagePath${dir.getName}."
        val (directories, files) =  Option(dir.listFiles).toList.flatten.partition(_.isDirectory)
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
    Files.write(pathForNew, newConfig.root().render(renderOpts).replaceFirst(""""classmapping"\s*:""", """"classmapping" += """).getBytes)
  }
}