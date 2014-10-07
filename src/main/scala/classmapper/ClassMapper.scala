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

case class ClassMapperSettings(configName:String, mappingConfig:Config, superclassName:Option[String],
                               storedMapping:Map[String, Int], counter:Int, classesDir:File, mappingFile:File)
object ClassMapperSettings {
  val superclassKey = "akka.actor.ecoproto.mapping-interface"
  val classmappingKey = "akka.actor.ecoproto.mappings"
  val minCounterValue = 1001

  def load(cmFile:File, classesDir:File):Try[ClassMapperSettings] = {
    val configName = cmFile.getName//s"$mapperName.conf"

    Try {
      val cfg = ConfigFactory.parseFile(cmFile).resolve()
      //имя суперкласса опционально, если не указано, то в конфиг будут включены все файлы
      val superclassName = Try(cfg.getString(superclassKey)).toOption
      //маппинг - это список, но мне удобнее, если бы он был конфигом
      val mapping = ConfigFactory.parseString(cfg.getList(classmappingKey).get(0).render())
      val storedMapping = configToMap(mapping)
      //добавляем minCounterValue в список, иначе max на пустом списке выбросит исключение
       val counter = (minCounterValue :: storedMapping.values.toList).max + 1
      ClassMapperSettings(configName, cfg, superclassName, storedMapping, counter, classesDir, cmFile)
    }
  }

  def configToMap(c:Config) = c.root().unwrapped().asScala.toMap.mapValues(v => v.toString.toInt)
}
class ClassMapper {
  val renderOpts = ConfigRenderOptions.defaults().setOriginComments(false)

  def updateMapping(cms:ClassMapperSettings):Map[String, Int] = {
    //находим класс-файлы для маппинга
    val actualClassNames = findMappableClassNames(cms.classesDir, cms.superclassName)
    val storedClassNames = cms.storedMapping.keySet

    //новые=найденные - те что есть
    val newClassNames = actualClassNames.diff(storedClassNames)

    //назначаем им айдишники
    val newClassNamesMapping = newClassNames.zipWithIndex.map { case (clazz, index) => (clazz, index + cms.counter)}.toMap
    val removedClassNames = storedClassNames.diff(actualClassNames)

    //оставшиеся = те что есть - удаленные
    val oldMapping = cms.storedMapping -- removedClassNames
    oldMapping ++ newClassNamesMapping
  }

  def remap(cms:ClassMapperSettings) = {
    val mapping = updateMapping(cms)
    //mapping.foreach(println)
    val javaMapping = mapping.mapValues(v=>v: java.lang.Integer).asJava
    val newConfig = cms.mappingConfig//.withOnlyPath(cms.configName)
      .withValue(ClassMapperSettings.classmappingKey, ConfigValueFactory.fromMap(javaMapping))
    println(newConfig.root().render())
    replaceConfig(cms.mappingFile, newConfig)
  }


  def loaderFor(path:File):URLClassLoader = URLClassLoader.newInstance(Array(path.toURI.toURL))

  def findMappableClassNames(path:File, superclassName:Option[String]):Set[String] = {
    val names = findFiles(path, ".class")
    //если нужно проверять, что классы унаследованы
    superclassName.map {superclazzName =>
      val loader = loaderFor(path)
      //пытаемся загрузить суперкласс
      val superclass = Try(loader.loadClass(superclazzName))
      val isRightClass = superclass match {
        case Success(s) => (clazz: Class[_]) => clazz != s && s.isAssignableFrom(clazz)//суперкласс находится в класспафе, проверяем рефлекшном
        case _ => (clazz: Class[_]) => clazz.getInterfaces.toList.map(_.getName).contains(superclazzName)//проверяем только по имени
      }

      names.map(loader.loadClass(_)).collect { case clazz if isRightClass(clazz) => clazz.getName}.toSet
    }.getOrElse(names)
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

  def replaceConfig(path:File, newConfig:Config) = {
    val pathForNew = Paths.get(path.toURI)
    val format = new SimpleDateFormat("yyyyddMMmmssss")
    val date = format.format(new Date)
    val pathForOld = Paths.get(new File(path.getParent, s"${path.getName}$date").toURI)
    Files.copy(pathForNew, pathForOld)
    //нехороший хак, не нашел способ сохранять массив с указанием, что он может быть смерджен в другом конфиге.
    Files.write(pathForNew, newConfig.root().render(renderOpts).replaceFirst(""""mappings"\s*:""", """"mappings" += """).getBytes)
  }
}