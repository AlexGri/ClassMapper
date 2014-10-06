package classmapper

import java.io.File

import org.scalatest.{Matchers, WordSpecLike}

/**
 * Created by alexgri on 23.09.14.
 */
class ConfigReaderSpec extends WordSpecLike with Matchers {
  val mapper = new ClassMapper()
  val path = new File("./unmanaged/").getAbsoluteFile
  val configName = "real.conf"
  val classMappingFile = new File(s"./unmanaged/$configName")
  val d1 = "org.me.Dependent1"
  val d2 = "org.me.Dependent2"
  val i = "org.me.Independent"
  val t = "org.me.JustTrait"
  "config app " must {
    "find classes" in {
      val found = mapper.findFiles(path, ".class")
      found shouldBe Set(d1, d2, i, t)
    }
    "be able to load unmanaged classes" in {
      val loader = mapper.loaderFor(path)
      loader shouldNot be(null)
      val found = mapper.findMappableClassNames(path, Some(t))
      found shouldBe Set(d1, d2)
    }
    "be able to load unmanaged classes woth no superclass defined" in {
      val found = mapper.findMappableClassNames(path, None)
      found shouldBe Set(d1, d2, i, t)
    }
    "correctly load config" in {
      val config = ClassMapperSettings.load(classMappingFile, path).get
      config.configName shouldBe "real.conf"
      config.counter shouldBe 1009
      config.superclassName shouldBe Some(t)
      config.storedMapping shouldBe Map(d1 -> 1005, "org.Test2" -> 1008)
    }
    "update mapping" in {
      val config = ClassMapperSettings.load(classMappingFile, path).get
      val updated = mapper.updateMapping(config)
      updated shouldBe Map(d1 -> 1005, d2 -> 1009)
    }
   /* "replace config" in {
      val config = ClassMapperSettings.load(classMappingFile, path).get
      mapper.replaceConfig(path, config.mappingConfig)

    }*/

    "work correctly with incorrect dir" in {
      mapper.findFiles(new File(""), ".class") shouldBe Set.empty
      mapper.findMappableClassNames(new File(""), Some(""))
    }
  }
}


