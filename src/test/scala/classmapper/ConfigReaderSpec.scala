package classmapper

import java.io.File

import org.scalatest.{Matchers, WordSpecLike}

/**
 * Created by alexgri on 23.09.14.
 */
class ConfigReaderSpec extends WordSpecLike with Matchers {
  val mapper = new ClassMapper("cm")
  val path = new File("/home/alex/IdeaProjects/ClassMapper/unmanaged")
  val configName = "cm"
  val d1 = "org.me.Dependent1"
  val d2 = "org.me.Dependent2"
  val i = "org.me.Independent"
  val t = "org.me.JustTrait"
  "config app " must {
    "find classes" in {
      val found = mapper.findClassNames(path)
      found shouldBe Set(d1, d2, i, t)
    }
    "be able to load unmanaged classes" in {
      val loader = mapper.loaderFor(path)
      loader shouldNot be(null)
      val found = mapper.findMappableClassNames(path, t)
      found shouldBe Set(d1, d2)
    }
    "correctly load config" in {
      val config = ClassMapperSettings.load(path, configName)
      config.configName shouldBe "cm.conf"
      config.counter shouldBe 3
      config.superclassName shouldBe t
      config.storedMapping shouldBe Map(d1 -> 1, "org.Test2" -> 2)
    }
    "update mapping" in {
      val config = ClassMapperSettings.load(path, configName)
      val updated = mapper.updateMapping(path, config)
      updated shouldBe Map(d1 -> 1, d2 -> 3)
    }
    "replace config" in {
      val config = ClassMapperSettings.load(path, configName)
      mapper.replaceConfig(path, "empty.conf", config.mappingConfig)

    }
  }
}


