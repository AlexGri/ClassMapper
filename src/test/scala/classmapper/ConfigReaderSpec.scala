package classmapper

import org.scalatest.{Matchers, WordSpecLike}

/**
 * Created by alexgri on 23.09.14.
 */
class ConfigReaderSpec extends WordSpecLike with Matchers {

  "config app " must {
    "reload config" in {
      new ClassMapper("cm").remap
    }
  }
}


