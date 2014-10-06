package classmapper

import java.io.File

/**
 * Created by alexgri on 25.09.14.
 */
object ClassMapperApp extends App {
  val cm = new File(args(0))
  println(cm)
  val settings = ClassMapperSettings.load(cm)
  settings.map(new ClassMapper().remap(_))
}
