package classmapper

import java.io.File

/**
 * Created by alexgri on 25.09.14.
 */
object ClassMapperApp extends App {
  val mappingFile = new File(args(0))
  val classesDir = new File(args(1))
  println(mappingFile)
  println(classesDir)
  val settings = ClassMapperSettings.load(mappingFile, classesDir)
  settings.map(new ClassMapper().remap(_))
}
