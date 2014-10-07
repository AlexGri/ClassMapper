package classmapper

import java.io.File

/**
 * классмаппер принимает на вход две строки - путь до файла конфига и путь до папки с классами.
 */
object ClassMapperApp extends App {
  val mappingFile = new File(args(0))
  val classesDir = new File(args(1))
  println(mappingFile)
  println(classesDir)
  //пытаемся загрузить конфиг и прочитать нужные нам настройки
  val settings = ClassMapperSettings.load(mappingFile, classesDir)
  //обновляем конфиг
  settings.map(new ClassMapper().remap(_))
}
