package classmapper

/**
 * Created by alexgri on 25.09.14.
 */
object ClassMapperApp extends App {
  val path = args(0)
  val name = args(1)
  val settings = ClassMapperSettings.load(path, name)
  settings.map(new ClassMapper().remap(_))
}
