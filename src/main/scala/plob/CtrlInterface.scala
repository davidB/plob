package plob


class CtrlInterface {
  case class Parameter[T](name : String, description : String, get : () => T, set : (String) => BasicLogs)

  private var _params = Map[String, Parameter[_]]()

  def add[T](name : String, description : String, get : () => T, set : (String) => BasicLogs) : this.type = {
    _params = _params + (name -> Parameter(name, description, get, set))
    this
  }

  def get(name : String) : Option[_] = _params.get(name).map{ x => x.get() }

  def set(name : String, v : String) : BasicLogs = {
    _params.get(name).map{ x =>
      x.set(v)
    } getOrElse(("unknown parameter : " + name) :: Nil)
  }

  def usageAndValues() : List[String] = (_params.values.toList
    .sortWith(_.name < _.name)
    .map{ x => "%s:\t%s\t#%s".format(x.name, x.get(), x.description)}
  )
}