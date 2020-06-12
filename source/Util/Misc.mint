module Misc {
  fun either (whenTrue : a, whenFalse : a, cond : Bool) : a {
    if (cond) {
      whenTrue
    } else {
      whenFalse
    }
  }
}

module Bools {
  fun andThen (whenTrue : Function(Bool), cond : Bool) : Bool {
    if (cond) {
      whenTrue()
    } else {
      false
    }
  }

  fun resolve (whenTrue : Function(a), defaultValue : a, cond : Bool) : a {
    if (cond) {
      whenTrue()
    } else {
      defaultValue
    }
  }

  fun toMaybe (cond : Bool, evaluate : Function(a, b), value : a) : Maybe(b) {
    if (cond) {
      Maybe::Just(evaluate(value))
    } else {
      Maybe::Nothing
    }
  }
}
