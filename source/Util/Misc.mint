module Misc {
  fun either (whenTrue : a, whenFalse : a, cond : Bool) : a {
    if (cond) {
      whenTrue
    } else {
      whenFalse
    }
  }

  fun log (text : String, value : a) : a {
    try {
      log =
        `console.log(#{text}, #{value})`

      value
    }
  }

  fun watch (text : String, tap : Function(a, b), value : a) : a {
    try {
      l =
        log(text, tap(value))

      value
    }
  }

  fun logIf (text : String, cond : Bool, value : a) : a {
    if (cond) {
      try {
        a =
          log(text, value)

        value
      }
    } else {
      value
    }
  }

  fun logWhen (text : String, tap : Function(a, Maybe(b)), value : a) : a {
    try {
      t0 =
        tap(value)

      t1 =
        case (t0) {
          Maybe::Just val => log(text, val)

          => Maybe::Nothing
        }

      value
    }
  }

  fun watchMeNoErr (val : Maybe(a)) : Maybe(a) {
    if (Maybe.isJust(val)) {
      val
    } else {
      try {
        a =
          log("error", val)

        val
      }
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
