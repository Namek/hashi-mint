module Misc {
  fun either (whenTrue : a, whenFalse : a, cond : Bool) : a {
    if (cond) {
      whenTrue
    } else {
      whenFalse
    }
  }
}
