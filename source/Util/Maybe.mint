module Maybe.Extra {
  /*
  Maps the value of a maybe with a possibility to discard it.

    Maybe::Just(4)
    |> Maybe.andThen((num : Number) : Maybe(String) {
      if (num > 4) {
        Maybe::Just(Number.toString(num))
      }
      else {
        Maybe::Nothing
      }
    })
  */
  fun andThen (transform : Function(a, Maybe(b)), maybe : Maybe(a)) : Maybe(b) {
    case (maybe) {
      Maybe::Just value => transform(value)
      Maybe::Nothing => Maybe::Nothing
    }
  }

  fun values (maybes : Array(Maybe(a))) : Array(a) {
    Array.compact(maybes)
  }
}
