module Util.Collection {
  fun updateFirst (
    predicate : Function(a, Bool),
    update : Function(a, a),
    collection : Array(a)
  ) : Array(a) {
    for (el of collection) {
      if (predicate(el)) {
        update(el)
      } else {
        el
      }
    }
  }

  fun repeat (el : a, count : Number) : Array(a) {
    `Array(#{count}).fill(#{el})`
  }

  fun zip (
    zipFn : Function(a, b, z),
    els1 : Array(a),
    els2 : Array(b)
  ) : Array(z) {
    try {
      len1 =
        Array.size(els1)

      len2 =
        Array.size(els2)

      len =
        Math.min(len1, len2)

      `(() => {
        const ret = [];
        for (let i = 0, n = #{len}; i < n; ++i) {
          ret.push(#{zipFn}(#{els1}[i], #{els2}[i]))
        }
        return ret;
      })()`
    }
  }

  /* Find an element and return it along with the rest of the input items. */
  fun findAndGetRest (predicate : Function(a, Bool), items : Array(a)) : Tuple(Maybe(a), Array(a)) {
    iterate([], items)
  } where {
    iterate =
      (checked : Array(a), unchecked : Array(a)) : Tuple(Maybe(a), Array(a)) {
        try {
          case (unchecked) {
            [] => {Maybe::Nothing, checked}

            [first, ...rest] =>
              if (predicate(first)) {
                {Maybe::Just(first), Array.append(Array.reverse(checked), rest)}
              } else {
                iterate(Array.push(first, checked), rest)
              }
          }
        }
      }
  }

  fun distinct (elToKey : Function(v, String), arr : Array(v)) : Array(v) {
    arr
    |> Array.reduce(
      Map.empty(),
      (acc : Map(String, v), el : v) {
        try {
          key =
            elToKey(el)

          if (Map.has(key, acc)) {
            acc
          } else {
            acc
            |> Map.set(key, el)
          }
        }
      })
    |> Map.values()
  }
}
