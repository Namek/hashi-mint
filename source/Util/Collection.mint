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
  fun findAndGetRest (predicate : Function(a, Bool), items : Array(a)) : FirstRest(a) {
    iterate([], items)
  } where {
    iterate =
      (checked : Array(a), unchecked : Array(a)) : FirstRest(a) {
        try {
          case (unchecked[0]) {
            Maybe::Nothing => FirstRest::A(Maybe::Nothing, [])

            Maybe::Just first =>
              try {
                rest =
                  Array.drop(1, unchecked)

                if (predicate(first)) {
                  FirstRest::A(Maybe::Just(first), Array.append(checked, rest))
                } else {
                  iterate(Array.push(first, checked), rest)
                }
              }
          }
        }
      }
  }
}

enum FirstRest(a) {
  A(Maybe(a), Array(a))
}
