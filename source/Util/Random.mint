/* state of randomization */
record Rand {
  a : Number,
  b : Number,
  c : Number,
  d : Number
}

module Random {
  /* const UINTMAX = `Number.MAX_SAFE_INTEGER` */
  const UINTMAX = 4294967295

  fun xorshift (seed : Number) : Number {
    `
    let x = seed[0], y = seed[1], z = seed[2], w = seed[3];
    let t = x;
    t = (t ^ (t << 11)) >>> 0;
    t = (t ^ (t >>> 8)) >>> 0;
    x = y; y = z; z = w;
    w = (w ^ (w >>> 19)) >>> 0;
    w = (w ^ t) >>> 0;
    return w;
    `
  }

  fun createSeed : Rand {
    `Date.now()`
  }

  fun init (seed : Number) : Rand {
    try {
      x =
        `#{seed} % #{UINTMAX}`

      y =
        `#{seed} << #{seed} >>> 0 % #{UINTMAX}`

      z =
        `#{y} * 11 % #{UINTMAX}`

      w =
        `#{x} * #{seed} % #{UINTMAX}`

      {
        a = x,
        b = y,
        c = z,
        d = w
      }
    }
  }

  fun nextInt (rand : Rand) : Tuple(Number, Rand) {
    {newValue, nextRand}
  } where {
    t0 =
      rand.d

    t1 =
      `#{t0} ^ (#{t0} << 13)`

    t2 =
      `#{t1} ^ (#{t1} >> 17)`

    newValue =
      `(#{t2} ^ #{rand.a} ^ (#{rand.a} >> 5)) % #{UINTMAX}`

    nextRand =
      {
        a = newValue,
        b = rand.a,
        c = rand.b,
        d = rand.c
      }
  }

  fun number (min : Number, max : Number, rand : Rand) : Tuple(Number, Rand) {
    {rounded1, rand1}
  } where {
    {randomized, rand1} =
      nextInt(rand)
      |> Debug.log

    randomizedold =
      `Math.random() ` * UINTMAX

    val =
      ((randomized / UINTMAX) * (max - min)) + min

    rounded0 =
      `Math.round(#{val})`

    rounded1 =
      if (rounded0 > max) {
        max
      } else if (rounded0 < min) {
        min
      } else {
        rounded0
      }
  }

  fun index (values : Array(a), rand : Rand) : Tuple(Number, Rand) {
    number(0, Array.size(values) - 1, rand)
  }

  fun indexSafe (values : Array(a), rand : Rand) : Maybe(Tuple(Number, Rand)) {
    if (Array.size(values) == 0) {
      Maybe::Nothing
    } else {
      Maybe::Just(index(values, rand))
    }
  }

  fun choice (values : Array(a), rand : Rand) : Tuple(a, Rand) {
    {el, rand1}
  } where {
    {idx, rand1} =
      index(values, rand)

    el =
      `#{values}[#{idx}]`
  }

  fun choiceOrDefault (defaultValue : a, values : Array(a), rand : Rand) : Tuple(a, Rand) {
    case (values) {
      [] => {defaultValue, rand}
      [x, ...xs] => choiceSafe(x, xs, rand)
    }
  }

  fun choiceSafe (firstValue : a, restValues : Array(a), rand : Rand) : Tuple(a, Rand) {
    if (Array.size(restValues) == 0) {
      {firstValue, rand}
    } else {
      choice((Array.unshift(firstValue, restValues)), rand)
    }
  }

  /* Returns random element and rest of elements. */
  fun choiceAndFilter (values : Array(a), rand : Rand) : Tuple(a, Array(a), Rand) {
    {el, restEls, rand1}
  } where {
    {idx, rand1} =
      index(values, rand)

    el =
      values[idx]

    restEls =
      Array.deleteAt(idx, values)
  }
}
