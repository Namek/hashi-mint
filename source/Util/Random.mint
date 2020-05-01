/* state of randomization */
record Rand {
  state : Number,
  inc : Number
}

/* [Permuted Congruential Generators][http://www.pcg-random.org/] by M. E. O'Neil. */
module Random {
  const MAX_INT = 2147483647

  fun createSeed : Rand {
    try {
      num1 =
        `Math.floor(Math.random() * 100000)`

      num2 =
        `Math.floor(Math.random() * 100000)`

      seed(num1, num2)
    }
  }

  fun seed (seedStateInitializer : Number, streamId : Number) : Rand {
    {
      state = seedStateInitializer,
      /* ensure that streamId is odd */
      inc = `Math.abs(#{streamId} % #{MAX_INT}) | 1`
    }
  }

  fun seedFromString (str : String) : Rand {
    seed(num1, num2)
  } where {
    num1 =
      xmur3(str) % MAX_INT

    num2 =
      xmur3(str + str) % MAX_INT
  }

  fun xmur3 (str : String) : Number {
    `(() => {
      let str = #{str}
      for (var i = 0, h = 1779033703 ^ str.length; i < str.length; i++)
        h = Math.imul(h ^ str.charCodeAt(i), 3432918353),
        h = h << 13 | h >>> 19;

      h = Math.imul(h ^ h >>> 16, 2246822507);
      h = Math.imul(h ^ h >>> 13, 3266489909);

      return (h ^= h >>> 16) >>> 0;
    })()
    `
  }

  fun init (seed : Rand) : Rand {
    try {
      streamId =
        seed.inc

      s0 =
        {
          state = 0,
          inc = `(#{streamId} << 1) | 1`
        }

      {num0, s1} =
        nextInt(s0)

      s2 =
        { s1 | state = s1.state + seed.state }

      {num1, s3} =
        nextInt(s2)

      s3
    }
  }

  fun nextInt (rand : Rand) : Tuple(Number, Rand) {
    {newValue, nextRand}
  } where {
    oldState =
      rand.state

    oldInc =
      rand.inc

    newState =
      `(#{oldState} * 1664525 + #{oldInc}) >>> 0`

    newValue =
      peel(rand)

    nextRand =
      { rand | state = newState }
  }

  fun next (rand : Rand) : Rand {
    try {
      oldState =
        rand.state

      oldInc =
        rand.inc

      newState =
        `(#{oldState} * 1664525 + #{oldInc}) >>> 0`

      { rand | state = newState }
    }
  }

  fun peel (rand : Rand) : Number {
    try {
      oldState =
        rand.state

      word =
        `Math.imul(#{oldState} ^ (#{oldState} >>> ((#{oldState} >>> 28) + 4)), 277803737)`

      `((#{word} >>> 22) % #{word}) >>> 0`
    }
  }

  /* Restrict number 'r' to bound, where 0 <= r < bound */
  fun bounded (bound : Number, rand : Rand) : Tuple(Number, Rand) {
    iter(rand)
  } where {
    threshold =
      `((-#{bound} >>> 0) % #{bound}) >>> 0`

    /* (MAX_INT - bound) % bound */
    iter =
      (rand1 : Rand) : Tuple(Number, Rand) {
        try {
          {num, rand2} =
            nextInt(rand1)

          if (num >= threshold) {
            {num % bound, rand2}
          } else {
            iter(rand2)
          }
        }
      }
  }

  fun number (min : Number, max : Number, rand : Rand) : Tuple(Number, Rand) {
    {val, rand1}
  } where {
    range =
      max - min

    {randomizedInt, rand1} =
      rand
      |> bounded(range + 1)

    val =
      randomizedInt + min
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