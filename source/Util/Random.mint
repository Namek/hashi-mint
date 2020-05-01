/* state of randomization */
record Rand {
  state : Number,
  inc : Number
}

/* [Permuted Congruential Generators][http://www.pcg-random.org/] by M. E. O'Neil. */
module Random {
  const MAX_INT = 2147483647

  fun initRandomly : Rand {
    try {
      num =
        `Math.floor(Math.random() * 100000)`

      init(num)
    }
  }

  fun init (seedStateInitializer : Number) : Rand {
    try {
      /* taken from Numerical Recipes */
      streamId =
        1013904223

      s0 =
        {
          state = 0,
          inc = `(#{streamId} << 1) | 1`
        }

      {num0, s1} =
        nextInt(s0)

      s2 =
        { s1 | state = s1.state + seedStateInitializer }

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
