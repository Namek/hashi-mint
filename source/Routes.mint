routes {
  / {
    try {
      seed =
        Random.seed(412321, 213210)

      Game.initPuzzle(seed, 8, 8)
    }
  }
}
