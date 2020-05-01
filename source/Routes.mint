routes {
  / {
    try {
      seed =
        412321

      Game.initPuzzle(seed, 8, 8)
    }
  }
}
