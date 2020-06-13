routes {
  / {
    try {
      seed =
        412321

      params =
        {
          width = 8,
          height = 8,
          maxConnectionCount = 2,
          targetIslandCount = 20,
          cycleImprovementPercent = 50,
          increaseConnectionCountsPercent = 30
        }

      Game.initPuzzle(seed, params)
    }
  }
}
