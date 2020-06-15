routes {
  / {
    try {
      seed =
        6103

      params =
        {
          width = 7,
          height = 10,
          maxConnectionCount = 2,
          targetIslandCount = 26,
          cycleImprovementPercent = 100,
          increaseConnectionCountsPercent = 20
        }

      Game.initPuzzle(seed, params)
    }
  }
}
