routes {
  / {
    try {
      seed =
        34662

      params =
        {
          width = 9,
          height = 8,
          maxConnectionCount = 2,
          targetIslandCount = 26,
          cycleImprovementPercent = 50,
          increaseConnectionCountsPercent = 20
        }

      Game.initPuzzle(seed, params)
    }
  }
}
