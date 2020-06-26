routes {
  /puzzle/:seed/:width/:height/:maxConnectionCount/:targetIslandCount/:cycleImprovementPercent/:increaseConnectionCountsPercent (seed : Number, width : Number, height : Number, maxConnectionCount : Number, targetIslandCount : Number, cycleImprovementPercent : Number, increaseConnectionCountsPercent : Number) {
    sequence {
      params =
        {
          width = width,
          height = height,
          maxConnectionCount = maxConnectionCount,
          targetIslandCount = targetIslandCount,
          cycleImprovementPercent = cycleImprovementPercent,
          increaseConnectionCountsPercent = increaseConnectionCountsPercent
        }

      App.setState(seed, params, UrlChange::Nothing)
    }
  }

  /puzzle/:seed/:width/:height (seed : Number, width : Number, height : Number) {
    sequence {
      params =
        {
          width = width,
          height = height,
          maxConnectionCount = 2,
          targetIslandCount = (width + height) * 2,
          cycleImprovementPercent = 100,
          increaseConnectionCountsPercent = 20
        }

      App.setState(seed, params, UrlChange::Replace)
    }
  }

  /puzzle/:seed (seed : Number) {
    sequence {
      params =
        {
          width = 7,
          height = 10,
          maxConnectionCount = 2,
          targetIslandCount = 26,
          cycleImprovementPercent = 100,
          increaseConnectionCountsPercent = 20
        }

      App.setState(seed, params, UrlChange::Replace)
    }
  }

  * {
    sequence {
      seed =
        `Math.floor(Math.random() * 100000)`

      params =
        {
          width = 7,
          height = 10,
          maxConnectionCount = 2,
          targetIslandCount = 26,
          cycleImprovementPercent = 100,
          increaseConnectionCountsPercent = 20
        }

      App.setState(seed, params, UrlChange::PushNew)
    }
  }
}
