record Bridge {
  connectionCount : Number,
  orientation : Orientation,
  idx1 : Number,
  idx2 : Number
}

module Generation {
  fun generatePuzzle (seed : Number, width : Number, height : Number) : Puzzle {
    {
      islands = islands,
      connections =
        {
          list = [],
          fieldss = Map.empty()
        },
      width = width,
      height = height,
      maxConnectionCount = 0
    }
  } where {
    maxConnectionCount =
      2

    bridges =
      []

    islands =
      bridgesToIslands(width, bridges)
  }

  fun sumConnectionSizes (conn1 : ConnectionSizes, conn2 : ConnectionSizes) : ConnectionSizes {
    {
      top = conn1.top + conn2.top,
      right = conn1.right + conn2.right,
      bottom = conn1.bottom + conn2.bottom,
      left = conn1.left + conn2.left
    }
  }

  fun directionToConnectionSizes (count : Number, dir : Direction) : ConnectionSizes {
    {
      top =
        if (dir == Direction::Up) {
          count
        } else {
          0
        },
      right =
        if (dir == Direction::Right) {
          count
        } else {
          0
        },
      bottom =
        if (dir == Direction::Down) {
          count
        } else {
          0
        },
      left =
        if (dir == Direction::Left) {
          count
        } else {
          0
        }
    }
  }

  fun bridgesToIslands (width : Number, bridges : Array(Bridge)) : Islands {
    addNextBridge(
      bridges,
      {
        list = [],
        fields = Map.empty()
      })
  } where {
    increaseIsland =
      (
        index : Number,
        connectionSizesToAdd : ConnectionSizes,
        islands : Islands
      ) : Islands {
        case (Map.get(index, islands.fields)) {
          Maybe::Just islandExists =>
            { islands |
              list =
                islands.list
                |> Util.Collection.updateFirst(
                  ((island : Island) : Bool { island.index == index }),
                  ((island : Island) : Island {
                    { island |
                      maxConnectionCounts =
                        sumConnectionSizes(
                          island.maxConnectionCounts,
                          connectionSizesToAdd)
                    }
                  }))
            }

          Maybe::Nothing =>
            {
              list =
                Array.push(
                  {
                    index = index,
                    maxConnectionCounts = connectionSizesToAdd,
                    currentConnectionCounts =
                      {
                        top = 0,
                        right = 0,
                        bottom = 0,
                        left = 0
                      }
                  },
                  islands.list),
              fields = Map.set(index, true, islands.fields)
            }
        }
      }

    addNextBridge =
      (leftBridges : Array(Bridge), acc : Islands) : Islands {
        case (leftBridges) {
          [] => acc

          =>
            case (leftBridges[0]) {
              Maybe::Nothing => acc

              Maybe::Just bridge =>
                try {
                  dir1 =
                    Model.directionFromIsland(width, bridge.idx1, bridge.idx2)

                  dir2 =
                    Model.oppositeDirection(dir1)

                  sizes1 =
                    directionToConnectionSizes(bridge.connectionCount, dir1)

                  sizes2 =
                    directionToConnectionSizes(bridge.connectionCount, dir2)

                  newAcc =
                    increaseIsland(bridge.idx1, sizes1, acc)
                    |> increaseIsland(bridge.idx2, sizes2)

                  restBridges =
                    Array.drop(1, leftBridges)

                  addNextBridge(restBridges, newAcc)
                }
            }
        }
      }
  }

  fun brg (
    connectionCount : Number,
    orientation : Orientation,
    idx1 : Number,
    idx2 : Number
  ) : Bridge {
    {
      connectionCount = connectionCount,
      orientation = orientation,
      idx1 = idx1,
      idx2 = idx2
    }
  }

  fun puzzle1 : Puzzle {
    {
      islands = bridgesToIslands(width, bridges),
      connections =
        {
          list = [],
          fieldss = Map.empty()
        },
      width = width,
      height = height,
      maxConnectionCount = 2
    }
  } where {
    width =
      4

    height =
      5

    bridges =
      [
        brg(2, Orientation::Horizontal, 0, 3),
        brg(1, Orientation::Vertical, 0, 16),
        brg(1, Orientation::Vertical, 3, 11),
        brg(1, Orientation::Horizontal, 9, 11),
        brg(1, Orientation::Horizontal, 11, 19),
        brg(1, Orientation::Horizontal, 16, 19)
      ]
  }
}
