record Puzzle {
  islands : Islands,
  connections : Connections,
  width : Number,
  height : Number,
  maxConnectionCount : Number
}

record Island {
  index : Number,
  maxConnectionCounts : ConnectionSizes,
  currentConnectionCounts : ConnectionSizes
}

record Islands {
  list : Array(Island),
  fields : Map(Number, Bool)
}

record Connection {
  idx1 : Number,
  idx2 : Number,
  connectionSize : Number,
  orientation : Orientation
}

record Connections {
  list : Array(Connection),
  fieldss : Map(Number, IndexPair)
}

record IndexPair {
  idx1 : Number,
  idx2 : Number
}

record ConnectionSizes {
  top : Number,
  right : Number,
  bottom : Number,
  left : Number
}

enum TimeDirection {
  Forward
  Backward
}

record Vec2 {
  x : Number,
  y : Number
}

enum Orientation {
  Vertical
  Horizontal
}

enum Direction {
  Up
  Right
  Down
  Left
}

module Model {
  fun xyIdx (width : Number, x : Number, y : Number) : Number {
    y * width + x
  }

  fun idxX (width : Number, idx : Number) : Number {
    idx % width
  }

  fun idxY (width : Number, idx : Number) : Number {
    Math.floor(idx / width)
  }

  fun isIslandFilled (island : Island) : Bool {
    (getIslandFreeConnectionSize(island)) == 0
  }

  fun getIslandFreeConnectionSize (island : Island) : Number {
    max.top - cur.top + max.right - cur.right + max.bottom - cur.bottom + max.left - cur.left
  } where {
    max =
      island.maxConnectionCounts

    cur =
      island.currentConnectionCounts
  }

  fun directionFromIsland (width : Number, fromIndex : Number, toIndex : Number) : Direction {
    if (dx > 0) {
      Direction::Right
    } else if (dx < 0) {
      Direction::Left
    } else if (dy > 0) {
      Direction::Down
    } else {
      Direction::Up
    }
  } where {
    dx =
      idxX(width, toIndex) - idxX(width, fromIndex)

    dy =
      idxY(width, toIndex) - idxY(width, fromIndex)
  }

  fun oppositeDirection (dir : Direction) : Direction {
    case (dir) {
      Direction::Left => Direction::Right
      Direction::Right => Direction::Left
      Direction::Up => Direction::Down
      Direction::Down => Direction::Up
    }
  }

  fun distanceBetweenIslands (width : Number, idx1 : Number, idx2 : Number) : Number {
    Math.max(dx, dy)
  } where {
    dx =
      idxX(width, idx1) - idxX(width, idx2)

    dy =
      idxY(width, idx1) - idxY(width, idx2)
  }

  fun directionToConnectionSize (sizes : ConnectionSizes, dir : Direction) : Number {
    case (dir) {
      Direction::Up => sizes.top
      Direction::Right => sizes.right
      Direction::Down => sizes.bottom
      Direction::Left => sizes.left
    }
  }

  fun directionToPosDiff (dir : Direction) : Vec2 {
    case (dir) {
      Direction::Up =>
        {
          x = 0,
          y = -1
        }

      Direction::Right =>
        {
          x = 1,
          y = 0
        }

      Direction::Down =>
        {
          x = 0,
          y = 1
        }

      Direction::Left =>
        {
          x = -1,
          y = 0
        }
    }
  }

  fun directionToOrientation (dir : Direction) : Orientation {
    case (dir) {
      Direction::Up => Orientation::Vertical
      Direction::Right => Orientation::Horizontal
      Direction::Down => Orientation::Vertical
      Direction::Left => Orientation::Horizontal
    }
  }

  fun traverseToNextIsland (
    puzzle : Puzzle,
    fromIndex : Number,
    direction : Direction
  ) : Maybe(Number) {
    iterate(startX + diff.x, startY + diff.y)
  } where {
    diff =
      directionToPosDiff(direction)

    startX =
      idxX(puzzle.width, fromIndex)

    startY =
      idxY(puzzle.width, fromIndex)

    iterate =
      (x : Number, y : Number) : Maybe(Number) {
        if (x < 0 || x >= puzzle.width || y >= puzzle.height || y < 0) {
          Maybe::Nothing
        } else {
          try {
            idx =
              xyIdx(puzzle.width, x, y)

            case (Map.get(idx, puzzle.islands.fields)) {
              Maybe::Just true => Maybe::Just(idx)
              => iterate(x + diff.x, y + diff.y)
            }
          }
        }
      }
  }

  fun changeConnectionSizeForIslandNeighbour (
    neighbourDirection : Direction,
    newConnectionSize : Number,
    island : Island
  ) : Island {
    {
      index = island.index,
      maxConnectionCounts = island.maxConnectionCounts,
      currentConnectionCounts =
        {
          top = newTop,
          right = newRight,
          bottom = newBottom,
          left = newLeft
        }
    }
  } where {
    curConns =
      island.currentConnectionCounts

    newTop =
      if (neighbourDirection == Direction::Up) {
        newConnectionSize
      } else {
        curConns.top
      }

    newRight =
      if (neighbourDirection == Direction::Right) {
        newConnectionSize
      } else {
        curConns.right
      }

    newBottom =
      if (neighbourDirection == Direction::Down) {
        newConnectionSize
      } else {
        curConns.bottom
      }

    newLeft =
      if (neighbourDirection == Direction::Left) {
        newConnectionSize
      } else {
        curConns.left
      }
  }

  fun switchValue (
    min : Number,
    max : Number,
    timeDirection : TimeDirection,
    current : Number
  ) : Number {
    if (step > max) {
      min
    } else if (step < min) {
      max
    } else {
      step
    }
  } where {
    diff =
      case (timeDirection) {
        TimeDirection::Forward => 1
        TimeDirection::Backward => -1
      }

    step =
      current + diff
  }

  /*
   Do the most basic mechanic of the game - switch a connection size between two islands.

  Does not check for a collision since it is called after the drag is stopped.
  However, it does check the current states of islands.
  */
  fun switchIslandConnections (
    timeDirection : TimeDirection,
    idx1 : Number,
    idx2 : Number,
    puzzle : Puzzle
  ) : Puzzle {
    { puzzle |
      connections = newConnections,
      islands = newIslands
    }
  } where {
    sortedIdx1 =
      Math.min(idx1, idx2)

    sortedIdx2 =
      Math.max(idx1, idx2)

    island1 =
      getIslandByIndex(puzzle.islands, sortedIdx1)

    island2 =
      getIslandByIndex(puzzle.islands, sortedIdx2)

    foundConn =
      puzzle.connections.list
      |> Util.Collection.findAndGetRest(
        (conn : Connection) : Bool {
          conn.idx1 == sortedIdx1 && conn.idx2 == sortedIdx2
        })

    maybeConn =
      case (foundConn) {
        FirstRest::A first rest => first
      }

    restConns =
      case (foundConn) {
        FirstRest::A first rest => rest
      }

    /* we need to check up local maximums for both islands */
    commonMaxNewConnectionsCount =
      Math.min(
        island1
        |> Maybe.map(getIslandFreeConnectionSize)
        |> Maybe.withDefault(0),
        island2
        |> Maybe.map(getIslandFreeConnectionSize)
        |> Maybe.withDefault(0))

    newConnectionsWithNewConnectionSize =
      case (maybeConn) {
        Maybe::Nothing =>
          try {
            /* insert a new connection */
            connectionOrientation =
              directionFromIsland(puzzle.width, idx1, idx2)
              |> directionToOrientation()

            newConnection =
              {
                idx1 = sortedIdx1,
                idx2 = sortedIdx2,
                connectionSize = 1,
                orientation = connectionOrientation
              }

            Pair::A(Array.push(newConnection, puzzle.connections.list), 1)
          }

        Maybe::Just conn =>
          try {
            /* replace the found one */
            commonMaxConnectionSize =
              Math.min(
                puzzle.maxConnectionCount,
                (conn.connectionSize + commonMaxNewConnectionsCount))

            newConnSize =
              conn.connectionSize
              |> switchValue(0, commonMaxConnectionSize, timeDirection)

            newConnection =
              {
                idx1 = sortedIdx1,
                idx2 = sortedIdx2,
                connectionSize = newConnSize,
                orientation = conn.orientation
              }

            Pair::A(Array.push(newConnection, restConns), newConnSize)
          }
      }

    newConnectionList =
      case (newConnectionsWithNewConnectionSize) {
        Pair::A list size => list
      }

    newConnectionSize =
      case (newConnectionsWithNewConnectionSize) {
        Pair::A list size => size
      }

    dir1to2 =
      directionFromIsland(puzzle.width, sortedIdx1, sortedIdx2)

    diff =
      directionToPosDiff(dir1to2)

    startX =
      idxX(puzzle.width, sortedIdx1)

    startY =
      idxY(puzzle.width, sortedIdx1)

    stepCount =
      distanceBetweenIslands(puzzle.width, sortedIdx1, sortedIdx2) - 2

    iterate =
      (
        x : Number,
        y : Number,
        fields : Map(Number, IndexPair),
        leftSteps : Number
      ) : Map(Number, IndexPair) {
        try {
          idx =
            xyIdx(puzzle.width, x, y)

          maybeField =
            Map.get(idx, fields)

          if (Maybe.isJust(maybeField) && newConnectionSize == 0) {
            Map.delete(idx, fields)
          } else if (Maybe.isNothing(maybeField)) {
            Map.set(
              idx,
              {
                idx1 = sortedIdx1,
                idx2 = sortedIdx2
              },
              fields)
          } else if (leftSteps > 0) {
            iterate((x + diff.x), (y + diff.y), fields, (leftSteps - 1))
          } else {
            fields
          }
        }
      }

    newConnectionFields =
      iterate(
        startX + diff.x,
        startY + diff.y,
        puzzle.connections.fieldss,
        stepCount)

    dir2to1 =
      oppositeDirection(dir1to2)

    newIsland1 =
      island1
      |> Maybe.map(
        changeConnectionSizeForIslandNeighbour(
          dir1to2,
          newConnectionSize))

    newIsland2 =
      island2
      |> Maybe.map(
        changeConnectionSizeForIslandNeighbour(
          dir2to1,
          newConnectionSize))

    newConnections =
      {
        list = newConnectionList,
        fieldss = newConnectionFields
      }

    updateEachIsland =
      (
        currentIslands : Array(Island),
        leftIslands : Array(Island)
      ) : Array(Island) {
        case (leftIslands[0]) {
          Maybe::Nothing => currentIslands

          Maybe::Just island =>
            try {
              rest =
                Array.drop(1, leftIslands)

              updatedIslands =
                currentIslands
                |> Util.Collection.updateFirst(
                  ((i : Island) : Bool { i.index == island.index }),
                  ((i : Island) : Island { island }))

              updateEachIsland(updatedIslands, rest)
            }
        }
      }

    islandsToUpdate =
      Maybe.Extra.values([
        newIsland1,
        newIsland2
      ])

    newIslandsList =
      updateEachIsland(puzzle.islands.list, islandsToUpdate)

    newIslands =
      {
        list = newIslandsList,
        fields = puzzle.islands.fields
      }
  }

  fun isSuccessfullyFinished (puzzle : Puzzle) : Bool {
    puzzle.islands.list
    |> Array.any(isIslandNotFilled)
  } where {
    isIslandNotFilled =
      (island : Island) : Bool {
        isIslandFilled(island)
      }
  }

  fun getIslandByIndex (islands : Islands, idx : Number) : Maybe(Island) {
    Array.find(
      (island : Island) : Bool { island.index == idx },
      islands.list)
  }

  /* Find a closest island to the given one, in given direction. Do not check for collisions. */
  fun findNeighbourIsland (
    puzzle : Puzzle,
    islandIndex : Number,
    direction : Direction
  ) : Maybe(Number) {
    getIslandByIndex(puzzle.islands, islandIndex)
    |> Maybe.Extra.andThen(
      (island : Island) : Maybe(Number) {
        traverseToNextIsland(puzzle, islandIndex, direction)
      })
  }

  fun canDraw (puzzle : Puzzle, fromIdx : Number, toIdx : Number) : Bool {
    try {
      fromIsland =
        getIslandByIndex(puzzle.islands, fromIdx)

      isFromFilled =
        fromIsland
        |> Maybe.map(isIslandFilled)
        |> Maybe.withDefault(false)

      direction =
        directionFromIsland(puzzle.width, fromIdx, toIdx)

      if (isFromFilled) {
        case (fromIsland) {
          Maybe::Just island =>
            try {
              currentConnectionSize =
                direction
                |> directionToConnectionSize(island.currentConnectionCounts)

              currentConnectionSize > 0 && isThereClearWay(puzzle, fromIdx, toIdx)
            }

          Maybe::Nothing => false
        }
      } else {
        case (fromIsland) {
          Maybe::Just island =>
            try {
              currentConnectionSize =
                direction
                |> directionToConnectionSize(island.currentConnectionCounts)

              maxConnectionSize =
                puzzle.maxConnectionCount

              toIsland =
                getIslandByIndex(puzzle.islands, toIdx)

              isToFilled =
                toIsland
                |> Maybe.map(isIslandFilled)
                |> Maybe.withDefault(false)

              (currentConnectionSize > 0 || (currentConnectionSize < maxConnectionSize && (!isToFilled))) && isThereClearWay(puzzle, fromIdx, toIdx)
            }

          Maybe::Nothing => false
        }
      }
    }
  }

  /* Check if there is no collision in a way between two puzzles. */
  fun isThereClearWay (puzzle : Puzzle, fromIndex : Number, toIndex : Number) : Bool {
    iterate(startX + diff.x, startY + diff.y)
  } where {
    direction =
      directionFromIsland(puzzle.width, fromIndex, toIndex)

    diff =
      directionToPosDiff(direction)

    startX =
      idxX(puzzle.width, fromIndex)

    startY =
      idxY(puzzle.width, fromIndex)

    endX =
      idxX(puzzle.width, toIndex)

    endY =
      idxY(puzzle.width, toIndex)

    expectedIdx1 =
      Math.min(fromIndex, toIndex)

    expectedIdx2 =
      Math.max(fromIndex, toIndex)

    iterate =
      (x : Number, y : Number) : Bool {
        if (x < 0 || x >= puzzle.width || y >= puzzle.height || y < 0) {
          false
        } else if (x == endX && y == endY) {
          true
        } else {
          try {
            idx =
              xyIdx(puzzle.width, x, y)

            case (Map.get(idx, puzzle.connections.fieldss)) {
              Maybe::Just pair =>
                if (pair.idx1 == expectedIdx1 && pair.idx2 == expectedIdx2) {
                  /*
                  If there is connection but it's the one between
                  those same islands then we don't break the traversal
                  */
                  iterate(x + diff.x, y + diff.y)
                } else {
                  false
                }

              => iterate(x + diff.x, y + diff.y)
            }
          }
        }
      }
  }
}
