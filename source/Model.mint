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

  fun switchIslandConnections (
    timeDirection : TimeDirection,
    idx1 : Number,
    idx2 : Number,
    puzzle : Puzzle
  ) : Puzzle {
    try {
      sortedIdx1 =
        Math.min(idx1, idx2)

      sortedIdx2 =
        Math.max(idx1, idx2)

      island1 =
        getIslandByIndex(puzzle.islands, sortedIdx1)

      island2 =
        getIslandByIndex(puzzle.islands, sortedIdx2)

      /* TODO */
      puzzle
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
