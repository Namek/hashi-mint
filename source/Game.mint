store Const {
  state fieldSize : Number = 10
  state scaleFactor : Number = 5
  state margin : Number = 2

  /* fieldSize / 2 */
  state circleRadius : Number = 4

  state connectionLineMargin : Number = 3
}

store Game {
  state puzzle : Puzzle = {
    islands =
      {
        list = [],
        fields = Map.empty()
      },
    connections =
      {
        list = [],
        fieldss = Map.empty()
      },
    connectionsMaxList = [],
    width = 0,
    height = 0,
    maxConnectionCount = 2
  }

  state seed : Number = 0

  state genParams : GenerationParams = {
    width = 0,
    height = 0,
    maxConnectionCount = 0,
    targetIslandCount = 0,
    cycleImprovementPercent = 0,
    increaseConnectionCountsPercent = 0
  }

  state puzzleStart : Puzzle = puzzle
  state isPuzzleDone : Bool = false
  state moveHistory : Array(IndexPair) = []
  state islandDrag : IslandDrag = IslandDrag::NoIslandsHovered

  fun initPuzzle (seed : Number, params : GenerationParams) {
    try {
      next
        {
          seed = seed,
          genParams = params
        }

      setPuzzle(Generation.generatePuzzle(seed, params))
    }
  }

  fun changeGenParams (params : GenerationParams) {
    Game.initPuzzle(seed, params)
  }

  fun setPuzzle (p : Puzzle) : Promise(Never, Void) {
    next
      {
        puzzle = p,
        puzzleStart = p,
        isPuzzleDone = false,
        moveHistory = [],
        islandDrag = IslandDrag::NoIslandsHovered
      }
  }

  fun stepBack : Promise(Never, Void) {
    case (Array.last(moveHistory)) {
      Maybe::Just move =>
        next
          {
            puzzle =
              Model.switchIslandConnections(
                TimeDirection::Backward,
                move.idx1,
                move.idx2,
                puzzle),
            moveHistory =
              moveHistory
              |> Array.dropRight(1),
            isPuzzleDone = false
          }

      => next {  }
    }
  }

  fun reset : Promise(Never, Void) {
    setPuzzle(puzzleStart)
  }

  fun getIslandRenderPos (index : Number) : Vec2 {
    try {
      {x, y} =
        Model.idxXY(puzzle.width, index)

      {
        x = x * Const.fieldSize + Const.circleRadius,
        y = y * Const.fieldSize + Const.circleRadius
      }
    }
  }

  fun isIslandHovered (idx : Number) : Bool {
    case (islandDrag) {
      IslandDrag::NoIslandsHovered => false
      IslandDrag::FirstIslandHovered idx1 => idx1 == idx
      IslandDrag::FirstIslandPinned idx1 => idx1 == idx
      IslandDrag::SecondIslandPicked percent idx1 idx2 => idx1 == idx || idx2 == idx
    }
  }

  fun gotIslandHovered (islandIndex : Number, evt : Html.Event) : Promise(Never, Void) {
    case (islandDrag) {
      IslandDrag::NoIslandsHovered => next { islandDrag = IslandDrag::FirstIslandHovered(islandIndex) }
      IslandDrag::FirstIslandHovered => next { islandDrag = IslandDrag::FirstIslandHovered(islandIndex) }
      => next {  }
    }
  }

  fun gotIslandUnhovered (islandIndex : Number, evt : Html.Event) : Promise(Never, Void) {
    case (islandDrag) {
      IslandDrag::FirstIslandHovered => next { islandDrag = IslandDrag::NoIslandsHovered }
      => next {  }
    }
  }

  fun gotDragShouldStop : Promise(Never, Void) {
    case (islandDrag) {
      IslandDrag::SecondIslandPicked percent idx1 idx2 =>
        try {
          newPuzzle =
            Model.switchIslandConnections(
              TimeDirection::Forward,
              idx1,
              idx2,
              puzzle)

          newMoveHistory =
            Array.push(
              {
                idx1 = idx1,
                idx2 = idx2
              },
              moveHistory)

          newIsPuzzleDone =
            Model.isSuccessfullyFinished(newPuzzle)

          next
            {
              islandDrag = IslandDrag::NoIslandsHovered,
              puzzle = newPuzzle,
              moveHistory = newMoveHistory,
              isPuzzleDone = newIsPuzzleDone
            }
        }

      => next { islandDrag = IslandDrag::NoIslandsHovered }
    }
  }

  fun pinIsland (islandIndex : Number, evt : Html.Event) : Promise(Never, Void) {
    next { islandDrag = IslandDrag::FirstIslandPinned(islandIndex) }
  }

  fun checkBridgeDirection (evt : Html.Event) : Promise(Never, Void) {
    try {
      inputPos =
        Html.Extra.getOffsetPos(evt)

      maybeFirstIslandIndex =
        case (islandDrag) {
          IslandDrag::FirstIslandPinned idx => Maybe::Just(idx)
          IslandDrag::SecondIslandPicked percent idx1 idx2 => Maybe::Just(idx1)
          => Maybe::Nothing
        }

      maybeFirstIslandIndex
      |> Maybe.Extra.andThen(
        (i1Idx : Number) : Maybe(Tuple(Number, Number, Number)) {
          try {
            islandPos =
              getIslandRenderPos(i1Idx)

            rescaledPos =
              rescalePosInputToLogic(inputPos)

            direction =
              directionFromPoint(
                physicalRenderPos(rescaledPos),
                physicalRenderPos(islandPos))

            neighbour =
              Model.findNeighbourIsland(puzzle, i1Idx, direction, false)

            case (neighbour) {
              Maybe::Just i2Idx =>
                try {
                  neighbourPos =
                    getIslandRenderPos(i2Idx)

                  cond =
                    Math.abs(neighbourPos.x - islandPos.x) > Math.abs(neighbourPos.y - islandPos.y)

                  to =
                    if (cond) {
                      neighbourPos.x
                    } else {
                      neighbourPos.y
                    }

                  from =
                    if (cond) {
                      islandPos.x
                    } else {
                      islandPos.y
                    }

                  pos =
                    if (cond) {
                      rescaledPos.x
                    } else {
                      rescaledPos.y
                    }

                  distancePercent =
                    pos
                    |> Util.Math.rescale(from, to, 0, 1)
                    |> Math.min(1)

                  Maybe::Just({i1Idx, i2Idx, distancePercent})
                }

              => Maybe::Nothing
            }
          }
        })
      |> Maybe.Extra.andThen(
        (triple : Tuple(Number, Number, Number)) : Maybe(IslandDrag) {
          case (triple) {
            {i1Idx, i2Idx, distancePercent} =>
              if (Model.canDraw(puzzle, i1Idx, i2Idx)) {
                Maybe::Just(IslandDrag::SecondIslandPicked(distancePercent, i1Idx, i2Idx))
              } else {
                case (islandDrag) {
                  IslandDrag::SecondIslandPicked a b c => Maybe::Just(IslandDrag::NoIslandsHovered)
                  => Maybe::Nothing
                }
              }
          }
        })
      |> Maybe.map(
        (newDrag : IslandDrag) : Promise(Never, Void) {
          next { islandDrag = newDrag }
        })
      |> Maybe.withDefault(next {  })
    }
  }

  fun rescalePosInputToLogic (pos : Vec2) : Vec2 {
    {
      x =
        Util.Math.rescale(
          0,
          puzzle.width * Const.fieldSize * Const.scaleFactor,
          -1 * Const.margin,
          puzzle.width * Const.fieldSize + Const.margin,
          pos.x),
      y =
        Util.Math.rescale(
          0,
          puzzle.height * Const.fieldSize * Const.scaleFactor,
          -1 * Const.margin,
          puzzle.height * Const.fieldSize + Const.margin,
          pos.y)
    }
  }

  fun physicalRenderPos (pos : Vec2) : Vec2 {
    {
      x = (pos.x - Const.margin) * Const.scaleFactor,
      y = (pos.y - Const.margin) * Const.scaleFactor
    }
  }

  fun directionFromPoint (from : Vec2, to : Vec2) : Direction {
    if (Math.abs(dx) > Math.abs(dy)) {
      if (dx > 0) {
        Direction::Left
      } else {
        Direction::Right
      }
    } else if (dy > 0) {
      Direction::Up
    } else {
      Direction::Down
    }
  } where {
    dx =
      to.x - from.x

    dy =
      to.y - from.y
  }
}

enum IslandDrag {
  NoIslandsHovered
  FirstIslandHovered(Number)
  FirstIslandPinned(Number)

  /* percent, island1Index, island2Index */
  SecondIslandPicked(Number, Number, Number)
}
