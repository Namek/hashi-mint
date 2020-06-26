store Const {
  state fieldSize : Number = 10
  state margin : Number = 2

  /* fieldSize / 2 */
  state circleRadius : Number = 4

  state connectionLineMargin : Number = 3
}

store Game {
  const MIN_VECTOR_DISTANCE_FROM_CENTER_TO_DRAW_BRIDGE = Const.circleRadius * 1.5
  state scaleFactor : Number = 1
  state svgInternalOffsetX : Number = 0

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
    sequence {
      Game.initPuzzle(seed, params)
      App.watchUrl(seed, params)
    }
  }

  fun changeSeed (seed : Number) {
    sequence {
      Game.initPuzzle(seed, genParams)
      App.watchUrl(seed, genParams)
    }
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

  fun recalculateScaleFactor (width : Number, height : Number) {
    try {
      scaleFactor =
        height / getLogicalHeight()

      next
        {
          scaleFactor = scaleFactor,
          svgInternalOffsetX = (getLogicalWidth() * scaleFactor - width) / 2
        }
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

  fun getLogicalWidth {
    puzzle.width * Const.fieldSize
  }

  fun getLogicalHeight {
    puzzle.height * Const.fieldSize
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

  fun getPosInsideSvg (evt : Html.Event) {
    try {
      inputPosInsideSvg =
        Html.Extra.getOffsetPos(evt)

      {
        x = inputPosInsideSvg.x + svgInternalOffsetX,
        y = inputPosInsideSvg.y
      }
    }
  }

  fun checkBridgeDirection (evt : Html.Event) : Promise(Never, Void) {
    try {
      inputPos =
        getPosInsideSvg(evt)

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

                  len =
                    Math.abs(to - from)

                  distance =
                    pos
                    |> Util.Math.rescale(from, to, 0, len)

                  distancePercent =
                    pos
                    |> Util.Math.rescale(from, to, 0, 1)
                    |> Math.min(1)

                  if (distance > MIN_VECTOR_DISTANCE_FROM_CENTER_TO_DRAW_BRIDGE) {
                    Maybe::Just({i1Idx, i2Idx, distancePercent})
                  } else {
                    try {
                      /* remove the temporary bridge if touch comes back closer to circle */
                      next
                        {
                          islandDrag =
                            case (islandDrag) {
                              IslandDrag::SecondIslandPicked percent idx1 idx2 => IslandDrag::FirstIslandPinned(idx1)

                              => islandDrag
                            }
                        }

                      Maybe::Nothing
                    }
                  }
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
          w * Game.scaleFactor,
          -1 * Const.margin,
          w + Const.margin,
          pos.x),
      y =
        Util.Math.rescale(
          0,
          h * Game.scaleFactor,
          -1 * Const.margin,
          h + Const.margin,
          pos.y)
    }
  } where {
    w =
      getLogicalWidth()

    h =
      getLogicalHeight()
  }

  fun physicalRenderPos (pos : Vec2) : Vec2 {
    {
      x = (pos.x - Const.margin),
      y = (pos.y - Const.margin)
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
