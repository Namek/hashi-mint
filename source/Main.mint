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

  state seed : Rand = Random.initRandomly()
  state puzzleStart : Puzzle = puzzle
  state isPuzzleDone : Bool = false
  state moveHistory : Array(IndexPair) = []
  state islandDrag : IslandDrag = IslandDrag::NoIslandsHovered

  fun initPuzzle (seed : Number, params : GenerationParams) {
    try {
      next { seed = Random.init(seed) }

      setPuzzle(Generation.generatePuzzle(seed, params))
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

component Main {
  connect Game exposing { puzzle }
  connect Const exposing { scaleFactor, fieldSize, margin }

  style base {
    width: 100vw;
    height: 100vh;
    margin: 0;
    background-color: #ccc;

    svg {
      user-select: none;
    }
  }

  fun testRandom {
    try {
      rand0 =
        Random.initRandomly()

      iter =
        (i : Number, rand : Rand, numbers : Array(Number)) {
          if (i < 100) {
            try {
              {num, rand1} =
                Random.number(0, 100, rand)

              iter(i + 1, rand1, Array.push(num, numbers))
            }
          } else {
            numbers
          }
        }

      iter(0, rand0, [])
      |> Array.map((num : Number) { Number.toString(num) })
      |> Array.intersperse(",")
    }
  }

  fun render : Html {
    <div::base
      onPointerLeave={Game.gotDragShouldStop}
      onPointerUp={Game.gotDragShouldStop}>

      <div>
        "seed:"
        <{ Number.toString(Game.seed.state) }>

        /*
        "randoms:"
                <{ testRandom() }>
        */
        <{ Number.toString(fieldSize * scaleFactor) }>
        <{ Number.toString(puzzle.width) }>
        <br/>

        <button
          onClick={Game.stepBack}
          disabled={Game.moveHistory == []}>

          "Step Back"

        </button>

        <button onClick={Game.reset}>
          "Reset"
        </button>
      </div>

      <{ renderPuzzle() }>

      if (Game.isPuzzleDone) {
        <div>
          "Puzzle Done!"
        </div>
      }

    </div>
  }

  fun renderPuzzle : Html {
    <svg
      width={Number.toString(puzzle.width * fieldSize * scaleFactor)}
      height={Number.toString(puzzle.height * fieldSize * scaleFactor)}
      viewBox="-#{margin} -#{margin} #{puzzle.width * fieldSize + margin * 2} #{puzzle.height * fieldSize + margin * 2}"
      onPointerMove={Game.checkBridgeDirection}>

      <{
        Array.map(
          renderConnection("stroke:rgb(255,0,0);stroke-width:0.5"),
          puzzle.connections.list)
      }>

      <{
        Array.map(
          renderConnection("stroke:rgb(127,127,127);stroke-width:0.1"),
          puzzle.connectionsMaxList)
      }>

      case (Game.islandDrag) {
        IslandDrag::SecondIslandPicked percent idx1 idx2 => renderTemporaryBridge(idx1, idx2, percent)
        => Html.empty()
      }

      for (island of puzzle.islands.list) {
        renderIsland(island)
      }

    </svg>
  }

  fun renderConnection (lineStyle : String, conn : Connection) : Html {
    case (conn.connectionSize) {
      0 => Html.empty()

      =>
        try {
          count =
            conn.connectionSize

          start =
            Game.getIslandRenderPos(conn.idx1)

          end =
            Game.getIslandRenderPos(conn.idx2)

          renderLines(lineStyle, count, start, end, conn.orientation)
        }
    }
  }

  fun renderLines (
    lineStyle : String,
    count : Number,
    start : Vec2,
    end : Vec2,
    orientation : Orientation
  ) : Html {
    <g>
      <{ lines }>
    </g>
  } where {
    m =
      (Const.connectionLineMargin) / -2

    distances =
      case (count) {
        1 => [0]

        2 =>
          [
            m / -2,
            m / 2
          ]

        3 => [m * -1]
        4 => [-1 * (m * 3 / 2)]
        => []
      }

    zeros =
      Util.Collection.repeat(0, count)

    zipFn =
      (x : Number, y : Number) : Vec2 {
        {
          x = x,
          y = y
        }
      }

    coordDiffs =
      case (orientation) {
        Orientation::Vertical => Util.Collection.zip(zipFn, distances, zeros)
        Orientation::Horizontal => Util.Collection.zip(zipFn, zeros, distances)
      }

    lines =
      for (d of coordDiffs) {
        Util.Render.line(
          start.x + d.x,
          start.y + d.y,
          end.x + d.x,
          end.y + d.y,
          lineStyle)
      }
  }

  fun renderTemporaryBridge (idx1 : Number, idx2 : Number, percent : Number) : Html {
    try {
      start =
        Game.getIslandRenderPos(idx1)

      to =
        Game.getIslandRenderPos(idx2)

      dx =
        to.x - start.x

      dy =
        to.y - start.y

      endX =
        start.x + dx * percent

      endY =
        start.y + dy * percent

      lineStyle =
        "stroke:rgb(0,255,0);stroke-width:5"

      Util.Render.line(start.x, start.y, endX, endY, lineStyle)
    }
  }

  fun renderIsland (island : Island) : Html {
    <g
      onPointerOver={Game.gotIslandHovered(island.index)}
      onPointerLeave={Game.gotIslandUnhovered(island.index)}
      onPointerDown={Game.pinIsland(island.index)}
      data-idx={Number.toString(island.index)}
      data-x={Number.toString(x)}
      data-y={Number.toString(y)}>

      <{ Util.Render.circle(number, pos.x, pos.y, isHovered, isFilled) }>

    </g>
  } where {
    maxConns =
      island.maxConnectionCounts

    number =
      maxConns.top + maxConns.right + maxConns.bottom + maxConns.left

    pos =
      Game.getIslandRenderPos(island.index)

    isHovered =
      Game.isIslandHovered(island.index)

    isFilled =
      Model.isIslandFilled(island)

    {x, y} =
      Model.idxXY(puzzle.width, island.index)
  }
}
