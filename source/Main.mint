component Main {
  connect Game exposing { puzzle }
  connect Const exposing { scaleFactor, fieldSize, margin }

  state showConfigurator = false

  fun toggleConfigurator {
    next { showConfigurator = !showConfigurator }
  }

  style base {
    width: 100vw;
    height: 100vh;
    margin: 0;
    background-color: #F8F8F8;

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
    <div::base>
      <input
        type="checkbox"
        checked={showConfigurator}
        onChange={toggleConfigurator}/>

      if (showConfigurator) {
        <LevelEditor/>
      }

      <div>
        "Link to share:"

        <input
          disabled={true}
          value={Number.toString(Game.seed)}/>
      </div>

      <div
        onPointerLeave={Game.gotDragShouldStop}
        onPointerUp={Game.gotDragShouldStop}>

        <div>
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
          renderConnection("stroke:rgb(127,127,127);stroke-width:0.5"),
          puzzle.connections.list)
      }>

      if (showConfigurator) {
        Array.map(
          renderConnection("stroke:rgb(255,0,0);stroke-width:0.5"),
          puzzle.connectionsMaxList)
      }

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
        to.x

      endY =
        to.y

      lineStyle =
        "stroke:#228CDB;stroke-width:3"

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

      if (showConfigurator) {
        <g>
          <text
            x={Number.toString(pos.x - 1)}
            y={Number.toString(pos.y + 1.5)}
            textAnchor="end"
            fontSize="2.5px"
            fill="black">

            <tspan>
              "#{island.index}"
            </tspan>

          </text>
        </g>
      }

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
