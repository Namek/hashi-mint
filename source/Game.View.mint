component Game.View {
  connect Game exposing { puzzle }
  connect Const exposing { margin }

  property isDebugMode : Bool = false

  style svg {
    user-select: none;
    width: 100%;
    height: auto;
    max-width: 100%;
    max-height: calc(100vh - 100px);
  }

  fun recalculateScaleFactor {
    theSvg
    |> Maybe.map(
      Dom.getDimensions())
    |> Maybe.map(
      (rect : Dom.Dimensions) { Game.recalculateScaleFactor(rect.width, rect.height) })
  }

  fun render : Html {
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
  }

  fun renderPuzzle : Html {
    <svg::svg as theSvg
      width={Number.toString(width)}
      height={Number.toString(height)}
      viewBox="-#{margin} -#{margin} #{width + margin * 2} #{height + margin * 2}"
      onPointerMove={Game.checkBridgeDirection}>

      <{
        Array.map(
          renderConnection("stroke:rgb(127,127,127);stroke-width:0.5"),
          puzzle.connections.list)
      }>

      if (isDebugMode) {
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
  } where {
    width =
      Game.getLogicalWidth()

    height =
      Game.getLogicalHeight()
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
