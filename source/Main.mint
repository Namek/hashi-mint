store Game {
  state puzzle : Puzzle = { islands = [], connections = [], width = 0, height = 0, maxConnectionCount = 0 }
  state puzzleStart : Puzzle = puzzle
  state isPuzzleDone : Bool = false
  state moveHistory : Array(Vec2) = []
  state islandDrag: IslandDrag = IslandDrag::NoIslandsHovered

  fun stepBack : Promise(Never, Void) {
    next {
      moveHistory = moveHistory |> Array.slice(0, Array.size(moveHistory))
    }
  }

  fun reset : Promise(Never, Void)  {
    next { puzzle = puzzleStart }
  }

  fun getIslandRenderPos(index : Number) : Vec2 {
    { x = Model.idxX(puzzle.width, index * Const.fieldSize + Const.circleRadius),
      y = Model.idxY(puzzle.width, index * Const.fieldSize + Const.circleRadius) }
  }

  fun isIslandHovered(idx : Number) : Bool {
    case (islandDrag) {
      IslandDrag::NoIslandsHovered => false
      IslandDrag::FirstIslandHovered idx1  => idx1 == idx
      IslandDrag::FirstIslandPinned idx1 => idx1 == idx
      IslandDrag::SecondIslandPicked percent idx1 idx2 =>
        idx1 == idx || idx2 == idx
    }
  }

}

enum IslandDrag {
  NoIslandsHovered
  FirstIslandHovered(Number)
  FirstIslandPinned(Number)
  /*percent, island1Index, island2Index*/
  SecondIslandPicked(Number, Number, Number)
}

component Main {
  connect Game exposing { puzzle }
  connect Const exposing { scaleFactor, fieldSize, margin }

  style base {
    width: 100vw;
    height: 100vh;
    margin: 0;

    svg {
      user-select: none;
    }
  }


  fun render : Html {
    <div::base>
      <div>
        <button onClick={Game.stepBack} disabled={Game.moveHistory == []}>
          "Step Back"
        </button>
        <button onClick={Game.reset}>
          "Reset"
        </button>
      </div>

      <{renderPuzzle()}>

      if (Game.isPuzzleDone) {
        "Puzzle Done"
      }

    </div>
  }

  fun renderPuzzle : Html {
    <svg
      width={Number.toString(puzzle.width * fieldSize * scaleFactor)}
      height={Number.toString(puzzle.height * fieldSize * scaleFactor)}
      viewBox={"-#{margin} -#{margin} #{puzzle.width * fieldSize + margin * 2} #{puzzle.height * fieldSize + margin * 2}"}
    >
      <{Array.map(renderConnection, puzzle.connections)}>

      <{
        case (Game.islandDrag) {
          IslandDrag::SecondIslandPicked percent idx1 idx2 =>
            renderTemporaryBridge(idx1, idx2, percent)

          => <g></g>
        }
      }>

      for (island of puzzle.islands) {
        renderIsland(island)
      }
    </svg>
  }

  fun renderConnection(conn : Connection) : Html {
    <div></div>
  }

  fun renderTemporaryBridge(idx1 : Number, idx2 : Number, percent : Number) : Html {
    try {
      start = Game.getIslandRenderPos(idx1)
      to = Game.getIslandRenderPos(idx2)
      dx = to.x - start.x
      dy = to.y - start.y
      endX = start.x + dx * percent
      endY = start.y + dy * percent
      lineStyle = "stroke:rgb(0,255,0);stroke-width:5"

      Render.line(start.x, start.y, endX, endY, lineStyle)
    }
  }

  fun renderIsland(island : Island) : Html {
    <g>
      <{Render.circle(number, pos.x, pos.y, isHovered, isFilled)}>
    </g>
  } where {
    maxConns = island.maxConnectionCounts
    number = maxConns.top + maxConns.right + maxConns.bottom + maxConns.left
    pos = Game.getIslandRenderPos(island.index)
    isHovered = Game.isIslandHovered(island.index)
    isFilled = Model.isIslandFilled(island)
  }
}

store Const {
  state fieldSize : Number = 10
  state scaleFactor : Number = 5
  state margin : Number = 4
  state circleRadius : Number = 5 /*fieldSize / 2*/
  state connectionLineMargin : Number = 3
}

module Render {
  fun circle(number : Number, posX : Number, posY : Number, isHovered : Bool, isFilled : Bool) : Html {
    <g>
      <circle
        cx={Number.toString(posX)}
        cy={Number.toString(posY)}
        r={Number.toString(Const.circleRadius)}
        fill="TODO"
        stroke={color(0, 0, 0)}
      />
      <text
        x={Number.toString(posX)}
        y={Number.toString(posY)}
        textAnchor="middle"
        dominantBaseline="central"
        fontSize="#{Const.circleRadius}"
      >
        "#{number}"
      </text>
    </g>
  }

  fun line(startX : Number, startY : Number, endX : Number, endY : Number, lineStyle : String) : Html {
    <line
      x1={Number.toString(startX)}
      y1={Number.toString(startY)}
      x2={Number.toString(endX)}
      y2={Number.toString(endY)}
      style={lineStyle}
    />
  }

  fun color(r : Number, g : Number, b : Number) : String {
    "rgb(#{r}, #{g}, #{b})"
  }
}
