record Bridge {
  connectionCount : Number,
  orientation : Orientation,
  idx1 : Number,
  idx2 : Number
}

record Rect {
  x : Number,
  y : Number,
  width : Number,
  height : Number
}

record Segment {
  x : Number,
  y : Number,
  orientation : Orientation,
  length : Number
}

record DivisionStats {
  smallerThan2 : Number
}

enum FieldType {
  Island(Number, ConnectionSizes)
  Connection
}

record GenerationState {
  width : Number,
  height : Number,
  maxConnectionCount : Number,
  fieldsMap : Map(Number, FieldType),
  islandIndices : Array(Number),
  connectionsMaxList : Array(Connection),
  minIslandCount : Number,
  randomizationFailCount : Number,
  maxRandomizationFailCount : Number,
  iterCallCount : Number,
  /* range: 0 - 100 */
  chanceToPickExistingIsland : Number
}

module Generation {
  const DIRECTIONS = [
    Direction::Right,
    Direction::Down,
    Direction::Left,
    Direction::Up
  ]

  fun log (text : String, value : a) : a {
    try {
      log =
        `console.log(#{text}, #{value})`

      value
    }
  }

  fun watchMeNoErr (val : Maybe(a)) : Maybe(a) {
    if (Maybe.isJust(val)) {
      val
    } else {
      try {
        a =
          Debug.log("error")

        val
      }
    }
  }

  fun generatePuzzle (seed : Number, width : Number, height : Number) : Puzzle {
    {
      islands =
        {
          list = islandList,
          fields = islandMap
        },
      connections =
        {
          list = [],
          fieldss = Map.empty()
        },
      connectionsMaxList = genState1.connectionsMaxList,
      width = width,
      height = height,
      maxConnectionCount = maxConnectionCount
    }
  } where {
    maxConnectionCount =
      2

    /* TODO: generation params should depend on map size and difficulty level */
    {minIslandCount, maxRandomizationFailCount, chanceToPickExistingIsland} =
      {12, 30, 40}

    genState0 =
      {
        width = width,
        height = height,
        maxConnectionCount = maxConnectionCount,
        fieldsMap = Map.empty(),
        islandIndices = [],
        connectionsMaxList = [],
        minIslandCount = minIslandCount,
        randomizationFailCount = 0,
        maxRandomizationFailCount = maxRandomizationFailCount,
        iterCallCount = 0,
        chanceToPickExistingIsland = chanceToPickExistingIsland
      }

    genState1 =
      log("RESULT", generateIslands(seed, genState0))

    islandList =
      genState1.islandIndices
      |> Array.map(
        (idx : Number) : Maybe(Island) {
          Map.get(idx, genState1.fieldsMap)
          |> Maybe.andThen(
            (field : FieldType) : Maybe(Island) {
              case (field) {
                FieldType::Island idx conns =>
                  try {
                    island =
                      {
                        maxConnectionCounts = conns,
                        currentConnectionCounts = ConnectionSizes(0, 0, 0, 0),
                        index = idx
                      }

                    Maybe::Just(island)
                  }

                => Maybe::Nothing
              }
            })
        })
      |> Array.compact()

    islandMap =
      genState1.islandIndices
      |> Array.reduce(
        Map.empty(),
        (map : Map(Number, Bool), idx : Number) : Map(Number, Bool) {
          Map.set(idx, true, map)
        })
  }

  fun generateIslands (seed : Number, genState0 : GenerationState) : GenerationState {
    try {
      rand0 =
        Random.init(seed)

      l =
        log("DIRECTIONS", DIRECTIONS)

      {firstIslandX, firstIslandY, rand1} =
        randomizePosition(genState0.width, genState0.height, rand0)

      l0 =
        log(
          "firstIsland x, y",
          {
            x = firstIslandX,
            y = firstIslandY
          })

      firstIslandIdx =
        Model.xyIdx(genState0.width, firstIslandX, firstIslandY)

      genState1 =
        addIsland(firstIslandX, firstIslandY, genState0)

      {genState2, rand2} =
        iter(genState1, firstIslandIdx, rand1)

      genState2
    }
  } where {
    randomizePosition =
      (width : Number, height : Number, rand0 : Rand) : Tuple(Number, Number, Rand) {
        try {
          {posY, rand1} =
            rand0
            |> Random.number(0, height - 1)

          {posX, rand2} =
            rand1
            |> Random.number(0, width - 1)

          {posX, posY, rand2}
        }
      }

    addIsland =
      (x : Number, y : Number, genState : GenerationState) : GenerationState {
        try {
          idx =
            Model.xyIdx(genState.width, x, y)

          l =
            log("addIsland ->", idx)

          { genState |
            islandIndices =
              Array.push(idx, genState.islandIndices)
              |> Array.sort((a : Number, b : Number) { a - b }),
            fieldsMap =
              Map.set(
                idx,
                FieldType::Island(idx, ConnectionSizes(0, 0, 0, 0)),
                genState.fieldsMap)
          }
        }
      }

    increaseConnectionToDirection =
      (
        idx : Number,
        direction : Direction,
        genState : GenerationState
      ) : GenerationState {
        try {
          l0 =
            log("increaseConnectionToDirection", "start")

          {idx2, isIslandIndex, distance} =
            traverse(genState, idx, direction, false)

          l =
            log(
              "increaseConnectionToDirection ->",
              `{
                idx: #{idx},
                direction: #{direction},
                idx2: #{idx2}
              }`)

          case (idx2) {
            Maybe::Just idx2 => increaseConnection(idx, idx2, genState)
            => genState
          }
        }
      }

    increaseConnection =
      (
        idx1 : Number,
        idx2 : Number,
        genState : GenerationState
      ) : GenerationState {
        try {
          conns1 =
            getIslandConnectionSizes(genState, idx1)
            |> watchMeNoErr
            |> Maybe.withDefault(ConnectionSizes(0, 0, 0, 0))

          conns2 =
            getIslandConnectionSizes(genState, idx2)
            |> watchMeNoErr
            |> Maybe.withDefault(ConnectionSizes(0, 0, 0, 0))

          dir1to2 =
            Model.directionFromIsland(genState.width, idx1, idx2)

          dir2to1 =
            Model.oppositeDirection(dir1to2)

          newConns1 =
            Model.changeConnectionSizes(
              (size : Number) : Number { size + 1 },
              dir1to2,
              conns1)

          newConns2 =
            Model.changeConnectionSizes(
              (size : Number) : Number { size + 1 },
              dir2to1,
              conns2)

          l =
            log(
              "increaseConnection ->",
              `{
                idx1: #{idx1},
                idx2: #{idx2},
                newCons1: #{newConns1},
                newCons2: #{newConns2}
              }`)

          {existingConnection, restConns} =
            genState.connectionsMaxList
            |> log("increaseConnection: before")
            |> Util.Collection.findAndGetRest(
              (conn : Connection) : Bool {
                conn.idx1 == idx1 && conn.idx2 == idx2 || conn.idx1 == idx2 && conn.idx2 == idx1
              })
            |> log("increaseConnection: existingConnection")

          newConnection =
            case (existingConnection) {
              Maybe::Just conn => { conn | connectionSize = conn.connectionSize + 1 }

              Maybe::Nothing =>
                {
                  idx1 = idx1,
                  idx2 = idx2,
                  connectionSize = 1,
                  orientation = Model.directionToOrientation(dir1to2)
                }
            }
            |> log("increaseConnection: newConnection")

          { genState |
            fieldsMap =
              genState.fieldsMap
              |> Map.set(idx1, FieldType::Island(idx1, newConns1))
              |> Map.set(idx2, FieldType::Island(idx2, newConns2)),
            connectionsMaxList = Array.push(newConnection, restConns)
          }
          |> fillConnectionBetween(idx1, idx2)
        }
      }

    fillConnectionBetween =
      (
        idx1 : Number,
        idx2 : Number,
        genState0 : GenerationState
      ) : GenerationState {
        try {
          {x1, y1} =
            Model.idxXY(genState0.width, idx1)

          dir =
            Model.directionFromIsland(genState0.width, idx1, idx2)

          diff =
            Model.directionToPosDiff(dir)

          {sx, sy} =
            {x1 + diff.x, y1 + diff.y}

          fillNext =
            (x : Number, y : Number, genState1 : GenerationState) : GenerationState {
              try {
                idx =
                  Model.xyIdx(genState1.width, x, y)

                if (idx != idx2) {
                  try {
                    genState2 =
                      { genState1 |
                        fieldsMap =
                          genState1.fieldsMap
                          |> Map.set(idx, FieldType::Connection)
                      }

                    fillNext(x + diff.x, y + diff.y, genState2)
                  }
                } else {
                  genState1
                }
              }
            }

          fillNext(sx, sy, genState0)
        }
      }

    iterFail =
      (
        genState0 : GenerationState,
        idx0 : Number,
        rand0 : Rand
      ) : Tuple(GenerationState, Rand) {
        iter(
          { genState0 | randomizationFailCount = genState0.randomizationFailCount + 1 },
          idx0,
          rand0)
      }

    filterIslandsWhichCanHaveNewNeighbours =
      (genState : GenerationState) : Array(Number) {
        try {
          for (idx of genState.islandIndices) {
            idx
          } when {
            try {
              conns =
                getIslandConnectionSizes(genState, idx)

              dirs =
                conns
                |> Maybe.map(
                  (conns : ConnectionSizes) : Array(Direction) {
                    for (dir of DIRECTIONS) {
                      dir
                    } when {
                      /* !hasNeighbour(idx0, dir, genState.islands) */
                      try {
                        connectionSize =
                          Model.directionToConnectionSize(conns, dir)

                        (connectionSize == 0)
                      }
                    }
                  })
                |> Maybe.withDefault([])

              Array.size(dirs) > 0
            }
          }
        }
      }

    iter =
      (
        genStateInitial : GenerationState,
        idxPrevious : Number,
        randInitial : Rand
      ) : Tuple(GenerationState, Rand) {
        try {
          genState =
            { genStateInitial | iterCallCount = genStateInitial.iterCallCount + 1 }

          l =
            log("ITER STEP -> ", genState)

          {idx0, rand0} =
            Random.choice(
              filterIslandsWhichCanHaveNewNeighbours(genState),
              randInitial)

          generatedIslandCount =
            Array.size(genState.islandIndices)

          if (genState.randomizationFailCount >= genState.maxRandomizationFailCount) {
            {genState, rand0}
          } else {
            try {
              {shouldTryToPickExistingIslandFirst, rand1} =
                if (generatedIslandCount < genState.minIslandCount) {
                  {false, rand0}
                } else {
                  try {
                    {num, rand1} =
                      Random.number(0, 100, rand0)

                    bool =
                      num <= genState.chanceToPickExistingIsland

                    {bool, rand1}
                  }
                }

              /*
              TODO: this should not be if/else but a rail.
                    if we can not choose an existing connection then we create a new island.
                    if that fails then there is iteration fail.
              */
              if (shouldTryToPickExistingIslandFirst) {
                try {
                  {idx, rand2} =
                    Random.choice(genState.islandIndices, rand1)

                  curIslandConnectionSizes =
                    getIslandConnectionSizes(genState, idx)

                  dirs =
                    curIslandConnectionSizes
                    |> Maybe.map(
                      (conns : ConnectionSizes) : Array(Direction) {
                        for (dir of DIRECTIONS) {
                          dir
                        } when {
                          try {
                            currentConnectionSize =
                              Model.directionToConnectionSize(conns, dir)

                            (currentConnectionSize > 0 && currentConnectionSize < genState.maxConnectionCount)
                          }
                        }
                      })
                    |> Maybe.withDefault([])

                  case (dirs) {
                    [dir, ...restDirs] =>
                      try {
                        {randomDir, rand3} =
                          Random.choiceSafe(dir, restDirs, rand2)

                        genState1 =
                          increaseConnectionToDirection(idx, randomDir, genState)

                        iter(genState1, idx, rand3)
                      }

                    => iterFail(genState, idx, rand2)
                  }
                }
              } else {
                try {
                  curIslandConnectionSizes =
                    getIslandConnectionSizes(genState, idx0)

                  possibleDirections =
                    curIslandConnectionSizes
                    |> Maybe.map(
                      (conns : ConnectionSizes) : Array(Direction) {
                        for (dir of DIRECTIONS) {
                          dir
                        } when {
                          /* !hasNeighbour(idx0, dir, genState.islands) */
                          try {
                            connectionSize =
                              Model.directionToConnectionSize(conns, dir)

                            (connectionSize == 0)
                          }
                        }
                      })
                    |> Maybe.withDefault([])

                  if (Array.size(possibleDirections) > 0) {
                    try {
                      {newDirection, rand2} =
                        Random.choice(possibleDirections, rand1)

                      {furthestIndex, isIslandIndex, atDistance} =
                        traverse(genState, idx0, newDirection, true)

                      maxDistance =
                        if (isIslandIndex) {
                          atDistance - 1
                        } else {
                          atDistance
                        }

                      case (furthestIndex) {
                        Maybe::Just furthestIndex =>
                          if (maxDistance >= 2) {
                            try {
                              {distance, rand3} =
                                Random.number(2, maxDistance, rand2)

                              targetIndex =
                                movePosition(genState.width, idx0, newDirection, distance)

                              {newX, newY} =
                                Model.idxXY(genState.width, targetIndex)

                              genState1 =
                                addIsland(newX, newY, genState)

                              genState2 =
                                increaseConnection(idx0, targetIndex, genState1)

                              iter(genState2, targetIndex, rand3)
                            }
                          } else {
                            /* should never happen */
                            iterFail(genState, idx0, rand2)
                          }

                        Maybe::Nothing => iterFail(genState, idx0, rand2)
                      }
                    }
                  } else {
                    iterFail(genState, idx0, rand1)
                  }
                }
              }
            }
          }
        }
      }
  }

  fun getIslandConnectionSizes (genState : GenerationState, idx : Number) : Maybe(ConnectionSizes) {
    Map.get(idx, genState.fieldsMap)
    |> Maybe.andThen(
      (field : FieldType) : Maybe(ConnectionSizes) {
        case (field) {
          FieldType::Island idx conns => Maybe::Just(conns)
          => Maybe::Nothing
        }
      })
  }

  /* returns: maybe index; bool: true when is index island or else when last index; distance travelled */
  fun traverse (
    genState : GenerationState,
    fromIndex : Number,
    direction : Direction,
    shouldCheckConnections : Bool
  ) : Tuple(Maybe(Number), Bool, Number) {
    try {
      diff =
        Model.directionToPosDiff(direction)

      {startX, startY} =
        Model.idxXY(genState.width, fromIndex)

      iterate =
        (x : Number, y : Number, distance : Number) : Tuple(Maybe(Number), Bool, Number) {
          try {
            idx =
              Model.xyIdx(genState.width, x, y)

            if (x < 0 || x >= genState.width || y >= genState.height || y < 0) {
              try {
                if (distance == 0) {
                  {Maybe::Nothing, false, 0}
                } else {
                  {Maybe::Just(idx), false, distance}
                }
              }
            } else {
              try {
                field =
                  Map.get(idx, genState.fieldsMap)

                shouldCheckIsland =
                  if (shouldCheckConnections) {
                    case (field) {
                      Maybe::Just whatever =>
                        case (whatever) {
                          FieldType::Connection => false
                          FieldType::Island idx conns => true
                        }

                      Maybe::Nothing => true
                    }
                  } else {
                    true
                  }

                if (shouldCheckIsland) {
                  case (field) {
                    Maybe::Just whatever =>
                      case (whatever) {
                        FieldType::Island => {Maybe::Just(idx), true, distance + 1}
                        FieldType::Connection => iterate(x + diff.x, y + diff.y, distance + 1)
                      }

                    => iterate(x + diff.x, y + diff.y, distance + 1)
                  }
                } else {
                  {Maybe::Just(idx), false, distance}
                }
              }
            }
          }
        }

      iterate(startX + diff.x, startY + diff.y, 0)
    }
  }

  fun movePosition (
    width : Number,
    fromIdx : Number,
    direction : Direction,
    distance : Number
  ) : Number {
    try {
      diff =
        Model.directionToPosDiff(direction)

      {startX, startY} =
        Model.idxXY(width, fromIdx)

      {x, y} =
        {startX + distance * diff.x, startY + distance * diff.y}

      Model.xyIdx(width, x, y)
    }
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

  fun puzzle1 : Puzzle {
    {
      islands =
        {
          list = [],
          fields = Map.empty()
        },
      /* bridgesToIslands(width, bridges), */
      connections =
        {
          list = [],
          fieldss = Map.empty()
        },
      connectionsMaxList = [],
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
        Bridge(2, Orientation::Horizontal, 0, 3),
        Bridge(1, Orientation::Vertical, 0, 16),
        Bridge(1, Orientation::Vertical, 3, 11),
        Bridge(1, Orientation::Horizontal, 9, 11),
        Bridge(1, Orientation::Horizontal, 11, 19),
        Bridge(1, Orientation::Horizontal, 16, 19)
      ]
  }
}
