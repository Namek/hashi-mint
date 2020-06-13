module Generation {
  const DIRECTIONS = [
    Direction::Right,
    Direction::Down,
    Direction::Left,
    Direction::Up
  ]

  const MIN_ISLANDS_DISTANCE = 2

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
    {targetIslandCount, cycleImprovementPercent, increaseConnectionCountsPercent} =
      {20, 50, 30}

    genState0 =
      {
        width = width,
        height = height,
        maxConnectionCount = maxConnectionCount,
        fieldsMap = Map.empty(),
        islandIndices = [],
        connectionsMaxList = [],
        targetIslandCount = targetIslandCount,
        cycleImprovementPercent = cycleImprovementPercent,
        increaseConnectionCountsPercent = increaseConnectionCountsPercent
      }

    genState1 =
      Misc.log("RESULT", generateIslands(seed, genState0))

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

      {firstIslandX, firstIslandY, rand1} =
        randomizePosition(genState0.width, genState0.height, rand0)

      firstIslandIdx =
        Model.xyIdx(genState0.width, firstIslandX, firstIslandY)

      genState1 =
        addIsland(firstIslandX, firstIslandY, genState0)

      /*
      Algorithm:
      0. input parameter: targetIslandCount; one island already exists
      1. generate islands. Do it until islandCount < targetIslandCount OR there is a failure
      1.1. pick (randomly) an existing island that can have new neighbours
      1.1.1. if it can't then cross out this one and pick another existing island
      1.2. try to create a neighbour
      1.3. go to 3 if no existing island can be picked anymore
      2. create cycles. Do it cycleImprovementPercent*targetIslandCount times
      2.1. find a pair of non-connected islands and make a connection
      3. increase connection counts. Got through all connections
        until 1/3 of island count is done or a failure appears.
      3.1. pick (randomly) an existing island that can increase existing connections
      3.2. increase picked connection
      */
      {state2, rand2} =
        generateIslandsOnly(genState1, rand1)

      {state3, rand3} =
        createCycles(state2, rand2)

      {state4, rand4} =
        increaseConnectionCounts(state3, rand3)

      state4
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
            Misc.log("addIsland ->", idx)

          { genState |
            islandIndices = Array.push(idx, genState.islandIndices),
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
          /* @optimize: we should look through the connectionsMaxList instead of traversing */
          res =
            traverse(genState, idx, direction, false)

          case (res.furthestLocationIndex) {
            Maybe::Just idx2 =>
              if (res.isIslandIndex) {
                increaseConnection(idx, idx2, genState)
              } else {
                genState
              }

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
            |> Misc.watchMeNoErr
            |> Maybe.withDefault(ConnectionSizes(0, 0, 0, 0))

          conns2 =
            getIslandConnectionSizes(genState, idx2)
            |> Misc.watchMeNoErr
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
            Misc.log(
              "increaseConnection ->",
              `{
                idx1: #{idx1},
                idx2: #{idx2},
                newCons1: #{newConns1},
                newCons2: #{newConns2}
              }`)

          {existingConnection, restConns} =
            genState.connectionsMaxList
            |> Util.Collection.findAndGetRest(
              (conn : Connection) : Bool {
                (conn.idx1 == idx1 && conn.idx2 == idx2) || (conn.idx1 == idx2 && conn.idx2 == idx1)
              })

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

          { genState |
            fieldsMap =
              genState.fieldsMap
              |> Map.delete(idx1)
              |> Map.delete(idx2)
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

    filterIslandsWhichCanIncreaseOrCreateConnectionToNeighbour =
      (state0 : GenerationState) : Array(Bridge) {
        state0.islandIndices
        |> Array.reduce(
          [],
          (acc : Array(Bridge), idx : Number) {
            try {
              islandBridges =
                getIslandConnectionSizes(state0, idx)
                |> Maybe.map(
                  (conns : ConnectionSizes) : Array(Bridge) {
                    DIRECTIONS
                    |> Array.map(
                      (dir : Direction) {
                        try {
                          connSize =
                            Model.directionToConnectionSize(conns, dir)

                          orientation =
                            Model.directionToOrientation(dir)

                          if (connSize < state0.maxConnectionCount) {
                            try {
                              res =
                                traverse(state0, idx, dir, connSize == 0)

                              neighbourIdx =
                                res.furthestLocationIndex

                              case (neighbourIdx) {
                                Maybe::Just idx2 =>
                                  if (res.isIslandIndex) {
                                    try {
                                      getIslandConnectionSizes(state0, idx2)
                                      |> Maybe.andThen(
                                        (conns2 : ConnectionSizes) {
                                          try {
                                            connSize2 =
                                              Model.directionToConnectionSize(conns2, Model.oppositeDirection(dir))

                                            if (connSize2 < state0.maxConnectionCount) {
                                              Maybe::Just(Maybe::Just(Bridge(connSize2, orientation, idx2, idx)))
                                            } else {
                                              Maybe::Just(Maybe::Nothing)
                                            }
                                          }
                                        })
                                      |> Maybe.withDefault(Maybe::Just(Bridge(0, orientation, idx, idx2)))
                                    }
                                  } else {
                                    Maybe::Nothing
                                  }

                                => Maybe::Nothing
                              }
                            }
                          } else {
                            Maybe::Nothing
                          }
                        }
                      })
                    |> Array.compact()
                  })
                |> Maybe.withDefault([])

              Array.append(acc, islandBridges)
              |> Util.Collection.distinct(
                (bridge : Bridge) {
                  try {
                    sortedIdx1 =
                      Math.min(bridge.idx1, bridge.idx2)

                    sortedIdx2 =
                      Math.max(bridge.idx1, bridge.idx2)

                    Number.toString(sortedIdx1) + "+" + Number.toString(sortedIdx2)
                  }
                })
            }
          })
      }

    /* returns directions with max distances (but minimum 2) */
    emptyDirsFromIsland =
      (genState : GenerationState, idx : Number) : Array(Tuple(Direction, Number)) {
        try {
          conns =
            getIslandConnectionSizes(genState, idx)

          dirs =
            conns
            |> Maybe.map(
              (conns : ConnectionSizes) : Array(Tuple(Direction, Number)) {
                DIRECTIONS
                |> Array.select(
                  (dir : Direction) { Model.directionToConnectionSize(conns, dir) == 0 })
                |> Array.reduce(
                  [],
                  (
                    acc : Array(Tuple(Direction, Number)),
                    dir : Direction
                  ) {
                    try {
                      res =
                        traverse(genState, idx, dir, true)

                      spaceBetweenIslands =
                        res.distance - 1

                      if (spaceBetweenIslands >= MIN_ISLANDS_DISTANCE) {
                        Array.push({dir, res.distance}, acc)
                      } else {
                        acc
                      }
                    }
                  })
              })
            |> Maybe.withDefault([])

          dirs
        }
      }

    /* @optimize: remove this function and cache left connections to islands? */
    filterIslandsWhichCanHaveNewNeighbours =
      (genState : GenerationState) {
        genState.islandIndices
        |> Array.reduce(
          {[], Map.empty()},
          (
            acc : Tuple(Array(Number), Map(Number, Array(Tuple(Direction, Number)))),
            idx : Number
          ) {
            try {
              {accIndices, accDirsMap} =
                acc

              dirs =
                emptyDirsFromIsland(genState, idx)

              if (Array.size(dirs) > 0) {
                {Array.push(idx, accIndices), Map.set(idx, dirs, accDirsMap)}
              } else {
                acc
              }
            }
          })
      }

    generateIslandsOnly =
      (state0 : GenerationState, rand0 : Rand) : Tuple(GenerationState, Rand) {
        try {
          iter =
            (
              state1 : GenerationState,
              rand1 : Rand,
              failCount : Number
            ) : Tuple(GenerationState, Rand) {
              if (failCount >= 3 || Array.size(state1.islandIndices) >= state1.targetIslandCount) {
                {state1, rand1}
              } else {
                try {
                  {filteredIslandsIdxs, idxToDirsWithDistance} =
                    filterIslandsWhichCanHaveNewNeighbours(state1)

                  {maybeIdx0, rand2} =
                    Random.choiceMaybe(filteredIslandsIdxs, rand1)

                  case (maybeIdx0) {
                    Maybe::Nothing => {state1, rand2}

                    Maybe::Just idx0 =>
                      try {
                        curIslandConnectionSizes =
                          getIslandConnectionSizes(state1, idx0)

                        possibleDirections =
                          emptyDirsFromIsland(state1, idx0)

                        if (Array.size(possibleDirections) > 0) {
                          try {
                            {d, rand3} =
                              Random.choice(possibleDirections, rand2)

                            {newDirection, maxDistance} =
                              d

                            {distance, rand4} =
                              Random.number(MIN_ISLANDS_DISTANCE, maxDistance, rand3)

                            targetIndex =
                              movePosition(state1.width, idx0, newDirection, distance)

                            /* check whether island can be actually placed on this position */
                            closestNeighbourDistance =
                              findClosestIslandDistance(targetIndex, state1)
                              |> Maybe.withDefault(MIN_ISLANDS_DISTANCE)

                            if (closestNeighbourDistance >= MIN_ISLANDS_DISTANCE) {
                              try {
                                {newX, newY} =
                                  Model.idxXY(state1.width, targetIndex)

                                state2 =
                                  addIsland(newX, newY, state1)

                                state3 =
                                  increaseConnection(idx0, targetIndex, state2)

                                iter(state3, rand3, failCount)
                              }
                            } else {
                              iter(state1, rand3, failCount + 1)
                            }
                          }
                        } else {
                          {state1, rand2}
                        }
                      }
                  }
                }
              }
            }

          iter(state0, rand0, 0)
        }
      }

    createCycles =
      (state0 : GenerationState, rand0 : Rand) : Tuple(GenerationState, Rand) {
        try {
          islandsBridges =
            filterIslandsWhichCanIncreaseOrCreateConnectionToNeighbour(state0)
            |> Array.select((bridge : Bridge) { bridge.connectionCount == 0 })

          chance =
            state0.cycleImprovementPercent

          islandsBridges
          |> Array.reduce(
            {state0, rand0},
            (acc : Tuple(GenerationState, Rand), bridge : Bridge) {
              try {
                {state1, rand1} =
                  acc

                {shouldProceed, rand2} =
                  Random.chance(chance, rand1)

                state2 =
                  shouldProceed
                  |> Bools.resolve(
                    () { increaseConnection(bridge.idx1, bridge.idx2, state1) },
                    state1)

                {state2, rand2}
              }
            })
        }
      }

    increaseConnectionCounts =
      (state0 : GenerationState, rand0 : Rand) : Tuple(GenerationState, Rand) {
        try {
          islandsBridges =
            filterIslandsWhichCanIncreaseOrCreateConnectionToNeighbour(state0)
            |> Array.select(
              (bridge : Bridge) { bridge.connectionCount > 0 && bridge.connectionCount < state0.maxConnectionCount })

          chance =
            state0.increaseConnectionCountsPercent

          islandsBridges
          |> Array.reduce(
            {state0, rand0},
            (acc : Tuple(GenerationState, Rand), bridge : Bridge) {
              try {
                {state1, rand1} =
                  acc

                {shouldProceed, rand2} =
                  Random.chance(chance, rand1)

                state2 =
                  shouldProceed
                  |> Bools.resolve(
                    () { increaseConnection(bridge.idx1, bridge.idx2, state1) },
                    state1)

                {state2, rand2}
              }
            })
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

  /*
  Traverses through map in a direction from starting point and returns:
     - maybe furthest achieved location (index);
     - bool: true when is index island or else when last index;
     - distance travelled
  */
  fun traverse (
    genState : GenerationState,
    fromIndex : Number,
    direction : Direction,
    shouldCheckConnections : Bool
  ) : TraverseResult {
    try {
      diff =
        Model.directionToPosDiff(direction)

      {startX, startY} =
        Model.idxXY(genState.width, fromIndex)

      iterate =
        (
          x : Number,
          y : Number,
          distance : Number,
          prevIdx : Number
        ) : TraverseResult {
          try {
            idx =
              Model.xyIdx(genState.width, x, y)

            if (x < 0 || x >= genState.width || y >= genState.height || y < 0) {
              try {
                if (distance == 0) {
                  TraverseResult(Maybe::Nothing, false, 0)
                } else {
                  TraverseResult(Maybe::Just(prevIdx), false, distance)
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
                        FieldType::Island => TraverseResult(Maybe::Just(idx), true, distance + 1)
                        FieldType::Connection => iterate(x + diff.x, y + diff.y, distance + 1, idx)
                      }

                    => iterate(x + diff.x, y + diff.y, distance + 1, idx)
                  }
                } else {
                  TraverseResult(if (distance == 0) {
                    Maybe::Nothing
                  } else {
                    Maybe::Just(prevIdx)
                  }, false, distance)
                }
              }
            }
          }
        }

      iterate(startX + diff.x, startY + diff.y, 0, fromIndex)
    }
  }

  fun findClosestIslandDistance (posIdx : Number, state0 : GenerationState) : Maybe(Number) {
    DIRECTIONS
    |> Array.reduce(
      Maybe::Nothing,
      (foundMin : Maybe(Number), dir : Direction) {
        try {
          res =
            traverse(state0, posIdx, dir, false)

          if (res.isIslandIndex) {
            case (foundMin) {
              Maybe::Just val => Maybe::Just(Math.min(val, res.distance))

              => Maybe::Just(res.distance)
            }
          } else {
            foundMin
          }
        }
      })
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
