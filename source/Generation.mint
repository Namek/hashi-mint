module Generation {
  fun generatePuzzle(seed : Number, width : Number, height : Number) : Puzzle {
    ret
  } where {
    maxConnectionCount = 2
    bridges = []
    islands = bridgesToIslands(width, bridges)
    noConnections = []
    ret = { islands = islands, connections = noConnections, width = width, height = height, maxConnectionCount = 0 }
  }

  fun sumConnectionSizes(conn1 : ConnectionSizes, conn2 : ConnectionSizes) : ConnectionSizes {
    { top = conn1.top + conn2.top, right = conn1.right + conn2.right, bottom = conn1.bottom + conn2.bottom, left = conn1.left + conn2.left }
  }

  fun bridgesToIslands(width : Number, bridges : Array(Bridge)) : Array(Island) {
    addNextBridge(bridges, [])
  } where {
    increaseIsland = (index : Number, connectionSizesToAdd : ConnectionSizes, islands : Array(Island)) : Array(Island) {
      case (Array.at(index, islands)) {
        Maybe::Just island =>
          islands |> Array.setAt(index, { island | maxConnectionCounts = sumConnectionSizes(island.maxConnectionCounts, connectionSizesToAdd) } )
        Maybe::Nothing =>
          islands |> Array.setAt(index, {
            index = index,
            maxConnectionCounts = connectionSizesToAdd,
            currentConnectionCounts = {top = 0, right = 0, bottom = 0, left = 0}
          })
      }
    }

    addNextBridge = (leftBridges : Array(Bridge), acc : Array(Island)) : Array(Island) {
      case (Array.at(0, leftBridges)) {
        Maybe::Nothing => acc
        Maybe::Just bridge =>
          addNextBridge(leftBridges |> Array.slice(0, Array.size(leftBridges)), acc)
      }
    }
  }

}

record Bridge {
  connectionCount : Number,
  orientation : Orientation,
  islandIdx1 : Number,
  islandIdx2 : Number
}
