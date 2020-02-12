record Puzzle {
  islands : Array(Island),
  connections : Array(Connection),
  width : Number,
  height : Number,
  maxConnectionCount : Number
}

record Island {
  index : Number,
  maxConnectionCounts : ConnectionSizes,
  currentConnectionCounts : ConnectionSizes
}

record Connection { idx1 : Number, idx2 : Number, connectionSize : Number, orientation : Orientation }

record ConnectionSizes { top : Number, right : Number, bottom : Number, left : Number }

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

module Model {
  fun idxX(width : Number, idx : Number) : Number {
    idx % width
  }

  fun idxY(width : Number, idx : Number) : Number {
    Math.floor(idx / width)
  }


  fun isIslandFilled(island : Island) : Bool {
    (getIslandFreeConnectionSize(island)) == 0
  }

  fun getIslandFreeConnectionSize(island : Island) : Number {
    max.top - cur.top + max.right - cur.right + max.bottom - cur.bottom + max.left - cur.left
  } where {
    max = island.maxConnectionCounts
    cur = island.currentConnectionCounts
  }
}