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
  targetIslandCount : Number,
  /* range: 0 - 100 */
  cycleImprovementPercent : Number,
  /* range: 0 - 100 */
  increaseConnectionCountsPercent : Number,
  totalConnectionCount : Number
}

record IslandConnection {
  islandIdx : Number,
  direction : Direction,
  size : Number
}

record TraverseResult {
  furthestLocationIndex : Maybe(Number),
  isIslandIndex : Bool,
  /* distance travelled until collision */
  distance : Number
}
