routes {
  / {
    /* Game.setPuzzle(Generation.puzzle1()) */
    Game.setPuzzle(
      Debug.log(Generation.generatePuzzle(214123, 8, 8)))
  }
}
