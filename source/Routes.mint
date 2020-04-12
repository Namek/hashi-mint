routes {
  / {
    /* Game.setPuzzle(Generation.puzzle1()) */
    Game.setPuzzle(
      Debug.log(Generation.generatePuzzle(Random.createSeed(), 8, 8)))
  }
}
