store Editor {
  fun setCycleImprovementPercent (evt : Html.Event) {
    try {
      genParams =
        Game.genParams

      newGenParams =
        { genParams | cycleImprovementPercent = getEventNumber(evt) }

      Game.changeGenParams(newGenParams)
    }
  }

  fun setIncreaseConnectionCountsPercent (evt : Html.Event) {
    try {
      genParams =
        Game.genParams

      newGenParams =
        { genParams | increaseConnectionCountsPercent = getEventNumber(evt) }

      Game.changeGenParams(newGenParams)
    }
  }

  fun newSeed {
    try {
      seed =
        `Math.floor(Math.random() * 100000)`

      Game.initPuzzle(seed, Game.genParams)
    }
  }

  fun setWidth (evt : Html.Event) {
    try {
      width =
        getEventNumber(evt)

      genParams =
        Game.genParams

      newGenParams =
        { genParams | width = width }

      Game.changeGenParams(newGenParams)
    }
  }

  fun setHeight (evt : Html.Event) {
    try {
      height =
        getEventNumber(evt)

      genParams =
        Game.genParams

      newGenParams =
        { genParams | height = height }

      Game.changeGenParams(newGenParams)
    }
  }

  fun setTargetIslandCount (evt : Html.Event) {
    try {
      targetIslandCount =
        getEventNumber(evt)

      genParams =
        Game.genParams

      newGenParams =
        { genParams | targetIslandCount = targetIslandCount }

      Game.changeGenParams(newGenParams)
    }
  }

  fun getEventNumber (evt : Html.Event) : Number {
    Number.fromString(`#{evt}.target.value`)
    |> Maybe.withDefault(0)
  }
}

component Editor.View {
  connect Game exposing { genParams }

  style editor {
    display: inline-block;
    position: absolute;
    top: 0;

    background: white;

    * {
      margin-right: 10px;
    }

    label {
      display: inline-block;
    }
  }

  style sizeInput {
    width: 35px;
    text-align: center;
  }

  fun render {
    <fieldset::editor>
      <legend>
        "Level Config"
      </legend>

      <label>
        <span>
          "Seed: "
          <{ Number.toString(Game.seed) }>
        </span>

        <button onClick={Editor.newSeed}>
          "new seed"
        </button>
      </label>

      <label>
        "Width: "

        <input::sizeInput
          type="number"
          min="4"
          value={Number.toString(genParams.width)}
          onInput={Editor.setWidth}/>
      </label>

      <label>
        "Height: "

        <input::sizeInput
          type="number"
          min="4"
          value={Number.toString(genParams.height)}
          onInput={Editor.setHeight}/>
      </label>

      <label>
        "Islands: "

        <input::sizeInput
          type="number"
          min="2"
          value={Number.toString(genParams.targetIslandCount)}
          onChange={Editor.setTargetIslandCount}/>
      </label>

      <input
        type="range"
        step="1"
        min="0"
        max="100"
        value={Number.toString(genParams.cycleImprovementPercent)}
        onInput={Editor.setCycleImprovementPercent}/>

      <input
        type="range"
        step="1"
        min="0"
        max="100"
        value={Number.toString(genParams.increaseConnectionCountsPercent)}
        onInput={Editor.setIncreaseConnectionCountsPercent}/>
    </fieldset>
  }
}
