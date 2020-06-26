enum UrlChange {
  Replace
  PushNew
  Nothing
}

store App {
  fun setState (
    seed : Number,
    params : GenerationParams,
    urlBehavior : UrlChange
  ) {
    sequence {
      Game.initPuzzle(seed, params)

      loc =
        paramsToUrl(seed, params)

      case (urlBehavior) {
        UrlChange::Replace => Window.navigate(loc)
        UrlChange::PushNew => `history.pushState({}, '', #{loc})`
        UrlChange::Nothing => next {  }
      }
    }
  }

  fun replaceUrl (seed : Number, params : GenerationParams) {
    Window.navigate(paramsToUrl(seed, params))
  }

  fun paramsToUrl (seed : Number, params : GenerationParams) : String {
    try {
      loc =
        [seed, params.width, params.height, params.maxConnectionCount, params.targetIslandCount, params.cycleImprovementPercent, params.increaseConnectionCountsPercent]
        |> Array.map(Number.toString)
        |> Array.intersperse("/")
        |> String.concat()

      "/puzzle/" + loc
    }
  }
}

component Main {
  use Provider.Resize { resizes = handleResizes }

  state showConfigurator = false

  fun toggleConfigurator {
    next { showConfigurator = !showConfigurator }
  }

  style base {
    width: 100vw;
    height: 100vh;
    margin: 0;
    background-color: #F8F8F8;
  }

  fun handleResizes (evt : Html.Event) : Promise(Never, Void) {
    try {
      gameView
      |> Maybe.map((v : Game.View) { v.recalculateScaleFactor() })

      next {  }
    }
  }

  fun componentDidMount {
    sequence {
      Timer.timeout(200, "")

      gameView
      |> Maybe.map((v : Game.View) { v.recalculateScaleFactor() })
    }
  }

  fun render : Html {
    <div::base>
      <input
        type="checkbox"
        checked={showConfigurator}
        onChange={toggleConfigurator}/>

      if (showConfigurator) {
        <Editor.View/>
      }

      <Game.View as gameView isDebugMode={showConfigurator}/>
    </div>
  }
}
