store App {
  state lastUrlNavigationState : Maybe(Tuple(Number, GenerationParams)) = Maybe::Nothing

  fun setState (seed : Number, params : GenerationParams) {
    sequence {
      Game.initPuzzle(seed, params)

      next { lastUrlNavigationState = Maybe::Just({seed, params}) }
    }
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

  fun watchUrl (seed : Number, params : GenerationParams) {
    case (lastUrlNavigationState) {
      Maybe::Just(tpl) =>
        try {
          {seed2, params2} =
            tpl

          if (seed != seed2 || params != params2) {
            try {
              `history.pushState({}, '', window.location.origin)`

              next { lastUrlNavigationState = Maybe::Nothing }
            }
          } else {
            next {  }
          }
        }

      => next {  }
    }
  }

  fun sharePuzzle {
    try {
      str =
        `window.location.origin` + paramsToUrl(Game.seed, Game.genParams)

      `
      (() => {
        var el = document.createElement('textarea');
        el.value = #{str};
        el.setAttribute('readonly', '');
        el.style.position = 'absolute';
        el.style.left = '-9999px';
        document.body.appendChild(el);
        el.select();
        document.execCommand('copy');
        document.body.removeChild(el);
      })()
      `
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
    button {
      font-size: 20px;
    }
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

      <button onClick={App.sharePuzzle}>
        "🔗 Copy Puzzle Link"
      </button>

      <Game.View as gameView isDebugMode={showConfigurator}/>
    </div>
  }
}
