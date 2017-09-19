(function() {
  'use strict'

  const container = document.querySelector("#container")
  const app = window.Elm.Challenge5.embed(container)
  const storage = window.localStorage

  app.ports.sendScore.subscribe( score => {
    let highscore = storage.getItem('highscore')
    if (!highscore || highscore < score) {
      highscore = score
      storage.setItem('highscore', score)
    }
    app.ports.getHighscore.send(parseInt(highscore))
  })

}())
