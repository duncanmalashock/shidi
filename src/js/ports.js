import Audio from './audio.js'

const handlers = {
  playNote: (data) => Audio.play([{midi: data, start: 0, duration: "8n"}]),
  playSong: (notes) => Audio.play(notes)
}

const init = (app) => {
  app.ports && app.ports.outgoing && app.ports.outgoing.subscribe(({ tag, data }) => {
    let fn = handlers[tag]
    return fn
      ? fn(data)
      : console.warn(`Unrecognized Port`, tag)
  })
}

export default { init }
