import Midi from './midi.js'

const handlers = {
  playNote: (data) => Midi.playNote(data)
}

const init = (app) => {
  app.ports.outgoing.subscribe(({ tag, data }) => {
    let fn = handlers[tag]
    return fn
      ? fn(data)
      : console.warn(`Unrecognized Port`, tag)
  })
}

export default { init }
