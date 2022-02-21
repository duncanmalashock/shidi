import Midi from './midi.js'

const init = (app) => {
  app.ports.outgoing.subscribe(({ tag, data }) => {
    switch (tag) {
      case 'playNote':
        return Midi.playNote(data)
      default:
        return console.warn(`Unrecognized Port`, tag)
    }
  })
}

export default { init }
