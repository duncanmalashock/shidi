import * as Tone from 'tone'

const play = (notes) => {
  const synth = new Tone.PolySynth(Tone.Synth).toDestination()
  const now = Tone.now()
  notes.forEach(note => {
    synth.triggerAttackRelease(Tone.Midi(note.midi).toFrequency(), note.duration, now + note.start)
  })
}

export default { play }
