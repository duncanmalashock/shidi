import * as Tone from 'tone'

const play = (notes) => {
  Tone.Transport.bpm.value = 120
  const synth = new Tone.PolySynth(Tone.Synth).toDestination()
  const now = Tone.now()
  notes.forEach(note => {
    synth.triggerAttackRelease(Tone.Midi(note.midi).toFrequency(), "8n", now + (note.index * 0.25))
  })
}

export default { play }
