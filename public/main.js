import { Elm } from '../src/Main.elm'
import { WebMidi } from 'webmidi'

const app = Elm.Main.init()

app.ports.outgoing.subscribe(({ tag, data }) => {
  switch (tag) {
    case 'playNote':
      console.log("playing note number ", data);
      WebMidi.outputs[4].playNote(data, 1, { duration: 100 });
      return
    default:
      return console.warn(`Unrecognized Port`, tag)
  }
})

// Enable WebMidi.js and trigger the onEnabled() function when ready
WebMidi.enable().then(onEnabled).catch(err => alert(err));

// Function triggered when WebMidi.js is ready
function onEnabled() {
  // Display available MIDI input devices
  if (WebMidi.inputs.length < 1) {
    document.body.innerHTML+= "No device detected.";
  } else {
    WebMidi.inputs.forEach((device, index) => {
      document.body.innerHTML+= `${index}: ${device.name} <br>`;
    });
  }
}
