import {WebMidi} from "webmidi";

// Function triggered when WebMidi.js is ready
const onEnabled = () => {
}

// Enable WebMidi.js and trigger the onEnabled() function when ready
WebMidi.enable().then(onEnabled).catch(err => alert(err));

const playNote = (noteNumber) => {
  console.log("playing note number ", noteNumber);
  WebMidi.outputs[4].playNote(noteNumber, 1, { duration: 100 });
}

export default {
  playNote
}
