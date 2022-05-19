import { Elm } from './elm/Main.elm'
import Ports from './js/ports.js'

// Initialize the Elm app
const app = Elm.Main.init()

// Initialize ports
Ports.init(app)

// Turn off contextual menus on right-click, so that right-clicks can be used
// for removing notes
document.addEventListener('contextmenu', event => event.preventDefault())

// scroll C4 into the center of the view
window.requestAnimationFrame(() => {
  document.querySelectorAll(".piano__key")[71].scrollIntoView({ block: "center"})
})
