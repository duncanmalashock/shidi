import { Elm } from './elm/Main.elm'
import Ports from './js/ports.js'

document.addEventListener('contextmenu', event => event.preventDefault())

const app = Elm.Main.init()
Ports.init(app)

window.requestAnimationFrame(() => {
  // scroll C4 into the center of the view
  document.querySelectorAll(".piano__key")[71].scrollIntoView({ block: "center"})
})
