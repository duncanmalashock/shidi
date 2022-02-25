import { Elm } from '../src/Main.elm'
import Ports from './js/ports.js'

document.addEventListener('contextmenu', event => event.preventDefault())

const app = Elm.Main.init()
Ports.init(app)
