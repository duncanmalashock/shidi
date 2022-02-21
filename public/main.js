import { Elm } from '../src/Main.elm'
import Ports from './js/ports.js'

const app = Elm.Main.init()
Ports.init(app)
