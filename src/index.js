import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const root = document.getElementById("root");

Elm.Main.init({
  node: root
});

registerServiceWorker();
