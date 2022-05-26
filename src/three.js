import { Elm } from "./three.elm";

var app = Elm.Three.init({ node: document.getElementById("root") });

app.ports.sendAlert.subscribe(function (message) {
  console.log(message);
  alert(message);
});
