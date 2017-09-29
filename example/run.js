 var Elm = require("./elm.js")

 var app = Elm.Example.worker();

 app.ports.log.subscribe(console.log)
