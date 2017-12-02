require('./index.html');

var Elm = require('./Main.elm'),
    AudioGraph = require('./AudioGraph')
    element = document.getElementById('mount'),
    app = Elm.Main.embed(element);

app.ports.output.subscribe(AudioGraph.update);
