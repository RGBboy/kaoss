require('./index.html');

var Elm = require('./Main.elm'),
    AudioGraph = require('./AudioGraph')
    element = document.getElementById('mount'),
    app = Elm.Main.embed(element);

AudioGraph(app.ports.input, app.ports.output);
