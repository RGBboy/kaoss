'use strict';

require('./index.html');

var Elm = require('./Main.elm'),
    createVirtualAudioGraph = require('virtual-audio-graph'),
    document = global.document,
    element = document.getElementById('mount'),
    app = Elm.Main.embed(element),
    started = false,
    audioContext,
    virtualAudioGraph,
    AudioContext = window.AudioContext || window.webkitAudioContext;

function frequencyRatio (x) {
  return Math.pow(Math.pow(2, x), 1/12)
}

window.addEventListener('touchend', start);
window.addEventListener('touchstart', function (event) {
  event.preventDefault();
});

function setup () {
  if (!started) {
    started = true;
    audioContext = new AudioContext();
    virtualAudioGraph = createVirtualAudioGraph({
      audioContext,
      output: audioContext.destination
    });
  };
}

function start (event) {
  event.preventDefault();
  setup();
  window.removeEventListener('touchend', start);
}

function handleOutput (graph) {

  if (!started) {
    setup();
    return;
  };

  virtualAudioGraph.update(graph);

};

app.ports.output.subscribe(handleOutput);
