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

function handleOutput (message) {

  console.log(message);

  if (!started) {
    setup();
    return;
  };

  var isDown = message[0],
      x = Math.ceil(message[1] * 12),
      y = isDown ? message[2] : 0,
      graph = {
    0: [
      'gain',
      'output',
      {
        gain: y * y
      }
    ],
    1: [
      'oscillator',
      0,
      {
        type: 'square',
        frequency: 110 * frequencyRatio(x)
      }
    ],
    2: [
      'oscillator',
      0,
      {
        type: 'sawtooth',
        frequency: 110 * frequencyRatio(x + 7),
        detune: 4
      }
    ]
  };

  virtualAudioGraph.update(graph);

};

app.ports.output.subscribe(handleOutput);
