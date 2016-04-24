'use strict';

require('./index.html');

var Elm = require('./Main.elm'),
    createVirtualAudioGraph = require('virtual-audio-graph'),
    document = global.document,
    element = document.getElementById('mount'),
    app = Elm.embed(Elm.Main, element, { swap: false }),
    audioContext,
    virtualAudioGraph;

audioContext = new AudioContext();

virtualAudioGraph = createVirtualAudioGraph({
  audioContext,
  output: audioContext.destination,
});

function frequencyRatio (x) {
  return Math.pow(Math.pow(2, x), 1/12)
}

function handleOutput (message) {

  console.log(message);

  var x = message ? Math.ceil(message[0] * 12) : 1,
      y = message? message[1]: 0,
      t = virtualAudioGraph.currentTime,
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

  virtualAudioGraph.update(graph)

};

app.ports.output.subscribe(handleOutput);
