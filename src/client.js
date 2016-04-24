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

function handleOutput (message) {

  console.log(message);

  var x = message ? message[0] : 1,
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
        frequency: 440 + (x * 440)
      }
    ],
    2: [
      'oscillator',
      0,
      {
        type: 'sawtooth',
        frequency: 660,
        detune: 4
      }
    ]
  };

  virtualAudioGraph.update(graph)

};

app.ports.output.subscribe(handleOutput);
