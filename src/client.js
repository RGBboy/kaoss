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

function handleOutput (message) {
  switch (message.type) {
    case 'init':
      console.log('init');
      audioContext = new AudioContext();
      virtualAudioGraph = createVirtualAudioGraph({
        audioContext,
        output: audioContext.destination
      });
      // must start with something for mobile
      virtualAudioGraph.update({
        0: ['gain', 'output', { gain: 0.001 }]
      });
    break;
    case 'update':
      console.log('update', message.data);
      if (virtualAudioGraph) {
        virtualAudioGraph.update(message.data);
      }
    break;
  }

};

app.ports.output.subscribe(handleOutput);
