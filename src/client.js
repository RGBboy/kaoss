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
      // little hack until subscriptions/tasks to get current time is figured out
      document.virtualAudioGraph = virtualAudioGraph;
      // must start with something for mobile
      virtualAudioGraph.update(message.data);
    break;
    case 'update':
      console.log('update', virtualAudioGraph.currentTime, message.data);
      if (virtualAudioGraph) {
        virtualAudioGraph.update(message.data);
      }
    break;
  }

};

app.ports.output.subscribe(handleOutput);
