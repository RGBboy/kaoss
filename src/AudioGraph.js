var VirtualAudioGraph = require('virtual-audio-graph'),
    audioContext,
    document = global.document,
    audioGraph,
    AudioContext = window.AudioContext || window.webkitAudioContext;

function update (message) {
  if (!audioGraph) {
    audioContext = new AudioContext();
    audioGraph = VirtualAudioGraph({
      audioContext,
      output: audioContext.destination
    });
    // little hack until subscriptions/tasks to get current time is figured out
    document.audioGraph = audioGraph;
  }
  console.log(audioGraph.currentTime, message.data);
  audioGraph.update(message.data);
};

module.exports = exports = { update };
