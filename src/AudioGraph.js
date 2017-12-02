var VirtualAudioGraph = require('virtual-audio-graph'),
    audioContext,
    document = global.document,
    audioGraph,
    pinkNoiseBuffer,
    AudioContext = window.AudioContext || window.webkitAudioContext;

var HandleMessage = input => message => {
  if (!audioGraph) {
    audioContext = new AudioContext();
    audioGraph = VirtualAudioGraph({
      audioContext,
      output: audioContext.destination
    });

    var pinkNoiseData = PinkNoiseData();
    pinkNoiseBuffer = audioContext.createBuffer(1, pinkNoiseData.length, audioContext.sampleRate)
    pinkNoiseBuffer.getChannelData(0).set(pinkNoiseData);

    // little hack until subscriptions/tasks to get current time is figured out
    document.audioGraph = audioGraph;
  }

  switch (message.type) {
    case 'update':
      console.log(audioGraph.currentTime, message.data);
      // loop over data to find any custom nodes. I.e. pink noise
      Object.keys(message.data).map(function(key, index) {
        if (message.data[key][0] === 'pinkNoise') {
          message.data[key] = [
            'bufferSource',
            message.data[key][1],
            {
              buffer: pinkNoiseBuffer,
              loop: true
            }
          ]
        };
      });

      audioGraph.update(message.data);
    break;
    case 'getCurrentTime':
      input.send(audioContext.currentTime);
    break;
  }


};

var PinkNoiseData = () => {
  var data = new Float32Array(44100 * 5);

  // http://noisehack.com/generate-noise-web-audio-api/
  var b = [0, 0, 0, 0, 0, 0, 0];

  for (var i = 0; i < data.length; i += 1) {
    var white = Math.random() * 2 - 1;

    b[0] = 0.99886 * b[0] + white * 0.0555179;
    b[1] = 0.99332 * b[1] + white * 0.0750759;
    b[2] = 0.96900 * b[2] + white * 0.1538520;
    b[3] = 0.86650 * b[3] + white * 0.3104856;
    b[4] = 0.55000 * b[4] + white * 0.5329522;
    b[5] = -0.7616 * b[5] - white * 0.0168980;

    data[i] = b[0] + b[1] + b[2] + b[3] + b[4] + b[5] + b[6] + white * 0.5362 * 0.11;
    b[6] = white * 0.115926;
  }
  return data;
}

var AudioGraph = (input, output) => {
  var handleMessage = HandleMessage(input);
  output.subscribe(handleMessage);
  return () => {
    output.unsubscribe(handleMessage);
  }
  // can return function to unsubscribe
};

module.exports = exports = AudioGraph;
