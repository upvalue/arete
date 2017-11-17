// arete-embed.js - utility functions for embedding Emscripten-compiled Arete into a web page

/* eslint-disable */
const { h, app } = hyperapp

// This should be a droppable REPL / file loader

// You just load it, point it at an element and the thing appears.


const loadArete = (addr = 'arete.html.mem') => {
  const meminitXHR = new XMLHTTPRequest();
  meminitXHR.open('GET', addr, true);
  meminitXHR.responseType = 'arraybuffer';
  meminitXHR.send(null);
};

export default {
  thing: true,
};
