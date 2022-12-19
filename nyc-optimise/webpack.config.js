const path = require('path');

module.exports = {
  entry: './src/test.js',
  mode: 'production',
  target: 'node16',
  output: {
    path: path.resolve(__dirname, 'temp'),
    filename: 'output.js',
  },
};
