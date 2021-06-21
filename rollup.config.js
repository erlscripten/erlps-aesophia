import babel from 'rollup-plugin-babel';
import resolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import json from '@rollup/plugin-json';

export default {
  input: 'dist/index.js',
  output: {
    file: 'dist/bundle.js',
    format: 'umd',
    name: 'erlps-aesophia'
  },
  plugins: [
    resolve({ browser: true, preferBuiltins: true }),
    commonjs(),
    json(),
    babel({
      exclude: 'node_modules/**'
    })
  ]
};
