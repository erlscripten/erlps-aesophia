set -ex

mkdir -p temp
sed 's/\t/  /g' ../dist/bundle.js > ./temp/bundle.js
node ./src/test.js

npx webpack
node ./temp/output.js

rm -rf ../.nyc_output
NODE_OPTIONS=--max-old-space-size=6000 npx nyc node ./src/test.js
node remove-unused.mjs
npx webpack
node ./temp/output.js

npx google-closure-compiler --js ./temp/bundle.js  --js_output_file ./temp/bundle-out.js > /dev/null 2>&1 # -O ADVANCED
mv ./temp/bundle-out.js ./temp/bundle.js
npx webpack
node ./temp/output.js
