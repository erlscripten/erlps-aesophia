import { readFile, writeFile, readdir } from 'fs/promises';
import { LinesAndColumns } from 'lines-and-columns';

const bundle = (await readFile('./temp/bundle.js', 'utf8')).toString();
const lines = new LinesAndColumns(bundle);

const file = (await readdir('../.nyc_output')).filter(n => n.endsWith('.json'))[0]
const nycOutput = JSON.parse(await readFile(`../.nyc_output/${file}`, 'utf8'));
const { fnMap, f: fnUsageCount } = nycOutput[
  Object.keys(nycOutput).filter(k => k.endsWith('bundle.js'))[0]
];

const fns = new Array(Object.keys(fnUsageCount).length)
  .fill()
  .map((_, idx) => ({ ...fnMap[idx], usages: fnUsageCount[idx] }))
  .filter(({ usages }) => usages === 0)
  .map(fn => {
    fn.loc.start.line -= 1;
    fn.loc.end.line -= 1;
    return fn;
  })
  .filter(({ loc }, idx, arr) => {
    if (idx === 0) return true;
    return !arr.slice(0, idx).some((prev) => {
      return lines.indexForLocation(loc.end) <= lines.indexForLocation(prev.loc.end);
    });
  })
  .map((fn) => {
    const indexStart = lines.indexForLocation(fn.loc.start);
    const indexEnd = lines.indexForLocation(fn.loc.end);
    if (indexStart == null || indexEnd == null) throw new Error('Invalid index');
    return { ...fn, len: indexEnd - indexStart, indexStart, indexEnd };
  })
  .sort((a, b) => b.len - a.len)
  .slice(0, 400)

const replaceChar = '§';
if (bundle.includes(replaceChar)) throw new Error('Invalid replace char');
const bundleOut = fns
  .reduce(
    (b, fn) => {
      const middle = b.slice(fn.indexStart, fn.indexEnd);
      if (middle.includes(replaceChar)) throw new Error('Find overlapping part');
      return [
        b.slice(0, fn.indexStart),
        replaceChar.repeat(middle.length),
        b.slice(fn.indexEnd),
      ].join('');
    },
    bundle,
  )
  .replaceAll(
    new RegExp(`${replaceChar}+`, 'gm'),
    '{throw new Error(\'This part was removed by optimisation\');}',
  );

await writeFile('./temp/bundle.js', bundleOut);
