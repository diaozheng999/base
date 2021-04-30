declare var global : {
  camlRandomSeed?: () => number[];
}


const float = new Float32Array(1);
const intOfFloat = new Uint32Array(float.buffer);

export function randomSeed() {
  if (typeof global.camlRandomSeed === "function") {
    const generate = global.camlRandomSeed();
    if (generate.length >= 12) {
      return generate;
    }
  }
  const n = Array(12);

  for (let i = 0; i < 12; ++i) {
    float[0] = Math.random();
    n[i] = intOfFloat[0];
  }

  return n;
}