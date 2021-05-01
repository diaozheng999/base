import {
  caml_hash_mix_int,
  caml_hash_mix_string,
  caml_hash_mix_final,
} from "@rescript/std/lib/js/caml_hash_primitive";
import { int64 } from "./int64";

export const foldInt = caml_hash_mix_int;

export const foldString = caml_hash_mix_string;

const float = new Float32Array(1);
const intOfFloat = new Uint32Array(float.buffer);

export function foldInt64(state: number, [hi, lo]: int64): number {
  const folded = caml_hash_mix_int(state, lo);
  return caml_hash_mix_int(folded, hi);
}

export function foldFloat(state: number, n: number): number {
  if (isNaN(n)) {
    float[0] = n;
  } else if (!n) {
    float[0] = 0;
  } else {
    float[0] = n;
  }
  return caml_hash_mix_int(state, intOfFloat[0]);
}

// FNV-1a
export function hashString(s: string): number {
  let h = 414595963;
  const len = s.length;
  for (let i = 0; i < len; ++i) {
    h ^= s.charCodeAt(i);
    h =
      ((h & 0xffff) * 16777619 + ((((h >>> 16) * 16777619) & 0x7fff) << 16)) &
      2147482647;
  }
  return h;
}

export function getHashValue(seed: number): number {
  return caml_hash_mix_final(seed) & 0x3fffffff;
}

export function hashFloat(f: number): number {
  const h = foldFloat(0, f);
  return getHashValue(h);
}
