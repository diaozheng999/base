import { is_zero, lsr_, mul, one, to_int32 } from "@rescript/std/lib/js/caml_int64";
import { inplace_lsr, int64 } from "./int64";

// Base_int_math_int_pow_stub
export function pow(base: number, exponent: number): number {
  let mul = [1, base, 1, 1];
  let res = 1;
  while (exponent !== 0) {
    mul[1] = (mul[1] * mul[3]) | 0;
    mul[2] = (mul[1] * mul[1]) | 0;
    mul[3] = (mul[2] * mul[1]) | 0;
    res = (res * mul[exponent & 3]) | 0;
    exponent = exponent >> 2;
  }
  return res;
}

// Base_int_math_int64_pow_stub
export function pow_int64(base: int64, exponent: int64): int64 {
  let state = [one, base, one, one];
  let res = one;
  while (!is_zero(exponent)) {
    state[1] = mul(state[1], state[3]);
    state[2] = mul(state[1], state[1]);
    state[3] = mul(state[2], state[1]);
    res = mul(res, state[exponent[1] & 3]);
    inplace_lsr(exponent, 2);
  }
  return res;
}

// Base_int_math_int32_clz
export function clz(x: number): number {
  let n = 32;
  let y;
  y = x >> 16;
  if (y != 0) {
    n = n - 16;
    x = y;
  }
  y = x >> 8;
  if (y != 0) {
    n = n - 8;
    x = y;
  }
  y = x >> 4;
  if (y != 0) {
    n = n - 4;
    x = y;
  }
  y = x >> 2;
  if (y != 0) {
    n = n - 2;
    x = y;
  }
  y = x >> 1;
  if (y != 0) return n - 2;
  return n - x;
}

// Base_int_math_int32_ctz
export function ctz(x: number): number {
  if (x === 0) {
    return 32;
  }
  var n = 1;
  if ((x & 0x0000ffff) === 0) {
    n = n + 16;
    x = x >> 16;
  }
  if ((x & 0x000000ff) === 0) {
    n = n + 8;
    x = x >> 8;
  }
  if ((x & 0x0000000f) === 0) {
    n = n + 4;
    x = x >> 4;
  }
  if ((x & 0x00000003) === 0) {
    n = n + 2;
    x = x >> 2;
  }
  return n - (x & 1);
}

export function bswap16(n: number): number {
  const hi = n & 0xff00;
  const lo = n & 0xff;
  return (lo << 8) | (hi >>> 8);
}

export function bswap32(n: number): number {
  const b1 = (n & 0xff000000) >>> 24;
  const b2 = (n & 0xff0000) >>> 16;
  const b3 = n & 0xff00;
  const b4 = n & 0xff;
  return (b4 << 24) | (b3 << 8) | (b2 >>> 8) | (b1 >>> 24);
}

export function clz_int32(n: number): number {
  return clz(n);
}

export function ctz_int32(n: number): number {
  return ctz(n);
}

export function bswap64(n: int64): int64 {
  return [bswap32(n[1]), bswap32(n[0]) >>> 0];
}

// Base_int_math_int64_clz
export function clz_int64(x: int64): number {
  let n = 64;
  let y = lsr_(x, 32);
  if (!is_zero(y)) {
    n -= 32;
    x = y;
  }
  y = lsr_(x, 16);
  if (!is_zero(y)) {
    n -= 16;
    x = y;
  }
  y = lsr_(x, 8);
  if (!is_zero(y)) {
    n -= 8;
    x = y;
  }

  y = lsr_(x, 4);
  if (!is_zero(y)) {
    n -= 4;
    x = y;
  }

  y = lsr_(x, 2);
  if (!is_zero(y)) {
    n -= 2;
    x = y;
  }
  return n - to_int32(x);
}

export function ctz_int64(x: int64): number {
  if (is_zero(x)) {
    return 64;
  }
  let n = 1;
  if (!x[0] && !(x[1] & 0xffffffff)) {
    n += 32;
    inplace_lsr(x, 32);
  }
  if (!x[0] && !(x[1] & 0xffff)) {
    n += 16;
    inplace_lsr(x, 16);
  }

  if (!x[0] && !(x[1] & 0xff)) {
    n += 8;
    inplace_lsr(x, 8);
  }

  if (!x[0] && !(x[1] & 0xf)) {
    n += 4;
    inplace_lsr(x, 4);
  }
  if (!x[0] && !(x[1] & 0x3)) {
    n += 2;
    inplace_lsr(x, 2);
  }
  return n - (x[1] & 1);
}
