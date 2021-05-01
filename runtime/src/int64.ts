export type int64 = [number, number];

export function inplace_lsr(n: int64, b: number): void {
  if (b === 0) {
    return;
  }

  const hi = n[0];
  const offset = (b - 32) | 0;

  if (offset === 0) {
    n[1] = hi >>> 0;
    n[0] = 0;
  } else if (offset > 0) {
    n[1] = hi >>> offset;
    n[0] = 0;
  } else {
    n[1] = ((hi << (-offset | 0)) | (n[1] >>> b)) >>> 0;
    n[0] = hi >>> b;
  }
}
