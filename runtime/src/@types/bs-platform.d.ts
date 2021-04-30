declare module "bs-platform/lib/js/caml_hash_primitive" {
  export function caml_hash_mix_int(state: number, int: number): number;
  export function caml_hash_mix_string(state: number, string: string): number;
  export function caml_hash_mix_final(state: number): number;
}
