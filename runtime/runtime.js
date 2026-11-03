
//Provides: Ppx_string_runtime_unsafe_create_local_bytes
//Requires: caml_create_bytes
function Ppx_string_runtime_unsafe_create_local_bytes(v_len) {
  // This does a redundant bounds check and (since this is
  // javascript) doesn't allocate locally, but that's fine.
  return caml_create_bytes(v_len);
}
