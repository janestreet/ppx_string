
(module
   (import "env" "caml_create_bytes"
      (func $caml_create_bytes (param (ref eq)) (result (ref eq))))
   (export "Ppx_string_runtime_unsafe_create_local_bytes" (func $caml_create_bytes))
   )
