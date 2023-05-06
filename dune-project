(lang dune 3.4)

(using mdx 0.3)

(generate_opam_files true)

(license ISC)

(authors "Andy Ray")

(package
 (name hardcaml_video_codecs)
 (synopsis "JPEG CODEC")
 (description "JEPG CODEC with model and hardware design.")
 (depends
  hardcaml
  hardcaml_fixed_point
  ppx_deriving_hardcaml
  (hardcaml_waveterm :with-test)
  core
  mdx
  tsdl))
