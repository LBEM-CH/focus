FILE(REMOVE_RECURSE
  "CMakeFiles/gen"
  "unix.for"
  "diskio.for"
  "ccplib.for"
  "subs.for"
  "parser.for"
  "symlib.for"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/gen.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
