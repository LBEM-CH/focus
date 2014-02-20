FILE(REMOVE_RECURSE
  "CMakeFiles/misc"
  "mtzlib.for"
  "lcflib.for"
  "miscsubs.for"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/misc.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
