suppressMessages(library(tidyverse))

for(fn in list.files(pattern = "*.rds")) {
  ubigeos <- readRDS(fn)
  out <- str_remove(fn, "\\.rds$")
  # digits = NA conserva la precisión completa; el valor por defecto (4)
  # redondea los decimales y hace que el JSON pierda datos frente al CSV.
  json_fmt <- jsonlite::toJSON(ubigeos, digits = NA)
  write_file(
    json_fmt,
    file = glue::glue("{out}.json")
  )
}
