# Combine individual models into one:

library(stringi)

noms <- stri_replace_all(dir(file.path("output", "json")), "", fixed = ".json")
x <- sapply(noms, function(v) {
  obj <- readLines(file.path("output", "json", sprintf("%s.json", v)))
  return(sprintf("\"%s\":%s", v, obj))
})
x <- sprintf("{%s}", paste(x, collapse = ","))
writeLines(x, file.path("output", "combine", "theme.json"))

noms <- stri_replace_all(dir(file.path("output", "geo")), "", fixed = ".json")
x <- sapply(noms, function(v) {
  obj <- readLines(file.path("output", "geo", sprintf("%s.json", v)))
  return(sprintf("\"%s\":%s", v, obj))
})
x <- sprintf("{%s}", paste(x, collapse = ","))
writeLines(x, file.path("output", "combine", "geo.json"))
