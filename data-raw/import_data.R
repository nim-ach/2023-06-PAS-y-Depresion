
library(readxl)
library(data.table)

poster <- read_excel(path = "data-raw/data_raw.xlsx",
           col_names = readLines("data-raw/col_names"),
           range = "C8:CF32", sheet = 1L)

poster <- as.data.table(poster);

anonymize = function(x) {as.factor(x = as.integer(x = factor(x)))}
poster[, id := anonymize(nombre)]
poster[, nombre := NULL]

poster[, sexo := factor(sexo)]

save(poster, file = "data/poster.RData")
