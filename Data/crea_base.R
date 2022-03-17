pacman::p_load(data.table, lubridate, magrittr)

# Empilement des bases
annees <- c(2018:2020, "20212022")
deces <- lapply(annees, function(an) fread(paste0("Data/2022-02-25_detail_2022/DC_", an, "_det.csv"))) %>%
  rbindlist(fill =  T)

# Date de décès
deces[, date_dec := ymd(paste(ADEC, MDEC, JDEC, sep = "-"))]

# Date de naissance (janvier quand mois inconnu, 1er quand jour inconnu)
deces[, date_nais := ymd(paste(ANAIS, 
                               fifelse(is.na(MNAIS) | MNAIS == 0, 1, MNAIS),
                               fifelse(is.na(JNAIS) | JNAIS == 0, 1, JNAIS), 
                               sep = "-"))]

# Âge au moment du décès
deces[, age := interval(date_nais, date_dec) %/% years(1)]


# Exportation de la base complète
# fwrite(deces, "Data/deces_2018-2022.csv")


