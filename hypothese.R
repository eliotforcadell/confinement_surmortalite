pacman::p_load(data.table, ggplot2)
theme_set(theme_minimal())

deces <- fread("Data/deces_2018-2022.csv")[ADEC != 2022]

# Périodes :
# 1 : 1er janvier - 16 mars
# 2 : 17 mars - 11 mai
# 3 : 12 mai - 29 octobre
# 4 : 30 octobre - 15 décembre
# 5 : 16 décembre - 31 décembre

deces[, md_dec := format(date_dec, "%m-%d")]
deces[, periode := fcase(md_dec >= "01-01" & md_dec <= "03-16", 1,
                         md_dec >= "03-17" & md_dec <= "05-11", 2,
                         md_dec >= "05-12" & md_dec <= "10-29", 3,
                         md_dec >= "10-30" & md_dec <= "12-16", 4,
                         md_dec >= "12-17" & md_dec <= "12-31", 5)]

# Nombre de jours par période
deces[, length(unique(date_dec)), keyby = .(periode, ADEC)]

# Comparaison entre le début d'année et le premier confinement
tab <- deces[date_dec != "2020-02-29" & 
               age >= 18 & age <= 30, 
             .(ndec = .N), keyby = .(periode, ADEC, SEXE)
             ][data.table(periode = 1:5, njours = c(75, 56, 171, 48, 15)),
               moydec := ndec/njours, on = "periode"]

ggplot(tab[periode %in% 1:2]) +
  geom_line(aes(x = factor(periode), y = ndec, group = SEXE, colour = SEXE)) +
  facet_wrap(~factor(ADEC), ncol = 1)

tab


# Seulement sur les mercredis
deces[, weekday_dec := format(date_dec, "%a")]
tab <- deces[weekday_dec == "Wed" & periode %in% 1:2 &
               age >= 18 & age <= 30, 
             .(ndec = .N), keyby = .(periode, ADEC, SEXE)]

ggplot(tab[periode %in% 1:2]) +
  geom_line(aes(x = factor(periode), y = ndec, group = SEXE, colour = SEXE)) +
  facet_wrap(~factor(ADEC), ncol = 1)

