library(data.table)
library(correlation)
library(ggplot2)

data("poster")

p_data <- poster[, list(edad, dusoc_social_total, dusoc_stress_total,
                        yesav_global_total, pas_global_total, sexo)]
p_data[, sexo := `levels<-`(sexo, c("Femenino", "Masculino"))]

corr = correlation(data = p_data,
                   p_adjust = "none",
                   method = "pearson",
                   bayesian_prior = "wide",
                   bayesian = TRUE)

plot(summary(corr))

p1 <- ggstatsplot::ggscatterstats(
  data = p_data,
  x = dusoc_stress_total,
  y = pas_global_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Estrés social", ylab = "Puntaje PAS"
)

p2 <- ggstatsplot::ggscatterstats(
  data = p_data,
  x = dusoc_stress_total,
  y = dusoc_social_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Estrés social", ylab = "Apoyo social"
)

p3 <- ggstatsplot::ggscatterstats(
  data = p_data,
  x = edad,
  y = pas_global_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Edad", ylab = "puntaje PAS"
)

p4 <- ggstatsplot::ggscatterstats(
  data = p_data,
  x = yesav_global_total,
  y = dusoc_stress_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Depresión", ylab = "Estrés social"
)

plots <- ((p1 / p2) | (p3 / p4)) +
  patchwork::plot_annotation(tag_levels = list("A"), tag_suffix = ".") &
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        plot.subtitle = element_text(size = 9),
        plot.tag = element_text(size = 14, face = "bold"))

ggsave(filename = "output/fig-quad.pdf", plot = plots, width = 10, height = 10, units = "in")
ggsave(filename = "output/fig-quad.jpeg", plot = plots, width = 10, height = 10, units = "in", dpi = 400)

rm(p1, p2, p3, p4, plots)

p1 <- ggstatsplot::grouped_ggscatterstats(
  data = p_data,
  grouping.var = sexo,
  x = dusoc_stress_total,
  y = pas_global_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Estrés social", ylab = "Puntaje PAS"
)

p2 <- ggstatsplot::grouped_ggscatterstats(
  data = p_data,
  grouping.var = sexo,
  x = dusoc_stress_total,
  y = dusoc_social_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Estrés social", ylab = "Apoyo social"
)

p3 <- ggstatsplot::grouped_ggscatterstats(
  data = p_data,
  grouping.var = sexo,
  x = edad,
  y = pas_global_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Edad", ylab = "puntaje PAS"
)

p4 <- ggstatsplot::grouped_ggscatterstats(
  data = p_data,
  grouping.var = sexo,
  x = yesav_global_total,
  y = dusoc_stress_total,
  type = "bayesian",
  bf.prior = "wide",
  ggtheme = theme_classic(),
  xlab = "Depresión", ylab = "Estrés social"
)

plots <- ((p1 / p2) | (p3 / p4)) +
  patchwork::plot_annotation(tag_levels = list(c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2")), tag_suffix = ".") &
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        plot.subtitle = element_text(size = 9),
        plot.tag = element_text(size = 14, face = "bold"))

ggsave(filename = "output/fig-quad-sex.pdf", plot = plots, width = 20, height = 10, units = "in")
ggsave(filename = "output/fig-quad-sex.jpeg", plot = plots, width = 20, height = 10, units = "in", dpi = 400)
