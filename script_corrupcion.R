#### Analisis de datos corrupcion municipal ####

library(dplyr)
library(tidyverse)
library(rstatix)
library(readxl)
library(writexl)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(stargazer)
sep2011 <- read.csv("sep2011.csv")

ggboxplot(sep2011$irregularidad_relativa_sep)
sep2011_clean <- sep2011 %>% filter(irregularidad_relativa_sep <= 0.3)

#h1: + gasto per capita = + irregularidad ####

# corr not clean
h1_corr_notcl <- cor.test(sep2011$gasto_per_capita_m,
                          sep2011$irregularidad_relativa_sep,
                          method = "pearson")
h1_p_value_corr_notcl <- h1_corr_notcl$p.value

h1_graph_corr_notcl <- ggscatter(sep2011, x = "gasto_per_capita_m",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Gasto per cápita",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación gasto per cápita e irregularidad relativa")

h1_graph_corr_notcl

#corr clean
h1_corr_clean <- cor.test(sep2011_clean$gasto_per_capita_m,
                          sep2011_clean$irregularidad_relativa_sep,
                          method = "pearson")
h1_p_value_corr_clean <- h1_corr_clean$p.value

h1_graph_corr_clean <- ggscatter(sep2011_clean,
                                 x = "gasto_per_capita_m",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Gasto per cápita",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación gasto per cápita e irregularidad relativa")

h1_graph_corr_clean

# Sin outliers, sí hay evidencia para correlación, aunque con un R muy bajo.

#h2: - nivel socioeconomico = + irregularidad ####

# corr not clean
h2_corr_notcl <- cor.test(sep2011$idse,
                          sep2011$irregularidad_relativa_sep,
                          method = "pearson")
h2_p_value_corr_notcl <- h2_corr_notcl$p.value

h2_graph_corr_notcl <- ggscatter(sep2011, x = "idse",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Nivel socioeconómico",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación nivel socioeconómico e irregularidad relativa")

h2_graph_corr_notcl

#corr clean
h2_corr_clean <- cor.test(sep2011_clean$idse,
                          sep2011_clean$irregularidad_relativa_sep,
                          method = "pearson")
h2_p_value_corr_clean <- h2_corr_clean$p.value

h2_graph_corr_clean <- ggscatter(sep2011_clean,
                                 x = "idse",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Nivel socioeconómico",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación nivel socioeconómico e irregularidad relativa")

h2_graph_corr_clean
# No hay evidencia para correlación.

#h3: - prop mujeres concejo = + irregularidad ####

# corr not clean
h3_corr_notcl <- cor.test(sep2011$prop_mujeres_concejo,
                          sep2011$irregularidad_relativa_sep,
                          method = "pearson")
h3_p_value_corr_notcl <- h3_corr_notcl$p.value

h3_graph_corr_notcl <- ggscatter(sep2011, x = "prop_mujeres_concejo",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Proporción de mujeres en el concejo",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación mujeres en el concejo e irregularidad relativa")

h3_graph_corr_notcl

#corr clean
h3_corr_clean <- cor.test(sep2011_clean$prop_mujeres_concejo,
                          sep2011_clean$irregularidad_relativa_sep,
                          method = "pearson")
h3_p_value_corr_clean <- h3_corr_clean$p.value

h3_graph_corr_clean <- ggscatter(sep2011_clean,
                                 x = "prop_mujeres_concejo",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Proporción de mujeres en el concejo",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación mujeres en el concejo e irregularidad relativa")

h3_graph_corr_clean
# Sin outliers, sí hay evidencia para correlación, aunque con un R muy bajo.

#h4: los municipios con alcaldes hombres son mas corruptos ####
#null_hyp = h4_dif_prom_ah_am = 0

sep2011 %>% group_by(genero_alcaldia) %>%
  get_summary_stats(irregularidad_relativa_sep, type = "mean_sd")
sep2011_clean %>% group_by(genero_alcaldia) %>%
  get_summary_stats(irregularidad_relativa_sep, type = "mean_sd")

# En la muestra general, el promedio es mayor para mujeres, pero sin outliers es menor.
# además, la muestra de mujeres es muy pequeña, por lo que habrá que usar t-student.

h4_ttest_notcl <- sep2011 %>% t_test(irregularidad_relativa_sep ~ genero_alcaldia)
h4_p_value_notcl <- h4_ttest_notcl$p

h4_ttest_clean <- sep2011_clean %>% t_test(irregularidad_relativa_sep ~ genero_alcaldia, alternative = "l")
h4_p_value_clean <- h4_ttest_clean$p

# Visualización de data
h4_bxp_notcl <- ggboxplot(sep2011, x = "genero_alcaldia", y = "irregularidad_relativa_sep", 
  ylab = "Nivel Corrupción", xlab = "Género Alcaldía (0 = mujer) (1 = hombre)", add = "jitter")
h4_ttest_notcl <- h4_ttest_notcl %>% add_xy_position(x = "genero_alcaldia")
h4_bxp_notcl + stat_pvalue_manual(h4_ttest_notcl, tip.length = 0) +
  labs(subtitle = get_test_label(h4_ttest_notcl, detailed = TRUE))

h4_bxp_clean <- ggboxplot(sep2011_clean, x = "genero_alcaldia", y = "irregularidad_relativa_sep", 
                    ylab = "Nivel Corrupción", xlab = "Género Alcaldía (0 = mujer) (1 = hombre)", add = "jitter")
h4_ttest_clean <- h4_ttest_clean %>% add_xy_position(x = "genero_alcaldia")
h4_bxp_clean + stat_pvalue_manual(h4_ttest_clean, tip.length = 0) +
  labs(subtitle = get_test_label(h4_ttest_clean, detailed = TRUE))

# No hay evidencia para descartar null_hyp.

#h5: + transparencia activa = - irregularidad ####

# corr not clean
h5_corr_notcl <- cor.test(sep2011$transparencia_activa,
                          sep2011$irregularidad_relativa_sep,
                          method = "pearson")
h5_p_value_corr_notcl <- h5_corr_notcl$p.value

h5_graph_corr_notcl <- ggscatter(sep2011, x = "transparencia_activa",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Índice de transparencia activa",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación transparencia activa e irregularidad relativa")

h5_graph_corr_notcl

#corr clean
h5_corr_clean <- cor.test(sep2011_clean$transparencia_activa,
                          sep2011_clean$irregularidad_relativa_sep,
                          method = "pearson")
h5_p_value_corr_clean <- h5_corr_clean$p.value

h5_graph_corr_clean <- ggscatter(sep2011_clean,
                                 x = "transparencia_activa",
                                 y = "irregularidad_relativa_sep",
                                 add = "reg.line",
                                 conf.int = TRUE,
                                 cor.coef = TRUE,
                                 cor.method = "pearson",
                                 xlab = "Índice de transparencia activa",
                                 ylab = "Irregularidad relativa",
                                 title = "Correlación transparencia activa e irregularidad relativa")

h5_graph_corr_clean
# No hay evidencia para correlación.

#h6: los municipios rurales son más corruptos ####

#hip_null = h6_dif_prom_rural_urbana = 0

sep2011 %>% group_by(nivel_ruralidad) %>%
  get_summary_stats(irregularidad_relativa_sep, type = "mean_sd")
sep2011_clean %>% group_by(nivel_ruralidad) %>%
  get_summary_stats(irregularidad_relativa_sep, type = "mean_sd")

# Los datos van de acuerdo a lo esperado. Por lo tanto, se sigue con un test de una cola.
# n rural es muy bajo, se sigue un test t.

h6_ttest_notcl <- sep2011 %>% t_test(irregularidad_relativa_sep ~ nivel_ruralidad, alternative = "g")
h6_p_value_notcl <- h6_ttest_notcl$p

h6_ttest_clean <- sep2011_clean %>% t_test(irregularidad_relativa_sep ~ nivel_ruralidad, alternative = "g")
h6_p_value_clean <- h6_ttest_clean$p

# Visualización de data
h6_bxp_notcl <- ggboxplot(sep2011, x = "nivel_ruralidad", y = "irregularidad_relativa_sep", 
          ylab = "Nivel Corrupción", xlab = "Ruralidad", add = "jitter")
h6_ttest_notcl <- h6_ttest_notcl %>% add_xy_position(x = "nivel_ruralidad")
h6_bxp_notcl + stat_pvalue_manual(h6_ttest_notcl, tip.length = 0) +
  labs(subtitle = get_test_label(h6_ttest_notcl, detailed = TRUE))

h6_bxp_clean <- ggboxplot(sep2011_clean, x = "nivel_ruralidad", y = "irregularidad_relativa_sep", 
                    ylab = "Nivel Corrupción", xlab = "Ruralidad", add = "jitter")
h6_ttest_clean <- h6_ttest_clean %>% add_xy_position(x = "nivel_ruralidad")
h6_bxp_clean + stat_pvalue_manual(h6_ttest_clean, tip.length = 0) +
  labs(subtitle = get_test_label(h6_ttest_clean, detailed = TRUE))

# No hay suficiente evidencia para descartar null_hyp.

#regresión múltiple ####

m_notcl_total<-lm(irregularidad_relativa_sep ~
                    idse +
                    nivel_ruralidad +
                    gasto_per_capita_m +
                    prop_mujeres_concejo +
                    transparencia_activa +
                    genero_alcaldia,
                  data = sep2011)

m_clean_total <-lm(irregularidad_relativa_sep ~
                     gasto_per_capita_m +
                     prop_mujeres_concejo +
                     transparencia_activa +
                     nivel_ruralidad +
                     idse +
                     genero_alcaldia,
                   data = sep2011_clean)

m_clean_corr <-lm(irregularidad_relativa_sep ~
                    gasto_per_capita_m +
                    prop_mujeres_concejo,
                  data = sep2011_clean)

stargazer(m_notcl_total, m_clean_total, m_clean_corr,
          type = "text",
          title = "Tabla 1: Modelo de regresión múltiple de irregularidad relativa",
          summary = TRUE,
          out = "tabla_multiple_2.html",
          column.labels = c("total", "sin outliers", "sin outliers y solo variables correlacionadas"),
          covariate.labels = c("Indice socioeconómico", "Ruralidad", "Gasto municipal", "Proporción de mujeres en el concejo", "Transparencia Activa", "Género alcaldía"),
          dep.var.caption = "",
          dep.var.labels = "Irregularidad relativa",
          notes = "Fuente: Elaboración propia")
