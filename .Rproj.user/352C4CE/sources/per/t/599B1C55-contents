##############################
####
####
####
####  Structural Topic Modeling - Differences in Differences
####
####
####
###############################

# loading packages 
library(stm)
library(SnowballC)
library(tidyverse)
library(stminsights)
library(tidytext)
library(ggridges)
library(RColorBrewer)
library(dotwhisker)
library(lubridate)
library(RColorBrewer)
library(tidyr)

###########################

# setting paths

root_decretos_LA <- here::here()

magna_folder <- paste(root_decretos_LA, 
                      "magna_folder", 
                      sep = "/")

raw_decretos_LA <- paste(root_decretos_LA,
                         "/decretos_LA_cloud_dir/data/raw",
                         sep = "")

data_decretos_LA <- paste(root_decretos_LA,
                          "/decretos_LA_cloud_dir/data",
                          sep = "")

dd_analysis <- paste(root_decretos_LA,
                     "/DD/analysis/",
                     sep = "")

dd_images <- paste(dd_analysis,
                     "/images/",
                     sep = "")

dd_tables <- paste(dd_analysis,
                     "/tables/",
                     sep = "")

dd_R <- paste(root_decretos_LA,
              "/DD/R/",
              sep = "")

##############################

# loading data

add_stopwords_ptb <- read_delim(
  paste(root_decretos_LA,
        "/support_files/stopwords-pt.txt",
        sep = ""),
  delim = ",", 
  col_names = F) # custom stopwords

brasil_numerado <- readxl::read_xlsx(paste(
  raw_decretos_LA,
  "Brasil_decretosNUMERADOS_1988_2019.xlsx", 
  sep = "/"),
  sheet = 1) %>%
  rename(ID = "ID (sequencia ano+numero do decreto",
         date = "Data",
         day = "Dia",
         month = "Mês",
         year = "Ano",
         year_4 = "Ano_4",
         president = "Presidente") %>%
  slice(-c(11441, 11647)) %>%
  filter(year_4 >= 1989) %>%
  select(ID,
         date,
         day,
         month,
         year,
         year_4, 
         Ementa,
         president) %>%
  mutate(pres_factor = fct_infreq(president, 
                                  ordered = F),
         numerado = 1) # data

brasil_naonumerado <- readxl::read_xlsx(paste(
  raw_decretos_LA,
  "Decretos_NAONUMERADOS_1988_2017.xlsx", 
  sep = "/"),
  sheet = 1) %>%
  rename(ID = "ID",
         date = "Data",
         day = "Dia",
         month = "Mês",
         year = "ano",
         year_4 = "Ano",
         president = "PRESIDENTE") %>%
  select(ID,
         date,
         day,
         month,
         year,
         year_4, 
         Ementa,
         president) %>%
  mutate(pres_factor = fct_infreq(president, 
                                  ordered = F),
         numerado = 0) # data


brasil_completo <- brasil_numerado %>%
  bind_rows(brasil_naonumerado) %>%
  mutate(pres_factor = fct_infreq(str_replace_all(president, "[ I+ ]", ""), 
                                  ordered = F)) %>%
  filter(month != 1992,
         year_4 >= 1989)

##################################
#### Impact of non-numbered on topic prevalence - DD model
##################################

dd_df <- brasil_completo %>% 
  tidyr::unite(date_temp, 
               c(day, month, year), 
               sep = "-") %>%
  dplyr::mutate(date_full = lubridate::parse_date_time(date_temp, "dmy"),
                treatment = case_when(numerado == 1 ~ 0,
                                      numerado == 0 ~ 1),
                after_treatment = case_when(date_full >= "1991-01-16" & date_full <= "2017-10-13" ~ 1,
                                            TRUE ~ 0),
                int_TD = treatment * after_treatment,
                post_autonomo = case_when(date_full >= "2001-09-11" ~ 1,
                                          TRUE ~ 0))

dd_df %>% 
  slice(which(is.na(.$date_full))) %>% 
  select(date_temp, date_full, year_4) %>% 
  group_by(year_4) %>% 
  summarise(n()) # número de ementas sem data por ano


################## STM preparation and analysis

ementa_brasil_completo <- dd_df %>%
  select(Ementa) %>%
  as.matrix()

proc_brasil_completo <- textProcessor(ementa_brasil_completo,
                                      metadata = dd_df,
                                      language = "por", 
                                      customstopwords = c(add_stopwords_ptb$X1,
                                                          c("janeiro", "fevereiro", "mar?o", "abril", "maio", "junho", "julho",
                                                            "agosto", "setembro", "outubro", "novembro", "dezembro")))

vocab_brasil_completo <- proc_brasil_completo$vocab
docs_brasil_completo <- proc_brasil_completo$documents
meta_brasil_completo <- proc_brasil_completo$meta #%>% 
  #mutate(year_4 = fct_infreq(factor(prep_brasil_completo_meta$year_4))) #tratando ano como fator ou ordinal

# lens <- unlist(lapply(docs_brasil_completo, function(x) ncol(x)))
# hist(lens)

prep_brasil_completo <- prepDocuments(docs_brasil_completo, vocab_brasil_completo, meta_brasil_completo)
prep_brasil_completo_docs <- prep_brasil_completo$documents
prep_brasil_completo_vocab <- prep_brasil_completo$vocab
prep_brasil_completo_meta <- prep_brasil_completo$meta

##### STM with DD

for (r in 4:7) {
  
  topics = r  
  stm_brasil_completo <- stm(prep_brasil_completo_docs, 
                             prep_brasil_completo_vocab, 
                             data = prep_brasil_completo_meta, 
                             init.type = "Spectral",
                             prevalence = ~ treatment * after_treatment + s(year_4),
                             topics)
  
  stm_brasil_completo_topics <- labelTopics(stm_brasil_completo)
  
  write_rds(stm_brasil_completo, paste(dd_R,
                                      "stm_topics_n_",
                                      r,
                                      sep = ""))
  
  prep <- estimateEffect(1:topics ~ treatment * after_treatment + s(year_4), 
                         stm_brasil_completo, 
                         meta = prep_brasil_completo_meta, 
                         uncertainty = "Global")
  
  write_rds(prep, paste(dd_R,
                        "effects_topics_n_",
                        r,
                        sep = ""))
  
  ## grpahs

  color_topics = colorRampPalette(brewer.pal(8, "Set2"))(topics)
  
  AP_brasil_completo <- tidy(stm_brasil_completo, matrix = "beta")
  
  graph_topic_highest_prob <- AP_brasil_completo %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = drlib::reorder_within(term, beta, topic)) %>% ### trocar drlib::reorder_within(term, beta, topic) por reorder(term, beta)
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = color_topics, 
                      labels = c(seq(1,topics))) +
    facet_wrap(~ topic, 
               scales = "free_y") +
    theme_light() +
    theme(strip.text.x = element_text(size = 14, 
                                      face = "bold")) +
    ylab("Word probability by topic") +
    xlab("Term") +
    coord_flip()+
    scale_x_reordered()
  
  ggsave(paste(dd_images,
               "brasil_completo_topics_n_", 
               r,
               ".png",
               sep = ""),
         graph_topic_highest_prob,
         height = 7, width = 12)
}


#################### loading models with 4 to 7 topics 

for (r in 4:7) {
  
  topics = r  
  
  stm_temp <- read_rds(paste(dd_R,
                           "stm_topics_n_",
                           r,
                           sep = ""))
  
  capture.output(labelTopics(stm_temp), 
                 file = paste(dd_analysis, 
                              "label_topics_n_", 
                              r,
                              ".txt",
                              sep = ""))
  
  effects <- read_rds(paste(dd_R,
                            "effects_topics_n_",
                            r,
                            sep = ""))
  
  assign(paste("stm_topics_n_",
               r,
               sep = ""),
         stm_temp)
  
  assign(paste("effects_topics_n_",
               r,
               sep = ""),
         effects)
  
}



