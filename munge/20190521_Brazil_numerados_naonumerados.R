##############################
####
####
####
####  Structural Topic Modeling - full data Brasil
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

images_decretos_LA <- paste(root_decretos_LA,
                            "/analysis/images/",
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

write_excel_csv(brasil_completo, paste(data_decretos_LA,
                                       "banco_completo.csv",
                                       sep = "/"))

####################################

month_pres <- brasil_completo %>%
  group_by(fct_inorder(pres_factor), year_4, month) %>%
  summarise(n()) %>%
  ungroup() %>%
  group_by(`fct_inorder(pres_factor)`) %>%
  summarise(n()) %>%
  rename(president = `fct_inorder(pres_factor)`,
         months = `n()`)

# initial descriptives - graph

brasil_completo %>%
  group_by(fct_inorder(pres_factor), numerado)  %>%
  summarise("decretos_adm" = n()) %>%
  rename("president" = `fct_inorder(pres_factor)`) %>%
  left_join(month_pres, by = "president") %>%
  mutate(avg_month = decretos_adm/months)

to_add <- bind_rows(c(president = "SARNEY", numerado = 0, decretos_adm = 0, months = 0, avg_month = 0),
          c(president = "BOLSONARO", numerado = 0, decretos_adm = 0, months = 0, avg_month = 0)) %>%
  type_convert()

topics_pres_factor <- brasil_completo %>%
    group_by(fct_inorder(pres_factor), numerado)  %>%
    summarise("decretos_adm" = n()) %>%
    rename("president" = `fct_inorder(pres_factor)`) %>%
    left_join(month_pres, by = "president") %>%
    mutate(avg_month = decretos_adm/months) %>%
  bind_rows(to_add) %>%
  ggplot() +
  geom_bar(aes(fct_inorder(president), avg_month, fill = fct_rev(as.factor(numerado))),
             position = position_dodge(preserve = "single"),
             stat = "identity",
             show.legend = T) +
  geom_text(aes(president, avg_month, 
                fill = fct_rev(as.factor(numerado)), 
                label = round(avg_month, 1)),
            vjust = -0.5,
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("gray19", "gray48"),
                      labels = c("Numbered", "Non-numbered"), 
                      name = NULL) +
  theme_light() +
  xlab("President") +
  ylab("Average monthly number of administrative decrees") +
  theme(axis.title.x = element_text(size=16, face = "bold"),
        axis.title.y = element_text(size=16, face = "bold"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16, face = "bold"),
        legend.position = c(0.85, 0.9))

ggsave(paste(images_decretos_LA,
             "avg_by_presidente_type_2.png",
             sep = ""),
       topics_pres_factor,
       height = 7, width = 12)

################## STM preparation and analysis


ementa_brasil_completo <- brasil_completo %>%
  select(Ementa) %>%
  as.matrix()

proc_brasil_completo <- textProcessor(ementa_brasil_completo,
                                      metadata = brasil_completo,
                                      language = "por", 
                                      customstopwords = c(add_stopwords_ptb$X1,
                                                          c("janeiro", "fevereiro", "mar?o", "abril", "maio", "junho", "julho",
                                                            "agosto", "setembro", "outubro", "novembro", "dezembro")))

vocab_brasil_completo <- proc_brasil_completo$vocab
docs_brasil_completo <- proc_brasil_completo$documents
meta_brasil_completo <- proc_brasil_completo$meta

# lens <- unlist(lapply(docs_brasil_completo, function(x) ncol(x)))
# hist(lens)

prep_brasil_completo <- prepDocuments(docs_brasil_completo, vocab_brasil_completo, meta_brasil_completo)
prep_brasil_completo_docs <- prep_brasil_completo$documents
prep_brasil_completo_vocab <- prep_brasil_completo$vocab
prep_brasil_completo_meta <- prep_brasil_completo$meta

for (r in 4:7) {
  
  topics = r  
  stm_brasil_completo <- stm(prep_brasil_completo_docs, 
                             prep_brasil_completo_vocab, 
                             data = prep_brasil_completo_meta, 
                             init.type = "Spectral",
                             prevalence = ~ pres_factor + numerado + s(year_4),
                             topics)
  
  stm_brasil_completo_topics <- labelTopics(stm_brasil_completo)

  write_rds(stm_brasil_completo_topics, paste(root_decretos_LA,
                                              "/R/",
                                              "topic_labels_n_",
                                              r,
                                              sep = ""))
  
  prep <- estimateEffect(1:topics ~ pres_factor + numerado + s(year_4), 
                         stm_brasil_completo, 
                         meta = prep_brasil_completo_meta, 
                         uncertainty = "Global")
  
  write_rds(prep, paste(root_decretos_LA,
                        "/R/",
                        "effects_topics_n_",
                        r,
                        sep = ""))
  

}




############### Estimated topic usage probability

labels_topics <- c("1" = "Topic 1: Foreign Affairs",
                   "2" = "Topic 2: Personnel regulation",
                   "3" = "Topic 3: Budget allocation",
                   "4" = "Topic 4: Social Benefits",
                   "5" = "Topic 5: Law implementation",
                   "6" = "Topic 6: Sector-based benefits")

for (t in 1:topics) {
  plot_standard <- plot(prep, "pres_factor", topics = t)
  topic_prob_graph <- as_tibble(t(rbind("labels" = as.character(plot_standard$uvals),
                    "coeffs" = plot_standard$means[[1]],
                    plot_standard$ci[[1]]))) %>%
    type_convert() %>%
    mutate(labels = fct_relevel(labels, c("SARNEY", "COLLOR", "ITAMAR", "FHC", "LULA", "DILMA", "TEMER", "BOLSONARO"))) %>%
  ggplot(aes(labels, coeffs)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`),
                 col = "darkgrey",
                 lwd = 1.3) +
  geom_hline(aes(yintercept=0),
             linetype="dashed",
             lwd = 1.3,
             col = "grey") +
  geom_point(size = 5, col= "darkgrey")  +
  geom_text(aes(label = labels), 
            nudge_x = .25) +
  labs(x = NULL, y = "Estimated topic usage probability") +
  coord_flip() +
  theme_light() +
  ggtitle(labels_topics[t]) +
  theme(axis.title = element_text(size=18, face = "bold"),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y=element_blank())
  
  ggsave(paste(images_decretos_LA,
               "probabilities_topic",
               t,
               ".png",
               sep = ""),
         topic_prob_graph,
         height = 7, width = 12)
}

####################### separating thetas - topic prevalence by document

theta_mean <- stm_brasil_completo$theta %>%
  data.frame()  %>%
  rename(topic_1 = X1,
         topic_2 = X2,
         topic_3 = X3,
         topic_4 = X4,
         topic_5 = X5,
         topic_6 = X6) %>%
  mutate(month = prep_brasil_completo_meta$month,
         pres_factor = fct_inorder(prep_brasil_completo_meta$pres_factor),
         year = prep_brasil_completo_meta$year_4,
         numerado = fct_rev(as.factor(prep_brasil_completo_meta$numerado))) %>%
  group_by(pres_factor, numerado) %>%
  summarise_all(mean) %>%
  select(-month, -year) %>%
  gather(value = starts_with("topic_"), 
         "topic", 
         -c(pres_factor, numerado)) %>%
  rename("value" = `starts_with("topic_")`)

dist_theta_pres <- stm_brasil_completo$theta %>%
  data.frame()  %>%
  rename(topic_1 = X1,
         topic_2 = X2,
         topic_3 = X3,
         topic_4 = X4,
         topic_5 = X5,
         topic_6 = X6) %>%
  mutate(pres_factor = fct_inorder(prep_brasil_completo_meta$pres_factor)) %>%
  group_by(pres_factor) %>%
  summarise_all(mean) %>%
  gather(value = starts_with("topic_"), 
         "topic", 
         -c(pres_factor)) %>%
  rename("value" = `starts_with("topic_")`)

####################################################################
## GRAPHS 
####################################################################

######## mean document by pres_factor and doc type

## NOT numbered only
color_topics = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#E5C494")

mean_topic_prevalence_n_numerado <- theta_mean %>%
    filter(numerado == 0) %>%
    ggplot() +
    geom_col(aes(pres_factor, value, fill = topic),
             position = "stack") + 
    scale_fill_manual(values = color_topics, 
                      labels = c("1 - Foreign Affairs", "2 - Personnel regulation",
                                 "3 - Budget allocation", "4 - Social Benefits", 
                                 "5 - Law implementation", "6 - Sector-based benefits"),
                      name = "Topic") +
    theme_light() +
    xlab("President") +
    ylab("Mean topic prevalence") +
    theme(axis.title.x = element_text(size=18, face = "bold"),
          axis.title.y = element_text(size=18, face = "bold"),
          axis.text = element_text(size=14),
          legend.text = element_text(size=18),
          legend.title = element_text(size=18, face = "bold"))

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_NOT_numbered.png",
             sep = ""),
       mean_topic_prevalence_n_numerado,
       height = 7, width = 12)


## numbered only
mean_topic_prevalence_numerado <- theta_mean %>%
    filter(numerado == 1) %>%
    ggplot() +
    geom_col(aes(pres_factor, value, fill = topic),
             position = "stack") + 
    scale_fill_manual(values = color_topics, 
                      labels = c("1 - Foreign Affairs", "2 - Personnel regulation",
                                 "3 - Budget allocation", "4 - Social Benefits", 
                                 "5 - Law implementation", "6 - Sector-based benefits"),
                      name = "Topic") +
    theme_light() +
    xlab("President") +
    ylab("Mean topic prevalence") +
    theme(axis.title.x = element_text(size=18, face = "bold"),
          axis.title.y = element_text(size=18, face = "bold"),
          axis.text = element_text(size=14),
          legend.text = element_text(size=18),
          legend.title = element_text(size=18, face = "bold")) 

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_numbered.png",
             sep = ""),
       mean_topic_prevalence_numerado,
       height = 7, width = 12)


labels_type <- c("1" = "Numbered",
                 "0" = "Not numbered")


## Comparison side by side

mean_topic_prevalence_compared <- theta_mean %>%
    ggplot() +
    geom_col(aes(pres_factor, value, fill = topic),
             position = "stack") + 
    scale_fill_manual(values = color_topics, 
                      labels = c("1 - Foreign Affairs", "2 - Personnel regulation",
                                 "3 - Budget allocation", "4 - Social Benefits", 
                                 "5 - Law implementation", "6 - Sector-based benefits"),
                      name = "Topic") +
    facet_wrap(~numerado,
               labeller = labeller(numerado = labels_type))+
    theme_light() +
    xlab("President") +
    ylab("Mean topic prevalence") +
    theme(axis.title.x = element_text(size=18, face = "bold"),
          axis.title.y = element_text(size=18, face = "bold"),
          axis.text = element_text(size=14),
          legend.text = element_text(size=18),
          legend.title = element_text(size=18, face = "bold"),
          strip.text.x = element_text(size = 16, face = "bold")) 

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_compared.png",
             sep = ""),
       mean_topic_prevalence_compared,
       height = 10, width = 22)

## All docs POOLED

mean_topic_prevalence_all <- dist_theta_pres %>%
    ggplot() +
    geom_col(aes(pres_factor, value, fill = topic),
             position = "stack") + 
    scale_fill_manual(values = color_topics, 
                      labels = c("1 - Foreign Affairs", "2 - Personnel regulation",
                                 "3 - Budget allocation", "4 - Social Benefits", 
                                 "5 - Law implementation", "6 - Sector-based benefits"),
                      name = "Topic") +
    theme_light() +
    xlab("President") +
    ylab("Mean topic prevalence") +
    theme(axis.title.x = element_text(size=18, face = "bold"),
          axis.title.y = element_text(size=18, face = "bold"),
          axis.text = element_text(size=14),
          legend.text = element_text(size=18),
          legend.title = element_text(size=18, face = "bold"))

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_all.png",
             sep = ""),
       mean_topic_prevalence_all,
       height = 7, width = 12)

###############################################

# (topic_prevalence_distrib <- dist_theta_pres %>%
#   ggplot() +
#   geom_density_ridges(aes(y= pres_factor, 
#                           x = value,
#                           fill = topic), 
#                       col = "white",
#                       scale = .9,
#                       alpha = .4) +
#   coord_flip() +
#   theme_ridges(center = TRUE) + 
#   scale_fill_manual(values = color_topics, 
#                     labels = c(seq(1,topics)),
#                     name = "Topic") +
#   theme_light())

# ggplot(theta.df, aes(y = value, x = month, color = pres_factor)) + geom_line() + facet_wrap(~variable)

###############################################

# word prevalence by topic

AP_brasil_completo <- tidy(stm_brasil_completo, matrix = "beta")


stm_brasil_completo$beta
ap_top_brasil_completo <- AP_brasil_completo %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

labels_topics <- c("1" = "Topic 1: Foreign Affairs",
                   "2" = "Topic 2: Personnel regulation",
                   "3" = "Topic 3: Budget allocation",
                   "4" = "Topic 4: Social Benefits",
                   "5" = "Topic 5: Law implementation",
                   "6" = "Topic 6: Sector-based benefits")

## graph word prevalance by topic

topics_brasil_completo <- ap_top_brasil_completo %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  scale_fill_manual(values = color_topics, 
                    labels = c(seq(1,topics))) +
  facet_wrap(~ topic, 
             scales = "free", 
             labeller = labeller(topic = labels_topics)) +
  theme_light() +
  theme(strip.text.x = element_text(size = 14, 
                                    face = "bold")) +
  ylab("Word probability by topic") +
  xlab("Term") +
  coord_flip()

ggsave(paste(images_decretos_LA,
             "brasil_completo_topics.png",
             sep = ""),
       topics_brasil_completo,
       height = 7, width = 12)

####################################

#### examples docs by topic

for (t in 1:topics) {
  png(paste(images_decretos_LA,
            "topic_BOTH_",
            t,
            "_quotes_brasil_completo.png",
            sep = ""),
      width     = 1800,
      height    = 1200)
  quotes <- findThoughts(stm_brasil_completo, 
                         texts = ementa_brasil_completo,
                         n = 5, 
                         topics = t)$docs[[1]]
  plotQuote(quotes,
            width = 90, 
            main = paste("Quote Topic ",
                         t,                
                         sep = ""),
            text.cex = 1.8,
            cex.main = 1.8)
  dev.off()
}

##################################
#### Impact of non-numbered on topic prevalence
##################################

dd_df <- brasil_completo %>% 
  tidyr::unite(date_full, 
               c(day, month, year), 
               sep = "-") %>%
  dplyr::mutate(date_full = lubridate::parse_date_time(date_full, "dmy"),
                treatment = case_when(numerado == 1 ~ 0,
                                      numerado == 0 ~ 1),
                after_treatment = case_when(date_full >= "1991-01-16" & date_full <= "2017-10-13" ~ 1,
                                               TRUE ~ 0),
                int_TD = treatment * after_treatment)


################## STM preparation and analysis

topics = 6

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
meta_brasil_completo <- proc_brasil_completo$meta %>% 
  mutate(year_4 = fct_infreq(factor(prep_brasil_completo_meta$year_4)))

# lens <- unlist(lapply(docs_brasil_completo, function(x) ncol(x)))
# hist(lens)

prep_brasil_completo <- prepDocuments(docs_brasil_completo, vocab_brasil_completo, meta_brasil_completo)
prep_brasil_completo_docs <- prep_brasil_completo$documents
prep_brasil_completo_vocab <- prep_brasil_completo$vocab
prep_brasil_completo_meta <- prep_brasil_completo$meta

##### 6 topics
topics = 6

stm_brasil_completo <- stm(prep_brasil_completo_docs, 
                           prep_brasil_completo_vocab, 
                           data = prep_brasil_completo_meta, 
                           init.type = "Spectral",
                           prevalence = ~ treatment * after_treatment + year_4,
                           topics)

stm_brasil_completo_topics <- labelTopics(stm_brasil_completo)

prep <- estimateEffect(1:topics ~ treatment * after_treatment + year_4, 
                       stm_brasil_completo, 
                       meta = prep_brasil_completo_meta, 
                       uncertainty = "Global")

summary(prep)

##### 7 topics
topics = 7

stm_brasil_completo <- stm(prep_brasil_completo_docs, 
                           prep_brasil_completo_vocab, 
                           data = prep_brasil_completo_meta, 
                           init.type = "Spectral",
                           prevalence = ~ treatment * after_treatment + year_4,
                           topics)

stm_brasil_completo_topics <- labelTopics(stm_brasil_completo)



prep <- estimateEffect(1:topics ~ treatment * after_treatment + year_4, 
                       stm_brasil_completo, 
                       meta = prep_brasil_completo_meta, 
                       uncertainty = "Global")















































