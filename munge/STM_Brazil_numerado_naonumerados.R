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


###########################

# setting paths

root_decretos_LA <- here::here()
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

brasil <- read_csv(paste(
  raw_decretos_LA,
  "Brasil_decretosNUMERADOS_1988_2019.csv", 
  sep = "/")) %>%
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

brasil_naonumerado <- read_csv(paste(
  raw_decretos_LA,
  "Decretos_NAONUMERADOS_1988_2017.csv", 
  sep = "/")) %>%
  rename(ID = "ID",
         date = "Data",
         day = "Dia",
         month = "Mês",
         year = "ano",
         year_4 = "Ano_4",
         president = "PRESIDENTE") %>%
  select(ID,
         date,
         day,
         month,
         year,
         year_4, 
         Ementa,
         president) %>%
  mutate(pres_factor = fct_infreq(greppresident, 
                                  ordered = F),
         numerado = 0) # data


brasil_completo <- brasil %>%
  bind_rows(brasil_naonumerado) %>%
  mutate(pres_factor = fct_infreq(sub("[ I+ ]?", "", president), 
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

(topics_pres_factor <- brasil_completo %>%
    group_by(fct_inorder(pres_factor), numerado)  %>%
    summarise("decretos_adm" = n()) %>%
    rename("president" = `fct_inorder(pres_factor)`) %>%
    left_join(month_pres, by = "president") %>%
    mutate(avg_month = decretos_adm/months) %>%
  ggplot() +
  geom_col(aes(president, avg_month, fill = as.factor(numerado)),
           position = "dodge",
           show.legend = T) +
  scale_fill_grey(labels = c("No", "Yes"), 
                  name = "Numbered") +
  theme_light() +
  xlab("President") +
  ylab("Average executive orders by month")) +
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text = element_text(size=18),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18, face = "bold"),
        legend.position = c(0.9, 0.9))

ggsave(paste(images_decretos_LA,
             "avg_by_presidente_type.png",
             sep = ""),
       topics_pres_factor)

################## STM preparation and analysis

topics = 6

ementa_brasil_completo <- brasil_completo %>%
  select(Ementa) %>%
  as.matrix()

proc_brasil_completo <- textProcessor(ementa_brasil_completo,
                             metadata = brasil_completo,
                             language = "por", 
                             customstopwords = c(add_stopwords_ptb$X1,
                                                 c("janeiro", "fevereiro", "marÃ§o", "abril", "maio", "junho", "julho",
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

stm_brasil_completo <- stm(prep_brasil_completo_docs, 
                  prep_brasil_completo_vocab, 
                  data = prep_brasil_completo_meta, 
                  init.type = "Spectral",
                  prevalence = ~ pres_factor + numerado,
                  topics)

stm_brasil_completo_topics <- labelTopics(stm_brasil_completo)

prep <- estimateEffect(1:topics ~ pres_factor + numerado, 
                       stm_brasil_completo, 
                       meta = prep_brasil_completo_meta, 
                       uncertainty = "Global")

# plot(prep, "pres_factor", topics = 1)
# plot(prep, "pres_factor", topics = 2)
# plot(prep, "pres_factor", topics = 3)
# plot(prep, "pres_factor", topics = 4)
# plot(prep, "pres_factor", topics = 5)
# plot(prep, "pres_factor", topics = 6)

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
         numerado = prep_brasil_completo_meta$numerado) %>%
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

## mean document by pres_factor - mean topic prevalence by pres_factor

topic_color <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#E5C494")

(mean_topic_prevalence_n_numerado <- theta_mean %>%
  filter(numerado == 0) %>%
  ggplot() +
  geom_col(aes(pres_factor, value, fill = factor(topic)),
           position = "stack") + 
  scale_fill_manual(values = topic_color,
                    labels = c(seq(1,topics)),
                    name = "Topic") +
  theme_light() +
  xlab("President") +
  ylab("Mean topic prevalence") +
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text = element_text(size=18),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18, face = "bold")))

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_UNnumbered.png",
             sep = ""),
       mean_topic_prevalence)


(mean_topic_prevalence_numerado <- theta_mean %>%
  filter(numerado == 1) %>%
  ggplot() +
  geom_col(aes(pres_factor, value, fill = topic),
           position = "stack") +
  scale_fill_manual(values = topic_color, 
                    labels = c(seq(1,topics)),
                    name = "Topic") +
  theme_light() +
  xlab("President") +
  ylab("Mean topic prevalence") +
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18, face = "bold")))

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_numbered.png",
             sep = ""),
       mean_topic_prevalence)

(mean_topic_prevalence_numerado <- dist_theta_pres %>%
  ggplot() +
  geom_col(aes(pres_factor, value, fill = topic),
           position = "stack") + 
  scale_fill_manual(values = topic_color, 
                    labels = c(seq(1,topics)),
                    name = "Topic") +
  theme_light() +
  xlab("President") +
  ylab("Mean topic prevalence") +
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18, face = "bold")))

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence_all.png",
             sep = ""),
       mean_topic_prevalence)
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
#   scale_fill_brewer(palette = "Paired", 
#                     labels = c(seq(1,topics)),
#                     name = "Topic") +
#   theme_light())

# ggplot(theta.df, aes(y = value, x = month, color = pres_factor)) + geom_line() + facet_wrap(~variable)

###############################################

# word prevalence by topic

AP_brasil_completo <- tidy(stm_brasil_completo, matrix = "beta")

ap_top_brasil_completo <- 
  AP_brasil_completo %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

labels_topics <- c("1" = "Topic 1",
                   "2" = "Topic 2",
                   "3" = "Topic 3",
                   "4" = "Topic 4",
                   "5" = "Topic 5",
                   "6" = "Topic 6")

## graph word prevalance by topic

(topics_brasil_completo <- ap_top_brasil_completo %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  scale_fill_manual(values = topic_color, 
                    labels = c(seq(1,topics))) +
  facet_wrap(~ topic, 
             scales = "free", 
             labeller = labeller(topic = labels_topics)) +
  theme_light() +
  theme(strip.text.x = element_text(size = 10, 
                                    face = "bold")) +
  ylab("Word probability by topic") +
  xlab("Term") +
  coord_flip() +
  theme(axis.title.x = element_text(size=18, face = "bold"),
          axis.title.y = element_text(size=18, face = "bold"),
          legend.text = element_text(size=18),
          legend.title = element_text(size=18, face = "bold")))

ggsave(paste(images_decretos_LA,
             "brasil_completo_topics.png",
             sep = ""),
       topics_brasil_completo)

####################################

#### examples docs by topic

for (t in 1:topics) {
  png(paste(images_decretos_LA,
            "topic_",
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






