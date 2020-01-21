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
  mutate(pres_factor = fct_infreq(president, 
                                  ordered = F)) # data


####################################

# initial descriptives - graph

topics_president <- brasil %>%
  group_by(fct_inorder(president)) %>%
  summarise(n = n()) %>%
  rename(president = `fct_inorder(president)`) %>%
  ggplot() +
  geom_col(aes(president, n),
           fill = "darkgrey",
           show.legend = F) +
  theme_light() +
  xlab("President") +
  ylab("Number of executive orders")

ggsave(paste(images_decretos_LA,
             "number_orders_president.png",
             sep = ""),
       topics_president)

################## STM preparation and analysis

ementa_brasil <- brasil %>%
  select(Ementa) %>%
  as.matrix()

proc_brasil <- textProcessor(ementa_brasil,
                             metadata = brasil,
                             language = "por", 
                             customstopwords = c(add_stopwords_ptb$X1,
                                                 c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho",
                                                   "agosto", "setembro", "outubro", "novembro", "dezembro")))

vocab_brasil <- proc_brasil$vocab
docs_brasil <- proc_brasil$documents
meta_brasil <- proc_brasil$meta

# lens <- unlist(lapply(docs_brasil, function(x) ncol(x)))
# hist(lens)

prep_brasil <- prepDocuments(docs_brasil, vocab_brasil, meta_brasil)
prep_brasil_docs <- prep_brasil$documents
prep_brasil_vocab <- prep_brasil$vocab
prep_brasil_meta <- prep_brasil$meta

stm_brasil <- stm(prep_brasil_docs, 
                  prep_brasil_vocab, 
                  data = prep_brasil_meta, 
                  init.type = "Spectral",
                  prevalence = ~ pres_factor,
                  5)

stm_brasil_topics <- labelTopics(stm_brasil)

prep <- estimateEffect(1:5 ~ pres_factor, 
                       stm_brasil, 
                       meta = prep_brasil_meta, 
                       uncertainty = "Global")

plot(prep, "pres_factor", topics = 1)
plot(prep, "pres_factor", topics = 2)
plot(prep, "pres_factor", topics = 3)
plot(prep, "pres_factor", topics = 4)
plot(prep, "pres_factor", topics = 5)

####################### separating thetas - topic prevalence by document

theta_mean <- stm_brasil$theta %>%
  data.frame()  %>%
  rename(topic_1 = X1,
         topic_2 = X2,
         topic_3 = X3,
         topic_4 = X4,
         topic_5 = X5) %>%
  mutate(month = prep_brasil_meta$month,
         president = fct_inorder(prep_brasil_meta$pres_factor),
         year = prep_brasil_meta$year_4) %>%
  group_by(president) %>%
  summarise_all(mean) %>%
  select(-month, -year) %>%
  gather(value = starts_with("topic_"), 
         "topic", 
         -president) %>%
  rename("value" = `starts_with("topic_")`)

dist_theta_pres <- stm_brasil$theta %>%
  data.frame()  %>%
  rename(topic_1 = X1,
         topic_2 = X2,
         topic_3 = X3,
         topic_4 = X4,
         topic_5 = X5) %>%
  mutate(president = fct_inorder(prep_brasil_meta$pres_factor)) %>%
  gather(value = starts_with("topic_"), 
         "topic", 
         -president) %>%
  rename("value" = `starts_with("topic_")`)

## mean document by president - mean topic prevalence by president

mean_topic_prevalence <- theta_mean %>%
  ggplot() +
  geom_col(aes(president, value, fill = topic),
           position = "stack") + 
  scale_fill_brewer(palette = "Set2", 
                    labels = c(seq(1,5)),
                    name = "Topic") +
  theme_light() +
  xlab("President") +
  ylab("Mean topic prevalence") 

ggsave(paste(images_decretos_LA,
             "mean_topic_prevalence.png",
             sep = ""),
       mean_topic_prevalence)

(topic_prevalence_distrib <- dist_theta_pres %>%
  ggplot() +
  geom_density_ridges(aes(y= president, 
                          x = value,
                          fill = topic), 
                      col = "white",
                      scale = .9,
                      alpha = .4) +
  coord_flip() +
  theme_ridges(center = TRUE) + 
  scale_fill_brewer(palette = "Set2", 
                    labels = c(seq(1,5)),
                    name = "Topic") +
  theme_light())

# ggplot(theta.df, aes(y = value, x = month, color = pres_factor)) + geom_line() + facet_wrap(~variable)

###############################################

# word prevalence by topic

AP_brasil <- tidy(stm_brasil, matrix = "beta")

ap_top_brasil <- 
  AP_brasil %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

labels_topics <- c("1" = "Topic 1",
                   "2" = "Topic 2",
                   "3" = "Topic 3",
                   "4" = "Topic 4",
                   '5' = "Topic 5")

## graph word prevalance by topic

topics_brasil <- ap_top_brasil %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  scale_fill_brewer(palette = "Set2", 
                    labels = c(seq(1,5))) +
  facet_wrap(~ topic, 
             scales = "free", 
             labeller = labeller(topic = labels_topics)) +
  theme_light() +
  theme(strip.text.x = element_text(size = 10, 
                                    face = "bold")) +
  ylab("Word probabilities by topic") +
  xlab("Term") +
  coord_flip()

ggsave(paste(images_decretos_LA,
             "brasil_topics.png",
             sep = ""),
       topics_brasil)

####################################

#### examples docs by topic

for (t in 1:5) {
  png(paste(images_decretos_LA,
            "topic_",
            t,
            "_quotes_brasil.png",
            sep = ""),
      width     = 1800,
      height    = 1200)
  quotes <- findThoughts(stm_brasil, 
                         texts = ementa_brasil,
                         n = 5, 
                         topics = t)$docs[[1]]
  plotQuote(quotes,
            width = 90, 
            main = paste("Quote Topic ",
                         t,                
                         sep = ""),
            text.cex = 1.8,
            cex.main=1.8)
  dev.off()
}








