##############################
####
####
####
####  Structural Topic Modeling - complete data
####
####
####
###############################

library(stm)
library(SnowballC)
library(tidyverse)
library(stminsights)
library(tidytext)
library(magrittr)

root_decretos_LA <- here::here()
raw_decretos_LA <- paste(root_decretos_LA,
                          "/decretos_LA_cloud_dir/data/raw",
                          sep = "")

images_decretos_LA <- paste(root_decretos_LA,
                         "/analysis/images/",
                         sep = "")

add_stopwords_ptb <- read_delim(
  paste(root_decretos_LA,
        "/support_files/stopwords-pt.txt",
        sep = ""),
  delim = ",", 
  col_names = F)

add_stopwords_spa <- read_delim(
  paste(root_decretos_LA,
        "/support_files/stopwords-es.txt",
        sep = ""),
  delim = ",", 
  col_names = F)

brasil <- read_csv(paste(
  raw_decretos_LA,
  "PRILA_DECRETOS_BRASIL_NUMERADOS.csv", 
  sep = "/"))

paraguay_row1 <- read_csv(paste(
  raw_decretos_LA,
  "PRILA_DECRETOS_PARAGUAY.csv", 
  sep = "/")) 
  
paraguay <- paraguay_row1 %>%
  slice(-1)

################## Brasil

ementa_brasil <- brasil %>%
  select(Ementa) %>%
  as.matrix()

topics = 4

proc_brasil <- textProcessor(ementa_brasil, 
                             language = "por", 
                             customstopwords = add_stopwords_ptb$X1)
vocab_brasil <- proc_brasil$vocab
docs_brasil <- proc_brasil$documents

prep_brasil <- prepDocuments(docs_brasil, vocab_brasil)
prep_brasil_docs <- prep_brasil$documents
prep_brasil_vocab <- prep_brasil$vocab

stm_brasil <- stm(prep_brasil_docs, 
                  prep_brasil_vocab, 
                  topics, 
                  init.type = "Spectral")

stm_brasil_topics <- labelTopics(stm_brasil)
stm_brasil_topics

AP_brasil <- tidy(stm_brasil, matrix = "beta")

ap_top_brasil <- 
  AP_brasil %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topics_brasil <- ap_top_brasil %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave(paste(images_decretos_LA,
             "brasil_topics_4.png",
             sep = ""),
       topics_brasil)

plot(stm_brasil, type = "summary", xlim = c(0, 1))

png(paste(images_decretos_LA,
          "quotes_brasil.png",
          sep = ""),
    width     = 1100,
    height    = 650)

par(mfrow = c(2, 2),mar = c(1, 1, 1, 1))
for (t in 1:topics) {
  quotes <- findThoughts(stm_brasil, 
                            texts = ementa_brasil,
                            n = 3, 
                            topics = t)$docs[[1]]
  plotQuote(quotes,
            width = 100, 
            main = paste("Quote Topic ",
                         t,                
                         sep = ""))
}
dev.off()


################## Paraguay

ementa_paraguay <- paraguay %>%
  select(Ementa) %>%
  as.matrix()

topics = 5

proc_paraguay <- textProcessor(ementa_paraguay, 
                               language = "spa", 
                               customstopwords = add_stopwords_spa$X1)
vocab_paraguay <- proc_paraguay$vocab
docs_paraguay <- proc_paraguay$documents

prep_paraguay <- prepDocuments(docs_paraguay, vocab_paraguay)
prep_paraguay_docs <- prep_paraguay$documents
prep_paraguay_vocab <- prep_paraguay$vocab

stm_paraguay <- stm(prep_paraguay_docs, 
                    prep_paraguay_vocab, 
                    topics, 
                    init.type = "Spectral")

stm_paraguay_topics <- labelTopics(stm_paraguay)
stm_paraguay_topics

AP_paraguay <- tidy(stm_paraguay, matrix = "beta")

ap_top_paraguay <- 
  AP_paraguay %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topics_paraguay <- ap_top_paraguay %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave(paste(images_decretos_LA,
             "paraguay_topics.png",
             sep = ""), topics_paraguay)

plot(stm_paraguay, type = "summary", xlim = c(0, 1))
# 
# png(paste(images_decretos_LA,
#           "quotes_paraguay.png",
#           sep = ""),
#     width     = 1100,
#     height    = 650)
# 
# par(mfrow = c(2, 3),mar = c(1, 1, 1, 1))
# for (t in 1:topics) {
#   quotes <- findThoughts(stm_paraguay, 
#                          texts = ementa_paraguay,
#                          n = 3, 
#                          topics = t)$docs[[1]]
#   plotQuote(quotes,
#             width = 100, 
#             main = paste("Quote Topic ",
#                          t,                
#                          sep = ""))
# }
# dev.off()

################## Checking examples
# 
# examples_ementa <- function(corpus, stm_corpus, text_col, n_examples){
#   stm_corpus_labels <- labelTopics(stm_corpus)
#   for(z in 1:stm_corpus$settings$dim$K){
#     out <- order(stm_corpus$theta[,z], decreasing=T)[1:n_examples]
#     print(paste("Topic:", z,
#                 paste(stm_corpus_labels$frex[z,],
#                       collapse = ".")))
#     for(m in 1:length(out)){
#       print(as.character(corpus[out[m], text_col]))
#       readline('next - press enter')
#     }
#   }
# }
# 
# examples_ementa(brasil, stm_brasil, text_col = 5, n_examples = 5)
# 
# 
# examples_ementa(paraguay, stm_paraguay, text_col = 4, topics =  4, n_examples = 5)

