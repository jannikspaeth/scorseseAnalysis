# Topic modelling for The Irishman

library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(slam)
library(wordcloud)
library(RColorBrewer)

# preprocessing
extra_stops <- c("frank", "sheeran", "russell", "bufalino", "jimmy", "hoffa", 
                 "peggy", "angelo", "bruno", "tony", "pro", "provenzano", 
                 "bill", "chuckie", "obrien", "sal", "sally", "bugs", "fat", 
                 "salerno", "joey", "gallo", "crazy", "irene", "carrie", 
                 "mary", "dolores", "whispers", "ditullio", "skinny", "razor", 
                 "felix", "fitz", "fitzsimmons", "kennedy", "bobby", "nixon",
                 "josephine", "jo", "barbara", "allen", "dorfman", "wont", "jake", "gottlieb",
                 "phil", "testa", "jerry", "couldnt", "whos", "vale", "don", "rickles", 
                 "jimmys", "hoffas", "john","whispers","lincoln", "francis", "umbertos", "jack", "wasnt", 
                 "russ", "howard", "johnsons", "bufalinos", "isnt")

irishman_clean <- scorsese_master %>%
  filter(title == "The Irishman") %>%
  filter(!word %in% extra_stops) %>%
  filter(nchar(word) > 3)

# DTM and LDA
dtm_prep <- irishman_clean %>%
  unite("doc_id", title, scene_id, remove = FALSE) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

DTM <- dtm_prep[slam::row_sums(dtm_prep) > 0, ]

K <- 3
topicModel <- LDA(DTM, K, method = "Gibbs", 
                  control = list(iter = 2000, seed = 123, alpha = 0.1))

# topic labels
topic_labels <- c(
  "1" = "System",
  "2" = "Interpersonal",
  "3" = "Operational"
)

# for a later comparison
profile_irishman <- get_profile_manually(topicModel, topic_labels, "The Irishman")

# top words for each topic
print(terms(topicModel, 20))

# data for plots
tmResults <- posterior(topicModel)
theta <- as.data.frame(tmResults$topics)
theta$scene_id <- as.numeric(gsub("The Irishman_", "", rownames(theta)))

plot_data <- theta %>%
  pivot_longer(cols = -c(scene_id), names_to = "Topic", values_to = "Probability") %>%
  mutate(TopicName = topic_labels[as.character(Topic)])

# plot for narrative dynamics
p7 <- ggplot(plot_data, aes(x = scene_id, y = Probability, color = TopicName)) +
  geom_line(alpha = 0.1) + 
  geom_smooth(method = "loess", span = 0.4, se = FALSE, size = 1.5) +
  facet_wrap(~TopicName, scales = "free_y") +
  scale_color_manual(values = c("Operational" = "#4682B4", 
                                "System" = "#2E8B57", 
                                "Interpersonal" = "#CD5C5C")) +
  theme_minimal() +
  labs(title = "Narrative Dynamics: The Irishman",
       x = "Narrative Progression (Chronological)", y = "Probability (Theta)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave("results/TheIrishman/theIrishman_narrative_dynamics.png",
       plot = p7,
       width = 8,
       height = 6,
       dpi = 600)

# plot for sequence of dominant themes
dominant_topics <- plot_data %>%
  group_by(scene_id) %>%
  slice_max(Probability, n = 1, with_ties = FALSE)

p8 <- ggplot(dominant_topics, aes(x = scene_id, y = 1, fill = TopicName)) +
  geom_tile() +
  scale_fill_manual(values = c("Operational" = "#4682B4", 
                               "System" = "#2E8B57", 
                               "Interpersonal" = "#CD5C5C")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), panel.grid = element_blank()) +
  labs(title = "Sequence of dominant themes: The Irishman", fill = "Topic") +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

ggsave("results/TheIrishman/theIrishman_dominant_themes.png",
       plot = p8,
       width = 8,
       height = 6,
       dpi = 600)

# wordcloud for topic 1 as an example
selected_id <- 1 
topic_words <- tidy(topicModel, matrix = "beta") %>%
  filter(topic == selected_id) %>%
  slice_max(beta, n = 50)

png("results/TheIrishman/theIrishman_wordcloud.png",
    width = 2400,
    height = 1800,
    res = 300)

wordcloud(words = topic_words$term, freq = topic_words$beta, colors = brewer.pal(8, "Dark2"))

dev.off()


# plot for word relevance
ap_topics <- tidy(topicModel, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p9 <- ggplot(ap_top_terms, aes(beta, reorder_within(term, beta, topic),fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic,scales = "free",labeller = labeller(topic = topic_labels)) +
  scale_y_reordered() +
  scale_fill_manual(values = c(
    "1" = "#2E8B57",
    "2" = "#CD5C5C",
    "3" = "#4682B4"
  )) +
  theme_minimal() +
  labs(title = "Word Relevance (Beta): The Irishman",
       x = "Beta (Relevance)", y = NULL)

ggsave("results/TheIrishman/irishman_word_relevance.png",
       plot = p9,
       width = 8,
       height = 6,
       dpi = 600)

