# Topic modelling for Goodfellas

library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(slam)
library(wordcloud)
library(RColorBrewer)

# preprocessing
extra_stops <- c("james", "karen", 
                 "paulie", "henrys", "lois", "mickey", "batt", "tuddy", "morris", 
                 "carbone", "frenchy", "sonny", "franky", "bill", "spider", 
                 "conway", "cicero", "door", "home", "house", "leave", "begin", 
                 "stand", "stay", "watch", "people", "doesnt", "matter", 
                 "suppose","suddenly","michael", "operational", "whats","janice", "look","billy","karens","sandy", "back", "thing", "henry", "jimmy", "tommy")

goodfellas_clean <- scorsese_master %>%
  filter(title == "Goodfellas") %>%
  filter(!word %in% extra_stops) %>%
  filter(nchar(word) > 3)

# DTM and LDA
dtm_prep <- goodfellas_clean %>%
  unite("doc_id", title, scene_id, remove = FALSE) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

DTM <- dtm_prep[slam::row_sums(dtm_prep) > 0, ]

K <- 3
topicModel <- LDA(DTM, K, method = "Gibbs", 
                  control = list(iter = 2000, seed = 123, alpha = 0.1))

# topic labels
topic_labels <- c(
  "1" = "Interpersonal",
  "2" = "System",
  "3" = "Operational"
)

# for a later comparison
profile_goodfellas <- get_profile_manually(topicModel, topic_labels, "Goodfellas")

# top words for each topic
print(terms(topicModel, 20))

# data for plots
tmResults <- posterior(topicModel)
theta <- as.data.frame(tmResults$topics)
theta$scene_id <- as.numeric(gsub("Goodfellas_", "", rownames(theta)))

plot_data <- theta %>%
  pivot_longer(cols = -scene_id, names_to = "Topic", values_to = "Probability") %>%
  mutate(TopicName = topic_labels[as.character(Topic)])

# plot for narrative dynamics
p4 <- ggplot(plot_data, aes(x = scene_id, y = Probability, color = TopicName)) +
  geom_line(alpha = 0.2) + 
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +
  facet_wrap(~TopicName, scales = "free_y") +
  scale_color_manual(values = c("Operational" = "#4682B4", 
                                "System" = "#2E8B57", 
                                "Interpersonal" = "#CD5C5C")) +
  theme_minimal() +
  labs(title = "Narrative Dynamics: Goodfellas ",
       x = "Narrative Progression (Chronological)", y = "Probability (Theta)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave("results/Goodfellas/goodfellas_narrative_dynamics.png",
       plot = p4,
       width = 8,
       height = 6,
       dpi = 600)

# plot for sequence of dominant themes
dominant_topics <- plot_data %>%
  group_by(scene_id) %>%
  slice_max(Probability, n = 1, with_ties = FALSE)

p5 <- ggplot(dominant_topics, aes(x = scene_id, y = 1, fill = TopicName)) +
  geom_tile() +
  scale_fill_manual(values = c("Operational" = "#4682B4", 
                               "System" = "#2E8B57", 
                               "Interpersonal" = "#CD5C5C")) +
  theme_minimal() +
  labs(title = "Sequence of dominant themes: Goodfellas", fill = "Topic") +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

ggsave("results/Goodfellas/goodfellas_dominant_themes.png",
       plot = p5,
       width = 8,
       height = 6,
       dpi = 600)

# plot for word relevance
ap_topics <- tidy(topicModel, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p6 <- ggplot(ap_top_terms, aes(beta, reorder_within(term, beta, topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", labeller = labeller(topic = topic_labels)) +
  scale_y_reordered() +
  scale_fill_manual(values = c("1" = "#CD5C5C", "2" = "#2E8B57", "3" = "#4682B4")) +
  theme_minimal() +
  labs(title = "Word Relevance (Beta): Goodfellas",
       subtitle = "Which words have the most impact on the topics?",
       x = "Beta (Relevance)", y = NULL)

ggsave("results/Goodfellas/goodfellas_word_relevance.png",
       plot = p6,
       width = 8,
       height = 6,
       dpi = 600)
