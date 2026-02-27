# topic modelling casino

library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(slam)
library(wordcloud)
library(RColorBrewer)

# preprocessing
extra_stops <- c("sam", "ace", "rothstein", "nicky", "santoro", "ginger", "mckenna", 
                 "lester", "diamond", "billy", "sherbert", "vincenzo", "artie", 
                 "piscano", "vegas", "frankie", "gingers", "gaggi", "tangiers", "ace", "charlie", "nicholas", "santoro", "nicky", "ginger", "marino", "sherbert", "amy", "dominick", "lester", "nickys", "nance", "jennifer")

casino_clean <- scorsese_master %>%
  filter(title == "Casino") %>%
  filter(!word %in% extra_stops) %>%
  filter(nchar(word) > 3)

# DTM and LDA
dtm_prep <- casino_clean %>%
  unite("doc_id", title, scene_id, remove = FALSE) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

DTM <- dtm_prep[slam::row_sums(dtm_prep) > 0, ]

K <- 3
topicModel <- LDA(DTM, K, method = "Gibbs", 
                  control = list(iter = 2000, seed = 123, alpha = 0.1))

# topic labels
topic_labels <- c(
  "1" = "Operational",
  "2" = "Interpersonal",
  "3" = "System"
)

# for a later comparison
profile_casino <- get_profile_manually(topicModel, topic_labels, "Casino")

# top words for each topic
print(terms(topicModel, 20))

# data for plots
tmResults <- posterior(topicModel)
theta <- as.data.frame(tmResults$topics)
theta$scene_id <- as.numeric(gsub("Casino_", "", rownames(theta)))

plot_data <- theta %>%
  pivot_longer(cols = -scene_id, names_to = "Topic", values_to = "Probability") %>%
  mutate(TopicName = topic_labels[as.character(Topic)])

# plot for narrative dynamics
p1 <- ggplot(plot_data, aes(x = scene_id, y = Probability, color = TopicName)) +
  geom_line(alpha = 0.2) + 
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +
  facet_wrap(~TopicName, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Operational"   = "#4682B4",
    "System"        = "#2E8B57",
    "Interpersonal" = "#CD5C5C"
  )) + 
  labs(title = "Narrative Dynamics: Casino",
       x = "Narrative Progression (Chronological)", y = "Probability (Theta)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave("results/Casino/casino_narrative_dynamics.png",
       plot = p1,
       width = 8,
       height = 6,
       dpi = 600)

# plot for sequence of dominant themes
dominant_topics <- plot_data %>%
  group_by(scene_id) %>%
  slice_max(Probability, n = 1, with_ties = FALSE)

p2 <- ggplot(dominant_topics, aes(x = scene_id, y = 1, fill = TopicName)) +
  geom_tile() +
  # had difficulties managing the colors so I used hexa
  scale_fill_manual(values = c("Operational" = "#4682B4", 
                               "System" = "#2E8B57", 
                               "Interpersonal" = "#CD5C5C")) +
  theme_minimal() +
  labs(title = "Sequence of dominant themes: Casino", fill = "Topic") +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

ggsave("results/Casino/casino_dominant_themes.png",
       plot = p2,
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

p3 <- ggplot(ap_top_terms, aes(beta, reorder_within(term, beta, topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", labeller = labeller(topic = topic_labels)) +
  scale_y_reordered() +
  scale_fill_manual(values = c("1" = "#CD5C5C", "2" = "#2E8B57", "3" = "#4682B4")) +
  theme_minimal() +
  labs(title = "Word Relevance (Beta): Casino",
       subtitle = "Which words have the most impact on the topics?",
       x = "Beta (Relevance)", y = NULL)

ggsave("results/Casino/casino_word_relevance.png",
       plot = p3,
       width = 8,
       height = 6,
       dpi = 600)