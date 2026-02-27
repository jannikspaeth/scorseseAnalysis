# Topic modelling for Mean Streets

if (!require("wordcloud")) install.packages("wordcloud")
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(slam)
library(wordcloud)
library(RColorBrewer)

# preprocessing
extra_stops <- c("charlie", "johnny", "tony", "michael", "teresa", "giovanni", 
                 "mario", "oscar", "diane", "vic", "joey", "clout", "jimmy", 
                 "scorsese", "civello", "pino", "sammy", "charlies","marie", "teresas", "benton", "groppi")

meanStreets_clean <- scorsese_master %>%
  filter(title == "Mean Streets") %>%
  filter(!word %in% extra_stops) %>%
  filter(nchar(word) > 3)

# DTM and LDA
dtm_prep <- meanStreets_clean %>%
  unite("doc_id", title, scene_id, remove = FALSE) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

DTM <- dtm_prep[slam::row_sums(dtm_prep) > 0, ]

K <- 3
topicModel <- LDA(DTM, K, method = "Gibbs", 
                  control = list(iter = 2000, seed = 123, alpha = 0.1))

# top words for each topic
print(terms(topicModel, 20))

# topic labels
topic_labels <- c(
  "1" = "Interpersonal",
  "2" = "System",
  "3" = "Operational"
)

# for a later comparison
profile_meanStreets <- get_profile_manually(topicModel, topic_labels, "Mean Streets")

# data for plots
tmResults <- posterior(topicModel)
theta <- as.data.frame(tmResults$topics)
theta$scene_id <- as.numeric(gsub("Mean Streets_", "", rownames(theta)))

plot_data <- theta %>%
  pivot_longer(cols = -c(scene_id), names_to = "Topic", values_to = "Probability") %>%
  mutate(TopicName = topic_labels[as.character(Topic)])

# plot for narrative dynamics
p13 <- ggplot(plot_data, aes(x = scene_id, y = Probability, color = TopicName)) +
  geom_line(alpha = 0.1) + 
  geom_smooth(method = "loess", span = 0.4, se = FALSE, size = 1.5) +
  facet_wrap(~TopicName, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("Operational" = "#4682B4", 
                                "System" = "#2E8B57", 
                                "Interpersonal" = "#CD5C5C")) +
  labs(title = "Narrative Dynamics: Mean Streets",
       x = "Narrative Progression (Chronological)",
       y = "Probability (Theta)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave("results/MeanStreets/meanStreets_narrative_dynamics.png",
       plot = p13,
       width = 8,
       height = 6,
       dpi = 600)


# plot for sequence of dominant themes
dominant_topics <- plot_data %>%
  group_by(scene_id) %>%
  slice_max(Probability, n = 1, with_ties = FALSE) %>% 
  ungroup()

p14 <- ggplot(dominant_topics, aes(x = scene_id, y = 1, fill = TopicName)) +
  geom_tile() +
  scale_fill_manual(values = c("Operational" = "#4682B4", 
                               "System" = "#2E8B57", 
                               "Interpersonal" = "#CD5C5C")) +
  theme_minimal() +
  labs(title = "Sequence of dominant themes: Mean Streets",
       fill = "Topic") +
  theme(
    axis.text.y = element_blank(), 
    panel.grid = element_blank(),
    legend.position = "bottom")

ggsave("results/MeanStreets/meanStreets_dominant_themes.png",
       plot = p14,
       width = 8,
       height = 6,
       dpi = 600)


# word relevance

ap_topics <- tidy(topicModel, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p15 <- ggplot(ap_top_terms, aes(beta, reorder_within(term, beta, topic),fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic,scales = "free",labeller = labeller(topic = topic_labels)) +
  scale_y_reordered() +
  scale_fill_manual(values = c(
    "1" = "#CD5C5C",
    "2" = "#2E8B57",
    "3" = "#4682B4"
  )) +
  theme_minimal() +
  labs(title = "Word Relevance (Beta): Mean Streets",
       x = "Beta (Relevance)", y = NULL)

ggsave("results/MeanStreets/meanStreets_word_relevance.png",
       plot = p15,
       width = 8,
       height = 6,
       dpi = 600)

