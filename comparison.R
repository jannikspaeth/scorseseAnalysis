library(tidyverse)


all_films <- bind_rows(
  profile_meanStreets,
  profile_casino,
  profile_goodfellas,
  profile_departed,
  profile_irishman
)

# correct order
all_films$Film <- factor(all_films$Film,
                          levels = c("Mean Streets",
                                     "Goodfellas",
                                     "Casino",
                                     "The Departed",
                                     "The Irishman"))

all_films$Thema <- factor(all_films$Thema,
                           levels = c("Interpersonal",
                                      "System",
                                      "Operational"))

# plot
p16 <- ggplot(all_films,
       aes(x = Film,
           y = Anteil_Prozent,
           fill = Thema)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Thematic Comparison",
       x = "Film",
       y = "Percentage of Topic",
       fill = "Topic") +
  scale_fill_manual(values = c(
    "Operational"   = "#4682B4",
    "System"        = "#2E8B57",
    "Interpersonal" = "#CD5C5C"
  )) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("results/Comparison/comparison_byside.png",
       plot = p16,
       width = 8,
       height = 6,
       dpi = 600)


p17 <- ggplot(all_films,
       aes(x = Film,
           y = Anteil_Prozent,
           fill = Thema)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(title = "Thematic Composition of Scorsese Crime Films",
       x = "Film",
       y = "Percentage of Topic",
       fill = "Topic") +
  scale_fill_manual(values = c(
    "Operational"   = "#4682B4",
    "System"        = "#2E8B57",
    "Interpersonal" = "#CD5C5C"
  )) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("results/Comparison/comparison_stack.png",
       plot = p17,
       width = 8,
       height = 6,
       dpi = 600)


# table
comparison_table <- all_films %>%
  pivot_wider(names_from = Thema,
              values_from = Anteil_Prozent)

print(comparison_table)