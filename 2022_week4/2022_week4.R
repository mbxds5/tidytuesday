library(pacman)
pacman::p_load(here,
               tidyverse,
               tidytuesdayR # load tidytuesday data
               )

# load data
tuesdata <- tuesdata <- tidytuesdayR::tt_load(2022, week = 4)
ratings <- tuesdata$ratings
details <- tuesdata$details

df <- left_join(ratings, details, by = "id")

view(df)

# data cleanup
df %>%
  filter(yearpublished >= 1965) %>%
  select(id, boardgamecategory, yearpublished) %>%
  mutate(boardgamecategory = str_remove_all(boardgamecategory, paste(c("\\[", "\\]", '\\"', "\\'"), collapse = "|"))) %>%
  separate_rows(boardgamecategory, sep = ",") %>%
  mutate(boardgamecategory = str_trim(boardgamecategory)) %>%
  distinct() %>%
  filter(boardgamecategory %in% "Card Game") %>%
  group_by(yearpublished) %>%
  tally() %>%
  
  # plot
  ggplot(aes(x = yearpublished, y = n)) +
  geom_area(fill = "#333333") +
  
  # scales
  scale_x_continuous(limits = c(1965,2020),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,550),
                     breaks = c(0,100,200,300,400,500,Inf),
                     expand = c(0,0)) +
  # captions and labels
  labs(title = "\nCard games have increased in
popularity since the 1970's",
  x = "",
  y = "Number of card games published \n",
  caption = "\n@danni_scales | source: Kaggle") +
  
  # theme
  theme(
    plot.title = element_text(family = "mono", face = "bold", size = 24, color = "#333333", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "#333333", size = 10),
    plot.margin = unit(c(0,1,0.5,0.5), "cm"), # t, r, b, l
    plot.background = element_rect(fill = "#e7cdd6"),
    panel.background = element_rect(fill = "#e7cdd6"),
    panel.grid.major = element_line(color = "#dfbbc8"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "#333333", size = 12, family = "mono"),
    axis.title.y = element_text(color = "#333333", size = 12),
    axis.text.y = element_text(color = "#333333", size = 12, family = "mono"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    text = element_text()
  ) +
  
  # arrows
  geom_curve(
    aes(x = 2004, xend = 2005, y = 250, yend = 170),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#7c668c",
    curvature = -0.4
  ) +
  geom_curve(
    aes(x = 2012, xend = 2016, y = 480, yend = 530),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#7c668c",
    curvature = -0.4
  ) +
  
  # text annotations
  annotate(
    "text",
    x = 2007.5,
    y = 445,
    label = paste0('Card games peaked in \npopularity during 2018'),
    color = "#7c668c",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 2004,
    y = 280,
    label = "Slight increase in 2007",
    color = "#7c668c",
    size = 3.5
  ) 

ggsave("2022_week4/2022_week4.png", width = 7.5, height = 5, device = png)