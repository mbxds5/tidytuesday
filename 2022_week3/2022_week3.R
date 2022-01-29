library(pacman)
pacman::p_load(here,
               tidyverse,
               tidytuesdayR, # load tidytuesday data
               MetBrewer,
               extrafont)

# load data
tuesdata <- tuesdata <- tidytuesdayR::tt_load(2022, week = 3)
chocolate <- tuesdata$chocolate

# data cleanup
chocolate %>%
  filter(company_location %in% c('U.S.A.', 'France',  'U.K.', 'Brazil', 'Belgium',  'Switzerland')) %>%
  filter(review_date %in% c('2010', '2011', '2012', '2013', '2014', '2015', '2016',  '2017', '2018', '2019',  '2020')) %>%
  mutate(cocoa_percent = map_dbl(cocoa_percent,
                                        ~mean(as.numeric(str_extract_all(.x, "\\d+")[[1]])))) %>%
  select(review_date, company_location, cocoa_percent) %>%
  group_by(review_date, company_location) %>%
  summarise(mean=mean(cocoa_percent)) %>%

# plot
ggplot(aes(x = factor(company_location), y = reorder(review_date, desc(review_date)))) +
  geom_line(aes(colour = company_location, group = company_location), show.legend = F) +
  geom_tile(fill = NA, size = .8) +
  geom_point(aes(colour = company_location, size = mean)) +
  geom_text(aes(label = round(mean,0)), size = 3.5, color = "white") +
  
  # scales
  scale_x_discrete(position = "top") +
  scale_color_manual(values = met.brewer("Morgenstern", 6)) +
  scale_size(name = "Percentage cocoa (%): ",
             range = c(10, 20),
             breaks = c(55, 60, 65, 70, 75, Inf),
             labels = c("55", "60", "65", "70", "75+", "80")) +

  # guides
  guides(colour = "none",
         size = guide_legend(override.aes = list(colour = "#845045",
                                                  fill = "#845045"))) + # override legend colours
  
  # captions and labels
  labs(title = "COCOA LOCO",
       subtitle = "Has the percentage of cocoa in chocolate from
  major chcolate producers changed over a decade?",
       x = "Company location",
       caption = "@danni_scales | source: Flavorsofcacao.com") +
  
  # theme
  theme(plot.title = element_text(family = "Arial Black", face="bold", size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10),
        plot.background = element_rect(fill = "#fef9f2", color = NA),
        plot.margin = unit(c(0.5,1,0.5,0.5), "cm"), # t, r, b, l
        panel.background = element_rect(fill = "#fef9f2",
                                                colour = NA),
        panel.grid.major = element_line(size = 0.5),
        text = element_text(color = "#845045"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 13, face = "bold", color = "#845045"),
        axis.text.x = element_text(size = 12, color = "#845045"),
        axis.text.y = element_text(size = 11, color = "#845045"),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "#fef9f2"),
        legend.background = element_rect(fill = "#fef9f2")
  )

ggsave("2022_week3/2022_week3.png", width = 6, height = 9, device = png)