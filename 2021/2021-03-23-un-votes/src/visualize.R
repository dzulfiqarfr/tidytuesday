dirYear <- "2021"
dirProject <- "2021-03-23-un-votes"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(ggbeeswarm)
library(ggrepel)
library(patchwork)
library(dfrtheme)


# Plot ----

## Voting coincidence, overall and by region ----

vcOverall <- read_csv(
  here(
    dirYear, 
    dirProject, 
    "result",
    "voting-coincidence-overall.csv"
  )
)

axisXbreak <- c(seq(1950, 2010, 10), 2019)
axisXlabel <- c("1950", "'60", "'70", "'80", "'90", "2000", "'10", "'19")

plotVCoverall <- ggplot(
  data = vcOverall,
  mapping = aes(x = year, y = voting_coincidence_share)
) +
  geom_point(
    pch = 21,
    size = 2,
    fill = "#26A69A",
    color = "white",
    alpha = 0.25
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    lwd = 1,
    color = "#26A69A"
  ) +
  scale_x_continuous(breaks = axisXbreak, labels = axisXlabel) +
  scale_y_continuous(
    breaks = seq(25, 100, 25),
    limits = c(25, 100),
    position = "right"
  ) +
  labs(
    subtitle = "Keseluruhan",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )

vcCountry <- read_csv(
  here(
    dirYear, 
    dirProject, 
    "result",
    "voting-coincidence-country.csv"
  )
)

vcCountrySubRegionOrdered <- vcCountry %>% 
  filter(year == 2019) %>% 
  mutate(region = fct_reorder(region, voting_coincidence_share))

textCountry <- vcCountrySubRegionOrdered %>% 
  group_by(region) %>% 
  filter(
    voting_coincidence_share == min(voting_coincidence_share),
    region %in% c(
      "Latin America & Caribbean",
      "East Asia & Pacific",
      "Middle East & North Africa"
    )
  ) %>% 
  ungroup() 

plotVCregion <- ggplot(
  data = vcCountrySubRegionOrdered,
  mapping = aes(x = voting_coincidence_share, y = region)
) +
  geom_quasirandom(
    groupOnX = FALSE,
    pch = 21,    
    size = 2,
    fill = "#26A69A",
    color = "white",
    alpha = 0.5
  ) +
  geom_text(
    data = tibble(x = 84.4, y = "Europe & Central Asia", label = "Median"),
    mapping = aes(x = x, y = y, label = label),
    nudge_x = -10,
    nudge_y = 0.5,
    size = dfr_convert_font_size(),
    color = "#26A69A",
    fontface = "bold"
  ) +
  geom_text(
    data = textCountry,
    mapping = aes(label = country),
    size = dfr_convert_font_size(),
    color = "#26A69A",
    hjust = "inward",
    nudge_y = 0.25,
    alpha = 0.75,
    check_overlap = TRUE
  ) +
  geom_text_repel(
    data = tibble(x = 0, y = "North America", label = "United States"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#26A69A",
    alpha = 0.75,
    hjust = 0,
    nudge_x = 7.5,
    nudge_y = 0.65,
    segment.curvature = 0.25,
    segment.ncp = 3
  ) +
  stat_summary(
    geom = "crossbar",
    fun = median, 
    color = "#26A69A",
    lwd = 0.4
  ) +
  scale_x_continuous(
    labels = seq(0, 100, 25),
    expand = c(0, 0),
    position = "top"
  ) +
  labs(
    subtitle = "Menurut kawasan, 2019",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks.y = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.y = element_blank()
  )

captionVC <- paste0(
  "Sumber: Erik Voeten, Data and Analyses of Voting in ",
  "the UN General Assembly, 2013; penulis<br>",
  "Grafik: @dzulfiqarfr"
)

plotVCoverall +
  plotVCregion +
  plot_annotation(
    title = "Suara Indonesia, anggota PBB lain sedikit menjauh",
    subtitle = paste0(
      "Kemiripan suara di Sidang Umum untuk resolusi terkait ",
      "konflik Israel-Palestina (persen)"
    ),
    caption = captionVC,
    theme = dfr_theme()
  )

ggsave(
  here(
    dirYear,
    dirProject,
    "result",
    "voting-coincidence-overall-and-region.png"
  ),
  width = 8.5,
  height = 4.5
)  


## Voting coincidence with P5 countries ----

vcP5 <- vcCountry %>% 
  filter(
    country %in% c(
      "Russia", 
      "United States", 
      "China", 
      "France", 
      "United Kingdom",
      "Israel"
    )
  )

labelP5 <- vcP5 %>% 
  filter(year == last(year)) %>% 
  mutate(
    voting_coincidence_share = case_when(
      country == "Russia" ~ 90,
      country == "United Kingdom" ~ 79,
      country == "United States" ~ 5,
      TRUE ~ voting_coincidence_share
    )
  )

paletteP5 <- c(
  China = "#FF7043",
  Russia = "#FFA726",
  France = "#26A69A", 
  `United Kingdom` = "#9CCC65",
  `United States` = "#42A5F5",
  Israel = "#26C6DA"
)

ggplot(data = vcP5, mapping = aes(year, voting_coincidence_share)) +
  geom_point(
    mapping = aes(fill = country), 
    pch = 21, 
    size = 2.5, 
    color = "white",
    alpha = 0.25,
    show.legend = FALSE
  ) +
  geom_smooth(
    mapping = aes(color = country),
    method = "loess",
    se = FALSE,
    lwd = 1,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = axisXbreak, labels = axisXlabel) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    limits = c(0, 100),
    expand = c(0, 0),
    sec.axis = dup_axis(
      breaks = labelP5$voting_coincidence_share,
      labels = labelP5$country,
      name = NULL,
      guide = guide_axis()
    )
  ) +
  scale_color_manual(values = paletteP5) +
  scale_fill_manual(values = paletteP5) +
  labs(
    title = "Divergensi suara AS, Israel dari Indonesia",
    subtitle = paste0(
      "Kemiripan suara di Sidang Umum untuk resolusi ",
      "terkait konflik Israel-Palestina,<br>",
      "anggota permanen Dewan Keamanan & Israel (persen)"
    ),
    x = NULL,
    y = NULL,
    caption = captionVC
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "voting-coincidence-p5.png"),
  width = 8,
  height = 4.5
)


## Voting coincidence by resolution

vcResolution <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "voting-coincidence-resolution.csv"
  )
)

vcResolutionSub <- vcResolution %>% filter(voting_coincidence_share > 0)

plotVCresolutionHist <- ggplot(
  data = vcResolutionSub,
  mapping = aes(x = voting_coincidence_share)
) +
  geom_histogram(binwidth = 2.5, fill = "#26A69A") +
  scale_y_continuous(
    breaks = seq(0, 300, 60),
    limits = c(0, 300),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_x_continuous(labels = seq(40, 100, 20)) +
  labs(
    subtitle = "Distribusi kemiripan suara",
    x = "Kemiripan suara (persen)",
    y = "Jumlah resolusi"
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

vcResolutionLackSupport <- read_csv(
  here(
    dirYear, 
    dirProject,
    "result",
    "voting-coincidence-lack-support.csv"
  )
)

vcResolutionLackSupportOrdered <- vcResolutionLackSupport %>% 
  mutate(unres = fct_reorder(unres, voting_coincidence_share))

plotVCresolutionLackSupport <- ggplot(
  data = vcResolutionLackSupportOrdered,
  mapping = aes(x = voting_coincidence_share, y = unres)
) +
  geom_col(
    width = 0.6, 
    fill = "#26A69A",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60),
    expand = c(0, 0),
    position = "top"
  ) +
  geom_text(
    mapping = aes(label = short), 
    size = dfr_convert_font_size(),
    color = "white",
    hjust = 1
  ) +
  geom_text(
    mapping = aes(label = year),
    size = dfr_convert_font_size(),
    hjust = 0
  ) +
  labs(
    subtitle = "Kemiripan suara terendah (persen)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks.y = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.y = element_blank()
  )

plotVCresolutionHist +
  plotVCresolutionLackSupport +
  plot_annotation(
    title = "Sedikit perselisihan",
    subtitle = paste0(
      "Kemiripan suara di Sidang Umum untuk resolusi ",
      "terkait konflik Israel-Palestina, per resolusi"
      ),
    caption = captionVC,
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "voting-coincidence-resolution.png"),
  width = 10,
  height = 5.5
)
