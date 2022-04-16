dirYear <- "2020"
dirProject <- "2022-12-22-big-mac-index"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(dfrtheme)
library(patchwork)


# Plot ----

## Rupiah's exchange rate and valuation against the US dollar ----

rupiahExRate <- read_csv(
  here(
    dirYear, 
    dirProject, 
    "result",
    "idr-usd-valuation.csv"
  )
)

### Exchange rate

plotExchangeRate <- ggplot(data = rupiahExRate, mapping = aes(x = date)) +
  geom_line(
    mapping = aes(y = dollar_ex), 
    size = 1, 
    color = "#80CBC4"
  ) +
  geom_line(
    mapping = aes(y = implied_exchange_rate), 
    size = 1, 
    color = "#00796B"
  ) +
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = seq(0, 15000, 5000),
    position = "right",
    expand = c(0, 0)
  ) +
  labs(
    subtitle = "Rupiah per US dollar",
    x = NULL,
    y = NULL
  ) +
  geom_text(
    data = tibble(
      x = c(as.Date("2003-01-01"), as.Date("2007-06-01")),
      y = c(11500, 3500),
      label = c("Actual exchange rate", "Implied PPP")
    ),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 0
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

### Rupiah's valuation against the US dollar

plotRupiahValuation <- ggplot(
  data = rupiahExRate, 
  mapping = aes(x = date, y = usd_raw )
) +
  geom_line(color = "#00796B", size = 1) +
  scale_y_continuous(
    breaks = seq(-0.8, -0.2 , 0.2),
    limits = c(-0.8, -0.2),
    labels =  seq(-80, -20, 20),
    position = "right"
  ) +
  labs(
    subtitle = "Rupiah valuation against US dollar (in percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )


### Patchwork: rupiah's exchange rate and valuation against the US dollar
plotExchangeRate + 
  plotRupiahValuation +
  plot_annotation(
    title = "Rupiah is undervalued against the US dollar",
    subtitle = "Big Mac Index",
    caption = paste0(
      "Source: The Economist; Reuters; author's analysis<br>",
      "Chart: @dzulfiqarfr"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "idr-usd-valuation.svg"),
  width = 8,
  height = 4.5
)


## Big Mac prices by income group ----

bigMacIncomeGroup <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "big-mac-income-group.csv"
  )
)

bigMacIncomeGroupPrep <- bigMacIncomeGroup %>% 
  mutate(GroupName = fct_reorder(GroupName, dollar_price))

ggplot(
  data = bigMacIncomeGroupPrep,
  mapping = aes(x = GroupName, y = dollar_price)
) +
  geom_boxplot(varwidth = TRUE) +
  scale_x_discrete(
    labels = c("Lower-middle income", "Upper-middle income", "High income")
  ) +
  scale_y_continuous(
    breaks = seq(0, 8, by = 2),
    limits = c(0, 8),
    position = "right",
    expand = c(0, 0)
  ) +
  labs(
    title = "Richer countries record higher prices",
    subtitle = "Big Mac prices in July 2020 by income group (in US dollars)",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: The Economist; World Bank; author's analysis<br>",
      "Chart: @dzulfiqarfr"
    )
  ) + 
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "big-mac-income-group.svg"),
  width = 8,
  height = 4.5
)


## Big Mac prices and GDP per capita ----

bigMacGDP <- read_csv(here(dirYear, dirProject, "result", "big-mac-gdp.csv"))

ggplot(
  data = bigMacGDP,
  mapping = aes(x = gdp_dollar, y = dollar_price)
) +
  geom_segment(
    mapping = aes(xend = gdp_dollar, yend = pred),
    lty = "dashed",
    color = "#B0BEC5"
  ) +
  geom_point(
    mapping = aes(
      fill = name %in% c("Indonesia", "United States"),
      alpha = name %in% c("Indonesia", "United States")
    ),
    pch = 21,
    size = 4.5,
    stroke = 0.5,
    color = "white",
    show.legend = FALSE
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#00796B",
    lty = "dashed"
  ) +
  scale_x_continuous(
    limits = c(0, 90000),
    breaks = seq(0, 90000, 15000)
  ) +
  scale_y_continuous(
    breaks = seq(0, 8, 2), 
    limits = c(0, 8),
    position = "right",
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c(`TRUE` = "#00796B", `FALSE` = "#B0BEC5")) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.75)) +
  annotate(
    geom = "text",
    x = 10000,
    y = 1,
    label = "Indonesia",
    hjust = 0
  ) +
  annotate(
    geom = "curve",
    x = 10000,
    y = 1,
    xend = 3870.562,
    yend = 2.1,
    curvature = -0.5,
    arrow = arrow(length = unit(2, "mm"), ends = "last")
  ) +
  annotate(
    geom = "text",
    x = 27500,
    y = 5,
    label = "Adjusted prices"
  ) +
  annotate(
    geom = "curve",
    x = 27500,
    y = 4.75,
    xend = 27500,
    yend = 3.6,
    curvature = 0,
    lty = "dashed"
  ) +
  annotate(
    geom = "text",
    x = 62500,
    y = 6.25,
    label = "United States",
    hjust = 0
  ) +
  labs(
    title = "Big Mac price in Indonesia lower than expected",
    subtitle = paste0(
      "Big Mac price in July 2020 and GDP per capita in 2019 ",
      "(in US dollars)"
    ),
    x = "GDP per capita",
    y = "Big Mac price",
    caption = paste0(
      "Source: The Economist; IMF; author's analysis<br>",
      "Chart: @dzulfiqarfr"
    )
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black")
  )

ggsave(
  here(dirYear, dirProject, "result", "big-mac-gdp.svg"),
  width = 8,
  height = 4.5
)
