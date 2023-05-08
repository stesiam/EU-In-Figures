## Libraries

library(ggplot2)
library(sf)
library(ggtext)
library(showtext)
library(readr)
library(dplyr)

## Add fonts

font_add_google("EB Garamond", family = "clim")
font_add_google("Montserrat", family = "mont")

showtext_auto()


## Read shapefile

eu_map = st_read("Shapefiles/EU_Map_2020_RG/CNTR_RG_20M_2020_4326.shp") 


eu_map = eu_map %>%
  dplyr::filter(EU_STAT == "T" | EFTA_STAT == "T") %>%
  select(CNTR_ID)

colnames(eu_map)[1] = "geo"


## Add observations

expenditure_rd <- read_csv("EIF-2/expenditure_rd.csv") %>%
  filter(TIME_PERIOD == 2021 & sectperf == "TOTAL") %>%
  select(geo, OBS_VALUE)


## Merge 

merged_dataset = left_join(eu_map, expenditure_rd, by = "geo")




# Visualization

map = ggplot2::ggplot(data = merged_dataset) +
  geom_sf(aes(fill = OBS_VALUE)) +
  coord_sf(xlim = c(-21.5,35), ylim = c(35, 70)) +
  geom_sf_text(aes(label = OBS_VALUE),
               colour = "black",
               size = 10,
               family="mont") +
  scale_fill_gradient(low='white', high='yellow2') +
  labs(
    title = "Expenditure on Research and Development",
    subtitle = "Percentage of Gross Domestic Product invested on R&D, 2021",
    caption = "**stesiam** | Source: Eurostat"
  ) +
  theme_void()+
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5, margin = margin(b = 5, unit = "pt")),
    legend.position = "none"
  )

ggsave(
  filename = "EIF-2/EIF-2.png",
  plot = map,
  device = "png",
  bg = "white",
  height = 7,
  width = 7)

