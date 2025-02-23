## Libraries

library(ggplot2)
library(sf)
library(ggtext)
library(showtext)
library(readr)
library(dplyr)
library(grid)


## Add fonts

font_add_google("Righteous", family = "clim")
font_add_google("Montserrat", family = "mont")

showtext_auto()


## Read shapefile

eu_map = st_read("Shapefiles/EU_Map_2020_RG/CNTR_RG_20M_2020_4326.shp") 
eu_map <- st_transform(eu_map, crs = 3035)

# eu_map = eu_map %>%
#   dplyr::filter((EU_STAT == "T" | EFTA_STAT == "T") | (CNTR_ID == "RU" | CNTR_ID =="TR" | CNTR_ID == "UA")) %>%
#   select(CNTR_ID)

colnames(eu_map)[1] = "geo"

eu_map = eu_map |>
  dplyr::filter(geo != "GL")


## Add observations

unemployment_labor_force <- read_csv("EIF-1/tps00203_page_linear.csv") %>%
  select(geo, OBS_VALUE)


## Merge 

merged_dataset = left_join(eu_map, unemployment_labor_force, by = "geo")

merged_dataset = merged_dataset |>
  dplyr::mutate(OBS_VALUE = case_when(
    geo == "UA" ~ 9.8,
    geo == "TR" ~ 9.41,
    geo == "UK" ~ 4.4,
    geo == "MD" ~ 3.3,
    geo == "AL" ~ 10.4,
    geo == "BA" ~ 10.2,
    geo == "MK" ~ 13.1,
    geo == "ME" ~ 12,
    .default = OBS_VALUE
  ))

# Visualization

sources = glue("<b>Πηγή:</b> Eurostat & World Bank<br><b>Γράφημα: <span style='font-family:fb;'  >&#xf09b;</span> stesiam | <span style='font-family:fb;'  >&#xe671;</span> stesiam")


bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Cross")[7:8]))

map = ggplot2::ggplot(data = merged_dataset) +
  geom_sf(aes(fill = OBS_VALUE)) +
  scale_x_continuous(limits = c(2550000, 7000000)) +
  scale_y_continuous(limits = c(1250000, 5600000)) +
  #geom_sf_text(data = merged_dataset, aes(label = round(OBS_VALUE,1)), color = "black", size = 12,fontface = "bold", family = "mont")+
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "IS"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 23,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "NO"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 23,
               fontface = "bold",
               family="mont",nudge_y = -400000, nudge_x = -200000) +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "ES"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 23,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "UK"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 23,
               fontface = "bold",
               family="mont", nudge_y = -240000) +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "PT"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "DE"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 25,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "FR"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 25,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "PL"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 25,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "EL"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 15,
               fontface = "bold",
               angle = -60,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "IE"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 17,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "IT"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 20,
               fontface = "bold",
               angle = -50,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "FI"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 20,
               fontface = "bold",
               angle = -50,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "RS"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 20,
               fontface = "bold",
               angle = -50,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "RO"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 22,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "BG"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 20,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "HU"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 20,
               fontface = "bold",
               angle = 20,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "EE"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "UA"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 22,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "TR"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 22,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "LT"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "SE"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 22,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "LV"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "DK"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = -70,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "AT"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "CH"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "CZ"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "SK"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 15,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "NL"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 13,
               fontface = "bold",
               angle = -10,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "BE"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 14,
               fontface = "bold",
               angle = -30,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "AL"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 11,
               fontface = "bold",
               angle = -60,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "BA"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 13,
               fontface = "bold",
               angle = -60,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "MK"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 11,
               fontface = "bold",
               angle = 0,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "MD"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 12.5,
               fontface = "bold",
               angle = -60,nudge_y = 35000, nudge_x = -40000,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "SI"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 11,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "HR"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 11,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "ME"),
               aes(label = OBS_VALUE),
               colour = "white",
               size = 10,
               fontface = "bold",
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(geo == "CY"),
               aes(label = OBS_VALUE),
               colour = "black",
               size = 10,
               angle = 40,
               fontface = "bold", nudge_x = -20000, nudge_y = -25000,
               family="mont") +
  geom_sf_text(data = merged_dataset |> dplyr::filter(!geo %in% c("ME", "CY","SI", "HR","MK","BA","MD","AL","NL", "BE","SK","CZ","CH","AT","DK","UK","SE", "NO", "TR","UA","LT", "LV","EE","HU","RO","BG","RS","FI","IT","IE","FR","PL","IS", "EL", "NO", "ES", "PT", "DE")),
               aes(label = round(OBS_VALUE,1)),
               colour = "black",
               size = 10,
               family="mont") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_richtext(y = 1335000, x = 2770000, label = sources,
                fill = NA, label.color = NA, hjust = "left", size = 12, lineheight = 0.4) +
  coord_sf(expand = FALSE) +
  theme_void()+
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5, margin = margin(b = 5, unit = "pt")),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = bg_gradient, color = "transparent"),
    panel.background = element_rect(fill = "transparent", color = NA)
  )



title_text <- "Ανεργία (%) στην Ευρώπη"

# Create the text grob
title_grob_text <- textGrob(title_text,
                            gp = gpar(col = "white", fontsize = 50, fontface = "bold", family = "mont"), 
                            just = "center")

grob <- rectGrob(gp = gpar(fill = "red4",col =NA))
# Adjust the annotation size relative to map coordinates
final_plot <- map +
  annotation_custom(grob, 
                    xmin = 2550000, xmax = 4500000, 
                    ymin = 5100000, ymax = 5400000) +
  annotation_custom(title_grob_text, 
                    xmin = 2550000, xmax = 4500000, 
                    ymin = 5200000, ymax = 5300000)



ggsave(
  filename = "EIF-1/EIF-1-el.png",
  height = 8.8,
  width = 9,
  plot = final_plot,
  device = "png",
  dpi = 300)

