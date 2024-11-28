source("R/00_libraries.R")

icb_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

icb_pop <- read.csv("data/gp-reg-pat-prac-quin-age.csv") |> 
  filter(
    AGE_GROUP_5 == "ALL",
    ORG_TYPE == "ICB",
    SEX == "ALL"
  ) |> 
  select(
    ICB23CD = "ONS_CODE",
    pop = "NUMBER_OF_PATIENTS"
  )

icb_shp <- sf::read_sf(icb_url) |> 
  left_join(
    icb_pop,
    by = join_by(ICB23CD)
  )

ggplot() +
  geom_sf(
    data = icb_shp,
    aes(fill = pop),
    colour = "black"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(
    title = "Integrated Care Systems and their GP registered\npopulations (2023 boundaries)",
    caption = "Source: NHS England Statistics,\nPatients Registered at a GP Practice, May 2024"
  ) + 
  scale_fill_viridis_c(
    name = "GP registered\npopulation",
    option = "F",
    direction = -1,
    label = scales::comma
  )


ggsave(
  "outputs/ICS_population_map.png",
  width = 6,
  height = 6,
  units = "in",
  bg = "white"
)
