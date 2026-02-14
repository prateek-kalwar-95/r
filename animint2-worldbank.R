library(animint2)
data(WorldBank)

WorldBank$Region <- sub(" [(].*", "", WorldBank$region)
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))

not.na$population[is.na(not.na$population)] <- 1700000

# Facet Helper Functions
FACETS <- function(df, top, side) {
  data.frame(
    df,
    top  = factor(top, c("Fertility rate", "Years")),
    side = factor(side, c("Years", "Life expectancy"))
  )
}

TS.LIFE <- function(df) FACETS(df, "Years", "Life expectancy")
SCATTER <- function(df) FACETS(df, "Fertility rate", "Life expectancy")
TS.FERT <- function(df) FACETS(df, "Fertility rate", "Years")
MAP.FAC <- function(df) FACETS(df, "Years", "Years")

years <- unique(not.na[, "year", drop = FALSE])

first.year <- min(not.na$year)
last.year <- max(not.na$year)

# World map preparation
map_df <- animint2::map_data("world")
map_names <- c(x = "long", y = "lat")

country2Region <- with(
  unique(not.na[, c("Region", "country")]),
  structure(Region, names = country)
)

map2wb <- c(
  Antigua = "Antigua and Barbuda",
  Brunei = "Brunei Darussalam",
  Bahamas = "Bahamas, The",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Republic of Congo" = "Congo, Rep.",
  "Ivory Coast" = "Cote d'Ivoire",
  Egypt = "Egypt, Arab Rep.",
  Micronesia = "Micronesia, Fed. Sts.",
  UK = "United Kingdom",
  Gambia = "Gambia, The",
  Iran = "Iran, Islamic Rep.",
  Kyrgyzstan = "Kyrgyz Republic",
  "Saint Kitts" = "St. Kitts and Nevis",
  "North Korea" = "Korea, Dem. Rep.",
  "South Korea" = "Korea, Rep.",
  Laos = "Lao PDR",
  "Saint Lucia" = "St. Lucia",
  "North Macedonia" = "Macedonia, FYR",
  Palestine = "West Bank and Gaza",
  Russia = "Russian Federation",
  Slovakia = "Slovak Republic",
  "Saint Martin" = "Sint Maarten (Dutch part)",
  Syria = "Syrian Arab Republic",
  Trinidad = "Trinidad and Tobago",
  Tobago = "Trinidad and Tobago",
  USA = "United States",
  "Saint Vincent" = "St. Vincent and the Grenadines",
  Venezuela = "Venezuela, RB",
  "Virgin Islands" = "Virgin Islands (U.S.)",
  Yemen = "Yemen, Rep."
)

map_disp <- with(
  map_df,
  data.frame(
    group,
    country = ifelse(region %in% names(map2wb), map2wb[region], region)
  )
)

map_disp$Region <- country2Region[map_disp$country]
map_disp <- as.data.frame(map_disp)

for (new.var in names(map_names)) {
  old.var <- map_names[[new.var]]
  old.val <- map_df[[old.var]]
  m <- min(old.val, na.rm = TRUE)
  old.01 <- (old.val - m) / (max(old.val, na.rm = TRUE) - m)
  map_disp[[new.var]] <- old.01 * (last.year - first.year) + first.year
}

# Plot parameters
line_alpha <- 3 / 5
line_size <- 4

# Build plots
ts.right <- ggplot() +
  make_tallrect(
    not.na, "year",
    data.fun = TS.LIFE,
    alpha = 1 / 2
  ) +
  geom_line(
    aes(year, life.expectancy, group = country, color = Region, key = country),
    clickSelects = "country",
    data = TS.LIFE(not.na),
    size = line_size,
    alpha = line_alpha
  )

ts.facet <- ts.right +
  theme_bw() +
  theme(panel.margin = grid::unit(0, "lines")) +
  facet_grid(side ~ top, scales = "free") +
  xlab("") +
  ylab("")

ts.scatter <- ts.facet +
  geom_point(
    aes(
      fertility.rate, life.expectancy,
      color = Region, size = population,
      key = country
    ),
    clickSelects = "country",
    showSelected = "year",
    data = SCATTER(not.na),
    alpha = 1,
    alpha_off = 0.3
  ) +
  geom_text(
    aes(
      fertility.rate, life.expectancy,
      label = country,
      key = country
    ),
    data = SCATTER(not.na),
    showSelected = c("country", "year"),
    vjust = 0,
    alpha = 0.7
  ) +
  geom_text(
    aes(
      5, 85,
      label = paste0("year = ", year),
      key = 1
    ),
    showSelected = "year",
    data = SCATTER(years)
  ) +
  scale_x_continuous(
    breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2, 4, 6, 8)
  ) +
  scale_size_animint(pixel.range = c(2, 20), breaks = 10^(9:5))

scatter.both <- ts.scatter +
  make_widerect(
    not.na, "year",
    data.fun = TS.FERT,
    alpha = 1 / 2
  ) +
  geom_path(
    aes(fertility.rate, year, group = country, color = Region, key = country),
    clickSelects = "country",
    data = TS.FERT(not.na),
    size = line_size,
    alpha = line_alpha,
    alpha_off = 0.1
  )
# geom_label_aligned(
#   aes(
#     fertility.rate, year,
#     key = country,
#     colour = Region,
#     label = country
#   ),
#   data = TS.FERT(min.years),
#   showSelected = "country",
#   clickSelects = "country",
#   alignment = "horizontal",
#   vjust = 1
# )

scatter.map <- scatter.both +
  geom_polygon(
    aes(
      x = x, y = y,
      key = group,
      group = group,
      fill = Region
    ),
    data = MAP.FAC(map_disp),
    title = "World map",
    clickSelects = "country",
    color = "black",
    color_off = "transparent",
    alpha = 1,
    alpha_off = 0.3
  )

viz <- animint(
  title = "World Bank data (multiple selection, facets, map)",
  scatter = scatter.map +
    theme_animint(width = 1000, height = 600),
  time = list(variable = "year", ms = 2000),
  duration = list(year = 1000, country = 1000, Region = 1000),
  first = list(
    year    = 1975,
    country = c("United States", "Canada", "France", "Japan", "India")
  ),
  selector.types = list(country = "multiple")
)

animint2dir(viz, "animint2-worldbank", open.browser = TRUE)
