
library(ggplot2)
library(tmap)


plot_tmap <- function(data, var = 'value_rel', color_direction = 1, filename_postfix = '') {
  
  ## palette for fill color
  if (color_direction == -1) {
    pal <- '-RdYlBu'
  } else {
    pal <- 'RdYlBu'
  }
  
  data.sf  <- data %>%
    merge(x = shp, 
          y = ., 
          by.x = "iso_a3", by.y = "iso3",  
          all.x = T)
  
  max <- max(data.sf[var], na.rm = T); max # 0.025
  min <- min(data.sf[var], na.rm = T); min # 0.000001094
  
  p <- tm_shape(data.sf) +
    tm_fill(col = var, style  = "quantile", n = 10, textNA = 'NA', 
            palette = pal,
            colorNA = 'gray90') +
    tm_borders(col = "grey", lwd = 0.1, lty = "solid", alpha = 0.99) +
    tm_layout(frame = F, frame.lwd = 0.1,
              legend.position = c(0,0),
              legend.title.size = 0.9, legend.text.size  = 0.7,
              legend.width = -0.5, legend.height = -0.5, outer.margins=0, inner.margins=0,
              panel.show = F)
  
  ## save plot
  fname <- paste0(dir.fig, 'map_', filename_postfix, '_', var, '.png'); 
  print(fname)
  tmap_save(tm = p, filename = fname, width=7, height=2.8, units="in", dpi = 200)
  
}



## plot US map ===========================================================================
net_migration <- get_estimates(geography = "county",
                               variables = "RNETMIG",
                               year = 2022,
                               geometry = TRUE,
                               resolution = "20m") %>%
  shift_geometry()

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2022 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")
