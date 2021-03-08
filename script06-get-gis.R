library(ggmaptile)

dir.create(file.path("output", "tiles"), FALSE)

mt_map_extent_data(
  xmin = -120, xmax = -50, ymin = 20, ymax = 45, zoom = 5,
  url = "http://tile.stamen.com/toner-lite/%d/%d/%d.png",
  cache_dir = file.path("output", "tiles")
)

mt_map_extent_data(
  xmin = -120, xmax = -50, ymin = 20, ymax = 45, zoom = 6,
  url = "http://tile.stamen.com/toner-lite/%d/%d/%d.png",
  cache_dir = file.path("output", "tiles")
)

mt_map_extent_data(
  xmin = -120, xmax = -50, ymin = 20, ymax = 45, zoom = 7,
  url = "http://tile.stamen.com/toner-lite/%d/%d/%d.png",
  cache_dir = file.path("output", "tiles")
)

mt_map_extent_data(
  xmin = -120, xmax = -50, ymin = 20, ymax = 45, zoom = 8,
  url = "http://tile.stamen.com/toner-lite/%d/%d/%d.png",
  cache_dir = file.path("output", "tiles")
)
