# Projection parameters: epsg.io/3979
study_proj <- 3979

# Radius for circular neighbourhood statistics (in metres)
radius <- 1000

# Tiling parameters for large computations
nx <- 20
ny <- 20

# Study years
time_steps <- seq(1985, 2020, 5)

# Study ggplot2 custom theme
theme_caribou <- ggplot2::theme_bw() +
  ggplot2::theme(
    title = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 12),
    panel.grid.minor = ggplot2::element_blank()
  )