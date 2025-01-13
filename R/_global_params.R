# Data sources
quebec_caribou_ranges_url <- "https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Faune/Aires_repartition_caribous/Forestier/SHP/Aire_repartition_populations_caribouForestier_SHP.zip"
ontario_caribou_ranges_url <- "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/CARIBRNG.zip"
statcan_admin_boundaries_url <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b06a_e.zip"
quebec_aqrp_roads_url <- "https://diffusion.mern.gouv.qc.ca/Diffusion/RGQ/Vectoriel/Carte_Topo/Local/AQReseauPlus/ESRI(SHP)/AQreseauPlus_SHP.zip"
ontario_mnr_roads_url <- "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/MNRRDSEG.zip"
ontario_orn_roads_url <- "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/ORNELEM.zip"
global_buildings_datalinks_url <- "https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv"


# Projection parameters: epsg.io/3979
study_proj <- 3979

# Study ggplot2 custom theme
theme_caribou <- ggplot2::theme_bw() +
  ggplot2::theme(
    title = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 12),
    panel.grid.minor = ggplot2::element_blank()
  )