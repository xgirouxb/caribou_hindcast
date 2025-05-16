# Data sources

# Québec caribou range polygons
# Metadata available at: https://www.donneesquebec.ca/recherche/dataset/aires-de-repartition-des-populations-de-caribous-forestier
quebec_caribou_ranges_url <- "https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Faune/Aires_repartition_caribous/Forestier/SHP/Aire_repartition_populations_caribouForestier_SHP.zip"

# Ontario caribou range polygons
# Metadata available at: https://geohub.lio.gov.on.ca/datasets/lio::caribou-range-boundary/about
ontario_caribou_ranges_url <- "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/CARIBRNG.zip"

# Statistics Canada administrative boundaries
# Metadata available at: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index-eng.cfm 
statcan_admin_boundaries_url <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b06a_e.zip"

# Québec AQRP roads dataset
# Metadata available at: https://www.donneesquebec.ca/recherche/dataset/adresses-quebec
quebec_aqrp_roads_url <- "https://diffusion.mern.gouv.qc.ca/Diffusion/RGQ/Vectoriel/Carte_Topo/Local/AQReseauPlus/ESRI(SHP)/AQreseauPlus_SHP.zip"

# Ontario MNR roads dataset
# Metadata available at: https://geohub.lio.gov.on.ca/datasets/lio::mnr-road-segments/about
ontario_mnr_roads_url <- "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/MNRRDSEG.zip"

# Ontario ORN roads dataset
# Metadata available at: https://geohub.lio.gov.on.ca/datasets/mnrf::ontario-road-network-orn-road-net-element/about
ontario_orn_roads_url <- "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/ORNELEM.zip"

# Microsoft's global building footprint dataset
# Metadata available at: https://planetarycomputer.microsoft.com/dataset/ms-buildings
global_buildings_datalinks_url <- "https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv"

# Unpaved road construction years estimated from CanLaD data products
# CanLaD 1985 - 2020 metadata available at: https://doi.org/10.23687/add1346b-f632-4eb9-a83d-a662b38655ad
# CanLaD 1965 - 1984 method: https://doi.org/10.1002/ecs2.4956
# Year extraction method: "js/extract_canlad_disturbance_years.js"
canlad_road_construction_years <- "data/canlad_road_construction_years/canlad_road_construction_years.csv"