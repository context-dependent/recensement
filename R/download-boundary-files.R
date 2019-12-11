package_root <- function() {

  res <- find.package("recensement")

  if(!dir.exists(file.path(res, "cache"))) {
    dir.create(file.path(res, "cache", "data", "2016"), recursive = TRUE)
    dir.create(file.path(res, "cache", "shp"), recursive = TRUE)
  }

  res

}




download_2016_census_profile <- function(dguid = "", topic = 0, browse = FALSE) {

  cat(paste0(dguid, "\n"))

  if(browse) browser()

  root_url <- "https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.json"


  req <- httr::GET(
    url = root_url
    , query = list(
      topic = topic,
      dguid = dguid
    )
  )

  raw <- httr::content(req, as = "text")

  raw <- stringr::str_remove(raw, "^//")

  raw <- jsonlite::fromJSON(raw)

  res <- as.data.frame(raw$DATA)

  names(res) <- tolower(raw$COLUMNS)

  res <- tibble::as_tibble(res)

  res

}

get_2016_census_profile <- function(dguid = "", topic = 8, browse = FALSE) {

  dat_file_path <- file.path(package_root(), "cache", "data", "2016", dguid, paste0(topic, ".rds"))

  dat_dir_path <- file.path(package_root(), "cache", "data", "2016", dguid)

  if(!file.exists(dat_file_path)) {

    if(!dir.exists(dat_dir_path)) {
      dir.create(dat_dir_path, recursive = TRUE)
    }

    cat(paste0(dat_file_path, " does not exist; initiating download"))

    res <- download_2016_census_profile(dguid = dguid, topic = topic, browse = browse)

    saveRDS(res, dat_file_path)

  } else {
    res <- readRDS(dat_file_path)
  }


  res
}


get_2016_census_geographies <- function(geos = "CSD", cpt = "00", browse = FALSE) {


  if(browse) browser()

  root_url <- "https://www12.statcan.gc.ca/rest/census-recensement/CR2016Geo.json"


  req <- httr::GET(
    url = root_url,
    query = list(
      geos = geos,
      cpt = cpt
    )
  )


  raw <- httr::content(req, as = "text")

  raw <- stringr::str_remove(raw, "^//")

  raw <- jsonlite::fromJSON(raw)

  res <- as.data.frame(raw$DATA, stringsAsFactors = FALSE)

  names(res) <- tolower(raw$COLUMNS)

  res <- tibble::as_tibble(res)

  res

}


census_geo_level_names = list(
  "CD"      = "Census divisions",
  "CMACA"   = "Census metropolitan areas and census agglomerations",
  "CSD"     = "Census subdivisions (municipalities)",
  "CT"      = "Census tracts",
  "DPL"     = "Designated places",
  "ER"      = "Economic regions",
  "FED"     = "Federal electoral districts (2013 Representation Order)",
  "FSA"     = "Forward sortation areas",
  "HR"      = "Health regions (including LHINs and PHUs)",
  "POPCNTR" = "Population centres",
  "PR"      = "Canada, provinces and territories"
)

census_geo_level_prefixes = list(
  "CD"      = "lcd_",
  "CMACA"   = "lcma",
  "CSD"     = "lcsd",
  "CT"      = "lct_",
  "DPL"     = "ldpl",
  "ER"      = "ler_",
  "FED"     = "lfed",
  "FSA"     = "lfsa",
  "HR"      = "lhr_",
  "POPCNTR" = "lpc_",
  "PR"      = "lpr_",
  "DA"      = "lda_"
)


download_boundaries <- function(geos = "CSD", year = 2016, browse = FALSE) {


  if(browse) browser()
  # For production
  package_root <- find.package("recensement")

  tfile <- tempfile()
  tdir <- tempdir()

  file_prefix <- census_geo_level_prefixes[[geos]]

  geo_url <- paste0("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/", file_prefix, "000b16a_e.zip")
  download.file(geo_url, tfile)

  unzip(tfile, exdir = tdir)

  shape_file <- list.files(tdir, pattern = "shp", full.names = TRUE)

  shp <- sf::st_read(shape_file) %>% sf::st_transform(4326)

  names(shp) <- tolower(names(shp))

  cache_dir = file.path(package_root(), "cache", "shp", year)

  if(!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  unlink(tfile)
  unlink(tdir)

  saveRDS(shp, file.path(package_root(), "cache", "shp", year, paste0(tolower(geos), ".rds")))
}

get_boundaries <- function(geos = "CSD", year = 2016, browse = FALSE) {

  if(browse) browser()

  geo_file_path <- file.path(package_root(), "cache", "shp", year, paste0(tolower(geos), ".rds"))

  if(!file.exists(geo_file_path)) {

    cat(paste0(geo_file_path, " does not exist; initiating download\n"))

    download_boundaries(geos = geos, year = year)

  }

  res <- readRDS(geo_file_path)

  res
}
