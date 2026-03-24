# plumber.R

# Load necessary libraries
library(plumber)
library(soilDB)
library(sf)

#* @apiTitle soilDB fetchSDA_spatial API
#* @apiDescription An API wrapper for the soilDB::fetchSDA_spatial function to query spatial data from the Soil Data Access (SDA) service.
#* Powered by soilDB

#* Get Spatial Data from Soil Data Access
#* @description Wraps the `soilDB::fetchSDA_spatial` function. Fetches spatial data based on various soil identifiers.
#* @param x A comma-separated string of identifiers (e.g., mukey, nationalmusym, areasymbol).
#* @param by.col The column name containing the identifier. Defaults to 'mukey'. Common values: 'mukey', 'nationalmusym', 'areasymbol', 'lkey'.
#* @param method Geometry result type. Defaults to 'feature'. Options: 'feature', 'bbox', 'point', 'extent', 'convexhull', 'union', 'collection'.
#* @param geom.src The geometry source table. Defaults to 'mupolygon'. Options: 'mupolygon', 'sapolygon', 'mlrapolygon', 'mupoint', 'muline', 'featpoint', 'featline'.
#* @param db The database source. Defaults to 'SSURGO'. Options: 'SSURGO', 'STATSGO'.
#* @param add.fields A comma-separated string of additional column names from `mapunit` or `legend` tables to add to the result (e.g., 'mapunit.muname').
#* @param chunk.size The number of identifiers to process per query chunk. Defaults to 10.
#* @param format The output format for the geometry. Either 'geojson' (default) or 'wkt'.
#* @serializer geojson
#* @get /fetchSDA_spatial
function(
    req,
    res,
    x,
    by.col = "mukey",
    method = 'feature',
    geom.src = 'mupolygon',
    db = 'SSURGO',
    add.fields = NULL,
    chunk.size = 10,
    format = "geojson"
  ) {


  if (!tolower(format) %in% c("geojson", "wkt")) {
    res$status <- 400 # Bad Request
    return(list(error = "Invalid 'format' parameter. Must be 'geojson' or 'wkt'."))
  }

  x_vec <- strsplit(URLdecode(x), ",")[[1]]
  add.fields_vec <- NULL
  if (!is.null(add.fields)) {
    add.fields_vec <- strsplit(URLdecode(add.fields), ",")[[1]]
  }

  chunk.size <- as.numeric(chunk.size)

  result <- tryCatch({

    fetchSDA_spatial(
      x = x_vec,
      by.col = by.col,
      method = method,
      geom.src = geom.src,
      db = db,
      add.fields = add.fields_vec,
      chunk.size = chunk.size,
      verbose = FALSE
    )

  }, error = function(e) {
    return(e)
  })

  if (inherits(result, "error")) {
    res$status <- 500 # Internal Server Error
    return(list(
      error = "An error occurred while querying Soil Data Access.",
      message = result$message
    ))
  }

  if (is.null(result) || nrow(result) == 0) {
    res$status <- 404 # Not Found
    return(list(message = "No spatial data found for the given identifiers."))
  }

  if (tolower(format) == "wkt") {
    # for WKT explicitly unset the serializer
    wkt_geom <- sf::st_as_text(sf::st_geometry(result))
    result_df <- sf::st_drop_geometry(result)
    result_df$geom_wkt <- wkt_geom
    plumber::forward(serializer = serializer_json(), res = res, result_df)
  } else {
    # for 'geojson', '@serializer geojson' converts the sf object
    return(result)
  }
}
