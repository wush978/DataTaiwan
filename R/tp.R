tp_get <- function(query, ...) {
  url <- "http://data.taipei/opendata/datalist/apiAccess"
  response <- httr::GET(url, query=query)
  httr::stop_for_status(response)
  .raw <- httr::content(response, as = "raw")
  e <- list(...)
  if ("encoding" %in% names(e)) {
    stringi::stri_encode(.raw, from = e$encoding, to = "UTF-8")
  } else {
    .enc <- stringi::stri_enc_detect(rawToChar(.raw))
    stringi::stri_encode(.raw, from = .enc[[1]]$Encoding[1], to = "UTF-8")
  }
}

tp_format <- function(format) {
  switch(format,
         "table" = "csv",
         format)
}

search_dataset_tp <- function(keyword, ...) {
  query <- list(scope = "datasetMetadataSearch", q = to_utf8(keyword), ...)
  response <- tp_get(query, ...)
  jsonlite::fromJSON(response)
}

list_dataset_tp <- function(id, ...) {
  query <- list(scope = "datasetMetadataSearch", q = sprintf("id:%s", to_utf8(id)), ...)
  response <- tp_get(query, ...)
  .result <- jsonlite::fromJSON(response)
  .result$result$results$resources[[1]]
}

download_data_tp <- function(rid, format, ...) {
  query <- list(scope = "resourceAquire", rid = to_utf8(rid), format = tp_format(format), ...)
  response <- tp_get(query, ...)
  switch(format,
         "table" = {
           read.table(textConnection(response), sep = ",", ...)
         },
         "xml" = response,
         "json" = response,
         "raw" = stop("TODO"),
         stop("Not supported"))
}
