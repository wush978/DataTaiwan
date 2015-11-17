tw_domain <- "data.gov.tw"

.tw_extract_node <- function(url, xpath_check, xpath_target, page, fun) {
  if (page == 0) response <- httr::GET(url) else {
    response <- httr::GET(url, query = list(page = page))
  }
  httr::stop_for_status(response)
  retval.doc <- httr::content(response, as = "parsed")
  stopifnot(length(XML::getNodeSet(retval.doc, xpath_check)) > 0)
  XML::xpathApply(retval.doc, xpath_target, fun)
}

tw_extract_node <- function(url, xpath_check, xpath_target, fun) {
  retval <- list()
  page <- 0
  .result <- try(.tw_extract_node(url, xpath_check, xpath_target, page, fun), silent = TRUE)
  while (class(.result)[1] != "try-error") {
    page <- page + 1
    retval[[page]] <- do.call(rbind, .result)
    .result <- try(.tw_extract_node(url, xpath_check, xpath_target, page, fun), silent = TRUE)
  }
  do.call(rbind, retval)
}


search_dataset_tw <- function(keyword) {
  url <- sprintf("http://%s/search/site/%s", tw_domain, to_utf8(keyword))
  xpath_check <- "//div[contains(@class, 'node-teaser')]"
  xpath_target <- "//div[contains(@class,'node-metadataset')]"
  fun <- function(node) {
    about <- XML::xmlGetAttr(node, "about")
    id <- as.integer(strsplit(about, "/node/", fixed = TRUE)[[1]][2])
    name <- XML::getChildrenStrings(node, asVector = FALSE)$h2
    data.frame(id = id, name = name)
  }
  tw_extract_node(url, xpath_check, xpath_target, fun)
}

list_dataset_tw <- function(id) {
  url <- sprintf("http://%s/node/%d", tw_domain, id)
  response <- httr::GET(url)
  httr::stop_for_status(response)
  retval.doc <- httr::content(response, as = "parsed")
  xpath_check <- xpath_target <- "//table[contains(@class, 'views-table')]//td[contains(@class, 'views-field-field-resource-url-g')]"
  fun <- function(node) {
    tr <- XML::xmlParent(node)
    rid <- sprintf("http://%s%s", tw_domain, XML::xmlAttrs(node[["a"]])[["href"]])
    format <- XML::xmlValue(node[["a"]])
    data.frame(rid = rid, name = XML::xmlValue(tr[[5]]), format = format)
  }
  tw_extract_node(url, xpath_check, xpath_target, fun)
}

download_data_tw <- function(rid, format) {
  response <- httr::GET(as.character(rid))
  httr::stop_for_status(response)
  switch(format,
         "table" = stop("TODO"),
         "json" = stop("TODO"),
         "xml" = stop("TODO"),
         "raw" = httr::content(response, as = "raw"),
         stop("Not supported"))
}
