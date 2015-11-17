get_source <- function(src) {
  switch(tolower(src),
         "taiwan" = "tw",
         "taipei" = "tp",
         src)
}

#'@title Search a Dataset
#'@param keyword string. The keyword to search.
#'@param src string. The name of the source platform. Possible values are one of:
#'  \code{c("Taiwan", "Taipei")} or the abbreviation: \code{c("tw", "tp")}.
#'@param ... Additional arguments which will be passed to the specified source platform.
#'@export
#'@examples
#'\dontrun{
#'
#'  search_dataset("power", src = "Taiwan")
#'
#'  search_dataset("youbike", src = "Taipei", limit = 5, offset = 0)
#'}
search_dataset <- function(keyword, src = "Taiwan", ...) {
  src_abb <- get_source(src[1])
  get(sprintf("search_dataset_%s", src_abb))(keyword, ...)
}

#'@title List the Data in the Dataset
#'@param id string. The id of the dataset.
#'@param src string. The name of the source platform. Possible values are one of:
#'  \code{c("Taiwan", "Taipei")} or the abbreviation: \code{c("tw", "tp")}.
#'@param ... Additional arguments which will be passed to the specified source platform.
#'@export
#'@examples
#'\dontrun{
#'  list_dataset(19813, "Taiwan")
#'
#'  list_dataset("8ef1626a-892a-4218-8344-f7ac46e1aa48", "Taipei")
#'}
list_dataset <- function(id, src, ...) {
  src_abb <- get_source(src[1])
  get(sprintf("list_dataset_%s", src_abb))(id, ...)
}

#'@title Download the Data from the Source Platform.
#'@param rid string. The id of the data.
#'@param src string. The name of the source platform. Possible values are:
#'  \code{c("Taiwan", "Taipei")} or the abbreviation: \code{c("tw", "tp")}.
#'@param format string. The format of returned object. Possible values are:
#'  \code{c("table", "json", "xml", "raw")}.
#'@param ... Additional arguments which will be passed to the specified source platform.
#'@export
#'@examples
#'\dontrun{
#'  download_data("55ec6d6e-dc5c-4268-a725-d04cc262172b", "Taipei", format = "table", encoding = "BIG-5", header = TRUE)
#'}
download_data <- function(rid, src, format = "table", ...) {
  src_abb <- get_source(src[1])
  get(sprintf("download_data_%s", src_abb))(rid, format, ...)
}
