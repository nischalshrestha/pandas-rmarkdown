
#' Takes in an unconverted pandas.DataFrame via `reticulate::py_eval` and returns
#' an R data.frame that has the look of a pandas.DataFrame
#'
#' @param pydf
#'
#' @return
#'
#' @examples
python_df <- function(pydf) {
  # if dataframe has a MultiIndex, reset index to turn them into regular columns
  # the first element is the Python class of the object
  if (identical(class(pydf$index)[[1]], "pandas.core.indexes.multi.MultiIndex")) {
    rdf <- pydf$reset_index()
  } else {
    rdf <- reticulate::py_to_r(pydf)
  }

  # some formatting
  # - leave date as is
  # - reduce decimal places for numerics
  # - missing values -> NaN
  rdf <- rdf %>%
    mutate(across(where(lubridate::is.POSIXct), as.character)) %>% 
    mutate(across(where(is.numeric), ~ as.numeric(formattable::digits(.x, 8)))) %>% 
    mutate(across(everything(), ~ ifelse(is.na(.x), "NaN", .x)))
  # 0-indexing for row index hehe
  rownames(rdf) <- as.numeric(rownames(rdf)) - 1
  rdf
}

