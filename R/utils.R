
#' Takes in an unconverted pandas.DataFrame via `reticulate` and returns
#' an R data.frame that has the look of a pandas.DataFrame
#'
#' @param pydf
#'
#' @return
#'
#' @examples
python_df <- function(pydf) {
  # TODO: handle repr for GroupBy dataframe
  # TODO: write some tests for this
  # First handle rownames
  original_rownames <- rownames(reticulate::py_to_r(pydf))
  # 0) You need to check if rownames are already numeric or not
  is_numeric_index <- all(!is.na(as.numeric(original_rownames)))
  
  # TODO check if dataframe is using MultiIndex
  if (identical(class(pydf$index)[[1]], "pandas.core.indexes.multi.MultiIndex")) {
    # TODO do we really need to convert to csv? check if we can stop at just reset_index
    rdf <- as.data.frame(
      readr::read_csv(
        as.character(reticulate::py_call(pydf$reset_index)$to_csv(index = FALSE))
      )
    )
    
    # TODO investigate whether we can get away with this for more than 2 levels etc.
    # in the case that we only set one index with something like `set_index('date', append=True)`
    # let's leave it alone, but if we see a column like "level_" let's set the rownames
    if (grepl("level_", colnames(rdf)[[1]])) {
      rownames(rdf) <- rdf[[1]]
      rdf <- rdf[2:length(rdf)]
    }
  } else if (is_numeric_index) {
    # TODO same thing to check here: do we need to conver to csv?
    # 1) First, read it as csv to preserve types (except for NaNs)
    rdf <- as.data.frame(readr::read_csv(as.character(pydf$to_csv(index=FALSE))))
  } else {
    rdf <- reticulate::py_to_r(pydf)
  }
  # TODO format floats to use 6 decimal places
  
  # 2) Turn some data types back to Python representation
  rdf <- rdf %>%
    purrr::map_df(~ ifelse(is.na(.), "NaN", as.character(.))) %>%
    as.data.frame
  # 3) change to 0-indexing for row index
  rownames(rdf) <- as.numeric(rownames(rdf)) - 1
  rdf
}

