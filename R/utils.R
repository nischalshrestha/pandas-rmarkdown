
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
  if ("pandas.core.indexes.multi.MultiIndex" %in% class(pydf$index)) {
    rdf <- as.data.frame(
      readr::read_csv(
        as.character(reticulate::py_call(pydf$reset_index)$to_csv(index = FALSE))
      )
    )
    # FIXME this is currently failing for when the multiindex have string names for the index
    
    # TODO investigate whether we can get away with this for more than 2 levels etc.
    # in the case that we only set one index with something like `set_index('date', append=True)`
    # let's leave it alone, but if we see a column like "level_" let's set the rownames
    if (grepl("level_", colnames(rdf)[[1]])) {
      rownames(rdf) <- rdf[[1]]
      rdf <- rdf[2:length(rdf)]
    }
  } else if (is_numeric_index) {
    # 1) First, read it as csv to preserve types (except for NaNs)
    rdf <- as.data.frame(readr::read_csv(as.character(pydf$to_csv(index=FALSE))))
  } else {
    rdf <- reticulate::py_to_r(pydf)
  }
  # 2) Turn some data types back to Python representation
  rdf <- rdf %>%
    purrr::map_df(~ ifelse(is.na(.), "NaN", as.character(.))) %>%
    as.data.frame
  # 3) change to 0-indexing for row index
  rownames(rdf) <- as.numeric(rownames(rdf)) - 1
  rdf
}

# TODO test this 
group_columns <- function(columns, start = 2) {
  colDefs <- list()
  start <- start + 1
  range <- start:length(columns)
  # make first column be as is
  colDefs[[columns[1]]] <- reactable::colDef(name = columns[1])
  # make
  for (i in range) {
    colDefs[[columns[i]]] <- reactable::colDef(
      name = " "
    )
  }
  colGroups <- list()
  colGroups <-
    lapply(
      columns[start:length(columns)],
      function(column) {
        reactable::colGroup(name = column, columns = c(column))
      }
    )
  list(colDefs, colGroups)
}

# TODO this is now testable so test this with multiple examples
# the options is just used for knitr context and could probably be NULL by default
format_python_df <- function(options = NULL, raw_result, do_knit_print = FALSE) {
  is_multi_index <- "pandas.core.indexes.multi.MultiIndex" %in% class(raw_result$index)
  
  # wizard of oz pandas dataframe by changing index as well
  converted_result <- python_df(raw_result)
  # debug_print(options$debug, "prepped a dataframe")
  
  # handle reactable for df with MultiIndex
  if (isTRUE(is_multi_index)) {
    cat('multiindex\n')
    # handle the case where we might already have renamed axis for Index
    # in this case, we should not show index, and group one more
    show_index <- !identical(rdf[[1]], rownames(rdf))
    grp_cols <- group_columns(
      colnames(converted_result),
      start = ifelse(show_index, 1, 2)
    )
    table <- reactable::reactable(
      converted_result,
      compact = TRUE,
      rownames = show_index,
      columns = grp_cols[[1]],
      columnGroups = grp_cols[[2]]
    )
    if (do_knit_print) {
      return(htmltools::knit_print.shiny.tag.list(table))
    }
    return(table)
  } else {
    table <- reactable::reactable(
      converted_result,
      pagination = TRUE,
      minRows = 10,
      compact = TRUE,
      # height = 300,
      highlight = TRUE,
      bordered = TRUE,
      rownames = TRUE,
      resizable = TRUE,
      theme = reactable::reactableTheme(
        borderWidth = "2px"
      ),
      defaultColDef = reactable::colDef(
        cell = function(value, index) {
          # turn NULL into NaN
          if (is.na(value) || is.null(value) || value == "") {
            return(as.character("NaN"))
          }
          return(value)
        }
      ),
      columns = list(
        .rownames = reactable::colDef(
          style = list(
            textAlign = "left"
          )
        )
      )
    )
    if (do_knit_print) {
      return(htmltools::knit_print.shiny.tag.list(table))
    }
    return(table)
  }
}

#' Takes a number of lists and appends them into a single list.
#' Instead of doing nested append calls, this will automate that
#' by doing the series of append calls for you.
#'
#' @param ...
#'
#' @return named list
#'
#' @examples
reappend <- function(...) {
  myList <- list()
  lists <- list(...)
  range <- 1:length(lists)
  # # Now the new experiments
  for (i in seq_along(range)) {
    myList <- append(myList, lists[[i]])
  }
  myList
}

