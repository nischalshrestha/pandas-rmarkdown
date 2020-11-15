library(tidyverse)
library(dplyr)
library(magrittr)
library(kableExtra)
library(reticulate)

source(here::here("R/utils.R"))

#' Takes in an unconverted Python dataframe and outputs a kableExtra table.
#'
#' @param df The unocvertend Python dataframe
#' @param rmd This is a flag to render in the RStudio IDE for TRUE, FALSE in a rendered document
#'
#' @return A character vector of the table source code (very similar to `kable`)
#' @export
#'
#' @examples
kable_pandas <- function(df, rmd = FALSE) {
  is_multi_index <- class(df$index)[[1]] == "pandas.core.indexes.multi.MultiIndex"
  
  # to curb performance costs, let's slice
  if (df$shape[[1]] > 30 && is_multi_index) {
    # unfortunately, we don't seem to have the ability to use slices 
    # cleanly with `reticulate` yet, this hack assumes your dataframe with be 
    # named `df` ... yikes! but works!
    df <- reticulate::py_eval("df.loc[slice(None, ), ].iloc[:9,]")
  } else if (df$shape[[1]] > 30) {
    df <- df$head(30)
  }
  
  # setup an R dataframe based on `df`
  rdf <- python_df(df)
  
  # grab the regular columns not part of the Index
  columns <- df$columns$values
  # then grab the Index columns
  rdf_column_names <- colnames(rdf)
  row_index_cols <- rdf_column_names[!(rdf_column_names %in% columns)]
  
  # this is the amount of space for the Index columns for grouping
  idx_column_space <- sum(!(colnames(rdf) %in% columns))
  if (is_multi_index) idx_column_space + 1
  # this is the space for the rest of columns for grouping
  column_space <- rep(1, length(columns))
  names(column_space) <- columns
  
  # base kbl
  setup_kbl <- rdf %>% 
    # this retains color for the row Index columns
    dplyr::rename_with(function(x) kableExtra::cell_spec(x, "html", color = "black"), dplyr::any_of(row_index_cols)) %>% 
    kableExtra::kbl(align = "l", escape = F, row.names = !is_multi_index)
  
  if (rmd) {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_paper(full_width = T) %>%
      kableExtra::kable_styling(bootstrap_options = c("hover", "responsive"), position = "center")
  } else {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_styling(bootstrap_options = c("hover", "responsive"), position = "center")
  }
  
  final_kbl <- setup_kbl
  # check if it is a MultiIndex, `reticulate` will list other classes but the very first
  # one is the Python class
  if (is_multi_index) {
    # TODO a check to see if we have many more levels in which case we have to stack
    # the column levels and sometimes above the row indices. In such a case, we do 
    # not "raise" the columns and can skip coloring rows white
    
    # if so, we "raise" the regular columns by making the columns invisible 
    # and moving them to a header above
    final_kbl <- 
      setup_kbl %>% 
      kableExtra::row_spec(0, color = "white") %>%
      kableExtra::add_header_above(c(" " = idx_column_space, column_space), align = "c", bold = T, line = F) %>% 
      kableExtra::collapse_rows(columns = 1, valign = "top")
  }
  
  final_kbl
}

# # this right assign may look weird, but actually it's great in cases when  
# # indentation matters, such as python code string since we can properly
# # copy paste almost as is
# "
# import pandas as pd
# import numpy as np
# 
# # create pd.MultiIndex
# arrays = [np.array(['bar', 'bar', 'baz', 'baz', 'foo', 'foo', 'qux', 'qux']),
#          np.array(['one', 'two', 'one', 'two', 'one', 'two', 'one', 'two'])]
# tuples = list(zip(*arrays))
# index = pd.MultiIndex.from_tuples(tuples, names=['first', 'second'])
# 
# # make a df with its Index as the MultiIndex
# df = pd.DataFrame(np.random.randn(8, 4), columns=['a','b','c','d'])
# df_multi = pd.DataFrame(np.random.randn(8, 4), index = index, columns=['a','b','c','d'])
# 
# " -> setup_code
# 
# # run setup code
# reticulate::py_run_string(setup_code)
# 
# # test cases
# kable_pandas("df")
# kable_pandas("df_multi")


# TODO: use real life examples 
# could replicate this
# https://towardsdatascience.com/how-to-use-multiindex-in-pandas-to-level-up-your-analysis-aeac7f451fce

# collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
#                                C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
#                                C3 = 1:15,
#                                C4 = sample(c(0,1), 15, replace = TRUE))
# 
# kbl(collapse_rows_dt, align = "c", row.names = F) %>%
#   # kable_paper(full_width = F) %>%
#   column_spec(1, bold = T) %>%
#   kable_styling(bootstrap_options = c("hover", "condensed"), full_width = F, position = "center") %>%
#   collapse_rows(columns = 1:2, valign = "top")




