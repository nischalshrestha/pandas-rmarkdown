library(tidyverse)
library(dplyr)
library(magrittr)
library(kableExtra)
library(reticulate)

source(here::here("R/utils.R"))

kable_pandas <- function(pydf_name = "df") {
  # grab the python df
  # note how we use py_eval instead of using py$df since we want to retain the python
  # structure still to check things like MultiIndex
  pydf <- reticulate::py_eval(pydf_name)
  # regular df case
  # pydf <- reticulate::py_eval("pd.DataFrame(np.random.randn(8, 4), columns=['a','b','c','d'])")
  
  # WoZ begins
  # setup an R dataframe based on `pydf`
  rdf <- python_df(pydf)
  # regular df case
  # rdf <- py$df 
  
  # grab the regular columns not part of the Index
  columns <- pydf$columns$values
  # then grab the Index columns
  rdf_column_names <- colnames(rdf)
  row_index_cols <- rdf_column_names[!(rdf_column_names %in% columns)]
  
  # this is the amount of space for the Index
  idx_column_space <- sum(!(colnames(rdf) %in% columns))
  # this is the space for the rest of columns
  column_space <- rep(1, length(columns))
  names(column_space) <- columns
  
  # base kbl
  setup_kbl <- rbind(rdf, rdf) %>% 
    # this retains color for the row Index columns
    rename_with(function(x) cell_spec(x, "html", color = "black"), any_of(row_index_cols)) %>% 
    kableExtra::kbl(align = "l", escape = F, row.names = T) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover"), full_width = F, position = "left")
  
  final_kbl <- setup_kbl
  # check if it is a MultiIndex
  if (class(pydf$index)[[1]] == "pandas.core.indexes.multi.MultiIndex") {
    # TODO a check to see if we have many more levels in which case we have to stack
    # the column levels and sometimes above the row indices
    
    # if so, we "raise" the regular columns by making the columns invisible 
    # and moving them to a header above
    final_kbl <- 
      setup_kbl %>% 
      kableExtra::row_spec(0, color = "white") %>%
      kableExtra::add_header_above(c(" " = idx_column_space + 1, column_space), align = "c", bold = T)
  }
  
  final_kbl
}


# this right assign may look weird, but actually it's great in cases when  
# indentation matters, such as python code string since we can properly
# copy paste almost as is
"
import pandas as pd
import numpy as np

# create pd.MultiIndex
arrays = [np.array(['bar', 'bar', 'baz', 'baz', 'foo', 'foo', 'qux', 'qux']),
         np.array(['one', 'two', 'one', 'two', 'one', 'two', 'one', 'two'])]
tuples = list(zip(*arrays))
index = pd.MultiIndex.from_tuples(tuples, names=['first', 'second'])

# make a df with its Index as the MultiIndex
df = pd.DataFrame(np.random.randn(8, 4), columns=['a','b','c','d'])
df_multi = pd.DataFrame(np.random.randn(8, 4), index = index, columns=['a','b','c','d'])

" -> setup_code

# run setup code
reticulate::py_run_string(setup_code)

# test cases
kable_pandas("df")
# kable_pandas("df_multi")


# TODO: use real life examples 
# could replicate this
# https://towardsdatascience.com/how-to-use-multiindex-in-pandas-to-level-up-your-analysis-aeac7f451fce



