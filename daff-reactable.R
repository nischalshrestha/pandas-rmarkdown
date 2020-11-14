library(reactable)
library(daff)
library(magrittr)
library(dplyr)

source(here::here("R", "utils.R"))
source(here::here("R", "diffing.R"))

# IDEA: visualize dataframe diffs in {reactable}

# it would be nice to be able to take the patch and render it such that (in order of priority)
# - represent daff as we normally see it in reactable
# - it works for R first
# - it works for python too (work off of csv; MultiIndex might prove difficult)
# - allow user to expand the collapsed rows and cols with basic expandable cols/rows
#   - rows this is fairly simple (just use expandable rows)
#   - cols this is complex, have to extend to behave like the expandable rows
# - we condense table with accordian UI for unaffected rows and columns: https://felixniklas.com/paperfold/

# One tool we can reuse is [`daff`](https://github.com/edwindj/daff), which visualizes data diffs.

# Notes:
# - https://glin.github.io/reactable/articles/sticky-columns.html stickcols might be nice when there are lots of cols

determine_color <- function(value, default = "#fff") {
  if (is.na(value))
    return(default)
  # message(value)
  if (value == "@@") {
    return(modified_col_color)
  } else if (value == "->") {
    return(modified_cell_color)
  } else if (value == "+++") {
    return(inserted_color)
  } else if (value == "---") {
    return(deleted_color)
  } else {
    return(default)
  }
}

determine_styler <- function(change_type) {
  color_styler <- function(value) {
    if (grepl("->", value)) {
      return(list(background = modified_cell_color))
    }
  }
  if (change_type == "+++") {
    color_styler <- function(value) {
      if (grepl("->", value)) {
        return(list(background = modified_cell_color))
      }
      return(list(background = inserted_color))
    }
  } else if (change_type == "---") {
    color_styler <- function(value) {
      if (grepl("->", value)) {
        return(list(background = modified_cell_color))
      }
      return(list(background = deleted_color))
    }
  }
  return(color_styler)
}

daff_reactable <- function(x, y) {
  patch <- diff_data(x, y)
  # get diff matrix
  m <- patch$raw()$data
  # coerce into data.frame
  df <- as.data.frame(m, stringsAsFactors = FALSE, row.names = NULL)
  # store away the schema row
  new_col_names <- df[1, ]
  # get rid of it
  df <- df[-1, ]
  # reset index
  rownames(df) <- NULL
  
  # create new colDefs that have new colDef names
  colDefs <- list()
  column_names <- names(df)
  range <- 1:length(column_names)
  for (i in seq_along(range)) {
    render_name <- new_col_names[[i]]
    color <- determine_color(render_name, default = grey_color)
    color_styler <- determine_styler(render_name)
    headerStyle <- list(background = color)
    colDefs[[column_names[[i]]]] <- reactable::colDef(
      name = render_name,
      headerStyle = headerStyle,
      style = color_styler
    )
  }
  sticky_style <- list(position = "sticky", left = 0, zIndex = 1, background = grey_color,
       borderRight = "1px solid #eee")
  # render table
  reactable::reactable(
    df,
    borderless = TRUE,
    compact = TRUE,
    pagination = FALSE,
    sortable = FALSE,
    bordered = TRUE,
    resizable = TRUE,
    rowStyle = function(index) {
      # style entire rows depending on action column value
      color <- determine_color(df[index, 1])
      fontWeight <- "normal"
      if (index == 1) {
        fontWeight = "bold"
      }
      if (df[index, 1] == "@@") {
        return(list(background = modified_col_color, fontWeight = fontWeight))
      } else if(df[index, 1] == "+++") {
        return(list(background = inserted_color, fontWeight = fontWeight))
      } else if(df[index, 1] == "---") {
        return(list(background = deleted_color, fontWeight = fontWeight))
      }
      return(list(fontWeight = fontWeight))
    },
    columns = reappend(
      list(
        "V1" = reactable::colDef(
          name = "!",
          headerStyle = sticky_style,
          style = function(value) {
            dynamic_sticky <- sticky_style
            dynamic_sticky[["background"]] <- determine_color(value)
            return(dynamic_sticky)
          }
        )
      ),
      colDefs[2:length(colDefs)]
    )
  )
}

y <- mtcars
x <- y

x <- x[-2:-(nrow(x)-3), ] # remove rows
x <- rbind(x, tail(x, 3)) # insert rows
x[1, 1] <- 10 # change a value
x$hello <- "world"  # add a column
x$disp <- NULL # remove a column

daff_reactable(y, x)


# patch <- diff_data(y, x)
# patch
# 
# # summary
# s <- attr(patch, "summary")
# s
# 
# diff_summary_html(s, reactable_output = TRUE)
# 
# daff_html(x, y, include_diff_table = TRUE, reactable_output = TRUE)
