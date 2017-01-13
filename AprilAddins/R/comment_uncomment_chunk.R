#' Comment out the highlighted code chunk
#' 
#' Call this as an addin to comment out selected rows by inserting a pound # 
#' in front of the row
#'
#' @export
comment_chunk_addin <- function() {

  context <- rstudioapi::getActiveDocumentContext()
  start_row <- context$selection[[1]]$range$start[1]
  end_row <- context$selection[[1]]$range$end[1]
  
  #for (row in start_row:end_row){
  #  rstudioapi::insertText(c(row, 1), '#')
  #}
  pos <- Map(c, start_row:end_row, 1)
  rstudioapi::insertText(pos, "#")
  
}

#' Uncomment the highlighted code chunk
#'
#' Call this as an addin to uncomment the selected rows by deleting the leading 
#' pound #. Selected rows that were originally not commented out will be ignored.
#'
#' @export
uncomment_chunk_addin <- function() {
  
  context <- rstudioapi::getActiveDocumentContext()
  
  start_row <- context$selection[[1]]$range$start[1]
  start_col <- context$selection[[1]]$range$start[2]
  end_row <- context$selection[[1]]$range$end[1]
  row_seq <- start_row:end_row
  col_seq <- c(start_col-1, rep(0, end_row-start_row))
  
  sel_text <- context$selection[[1]]$text
  row_str <- strsplit(sel_text, '\n')
  
  # which one has leading pound
  rltv_idx <- which(substr(trimws(row_str[[1]]), 1, 1) == '#')
  rows <- row_seq[rltv_idx]
  cols <- as.numeric(col_seq+regexpr("#", row_str[[1]]))[rltv_idx]
  
  rng <- Map(c, Map(c, rows, cols), Map(c, rows, cols+1))
  rstudioapi::modifyRange(rng, " ")
 
}
