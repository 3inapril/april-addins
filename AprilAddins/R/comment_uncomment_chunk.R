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
  #start_col <- context$selection[[1]]$range$start[2]
  end_row <- context$selection[[1]]$range$end[1]
  #end_col <- context$selection[[1]]$range$end[2]
  
  sel_text <- context$selection[[1]]$text
  row_str <- strsplit(sel_text, '\n')
  
  # which one has leading pond
  idx <- which(substr(trimws(row_str[[1]]), 1, 1) == '#')
  pos <- regexpr("#", row_str[[1]])
  
  for (i in idx){
    
    row <- (start_row:end_row)[i]
    p <- pos[idx]
    rstudioapi::modifyRange(c(row, p), "")
  }
 
}