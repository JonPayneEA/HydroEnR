#' @title csvFix
#'
#' @description Data preprocessing for WISKI files, Constrains an irregular csv
#' columns to those of a user specified value. This negates the issue where
#' commented data have commas included.
#'
#' @description Adapted from https://gist.github.com/noamross/4114258
#'
#' @param file File link
#' @param new.name Set as TRUE, you will require to give fixed file a new name
#' @param sep Delimiter used in file
#' @param comment.char Include if comments are differentiated
#' @param rowskip Set as 0, but can offset where the function starts
#' @param expected_col Constrains the csv to a certain number of columns
#' @param edit If you wish to manually change file contents set as TRUE
#'
#' @return
#' @export
#'
#' @examples
#' #csvFix("C:/Users/jpayne05/Downloads/Wellingborough.RE[RainfallEvent].15min.csv.all")
csvFix <- function(file,
                   new.name = TRUE,
                   sep = '|',
                   comment.char = '',
                   rowskip = 0,
                   expected_col = 5,
                   edit = FALSE
                   ) {

  # Read data into temporary data frame
  tmpframe <- read.table(file, sep=sep,
                         quote = '',
                         colClasses = 'character',
                         stringsAsFactors = FALSE,
                         comment.char = '',
                         blank.lines.skip = FALSE,
                         na.strings = '',
                         skip = rowskip)

  # Removes anything after the expected column delim
  tmpframe <- sub(sprintf("^((?:[^,]*,){%d}).*",
                          expected_col -1), "\\1", tmpframe[1:dim(tmpframe)[1],])

  # Open data editor if required by user
  if(edit == TRUE) tmpframe <- edit(tmpframe)

  # Determine whether to overwrite existing document or save as new
  if(is.character(new.name)) {
    out.name <- new.name
  } else if(new.name <- TRUE) {
    out.name <- readline(prompt = 'Enter file name to save (Hit enter to use original):')
  } else {
    out.name <- file
  }

  # Export
  if(out.name == '') out.name <- file
  write.table(tmpframe,
              file = out.name,
              append = FALSE,
              quote = FALSE,
              sep = ',',
              row.names = FALSE,
              col.names = FALSE)
}


