# Data preprocess
# Adapted from https://gist.github.com/noamross/4114258
# Constrains an irregular csv columns to those of a user specified value
# Fixes uncommented tags from H&T with additional commas

csvFix <- function(file,
                   new.name=TRUE,
                   sep = '|',
                   comment.char = '',
                   rowskip = 0,
                   expected_row = 5,
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
                          expected_row -1), "\\1", z[1:dim(tmpframe)[1],])

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

csvFix("C:/Users/jpayne05/Downloads/Wellingborough.RE[RainfallEvent].15min.csv.all")

