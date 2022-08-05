#' @title metaCollate
#'
#' @description Collate the metadata of numerous WISKI datasets.
#'
#' @param ... WISKI deived datasets of classes; 'flowLoad', 'rrainLoad', and
#' 'stageLoad'
#'
#' @return
#' @export
#'
#' @examples
#' # meta <- metaCollate(brigstock, corby, kingscliffe, oundle, wellingborough, yelden)
metaCollate <- function(...){
  lst <- list(...)
  colClasses = c("character", "character", "numeric", "numeric", "numeric",
                 "numeric", "character", "character", "character", "character",
                 "character", "numeric", "numeric", "numeric", "numeric")
  col.names = c("Area", "Site", "Station_num", "LocalX", "LocalY", "Datum",
                "Param_name", "Param_type", "Type_name", "TS_name", "TS_unit",
                "GlobalX", "GlobalY", "Long", "Lat")
  df <- read.table(text = "",
                   colClasses = colClasses,
                   col.names = col.names)
  for(i in seq_along(lst)){
    df[i, 1] <- lst[[i]]$Metadata[1,2]
    df[i, 2] <- lst[[i]]$Metadata[2,2]
    df[i, 3] <- lst[[i]]$Metadata[3,2]
    df[i, 4] <- lst[[i]]$Metadata[4,2]
    df[i, 5] <- lst[[i]]$Metadata[5,2]
    df[i, 6] <- lst[[i]]$Metadata[6,2]
    df[i, 7] <- lst[[i]]$Metadata[7,2]
    df[i, 8] <- lst[[i]]$Metadata[8,2]
    df[i, 9] <- lst[[i]]$Metadata[9,2]
    df[i, 10] <- lst[[i]]$Metadata[10,2]
    df[i, 11] <- lst[[i]]$Metadata[11,2]
    df[i, 12] <- lst[[i]]$Metadata[12,2]
    df[i, 13] <- lst[[i]]$Metadata[13,2]
    df[i, 14] <- lst[[i]]$Metadata[14,2]
    df[i, 15] <- lst[[i]]$Metadata[15,2]
  }
  return(df)
}
