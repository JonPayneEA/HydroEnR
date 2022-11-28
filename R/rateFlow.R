#' @title rateFlow - convert stage data to flow
#'
#' @description The rateFlow function uses Q = A *(h-B)^C to convert stage data to flow
#'
#' @param data
#' @param rating
#' @param start
#'
#' @return
#' @export
#'
#' @examples
#'
#' ratings <- data.frame(C = c(20.536, 10.077, 2.2243, 6.67),
#' A = c(2.032161, 0.432618, 0.290283, 0.026064),
#' B = c(68, 68, 68, 68),
#' # min = c(0, 0.352, 2.076, 3),
#' max = c(68.863, 68.950, 69.720, 69.728))
#'
#' library(readr)
#' obs <- read_csv("C:/Users/jpayne05/Desktop/Datasets/Ratings/Example_rating_data.csv")
#' test <- rateFlow(data = obs$Stage, rating = ratings)
#' plot(test$flow~test$stage,
#'      type = 'l',
#'      xlab = 'Stage (mAOD)',
#'      ylab = 'Rated flow (m3 s-1)',
#'      main = 'Adapted rating methodology from QH3',
#'      lwd = 2)
#' for(i in seq_along(ratings$max)){
#'     abline(v = ratings$max[i],
#'            lty = 2,
#'            col = 'red')
#' }
rateFlow <- function(data, rating, start = 0){
  rate_tbl <- data.frame(row = seq_along(rating[,1]),
                        # limb = paste0('Limb_', seq_along(ratings[,1])),
                        rating)
  df <- cut(data, breaks = c(start, rate_tbl$max), labels = rate_tbl$row)
  total <- max(rate_tbl$row)
  limbs_ext <- replace(df, is.na(df), total)
  # df_ext <-
  # limb <-
  # info <- ifelse()
  flow <- c()
  for(i in seq_along(data)){
    A <- rate_tbl$A[limbs_ext[i]]
    B <- rate_tbl$B[limbs_ext[i]]
    C <- rate_tbl$C[limbs_ext[i]]
    # print(A * (data[i] - B)^C)
    est <- A*(data[i] - B)^C
    # print(est)
    flow[i] <- est
  }
  df_all <- data.frame(stage = data, flow = flow, limb = limbs_ext)
  return(df_all)
}

test <- rateFlow(data = obs$Stage, rating = ratings)
plot(test$flow~test$stage,
     type = 'l',
     xlab = 'Stage (mAOD)',
     ylab = 'Rated flow (m3 s-1)',
     main = 'Adapted rating methodology from QH3',
     lwd = 2)
for(i in seq_along(ratings$max)){
  abline(v = ratings$max[i],
  lty = 2,
  col = 'red')
}


# cut(obs$Stage, breaks = c(0, ratings$max), labels = 1:4)


