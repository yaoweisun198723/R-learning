source("complete.R")
corr <- function(directory, threshold = 0) {
    each_obs <- complete(directory, id = 1:332)
    gtthresh_ids <- each_obs[each_obs$nobs>threshold, "id"]
    len <- length(gtthresh_ids)
    result <- numeric(len)
#    browser()
    if (len > 0) {
        for (i in 1:len) {
            file <- read.csv(paste("D:/", directory, "/", zeropad(gtthresh_ids[i], 3), ".csv", sep = ""))
            result[i] <- cor(file$sulfate, file$nitrate, use = "complete.obs")
        }
    }
    result
}