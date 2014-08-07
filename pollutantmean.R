pollutantmean <- function(directory, pollutant, id = 1:332) {
    val_sum <- numeric(max(id))
    cnt_sum <- numeric(max(id))
    for (i in id) {
       # print(i)
        file <- read.csv(paste("D:/", directory, "/", zeropad(i, 3), ".csv", sep = ""))
        val_sum[i] <- sum(file[[pollutant]], na.rm = T)
        cnt_sum[i] <- sum(!is.na(file[[pollutant]]))
    }
    mean <- sum(val_sum) / sum(cnt_sum)
    print(mean)
}
zeropad <- function(number, len) {
    x <- number
    num_len <- 0
    while(x >0) {
        x <- x %/% 10
        num_len <- num_len + 1
    }
    if (num_len >= len) number
    else {
        zero <- paste(rep("0", len - num_len), collapse = "")
        paste(zero, number, sep = "")
    }
}