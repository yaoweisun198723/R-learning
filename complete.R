complete <- function(directory, id = 1:332) {
    len = length(id)
    cnt <- numeric(len)    
    j <- 1
    for (i in id) {
        file <- read.csv(paste("D:/", directory, "/", zeropad(i, 3), ".csv", sep = ""))
        cnt[j] <- sum(!apply(is.na(file), 1, any))
        j <- j + 1
    }
    
    
    result <- cbind.data.frame(id = id, nobs = cnt)
    print(result)
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