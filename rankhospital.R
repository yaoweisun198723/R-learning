rankhospital <- function(state, outcome, rank = "best") {
    file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(file[,11] <- as.numeric(file[,11]))
    suppressWarnings(file[,17] <- as.numeric(file[,17]))
    suppressWarnings(file[,23] <- as.numeric(file[,23]))
    title <- names(file)
    idxes <- 1:length(title)
    if (!any(state == file$State)) stop("invalid state")
    switch(outcome, 
           "heart attack"=oc_type<-idxes[title=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"],
           "heart failure"=oc_type<-idxes[title=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"],
           "pneumonia"=oc_type<-idxes[title=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"],
           stop("invalid outcome"))
 #   browser()
    tmp1 <- file[file$State == state & !is.na(file[, oc_type]),]
    tmp2 <- tmp1[, c(2, oc_type)]
#    browser()
    rankings <- tmp2[order(tmp2[2], tmp2[1]),]
    rankings <- cbind(rankings, 1:nrow(tmp2))
    colnames(rankings)[c(2, 3)] <- c("Rate", "Rank")
    if (rank == "best") rank <- 1
    else if (rank == "worst") rank <- nrow(rankings)
    print(rankings[rank, 1])
}