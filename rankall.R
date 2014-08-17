rankall<- function(outcome, rank = "best") {
    file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(file[,11] <- as.numeric(file[,11]))
    suppressWarnings(file[,17] <- as.numeric(file[,17]))
    suppressWarnings(file[,23] <- as.numeric(file[,23]))
    title <- names(file)
    idxes <- 1:length(title)
#    if (!any(state == file$State)) stop("invalid state")
    
    switch(outcome, 
           "heart attack"=oc_type<-idxes[title=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"],
           "heart failure"=oc_type<-idxes[title=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"],
           "pneumonia"=oc_type<-idxes[title=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"],
           stop("invalid outcome"))
    
    if (rank == "best") rank <- 1
    else if (rank == "worst") {}
    else if (!is.numeric(rank)) stop("rank must be \"best\", \"worst\" or a number")
#    browser()
    allstates <- file$State[!duplicated(file$State )]
    len <- length(allstates)
    target_hospital <- character(len)
    
    for (i in 1:len) {
        tmp1 <- file[file$State == allstates[i] & !is.na(file[, oc_type]),]
        tmp2 <- tmp1[, c(2, oc_type)]
#    browser()
        rankings <- tmp2[order(tmp2[2], tmp2[1]),]
        rankings <- cbind(rankings, 1:nrow(tmp2))
        if (rank == "worst") rank <- nrow(rankings)
        target_hospital[i] <- rankings[rank, 1]
    }
    result <- data.frame(cbind(target_hospital, allstates))
    colnames(result) <- c("hospital", "state")
    result
}