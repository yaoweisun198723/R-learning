best <- function(state, outcome) {
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
    result <- tmp2[tmp2[,2] == min(tmp2[, 2]), 1]    
    print(result)
    
}