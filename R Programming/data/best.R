best <- function(state, outcome) {
        options(warn=-1)
        ## Read outcome data
        outcome_complete <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome_complete[, 11] <- as.numeric(outcome_complete[, 11])
        outcome_complete[, 17] <- as.numeric(outcome_complete[, 17])
        outcome_complete[, 23] <- as.numeric(outcome_complete[, 23])
        outcome_ordered <- outcome_complete[order(outcome_complete$Hospital.Name),]
        
        #outcome_simple <- outcome_complete[,c(2,7,11,17,23)]
        
        if (outcome == "heart attack"){
                outcome_simple <- outcome_complete[,c(2,7,11)]
                outcome_cc <- complete.cases(outcome_simple)
                outcome_simple <- outcome_simple[outcome_cc,]
        } else if (outcome == "heart failure"){
                outcome_simple <- outcome_complete[,c(2,7,17)]
                outcome_cc <- complete.cases(outcome_simple)
                outcome_simple <- outcome_simple[outcome_cc,]
        } else if (outcome == "pneumonia"){
                outcome_simple <- outcome_complete[,c(2,7,23)]
                outcome_cc <- complete.cases(outcome_simple)
                outcome_simple <- outcome_simple[outcome_cc,]
        } else {
                options(warn=0)
                stop("invalid outcome")
        }  
        
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        if (state %in% outcome_simple$State == TRUE){
                outcome_st <- subset(outcome_simple, outcome_simple$State == state)
                outcome_st <- outcome_st[order(outcome_st[,3], outcome_st[,1]),]
                options(warn=0)
                return(outcome_st[1,1])
        }
        else {
                options(warn=0)
                stop("invalid state")
        }
        
        
}
