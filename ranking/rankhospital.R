## function which converts values of columns in a dataframe 
## to numeric by a given vector of names and hides warnings
toNumeric <- function(data, paths){
        for(path in paths){
                data[, path] <- 
                        suppressWarnings(as.numeric(data[, path]))
        }
}

## rank hospital
rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        measures <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
        ## Set paths
        heart_attack <-
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        heart_failure <-
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        pneumonia <-
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        
        ## Paths into a vector
        outcomes <- c(heart_attack, heart_failure, pneumonia)
        
        ## Convert to numeric 
        toNumeric(measures, outcomes)
        
        ## Set vector of outcome names
        names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% measures$State) {
                stop("invalid state")
        } else if (is.na(outcomes[as.character(outcome)])) {
                stop("invalid outcome")
        } else {
                ## get subset by state
                state_hospitals <- measures[measures$State == state, ]
                
                ## Set a proper column number to grab data
                number <- which(colnames(state_hospitals) == 
                                        as.character(outcomes[outcome]))
                
                ## Set a proper column to grab data
                out_numbers <- suppressWarnings(as.numeric(
                        state_hospitals[, number]))
                
                ## Set the maximum possible amount of hospitals
                size <- dim(state_hospitals[!is.na(out_numbers), ])[1]
                
                ## Find a hospital by rank
                if (num == "worst") {
                        rank <- state_hospitals[, 2]
                        [order(out_numbers, state_hospitals[, 2])[size]]
                } else if (num == "best")
                {
                        rank <- state_hospitals[, 2]
                        [order(out_numbers, state_hospitals[, 2])[1]]
                }
                else if (num > size) {
                        rank <- NA
                } else {
                        rank <- state_hospitals[, 2]
                        [order(out_numbers, state_hospitals[, 2])[num]]
                }
        }
                
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        return(rank)
}