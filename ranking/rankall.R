## function which converts values of columns in a dataframe 
## to numeric by a given vector of names and hides warnings
toNumeric <- function(data, paths){
        for(path in paths){
                data[, path] <- 
                        suppressWarnings(as.numeric(data[, path]))
        }
}

## rank hospital
rankall <- function(outcome, num = "best") {
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
        
        ## Get all the states sorted by alphabetical order
        states <- sort(unique(measures$State))
        
        ## Create an empty vector
        hospitals <- rep("", length(states))
        
        ## Check that outcome is valid
        if (is.na(outcomes[as.character(outcome)])) {
                stop("invalid outcome")
        } else {
                for(i in 1:length(states)) {
                        state_hospitals <- measures[measures[, 7] == 
                                                            states[i], ]
                        
                        ## Set a proper column number to grab data
                        number <- which(colnames(state_hospitals) == 
                                                as.character(outcomes[outcome]))
                        
                        out_numbers <- suppressWarnings(as.numeric(
                                state_hospitals[, number]))
                        
                        ## Set the maximum possible amount of hospitals
                        size <- dim(state_hospitals[!is.na(out_numbers), ])[1]
                        
                        ## Find a hospital by rank
                        if (num == "worst") {
                                hospitals[i] <- state_hospitals[, 2][order(
                                        out_numbers, state_hospitals[, 2])[size]]
  
                        } else if (num == "best")
                        {
                                hospitals[i] <- state_hospitals[, 2][order(
                                        out_numbers, state_hospitals[, 2])[1]]
                        }
                        else if (num > size) {
                                hospitals[i] <- NA
                        } else {
                                hospitals[i] <- state_hospitals[, 2][order(
                                        out_numbers, state_hospitals[, 2])[num]]
                        }
                        
                }
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        result <- data.frame(hospital=hospitals, state=states)
        return(result)
}