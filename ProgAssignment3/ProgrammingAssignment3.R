
#Creating a best function which, when given a state and an ailment (heart attack,
#heart failure or pneumonia), returns the best-performing hospital in the state.
#This means the hospital with the lowest non-NA 30-day mortality rate

best <- function(state = "None", ailment = "None") {
        
        states <- c("AL", "AK", "AZ","AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI")
        
        ailments <- c("heart attack", "heart failure", "pneumonia")
        
        # First we have to check to make sure the inputs are correct by checking them 
        #against our master list for each one
        
        if ((state %in% states) == FALSE) {
                stop('invalid state')
        }
        
        if ((ailment %in% ailments) == FALSE) {
                stop('invalid outcome') 
        }
        
        # Let's load in our data
        
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"), stringsAsFactors = FALSE)
        
        #I know that the 7th col is the state code, the 2nd is the hospital name, and
        #the 11/17/23 is the relevant column depending on the ailment
        
        ailmentcode <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        #now i need to subset so i have the three columns i need, hospital name, the ailment and then the state
        
        ssoutcome <- outcome[, c(2, ailmentcode[ailment], 7)]
        
        #now i want to make the mortality rate a numeric so that it will sort properly
        
        ssoutcome[2] <- as.numeric(as.character(ssoutcome[, 2]))
        
        #now i need to filter so that i have only the state i want
        
        finout <- filter(ssoutcome, State == state)
        
        #now i need to remove NAs
        
        finout <- na.omit(finout)
        
        #now i need to sort so that the lowest rate is at the top AND in alpha order
        #to do this i use the order function and specify first the mortality rate and
        #then the hospital name
        
        ordfinout <- finout[order(finout[,2], finout[,1]), ]
        
        #head(ordfinout)
        
        #now i need to sort in alpha order
        
        #print(head(ordfinout))
        
        
        #now i need to return the name of the first row in this sorted data set
        
        Hname <- ordfinout[1, "Hospital.Name"]        
        
        Hname
        
        
        
        
        
}

