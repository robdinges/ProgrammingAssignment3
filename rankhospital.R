## returns a character vector with the name of the hospital that 
## has the best (i.e. lowest) 30-day mortality for the specified 
## outcome in that state
##
## Hospitals that do not have data on a particular outcome should be excluded


rankhospital <- function(state, outcomename, num='best') {

        ## check outcomename
        if (outcomename=='heart attack') {colnr=11}
        else if (outcomename=='heart failure') {colnr=17}
        else if (outcomename=='pneumonia') {colnr=23}
        else {
                print('unknown outcome name.')
                return
        }

        ## Read outcome data
        outcome <- read.csv('data/outcome-of-care-measures.csv')        
        
        ## Check that state and outcome are valid
        outcome[outcome == "Not Available"] <-- NA
        outcome[outcome == "NA"] <-- NA
        
        if (sum(outcome[7]==state) > 0) {
                a <- outcome[outcome[7]==state,c(2,7,colnr)]
                a[,3] <- as.numeric(a[,3])
                a <- na.omit(a)
                b <- a[order(-rank(a[3]),a[1],decreasing=TRUE),]
                if (num == 'best') { num = 1 }
                else if (num == 'worst') { num = nrow(b)}
                #print(b[num,1])
        }
        else {
                print('unknown state name')
                return
        }
        
        b[num,1]
                
}