

best <- function(state, outcomename) {
        # outcomename = “heart attack” (11), “heart failure” (17), 
        # or “pneumonia” (23)
        # exclude hospitals that have no data
        # use hospital name from field Hospital.Name
        
        ## check outcomename
        if (outcomename=='heart attack') {colnr=11}
        else if (outcomename=='heart failure') {colnr=17}
        else if (outcomename=='pneumonia') {colnr=23}
        else {
                print('unknown outcome name.')
                return
        }
        
        ## Read outcome data
        #print(colnr)
        ## Check that state and outcome are valid
        outcome[outcome == "Not Available"] <-- NA
        outcome[outcome == "NA"] <-- NA
        
        if (sum(outcome[7]==state) > 0) {
                a <- outcome[outcome[7]==state,c(2,7,colnr)]
                a[,3] <- as.numeric(a[,3])
                #print(a[,c(1.2,3)])
                b <- a[order(a[1],a[3]),]
                c <- min(b[3],na.rm =TRUE)
                #print(c)
                d <- which(b[3]==c)
                #print(d)
        }
        else {
                #print('unknown state name')
                return
        }
        ##outcome[outcome$State==state,colnr]

        ## Return hospital name in that state with lowest 30-day death
        b[d,1]
        
        ## rate
}