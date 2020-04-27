##Read the outcome data into R via the read.csv function and look at the first
##few rows
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)




##Simple histogram of the 30-day death rates from heart attack
##(column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

##Takes two arguments: the 2-character abbreviated name of a state and an
##outcome name. The function reads the outcome-of-care-measures.csv file and
##returns a character vector with the name of the hospital that has the best
##(i.e. lowest) 30-day mortality for the specified outcome in that state.
best <- function(state, outcome) {
       
        ##Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
        # 2. Get distinct 'States' from data:
        States <- levels(factor(data[, 7]))
       
        # 3. Define the possible distinct acceptable 'Outcomes':
        Outcomes <- c("heart attack", "heart failure", "pneumonia")
       
        # 4. check validity of the input: 'state' and 'outcome'
       
        # 4.1 stops when the input 'state' is not exist within the 'States'
        if ((state %in% States) == FALSE) {stop(print("-- invalid state input! --"))}
       
        # 4.2 stops when the input 'outcome' is not exist within the 'Outcomes'
        else if ((outcome %in% Outcomes) == FALSE) {stop(print("-- invalid outcome input! --"))}
       
        # 5. Define mapping of outcome column according to 'outcome' input
        colNumber <- if (outcome == "heart attack") {11}
        else if (outcome == "heart failure") {17}
        else {23}
       
        # 6. Slice and clean data based on the requested input 'state and 'outcome'
       
        # 6.1 select data based on requested input 'state'
        selectedData <- subset(data, State == state)
       
        # 6.2 prepare selection of column by outcome
        selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
       
        # 6.3. re-select data based on requested input 'outcome' and clean it from all 'na' values
        selectedData <- selectedData[!(is.na(selectedColumns)), ]
       
        # 7. select the necessary columns and rows based on cleaned data
       
        # 7.1 re-select column by outcome from the cleaned data
        selectedColumns <- as.numeric(selectedData[, colNumber])
       
        # 7.2 select row(s) which has the minimum value
        selectedRows <- which(selectedColumns == min(selectedColumns))
       
        # 8. get hospital data based on selected rows data
       
        # 8.1 select hospital data from selected rows data
        selectedHospital <- selectedData[selectedRows, 2]
       
        # 8.2 sort hospital alphabetically in case of multiple result
        sortedHospital <- sort(selectedHospital)
       
        return(sortedHospital)
       
}


rankhospital <- function(state, outcome, num = "best") {
       
        # 'rankhospital'
        # find the best hospital in a state with a given rank
        # as a function of 'state', 'outcome' and 'num'
        #---------------------------------------------------------
        # 1. Read from file to be taken as data:
        data <- read.csv("outcome-of-care-measures.csv")
       
        # 2. Get distinct 'States' from data:
        States <- levels(factor(data[, 7]))
       
        # 3. Define the possible acceptable 'Outcomes':
        Outcomes <- c("heart attack", "heart failure", "pneumonia")
       
        # 4. check validity of the input: 'state', 'outcome' and 'num':
       
        # 4.1 stops when the input 'state' is not exist within the 'States';
        if ((state %in% States) == FALSE) {stop(print("-- invalid state input! --"))}
       
        # 4.2 stops when the input 'outcome' is not exist within the 'Outcomes';
        else if ((outcome %in% Outcomes) == FALSE) {stop(print("-- invalid outcome input! --"))}
       
        # 4.3 returns 'NA' if num is greater that the number of hospitals in the data.
        if (is.numeric(num) == TRUE) {
                if (length(data[,2]) < num) {return(NA)}
        }
       
        # 5. Define mapping of outcome column according to 'outcome' input:
        colNumber <- if (outcome == "heart attack") {11}
        else if (outcome == "heart failure") {17}
        else {23}
       
        # 6. Slice and clean data based on the requested input 'state and 'outcome':
       
        # 6.1 regard data in column 'colNumber' as numeric;
       
        data[, colNumber] <- suppressWarnings(as.numeric(levels(data[, colNumber])[data[, colNumber]]))
       
        # 6.2 regard data in column '2' as character;
        #> names(data)[2]
        #[1] "Hospital.Name"
        # since column #2 is Hospital.Name, it's should be considered as character
       
        data[, 2] <- as.character(data[, 2])
       
        # 6.3. select data based on requested input 'state';
        selectedData <- subset(data, State == state)
        #selectedData <- data[grep(state, data$State), ]
       
        # 6.4. re-select data based on requested input 'outcome' and clean it from all 'na' values;
        selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
        selectedData <- selectedData[!(is.na(selectedColumns)), ]
       
        # 7. rank hospital by 'outcome':
       
        rankedHospital <- selectedData[order(selectedData[, colNumber], selectedData[, 2]), ]
       
        # 7.1 if input 'num' == 'best' then then 'numRank' = first;
        if(num == "best") {numRank = 1}
       
        # 7.2 if input 'num' == 'worst' then 'numRank' = last;
        else if(num == "worst") {numRank = nrow(rankedHospital)}
       
        # 7.3 if input 'num' == number then 'numRank' = 'num'.
        else{numRank = num}
       
        # 8. return the hospital in the requested rank.
       
        return(rankedHospital[numRank, 2])

}


rankall <- function(outcome, num = "best") {
       
        # 'rankhospital'
        # find the best hospital in all state for a given outcome
        # as a function of 'outcome' and 'num'
        #---------------------------------------------------------
        # 1. Read from file to be taken as data:
        data <- read.csv("outcome-of-care-measures.csv")
       
        # 2. Get distinct 'States' from data:
        States <- levels(factor(data[, 7]))
       
        # 3. Define the possible acceptable 'Outcomes':
        Outcomes <- c("heart attack", "heart failure", "pneumonia")
       
        # 4. check validity of the input: 'state', 'outcome' and 'num':
       
        # 4.1 stops when the input 'outcome' is not exist within the 'Outcomes';
        if ((outcome %in% Outcomes) == FALSE) {stop(print("-- invalid outcome input! --"))}
       
        # 4.2 returns 'NA' if num is greater that the number of HospitalsRanked in the data.
        if (is.numeric(num) == TRUE) {
                if (length(data[,2]) < num) {return(NA)}
        }
       
        # 5. Define mapping of outcome column according to 'outcome' input:
        colNumber <- if (outcome == "heart attack") {11}
        else if (outcome == "heart failure") {17}
        else {23}
       
        # 6. Regard 'outcome' as numeric and 'Hospital.Name' as character:
       
        # 6.1 regard data in column 'colNumber' as numeric;
       
        data[, colNumber] <- suppressWarnings(as.numeric(levels(data[, colNumber])[data[, colNumber]]))
       
        # 6.2 regard data in column '2' as character;
        #> names(data)[2]
        #[1] "Hospital.Name"
        # since column #2 is Hospital.Name, it's should be considered as character:
       
        data[, 2] <- as.character(data[, 2])
       
       
        # 7. initialize empty AllStatesHospitalRanking:
       
        AllStatesHospitalRanking <- vector()
       
        # 8. compute AllStatesHospitalRanking.
       
        # 8.1 loop for all States to compute AllStatesHospitalRanking:
       
        for(i in 1:length(States)) {
               
                # 8.1.1. selected data for each state;
                selectedData <- subset(data, State == States[i])
                #selectedData <- data[grep(state, data$State), ]
               
                # 8.1.2. re-select data based on requested input 'outcome' and clean it from all 'na' values;
                selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
                selectedData <- selectedData[!(is.na(selectedColumns)), ]
               
                # 8.1.3. rank hospital by 'outcome':
               
                HospitalRankPerState <- selectedData[order(selectedData[, colNumber], selectedData[, 2]), ]
               
                # 8.1.3.1. if input 'num' == 'best' then then 'numRank' = first;
                if(num == "best") {numRank = 1}
               
                # 8.1.3.2. if input 'num' == 'worst' then 'numRank' = last;
                else if(num == "worst") {numRank = nrow(HospitalRankPerState)}
               
                # 8.1.3.3. if input 'num' == number then 'numRank' = 'num'.
                else{numRank = num}
               
                HospitalsRanked <- HospitalRankPerState[numRank, 2]
               
                # 8.1.4 append AllStatesHospitalRanking:
               
                AllStatesHospitalRanking <- append(AllStatesHospitalRanking, c(HospitalsRanked, States[i]))
        }
       
        #8.2 AllStatesHospitalRanking as data frame along with column names and row names:
       
        AllStatesHospitalRanking <- as.data.frame(matrix(AllStatesHospitalRanking, length(States), 2, byrow = TRUE))
        colnames(AllStatesHospitalRanking) <- c("hospital", "state")
        rownames(AllStatesHospitalRanking) <- States
       
        #8.3. return AllStatesHospitalRanking:
       
        return(AllStatesHospitalRanking)
}
