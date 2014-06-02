rankhospital <- function(state, outcome, num = "best") {
	csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	csv <- subset(csv[order(csv$Hospital.Name),], State == state)

	if (nrow(csv) == 0) {
		stop("invalid state")
	}

	if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}

	column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
	vals   <- suppressWarnings(as.numeric(csv[, column]))
	
	ifelse(num == "best",  csv[which.min(vals),2],
		ifelse(num == "worst", csv[which.max(vals),2],
			csv[order(vals)[num],2][1]))
}