

best <- function(state, outcome) {
	csv  <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	ocsv <- subset(csv[order(csv$Hospital.Name),], State == state)

	if (nrow(ocsv) == 0) {
		stop("invalid state")
	}

	if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}

	column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
	vals   <- suppressWarnings(as.numeric(ocsv[, column]))

	ocsv[which.min(vals), 2]
}