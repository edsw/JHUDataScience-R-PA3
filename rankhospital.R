rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomeFiltered <- outcomeFile[outcomeFile$State == state,]
	outcomeFiltered <- outcomeFiltered[order(outcomeFiltered$Hospital.Name),]

	## Check that state and outcome are valid
	if (nrow(outcomeFiltered) == 0) {
		stop("invalid state")
	}

	if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
		stop("invalid outcome")
	}

	## Return hospital name in that state with the given rank 30-day death rate
	columns <- list(heart_attack=11, heart_failure=17, pneumonia=23)
	column <- as.numeric(columns[gsub(" ", "_", outcome)])
	data <- suppressWarnings(as.numeric(outcomeFiltered[, column]))

	output = NA
	if (num == "best") {
		row <- which.min(data)
		output <- outcomeFiltered[row,2]
	}
	else if (num == "worst") {
		row <- which.max(data)
		output <- outcomeFiltered[row,2]
	}
	else if (is.numeric(num)) {
		output <- outcomeFiltered[order(data)[num],2][1]
	}

	output
}