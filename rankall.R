rankall <- function(outcome, num = "best") {
	if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}

	csv    <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	csv    <- csv[order(csv$State, csv$Hospital.Name),]
	states <- unique(csv$State)	
	column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
	output <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)

	for (i in 1:length(states)) {
		sub  <- subset(csv, State == states[i])
		vals <- suppressWarnings(as.numeric(sub[, column]))

		if (num == "best") {
			output <- rbind(output, data.frame(
				hospital = sub[which.min(vals),2],
				state = states[i], stringsAsFactors=FALSE))
		}
		else if (num == "worst") {
			output <- rbind(output, data.frame(
				hospital = sub[which.max(vals),2],
				state = states[i], stringsAsFactors=FALSE))
		}
		else if (is.numeric(num)) {
			output <- rbind(output, data.frame(
				hospital = sub[order(vals)[num],2][1],
				state = states[i], stringsAsFactors=FALSE))
		}
		else {
			stop("invalid num")
		}
	}

	output
}