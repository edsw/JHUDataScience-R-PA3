rankall <- function(outcome, num = "best") {
	## Read outcome data
	outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomeOrdered <- outcomeFile[order(outcomeFile$State, outcomeFile$Hospital.Name),]

	if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
		stop("invalid outcome")
	}

	columns <- list(heart_attack=11, heart_failure=17, pneumonia=23)
	column <- as.numeric(columns[gsub(" ", "_", outcome)])
	states <- names(split(outcomeOrdered, outcomeOrdered$State))

	output = data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
	if (num == "best") {
		for (i in 1:length(states)) {
			tmp <- subset(outcomeOrdered, State == states[i])
			data <- suppressWarnings(as.numeric(tmp[, column]))
			row <- which.min(data)
			output <- rbind(output, data.frame(hospital = tmp[row,2] , state = states[i], stringsAsFactors=FALSE))
		}
	}
	else if (num == "worst") {
		for (i in 1:length(states)) {
			tmp <- subset(outcomeOrdered, State == states[i])
			data <- suppressWarnings(as.numeric(tmp[, column]))
			row <- which.max(data)
			output <- rbind(output, data.frame(hospital = tmp[row,2] , state = states[i], stringsAsFactors=FALSE))
		}
	}
	else if (is.numeric(num)) {
		for (i in 1:length(states)) {
			tmp <- subset(outcomeOrdered, State == states[i])
			data <- suppressWarnings(as.numeric(tmp[, column]))
			output <- rbind(output, data.frame(hospital = tmp[order(data)[num],2][1] , state = states[i]))
		}
	}

	output
}