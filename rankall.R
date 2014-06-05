library("data.table")

rankall <- function(outcome, num = "best") {
	if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}

	csv    <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	csv    <- csv[order(csv$State, csv$Hospital.Name),]
	states <- unique(csv$State)	
	column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
	output <- NULL

	for (i in 1:length(states)) {
		sub  <- subset(csv, State == states[i])
		vals <- suppressWarnings(as.numeric(sub[, column]))

		if (num == "best") {
			output <- rbind(output, matrix(c(sub[which.min(vals),2], states[i]), ncol = 2))
		}
		else if (num == "worst") {
			output <- rbind(output, matrix(c(sub[which.max(vals),2], states[i]), ncol = 2))
		}
		else if (is.numeric(num)) {
			output <- rbind(output, matrix(c(sub[order(vals)[num],2][1], states[i]), ncol = 2))
		}
		else {
			stop("invalid num")
		}
	}

	colnames(output) <- c("hospital", "state")
	as.data.frame(output)
}

rankall2 <- function(outcome, num = "best") {
	if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}

	csv    <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
	colnames(csv)[column] <- "Outcome"
	
	output <- aggregate(Outcome ~ State, csv, function(x) {
		x <- as.numeric(x)
		y <- NULL
		if (num == "best") {
			y <- min(x, na.rm = TRUE)
		} else if (num == "worst") {
			y <- max(x, na.rm = TRUE)
		}
		else if (is.numeric(num)) {
			y <- sort(x)[num][1]
		}
		else {
			stop("invalid num")
		}
		as.character(y)
	})

	output
}

rankall3 <- function(outcome, num = "best") {
	if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}

	csv    <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
	colnames(csv)[column] <- "Outcome"

	DT <- data.table(csv)
	setkey(DT, "State")
		
	a <- DT[, list(Hospital.Name = .SD$Hospital.Name), by=list(State, Outcome)]
}