
library(httr)
library(hgvsParseR)
library(RJSONIO)

#Internal constructor (not exported) creates an immutable new user object.
new.user <- function(data) {
	.data <- data
	expectedFields <- c("username","first_name","last_name","experimentsets","experiments")
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.user()")
	}
	#TODO: Validate data!
	structure(list(
		getUsername=function() .data$username,
		getFirstName=function() .data$first_name,
		getLastName=function() .data$last_name,
		getExperimentSets=function() .data$experimentsets,
		getExperiments=function() .data$experiments
	),class="rapimaveUser")
}

print.rapimaveUser <- function(obj) {
	cat(paste0(
		"MaveDB user ",obj$getUsername(),
		" (",obj$getFirstName()," ",obj$getLastName(),")"#,
		# "\n\nThe following functions are available:\n",
		# paste0(names(obj),"()",collapse="; ")
	),"\n")
}

#Internal constructor (not exported) creates an immutable new experimentset object.
new.experimentSet <- function(data) {
	.data <- data
	expectedFields <- c("experiments","authors","accession")
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.experimentSet()")
	}
	#TODO: Validate data!
	structure(list(
		getExperiments=function() .data$experiments,
		getAuthors=function() .data$authors,
		getAccession=function() .data$accession
	),class="rapimaveExpSet")
}

print.rapimaveExpSet <- function(obj) {
	cat(paste0(
		"MaveDB ExperimentSet ",obj$getAccession()#,
		# "\n\nThe following functions are available:\n",
		# paste0(names(obj),"()",collapse="; ")
	),"\n")
}

#' MaveDB R-API client constructor
#'
#' Creates an object of type rapimave with the following functions:
#'
#' @param baseURL MaveDB API base-URL. Defaults to "https://www.mavedb.org/api/"
#' @param aacode allowed values: 1, 3, or NA. Determines whether 1-letter codes or 3-letter codes should be forced. NA uses input format.
#' @return A \code{data.frame} with the following columns: 
#' @keywords HGVS parsing
#' @export
#' @examples
#' result <- parseHGVS(c("g.1318G>T","c.123_125inv","p.R123_L152del"))
new.rapimave <- function(baseURL="https://www.mavedb.org/api/",certifySSL=FALSE,encoding="UTF-8") {

	if (!certifySSL) {
		set_config(config(ssl_verifypeer = 0L))
	}

	getAllUsers <- function() {
		url <- paste0(baseURL,"get/user/all/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0 && "users" %in% names(returnData)) {
				if (length(returnData$users) > 0) {
					return(lapply(returnData$users, new.user))
				} else {
					return(list())
				}
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getUser <- function(username) {
		url <- paste0(baseURL,"get/user/",username,"/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.user(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such user!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getAllExperimentSets <- function() {
		url <- paste0(baseURL,"get/experimentset/all/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0 && "experimentsets" %in% names(returnData)) {
				if (length(returnData$experimentsets) > 0) {
					return(lapply(returnData$experimentsets, new.experimentSet))
				} else {
					return(list())
				}
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getExperimentSet <- function(accession) {
		url <- paste0(baseURL,"get/experimentset/",accession,"/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.experimentSet(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ExperimentSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	structure(list(
		getAllUsers=getAllUsers,
		getUser=getUser,
		getAllExperimentSets=getAllExperimentSets,
		getExperimentSet=getExperimentSet
	),class="rapimave")
}

print.rapimave <- function(obj) {
	cat(paste0(
		"A rapimave API object.\n\n",
		"The following functions are available:\n",
		paste0(names(obj),"()",collapse="; ")
	),"\n")
}

# get/user/all/
# get/user/(?P<username>.+)/$
# get/experimentset/all/
# get/experimentset/(?P<accession>(EXPS|exp)\d{6})/$
# get/experiment/all/
# get/experiment/(?P<accession>(EXP|exp)\d{6}[A-Z]+)/$
# get/scoreset/all/
# get/scoreset/(?P<accession>(SCS|scs)\d{6}[A-Z]+.\d+)/$
# get/scoreset/(?P<accession>(SCS|scs)\d{6}[A-Z]+.\d+)/scores/$
# get/scoreset/(?P<accession>(SCS|scs)\d{6}[A-Z]+.\d+)/counts/$
