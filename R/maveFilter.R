
#' New MaveDB Filter Object
#'
#' Constructor that creates a MaveDB filter object
#'
#' The resulting object offers a number of filter functions. All these functions return 
#' vectors of type \code{logical}, which can be combined with \code{&} and \code{|} to form
#' more complex filters before being applied to the data. (See example below). The following
#' functions are available.
#' \itemize{
#'   \item{mutationCount()} filters by number of mutations in the variant. Parameters
#'     \code{min} and \code{max} default to 0 and Inf respectively.
#'   \item{position()} filters by the (start) position of the mutations. Parameters
#'     \code{min} and \code{max} default to -Inf and Inf respectively. Parameter \code{multi}
#'     determines whether in case of multi-mutants, the criterium must match any or all of the
#'     individual mutations. Allowed values are "any" and "all".
#'   \item{residues()} filters by amino acid residues. Parameter from and to are vectors of allowed
#'     ancestral and variant amino acid residues.Parameter \code{multi}
#'     determines whether in case of multi-mutants, the criterium must match any or all of the
#'     individual mutations. Allowed values are "any" and "all".
#'   \item{numerical()} filters by numerical columns. Parameter \code{col} is the name of the numerical
#'     column to filter by. Parameters \code{min} and \code{max} default to -Inf and Inf respectively.
#'   \item{categorical()} filters by categorical columns. Parameter \code{col} is the name of the categorical
#'     column (of type \code{character} or \code{factor}) to filter by. Parameter values is a vector of
#'     allowed values in that column to filter for.
#' }
#'
#' @param data A data.frame containing a MaveDB data set containing 
#'    at least the two columns "hgvs" and "score".
#' @param verbose Logical. If TRUE, prints status messages while loading data.
#' @return a new MaveDB filter object
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' data <- mave$getScores("SCS000001A.2")
#' mfilter <- new.mave.filter(data)
#' filter <- with(mfilter,
#'    position(min=5,multi="all") & residues(to="Ala",multi="all") & numerical("score",min=0.1)
#' )
#' filtered.data <- data[filter,]
#' }
new.mave.filter <- function(data,verbose=TRUE) {

	library(hgvsParseR)

	if (!is.data.frame(data) || !all(c("hgvs","score") %in% colnames(data))) {
		stop("Illegal argument: new.mave.filter() requires a MaveDB data.frame!")
	}

	.data <- data
	cat("Parsing HGVS strings and building index...")
	.mutations <- parseHGVS(data$hgvs)
	if ("multiPart" %in% colnames(.mutations)) {
		.index <- as.integer(sapply(strsplit(rownames(.mutations),"\\."),`[[`,1))
		.nmut <- table(.index)
	} else {#no multi-mutations should exist in this case
		if (nrow(data) != nrow(.mutations)) {
			stop("Non-matching HGVS. If you see this, report this as a bug.")
		}
		.index <- 1:nrow(.data)
		.nmut <- rep(1,nrow(.data))
	}
	cat("done\n")

	mutationCount <- function(min=0,max=Inf) {
		if (min > max){
			stop("min must be less than or equal to max")
		}
		filter <- .nmut >= min & .nmut <= max
		return(filter)
	}

	position <- function(min=-Inf,max=Inf,multi=c("any","all")) {
		multi <- multi[[1]]
		if (multi=="any") {
			hits <- unique(.index[with(.mutations,which(start >= min & start <= max))])
			return(1:nrow(data) %in% hits)
		} else if (multi=="all") {
			hits <- .index[with(.mutations,which(start >= min & start <= max))]
			hitCounts <- table(hits)
			hitIDs <- as.numeric(names(hitCounts))
			completeHits <- hitIDs[hitCounts == .nmut[hitIDs]]
			return(1:nrow(data) %in% completeHits)
		} else {
			stop("Illegal argument: multi must be all or any")
		}
	}

	aas <- c(A="Ala",C="Cys",D="Asp",E="Glu",F="Phe",G="Gly",H="His",
			I="Ile",K="Lys",L="Leu",M="Met",N="Asn",P="Pro",Q="Gln",R="Arg",
			S="Ser",T="Thr",V="Val",W="Trp",Y="Tyr",`*`="Ter")

	residues <- function(from=aas,to=aas,multi=c("any","all")) {
		multi <- multi[[1]]
		if (multi=="any") {
			hits <- unique(.index[with(.mutations,which(ancestral %in% from & variant %in% to))])
			return(1:nrow(data) %in% hits)
		} else if (multi=="all") {
			hits <- .index[with(.mutations,which(ancestral %in% from & variant %in% to))]
			hitCounts <- table(hits)
			hitIDs <- as.numeric(names(hitCounts))
			completeHits <- hitIDs[hitCounts == .nmut[hitIDs]]
			return(1:nrow(data) %in% completeHits)
		} else {
			stop("Illegal argument: multi must be all or any")
		}
	}

	numerical <- function(col,min=-Inf,max=Inf) {
		if (!(col %in% colnames(.data))) {
			stop("The given column name does not exist in the data set.")
		}
		if (!is.numeric(.data[,col])) {
			stop("The given column name does not contain numerical data.")
		}
		return(.data[,col] >= min & .data[,col] <= max)
	}

	categorical <- function(col,values) {
		if (!(col %in% colnames(.data))) {
			stop("The given column name does not exist in the data set.")
		}
		if (!(is.character(.data[,col]) || is.factor(.data[,col]))) {
			stop("The given column name does not contain categorical data.")
		}
		if (!is.character(values)) {
			stop("The given values are not categorical.")
		}
		return(.data[,col] %in% values)
	}

	structure(list(
		mutationCount=mutationCount,
		position=position,
		residues=residues,
		numerical=numerical,
		categorical=categorical
	),class="rapimaveFilter")

}