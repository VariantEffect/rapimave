# Copyright (C) 2018  Jochen Weile, Roth Lab
#
# This file is part of rapimave.
#
# rapimave is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rapimave is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rapimave.  If not, see <https://www.gnu.org/licenses/>.

#' New R-API MaveDB User
#'
#' Internal constructor that creates an immutable new user object. This function is not exported
#'   and should not be called outside of the rapimave package itself. User objects are generated
#'   by the \code{getAllUsers()} and \code{getUser()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A user object offers the following getter functions:
#' \itemize{
#'   \item{getUsername()} return the username.
#'   \item{getFirstName()} return the user's first name.
#'   \item{getLastName()} return the user's last name.
#'   \item{getDisplayName()} return the user's preferred display name.
#'   \item{getExperimentSets()} return the ExperimentSets attributed to the user.
#'   \item{getExperiments()} return the Experiments attributed to the user.
#'   \item{getScoreSets()} return the ScoreSets attributed to the user.
#' }
#'
#' @return a new R-API MaveDB user object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' user <- mave$getUser("0000-0003-1474-605X")
#' cat(user$getFirstName(),user$getLastName())
#' }
new.user <- function(data) {
	.data <- data
	expectedFields <- c(
		"username","first_name","last_name","display_name",
		"contributor_experimentsets","contributor_experiments",
		"contributor_scoresets"
	)
	#field names can differ!
	expectedFieldsAlt <- c(
		"username","first_name","last_name","display_name",
		"experimentsets","experiments",
		"scoresets"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		if (!is.list(data) || !all(expectedFieldsAlt %in% names(.data))) {
			stop("Illegal argument for new.user()")
		}
		return(structure(list(
			getUsername=function() .data$username,
			getFirstName=function() .data$first_name,
			getLastName=function() .data$last_name,
			getDisplayName=function() .data$display_name,
			getExperimentSets=function() .data$experimentsets,
			getExperiments=function() .data$experiments,
			getScoreSets=function() .data$scoresets
		),class="rapimaveUser"))
	}
	#TODO: Validate data!
	structure(list(
		getUsername=function() .data$username,
		getFirstName=function() .data$first_name,
		getLastName=function() .data$last_name,
		getDisplayName=function() .data$display_name,
		getExperimentSets=function() .data$contributor_experimentsets,
		getExperiments=function() .data$contributor_experiments,
		getScoreSets=function() .data$contributor_scoresets
	),class="rapimaveUser")
}

#' Print R-API MaveDB User
#'
#' Prints a human-readable summary of a R-API MaveDB User object:
#'
#' @param obj the User object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getUser("0000-0003-1474-605X"))
#' }
print.rapimaveUser <- function(obj) {
	cat(paste0(
		"MaveDB user ",obj$getUsername(),
		" (",obj$getFirstName()," ",obj$getLastName(),")","\n"
	))
}


#' New R-API MaveDB ExperimentSet
#'
#' Internal constructor that creates an immutable new ExperimentSet object. This function is not exported
#'   and should not be called outside of the rapimave package itself. ExperimentSet objects are generated
#'   by the \code{getAllExperimentSets()} and \code{getExperimentSet()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ExperimentSet object offers the following getter functions:
#' \itemize{
#'   \item{getContributors()} returns a list of users that contributed to this record.
#'   \item{getURN()} returns the URN of this record.
#'   \item{getCreationDate()} returns the date when this record was originally created.
#'   \item{getModificationDate()} returns the date when this record was last modified.
#'   \item{getPublicationDate()} returns the date when this record was published.
#'   \item{getCreatedBy()} returns the user who originally created this record.
#'   \item{getModifiedBy()} returns the user who last modified this record.
#'   \item{getMetadata()} returns the metadata for this record.
#'   \item{getTitle()} returns the title of this record.
#'   \item{getShortDescription()} returns a short description text of this record.
#'   \item{getAbstract()} returns the abstract text of this record.
#'   \item{getMethods()} returns the methods description text of this record.
#'   \item{getKeywords()} returns the list of keywords associated with this record.
#'   \item{getDOIs()} returns the list of Digital Object Identifiers (DOIs) associated with this record.
#'   \item{getXrefSRA()} returns cross-references to the NCBI Short Read Archive (SRA).
#'   \item{getXRefPubmed()} returns cross-references to Pubmed.
#'   \item{getExperiments()} returns the URNs of the experiments belonging to this ExperimentSet.
#' }
#'
#' @return a new R-API MaveDB ExperimentSet object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' set1 <- mave$getExperimentSet("urn:mavedb:00000001")
#' expURNs <- set1$getExperiments()
#' }
new.experimentSet <- function(data) {
	.data <- data

	expectedFields <- c(
		"creation_date","modification_date","urn","publish_date","created_by",
		"modified_by","extra_metadata","abstract_text","method_text","short_description",
		"title","keywords","sra_ids","doi_ids","pubmed_ids","contributors","experiments"
	)
	if (!is.list(.data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.experimentSet()")
	}
	#TODO: Validate data!
	structure(list(
		getAccession=function() {
			.Deprecated("getURN")
			.data$urn
		},
		getURN=function() .data$urn,
		getAuthors=function() {
			.Deprecated("getContributors")
			lapply(.data$contributors,new.user)
		},
		getContributors=function() lapply(.data$contributors,new.user),
		getCreationDate=function() .data$creation_date,
		getModificationDate=function() .data$modification_date,
		getPublicationDate=function() .data$publish_date,
		getCreatedBy=function() .data$created_by,
		getModifiedBy=function() .data$modified_by,
		getMetadata=function() .data$extra_metadata,
		getTitle=function() .data$title,
		getShortDescription=function() .data$short_description,
		getAbstract=function() .data$abstract_text,
		getMethods=function() .data$method_text,
		getKeywords=function() .data$keywords,
		getDOIs=function() {
			if (!is.null(.data$doi_ids)) {
				lapply(.data$doi_ids,new.xref)
			} else NULL
		},
		getXRefSRA=function() {
			if (!is.null(.data$sra_ids)) {
				lapply(.data$sra_ids,new.xref)
			} else NULL
		},
		getXRefPubmed=function() {
			if (!is.null(.data$pubmed_ids)) {
				lapply(.data$pubmed_ids,new.xref)
			} else NULL
		},
		getExperiments=function() .data$experiments
	),class="rapimaveExpSet")
}

#' Print R-API MaveDB ExperimentSet
#'
#' Prints a human-readable summary of a R-API MaveDB ExperimentSet object:
#'
#' @param obj the ExperimentSet object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getExperimentSet("urn:mavedb:00000001"))
#' }
print.rapimaveExpSet <- function(obj) {
	cat("MaveDB ExperimentSet",obj$getURN(),"\n")
}

#' New R-API MaveDB Experiment
#'
#' Internal constructor that creates an immutable new Experiment object. This function is not exported
#'   and should not be called outside of the rapimave package itself. Experiment objects are generated
#'   by the \code{getAllExperiments()} and \code{getExperiment()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A Experiment object offers the following getter functions:
#' \itemize{
#'   \item{getContributors()} returns a list of users that contributed to this record.
#'   \item{getURN()} returns the URN of this record.
#'   \item{getCreationDate()} returns the date when this record was originally created.
#'   \item{getModificationDate()} returns the date when this record was last modified.
#'   \item{getPublicationDate()} returns the date when this record was published.
#'   \item{getCreatedBy()} returns the user who originally created this record.
#'   \item{getModifiedBy()} returns the user who last modified this record.
#'   \item{getMetadata()} returns the metadata for this record.
#'   \item{getTitle()} returns the title of this record.
#'   \item{getShortDescription()} returns a short description text of this record.
#'   \item{getAbstract()} returns the abstract text of this record.
#'   \item{getMethods()} returns the methods description text of this record.
#'   \item{getKeywords()} returns the list of keywords associated with this record.
#'   \item{getDOIs()} returns the list of Digital Object Identifiers (DOIs) associated with this record.
#'   \item{getXrefSRA()} returns cross-references to the NCBI Short Read Archive (SRA).
#'   \item{getXRefPubmed()} returns cross-references to Pubmed.
#'   \item{getExperimentSet()} return the accession of the ExperimentSet to which this Experiment belongs.
#'   \item{getScoreSets()} return the list of URNs of the Experiment's ScoreSets.
#' }
#'
#' @return a new R-API MaveDB experiment object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' exp1 <- mave$getExperiment("urn:mavedb:00000001-a")
#' authorIds <- exp1$getAuthors()
#' }
new.experiment <- function(data) {
	.data <- data
	# expectedFields <- c("scoresets","authors","accession","experimentset")
	expectedFields <- c("creation_date","modification_date","urn",
		"publish_date","created_by","modified_by",
		"extra_metadata","abstract_text","method_text",
		"short_description","title","keywords",
		"sra_ids","doi_ids","pubmed_ids",
		"contributors","scoresets","experimentset"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.experiment()")
	}
	#TODO: Validate data!
	structure(list(
		getAccession=function() {
			.Deprecated("getURN")
			.data$urn
		},
		getURN=function() .data$urn,
		getAuthors=function() {
			.Deprecated("getContributors")
			lapply(.data$contributors,new.user)
		},
		getContributors=function() lapply(.data$contributors,new.user),
		getCreationDate=function() .data$creation_date,
		getModificationDate=function() .data$modification_date,
		getPublicationDate=function() .data$publish_date,
		getCreatedBy=function() .data$created_by,
		getModifiedBy=function() .data$modified_by,
		getMetadata=function() .data$extra_metadata,
		getTitle=function() .data$title,
		getShortDescription=function() .data$short_description,
		getAbstract=function() .data$abstract_text,
		getMethods=function() .data$method_text,
		getKeywords=function() .data$keywords,
		getDOIs=function() {
			if (!is.null(data$doi_ids)) {
				lapply(.data$doi_ids,new.xref)
			} else NULL
		},
		getXRefSRA=function() {
			if (!is.null(data$sra_ids)) {
				lapply(.data$sra_ids,new.xref)
			} else NULL
		},
		getXRefPubmed=function() {
			if (!is.null(data$pubmed_ids)) {
				lapply(.data$pubmed_ids,new.xref)
			} else NULL
		},
		getExperimentSet=function() .data$experimentset,
		getScoreSets=function() .data$scoresets
	),class="rapimaveExperiment")
}

#' Print R-API MaveDB Experiment
#'
#' Prints a human-readable summary of a R-API MaveDB Experiment object:
#'
#' @param obj the Experiment object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getExperiment("urn:mavedb:00000001-a"))
#' }
print.rapimaveExperiment <- function(obj) {
	cat("MaveDB Experiment",obj$getURN(),"\n")
}

#' New R-API MaveDB ScoreSet
#'
#' Internal constructor that creates an immutable new ScoreSet object. This function is not exported
#'   and should not be called outside of the rapimave package itself. ScoreSet objects are generated
#'   by the \code{getAllScoreSets()} and \code{getScoreSet()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ScoreSet object offers the following getter functions:
#' \itemize{
#'   \item{getContributors()} returns a list of users that contributed to this ScoreSet.
#'   \item{getURN()} returns the URN of this ScoreSet.
#'   \item{getCreationDate()} returns the date when this record was originally created.
#'   \item{getModificationDate()} returns the date when this record was last modified.
#'   \item{getPublicationDate()} returns the date when this record was published.
#'   \item{getCreatedBy()} returns the user who originally created this record.
#'   \item{getModifiedBy()} returns the user who last modified this record.
#'   \item{getMetadata()} returns the metadata for this record.
#'   \item{getTitle()} returns the title of this record.
#'   \item{getShortDescription()} returns a short description text of this record.
#'   \item{getAbstract()} returns the abstract text of this record.
#'   \item{getMethods()} returns the methods description text of this record.
#'   \item{getKeywords()} returns the list of keywords associated with this record.
#'   \item{getDOIs()} returns the list of Digital Object Identifiers (DOIs) associated with this record.
#'   \item{getXrefSRA()} returns cross-references to the NCBI Short Read Archive (SRA).
#'   \item{getXRefPubmed()} returns cross-references to Pubmed.
#'   \item{getTarget()} returns the Target object for this scoreset, which describes the molecule that
#'      that was targeted in the experiment.
#'   \item{getLicence()} returns the licencing information governing the use of this ScoreSet.
#'   \item{getCurrentVersion()} If a more up-to-date version of this ScoreSet exists, 
#'     that has since replaced it, this function will return the accession of the most recent version.
#'   \item{getPreviousVersion()} If this ScoreSet replaces an older ScoreSet, this function returns
#'     the accession of that older ScoreSet.
#'   \item{getNextVersion()} If this ScoreSet was replaced by newer ScoreSet, this function returns
#'     the accession of that newer ScoreSet in versioning history. To obtain the most recent version,
#'     use \code{getCurrentVersion()}.
#'   \item{getCountColumns()} returns the column names in the count table for this ScoreSet.
#'   \item{getScoreColumns()} returns the column names in the score table for this ScoreSet.
#'   \item{getMetaDataColumns()} returns the column names in the metadata table for this ScoreSet.
#'   \item{getVariantCount()} returns the number of variants in this scoreset, i.e. the number of rows
#'      to be expected in the scores and counts tables.
#'   \item{getExperiment()} returns the URN of the experiment record to which this scoreset belongs.
#' }
#'
#' @return a new R-API MaveDB ScoreSet object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' set1 <- mave$getScoreSet("urn:mavedb:00000001-a-1")
#' scoreColNames <- set1$getScoreColumns()
#' }
new.scoreSet <- function(data) {
	.data <- data
	# expectedFields <- c(
	# 	"authors","accession","licence","current_version",
	# 	"replaces","replaced_by","count_columns","score_columns"
	# )
	expectedFields <- c(
		"creation_date","modification_date","urn",
		"publish_date","created_by","modified_by",
		"extra_metadata","abstract_text","method_text",
		"short_description","title","keywords",
		# "sra_ids",
		"doi_ids","pubmed_ids",
		"contributors","licence","target",
		"score_columns","count_columns",#"metadata_columns",
		"previous_version","next_version","current_version",
		"variant_count","experiment"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.scoreSet()")
	}
	#TODO: Validate data!
	structure(list(
		getAccession=function() {
			.Deprecated("getURN")
			.data$urn
		},
		getURN=function() .data$urn,
		getAuthors=function() {
			.Deprecated("getContributors")
			lapply(.data$contributors,new.user)
		},
		getContributors=function() lapply(.data$contributors,new.user),
		getCreationDate=function() .data$creation_date,
		getModificationDate=function() .data$modification_date,
		getPublicationDate=function() .data$publish_date,
		getCreatedBy=function() .data$created_by,
		getModifiedBy=function() .data$modified_by,
		getMetadata=function() .data$extra_metadata,
		getTitle=function() .data$title,
		getShortDescription=function() .data$short_description,
		getAbstract=function() .data$abstract_text,
		getMethods=function() .data$method_text,
		getKeywords=function() .data$keywords,
		getDOIs=function() {
			if (!is.null(data$doi_ids)) {
				lapply(.data$doi_ids,new.xref)
			} else NULL
		},
		# getXRefSRA=function() {
		# 	if (!is.null(data$sra_ids)) {
		# 		lapply(.data$sra_ids,new.xref)
		# 	} else NULL
		# },
		getXRefPubmed=function() {
			if (!is.null(data$pubmed_ids)) {
				lapply(.data$pubmed_ids,new.xref)
			} else NULL
		},
		getTarget=function() {
			if (!is.null(.data$target)) {
				new.target(.data$target)
			} else NULL
		},
		getLicence=function() .data$licence,
		getCurrentVersion=function() .data$current_version,
		getPreviousVersion=function() .data$previous_version,
		getReplaces=function() {
			.Deprecated("getPreviousVersion")
			.data$previous_version
		},
		getReplacedBy=function() {
			.Deprecated("getNextVersion")
			.data$next_version
		},
		getNextVersion=function() .data$next_version,
		getCountColumns=function() .data$count_columns,
		getScoreColumns=function() .data$score_columns,
		# getMetaDataColumns=function() .data$metadata_columns,
		getVariantCount=function() .data$variant_count,
		getExperiment=function() .data$experiment
	),class="rapimaveScoreSet")
}

#' Print R-API MaveDB ScoreSet
#'
#' Prints a human-readable summary of a R-API MaveDB ScoreSet object:
#'
#' @param obj the ScoreSet object to print
#' @keywords MaveDB ScoreSet
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getScoreSet("SCS000001A.2"))
#' }
print.rapimaveScoreSet <- function(obj) {
	cat("MaveDB ScoreSet ",obj$getURN(),"\n")
}


#' New R-API MaveDB Target
#'
#' Internal constructor that creates an immutable new Target object. This function is not exported
#'   and should not be called outside of the rapimave package itself. Target objects are generated
#'   by the getTarget() method of a scoreset object (which itself
#'	 is generated using the \code{getAllScoreSets()} and \code{getScoreSet()} methods of a rapimave 
#'   object). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ScoreSet object offers the following getter functions:
#' \itemize{
#'   \item{getName()} returns the name of the target
#'   \item{getSequence()} returns wildtype sequence of the target
#'   \item{getXrefUniprot()} returns a Xref object for Uniprot
#'   \item{getXrefEnsembl()} returns a Xref object for Ensembl
#'   \item{getXrefRefseq()} returns a Xref object for Refseq
#'   \item{getReferenceMaps()} returns a list of reference map objects that describe how this 
#'      target maps to different reference genomes.
#'   \item{getScoreset()} retunrs the URN of the scoreset to which this target belongs.
#' }
#'
#' @return a new R-API MaveDB Target object.
new.target <- function(data) {
	.data <- data
	expectedFields <- c(
		"name","reference_sequence","uniprot","ensembl",
		"refseq","reference_maps","scoreset","type"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		cat(names(.data))
		stop("Illegal argument for new.target()")
	}

	structure(list(
		getName=function() .data$name,
		getSequence=function() .data$reference_sequence,
		getXrefUniprot=function() {
			if (!is.null(data$uniprot)) {
				new.xref(.data$uniprot)
			} else NULL
		},
		getXrefEnsembl=function() {
			if (!is.null(data$ensembl)) {
				new.xref(.data$ensembl)
			} else NULL
		},
		getXrefRefseq=function() {
			if (!is.null(data$refseq)) {
				new.xref(.data$refseq)
			} else NULL
		},
		getReferenceMaps=function() {
			if (!is.null(.data$reference_maps)) {
				lapply(.data$reference_maps,new.refmap)
			} else NULL
		},
		getScoreset=function() .data$scoreset,
		getType=function() .data$type
	),class="rapimaveTarget")
}

#' Print R-API MaveDB target object
#'
#' Prints a human-readable summary of a R-API MaveDB target object:
#'
#' @param obj the object to print
#' @keywords MaveDB print
#' @export
print.rapimaveTarget <- function(obj) {
	cat("MaveDB Target ",obj$getName()," of scoreset ",obj$getScoreset(),"\n")
}



#' New R-API MaveDB ReferenceMap
#'
#' Internal constructor that creates an immutable new ReferenceMap object. This function is not exported
#'   and should not be called outside of the rapimave package itself. ReferenceMap objects are generated
#'   by the getReferenceMap() method of a target object (which itself
#'	 is generated using the \code{getTarget()} method on a scoreset object). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ScoreSet object offers the following getter functions:
#' \itemize{
#'   \item{getGenome()} returns the genome object for this reference map
#'   \item{isPrimary()} returns whether this is the primary reference map for this scoreset
#'   \item{getIntervals()} returns a data.frame listing the intervals in the genome to which 
#'      the target is mapped.
#' }
#'
#' @return a new R-API MaveDB ReferenceMap object.
new.refmap <- function(data) {
	.data <- data
	expectedFields <- c(
		"genome","is_primary","intervals"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.refmap()")
	}
	structure(list(
		getGenome=function() new.genome(.data$genome),
		isPrimary=function() .data$is_primary,
		getIntervals=function() do.call(rbind,.data$intervals)
	),class="rapimaveRefmap")
}


#' Print R-API MaveDB reference map object
#'
#' Prints a human-readable summary of a R-API MaveDB reference map object:
#'
#' @param obj the object to print
#' @keywords MaveDB print
#' @export
print.rapimaveRefmap <- function(obj) {
	cat("MaveDB ReferenceMap\n")
}


#' New R-API MaveDB genome
#'
#' Internal constructor that creates an immutable new genome object. This function is not exported
#'   and should not be called outside of the rapimave package itself. Genome objects are generated
#'   by the getGenome() method of a reference map object (which itself
#'	 is generated using the \code{get.referenceMaps()} method on a target object). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ScoreSet object offers the following getter functions:
#' \itemize{
#'   \item{getShortName()} returns the name of this genome object
#'   \item{getSpecies()} returns the species to which this genome belongs.
#'   \item{getXRefEnsembl()} returns the a cross-reference object for Ensembl (if it exists).
#'   \item{getXrefRefseq()} returns the a cross-reference object for Refseq (if it exists).
#' }
#'
#' @return a new R-API MaveDB genome object.
new.genome <- function(data) {
	.data <- data
	expectedFields <- c(
		"short_name","species_name","ensembl","refseq"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.genome()")
	}
	structure(list(
		getShortName=function() .data$short_name,
		getSpecies=function() .data$species_name,
		getXRefEnsembl=function() {
			if (!is.null(.data$ensembl)) {
				new.xref(.data$ensembl)
			} else NULL
		},
		getXRefRefseq=function() {
			if (!is.null(.data$refseq)) {
				new.xref(.data$refseq)
			} else NULL
		}
	),class="rapimaveGenome")
}

#' Print R-API MaveDB Genome object
#'
#' Prints a human-readable summary of a R-API MaveDB genome object:
#'
#' @param obj the object to print
#' @keywords MaveDB print
#' @export
print.rapimaveGenome <- function(obj) {
	cat("MaveDB Genome",obj$getShortName(),"\n")
}


#' New R-API MaveDB cross-reference
#'
#' Internal constructor that creates an immutable new Xref object. This function is not exported
#'   and should not be called outside of the rapimave package itself. Xref objects are generated
#'   by various methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ScoreSet object offers the following getter functions:
#' \itemize{
#'   \item{getID()} returns the Identifier or Accession of this cross-reference
#'   \item{getURL()} returns the database URL
#'   \item{getDB()} returns the database name
#'   \item{getDBVersion()} returns the database version
#'   \item{getOffset()} returns the numerical sequence offset compared to the database entry. 
#'      Only applicable for some databases. Returns NULL where not defined.
#' }
#'
#' @return a new R-API MaveDB Xref object.
new.xref <- function(data) {
	.data <- data
	expectedFields <- c(
		"identifier","url","dbversion","dbname"
	)
	if (!is.list(.data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.xref()")
	}
	structure(list(
		getID=function() .data$identifier,
		getURL=function() .data$url,
		getDB=function() .data$dbname,
		getDBVersion=function() .data$dbversion,
		getOffset=function() {
			if ("offset" %in% names(.data)) {
				.data$offset
			} else NULL
		}
	),class="rapimaveXref")
}

#' Print R-API MaveDB cross-reference
#'
#' Prints a human-readable summary of a R-API MaveDB cross-reference object:
#'
#' @param obj the object to print
#' @keywords MaveDB print
#' @export
print.rapimaveXref <- function(obj) {
	with(obj,cat(getDB(),"::",getID(),"\n"))
}


#' MaveDB R-API client constructor
#'
#' Creates an object of type rapimave.
#'
#' The resulting object offers the following functions:
#' \itemize{
#' 	\item{getAllUsers()} returns a list of all user objects. 
#'     See \code{\link{new.user}} for available methods on user objects.
#' 	\item{getUser(username)} returns the user object for the given username. 
#'     Usernames are usually ORCID IDs. See \url{https://orcid.org/} for information on ORCID.
#'     See \code{\link{new.user}} for available methods on user objects.
#' 	\item{findExperimentSets(filter)} returns a list of all ExperimentSet objects that match the given filter word.
#'     See \code{\link{new.experimentSet}} for available methods on ExperimentSet objects.
#' 	\item{getAllExperimentSets()} returns a list of all ExperimentSet objects.
#'     See \code{\link{new.experimentSet}} for available methods on ExperimentSet objects.
#' 	\item{getExperimentSet(urn)} returns the ExperimentSet object for the given URN.
#'     ExperimentSet URNs usually follow the syntax /^urn:mavedb:\\d+$/ .
#'     See \code{\link{new.experimentSet}} for available methods on ExperimentSet objects.
#' 	\item{getAllExperiments()} returns a list of all Experiment objects.
#'     See \code{\link{new.experiment}} for available methods on Experiment objects.
#' 	\item{findExperiments(filter)} returns a list of all Experiment objects that match the given filter word.
#'     See \code{\link{new.experiment}} for available methods on Experiment objects.
#' 	\item{getExperiment(urn)} returns the Experiment object for the given URN.
#'     Experiment URNs usually follow the syntax /^urn:mavedb:\\d+-\\w+/ .
#'     See \code{\link{new.experiment}} for available methods on Experiment objects.
#' 	\item{getAllScoreSets()} returns a list of all ScoreSet objects.
#'     See \code{\link{new.scoreSet}} for available methods on ScoreSet objects.
#' 	\item{findScoreSets(filter)} returns a list of all ScoreSet objects that match the given filter word.
#'     See \code{\link{new.scoreSet}} for available methods on ScoreSet objects.
#' 	\item{getScoreSet(urn)} returns the ScoreSet object for the given URN.
#'     ScoreSet URNs usually follow the syntax /^urn:mavedb:\\d+-\\w+-\\d+$/ .
#'     See \code{\link{new.scoreSet}} for available methods on ScoreSet objects.
#' 	\item{getScores(urn)} returns a \code{data.frame} with the scores for the given ScoreSet URN
#' 	\item{getCounts(urn)} returns a \code{data.frame} with the counts for the given ScoreSet URN
#' 	\item{getMetadata(urn)} returns a \code{data.frame} with the metadata for the given ScoreSet URN
#' }
#'
#' @param baseURL MaveDB API base-URL. Defaults to "https://www.mavedb.org/api/"
#' @param certifySSL Logical indicating whether to certify SSL certificates when querying
#'     the database. Defaults to \code{FALSE}.
#' @param encoding A string specifying the expected character encoding used by the webservice.
#'     Defaults to "UTF-8"
#' @return MaveDB R-API object with functions accessbile via the $ operator.
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' }
new.rapimave <- function(baseURL="https://www.mavedb.org/api/",certifySSL=FALSE,encoding="UTF-8") {

	library(httr)
	library(RJSONIO)

	if (!certifySSL) {
		set_config(config(ssl_verifypeer = 0L))
	}

	#TODO: implement ORCID checksum checking?

	#Regular expressions for URNs
	orcidRE <- "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$"
	expsRE  <- "^urn:mavedb:\\d{8}$"
	expRE   <- "^urn:mavedb:\\d{8}-\\w+$"
	scsRE   <- "^urn:mavedb:\\d{8}-\\w+-\\d+$"

	getAllUsers <- function() {
		url <- paste0(baseURL,"users/")
		htr <- GET(url,query=list(format="json"))
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(lapply(returnData, new.user))
			} else {
				return(list())
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getUser <- function(username) {
		if (!grepl(orcidRE,username)) {
			stop(username," is not a valid ORCID!")
		}
		url <- paste0(baseURL,"users/",username,"/")
		htr <- GET(url,query=list(format="json"))
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

	getAllExperimentSets <- function() findExperimentSets(NULL)

	findExperimentSets <- function(filter) {
		url <- paste0(baseURL,"experimentsets/")
		if (!is.null(filter)) {
			params <- list(format="json",target=filter)
		} else {
			params <- list(format="json")
		}
		htr <- GET(url,query=params)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(lapply(returnData, new.experimentSet))
			} else {
				return(list())
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getExperimentSet <- function(urn) {
		if (!grepl(expsRE,urn)) {
			stop(urn," is not a valid Experiment Set URN!")
		}
		url <- paste0(baseURL,"experimentsets/",urn,"/")
		htr <- GET(url,query=list(format="json"))
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

	getAllExperiments <- function() findExperiments(NULL)

	findExperiments <- function(filter) {
		url <- paste0(baseURL,"experiments/")
		if (!is.null(filter)) {
			params <- list(format="json",target=filter)
		} else {
			params <- list(format="json")
		}
		htr <- GET(url,query=params)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(lapply(returnData, new.experiment))
			} else {
				return(list())
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getExperiment <- function(urn) {
		if (!grepl(expRE,urn)) {
			stop(urn," is not a valid Experiment URN!")
		}
		url <- paste0(baseURL,"experiments/",urn,"/")
		htr <- GET(url,query=list(format="json"))
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.experiment(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such Experiment!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getAllScoreSets <- function() findScoreSets(NULL)

	findScoreSets <- function(filter) {
		url <- paste0(baseURL,"scoresets/")
		if (!is.null(filter)) {
			params <- list(format="json",target=filter)
		} else {
			params <- list(format="json")
		}
		htr <- GET(url,query=params)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(lapply(returnData, new.scoreSet))
			} else {
				return(list())
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getScoreSet <- function(urn) {
		if (!grepl(scsRE,urn)) {
			stop(urn," is not a valid ScoreSet URN!")
		}
		url <- paste0(baseURL,"scoresets/",urn,"/")
		htr <- GET(url,query=list(format="json"))
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.scoreSet(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	extract.groups <- function(x, re) {
		matches <- regexpr(re,x,perl=TRUE)
		start <- attr(matches,"capture.start")
		end <- start + attr(matches,"capture.length") - 1
		do.call(cbind,lapply(1:ncol(start), function(i) {
			sapply(1:nrow(start),function(j){
				if (start[j,i] > -1) substr(x[[j]],start[j,i],end[j,i]) else NA
			})
		}))
	}

	#custom parsing function, as the new comment lines are not compatible with the default
	#read.csv function
	parseCSV <- function(returnData,forceNumeric=TRUE) {
		lines <- strsplit(returnData,"\n")[[1]]
		isHeader <- grepl("^#",lines)
		if (all(isHeader)) {
			#then it's an empty file!
			return(NA)
		}
		mainLines <- lines[!isHeader]
		cnames <- strsplit(mainLines[[1]],",")[[1]]
		values <- strsplit(mainLines[-1],",")
		scoreTable <- do.call(data.frame,c(lapply(1:length(cnames),function(i) {
			colvals <- trimws(sapply(values,`[`,i))
			if (forceNumeric && !(cnames[[i]] %in% c("accession","urn","hgvs","hgvs_nt","hgvs_pro"))) {
				colvals <- suppressWarnings(as.numeric(colvals))
			}
			colvals
		}),stringsAsFactors=FALSE))
		colnames(scoreTable) <- trimws(cnames)

		headerLines <- lines[isHeader]
		#extract only structured (key-value-pair) header lines 
		headerLines <- headerLines[which(grepl("# [^:]+: .+",headerLines))]
		if (length(headerLines) > 0) {
			keyval <- extract.groups(headerLines,"# ([^:]+): (.+)")
			for (i in 1:length(headerLines)) {
				attr(scoreTable,keyval[i,1]) <- keyval[i,2]
			}
		}
		return(scoreTable)
	}

	getScores <- function(urn) {
		if (!grepl(scsRE,urn)) {
			stop(urn," is not a valid ScoreSet URN!")
		}
		url <- paste0(baseURL,"scoresets/",urn,"/scores/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- content(htr,as="text",encoding=encoding)
			if (nchar(returnData) > 0) {
				# con <- textConnection(returnData,open="r")
				# scoreTable <- read.csv(con,comment.char="#",stringsAsFactors=FALSE)
				# close(con)
				scoreTable <- parseCSV(returnData)
				return(scoreTable)
			} else {
				return(NA)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getCounts <- function(urn) {
		if (!grepl(scsRE,urn)) {
			stop(urn," is not a valid ScoreSet URN!")
		}
		url <- paste0(baseURL,"scoresets/",urn,"/counts/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- content(htr,as="text",encoding=encoding)
			if (nchar(returnData) > 0) {
				# con <- textConnection(returnData,open="r")
				# countTable <- read.csv(con,comment.char="#",stringsAsFactors=FALSE)
				# close(con)
				countTable <- parseCSV(returnData)
				return(countTable)
			} else {
				return(NA)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getMetadata <- function(urn) {
		if (!grepl(scsRE,urn)) {
			stop(urn," is not a valid ScoreSet URN!")
		}
		url <- paste0(baseURL,"scoresets/",urn,"/metadata/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- content(htr,as="text",encoding=encoding)
			if (nchar(returnData) > 0) {
				# con <- textConnection(returnData,open="r")
				# mdTable <- read.csv(con,comment.char="#",stringsAsFactors=FALSE)
				# close(con)
				mdTable <- parseCSV(returnData,forceNumeric=FALSE)
				return(mdTable)
			} else {
				return(NA)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	structure(list(
		getAllUsers=getAllUsers,
		getUser=getUser,
		findExperimentSets=findExperimentSets,
		getAllExperimentSets=getAllExperimentSets,
		getExperimentSet=getExperimentSet,
		findExperiments=findExperiments,
		getAllExperiments=getAllExperiments,
		getExperiment=getExperiment,
		findScoreSets=findScoreSets,
		getAllScoreSets=getAllScoreSets,
		getScoreSet=getScoreSet,
		getScores=getScores,
		getCounts=getCounts,
		getMetadata=getMetadata
	),class="rapimave")
}


#' Print R-API MaveDB object
#'
#' Prints a human-readable summary of a R-API MaveDB object.
#'
#' Prints a list of available functions for the object.
#'
#' @param obj the object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave)
#' }
print.rapimave <- function(obj) {
	cat(paste0(
		"A rapimave API object.\n\n",
		"The following functions are available:\n",
		paste0(names(obj),"()",collapse="; ")
	),"\n")
}
