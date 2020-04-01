#' Download survey response data from Survey Gizmo, storing an R dataframe.
#'
#' This function downloads a survey's response data from Survey Gizmo (SG), saving
#' the returned data as an R \code{\link{data.frame}}. Because SG limits the size of JSON data pulls via the API (currently, the limit is 250),
#' it calculates the number of pulls needed to download the entire response set and binds the returned frames.
#' To ensure that variable names are interpretable in the returned data frame, it is strongly recommended
#' that users first assign question \href{https://help.surveygizmo.com/help/article/link/using-question-aliases}{aliases} to each question prior to utilizing this function.
#' For sub-questions (e.g., a radio button grid), assign SPSS variable names.
#'
#' @param surveyid The survey's unique SG ID number (in V4 of the API, the portion of the \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-sub-object}{surveyresponse} call URL which follows "id/", e.g.: "...build/id/1234567"
#' @param api The user's private API key for Survey Gizmo
#' @param secret The user's private secret for Survey Gizmo
#' @param locale The user's Survey Gizmo domain. Can be US (default), EU or CA.
#' @param verbose When true (the default), download progress is printed to standard output.
#' @param mergecampaign When true, contact emails are downloaded from the SG "contact" object and merged with the survey responses. Note: this parameter should \emph{only} be used with survey projects that have an active email campaign.
#' @param small Only merge email address when mergecampaign is true.
#' @param completes_only When true (the Default), survey responses with a status of
#' "Complete" are saved. Responses with a status of "disqualified", "partial", etc.
#' are deleted (see \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-returned-fields}{this link} for documentation)
#' @param delete_sys_vars When true, deletes SG system variables (i.e., fields
#' of the form [[variable(...)]] that do \emph{not} contain question responses.
#' \strong{NOTE:} When set to false, pullsg will change the stub of SG system variables to \emph{sys_*}
#' See \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-returned-fields}{this link}
#' for more information about fields returned by SG via the API).
#' @param keep_geo_vars When true (the default), SG's geographic variables
#' (lat/long, city, etc.), which are estimated using the respondent's IP address,
#' are preserved in the returned data.frame. This option also changes the name of
#' the geographic variables to:
#' \itemize{
#'   \item \emph{rsp_lng}
#'   \item \emph{rsp_lat}
#'   \item \emph{rsp_cntr}
#'   \item \emph{rsp_city}
#'   \item \emph{rsp_regi}
#'   \item \emph{rsp_post}
#'   }
#' @param reset_row_names When true (the default), resets row names to 1, 2,..N in the returned dataframe
#' @param default_var_name_policy Variable name for those without an alias.
#'  "id" uses only variable names,
#'  "title" uses some of the question title,
#'  "title_plus_id" uses some of the title and
#'  appends the stub "_ID[question_numer] to questions \emph{without} an alias
#'  to avoid variable name conflicts.
#' @param clean This option performs three transformations to the returned data.frame:
#' \enumerate{
#'   \item attempts to coerce vectors to numeric if all values are numbers or "" (uses \code{\link{type.convert}})
#'   \item sets 'delete_sys_vars' to true
#'   \item removes other non-survey-question variables returned by the Survey Gizmo API, including: "contactid", "istestdata", "sessionid", "language", "ilinkid", and "sresponsecomment" (as of V4 of the SG API)
#' }

#' @importFrom jsonlite fromJSON
#' @export


pullsg <- function(surveyid, api, secret, locale="US", completes_only=TRUE, verbose=TRUE,
				   default_var_name_policy=c("id", "title", "title_plus_id"),
				   mergecampaign=FALSE, delete_sys_vars=FALSE, keep_geo_vars=TRUE,
				   clean=FALSE, reset_row_names=TRUE, small=FALSE) {
	default_var_name_policy <- match.arg(default_var_name_policy)

	options(stringsAsFactors=FALSE)
	if(small & mergecampaign==FALSE) warning('\nThe "small" parameter should be false when "mergecampaign" is false. This parameter was ignored.')
	# Set hard-coded URL parameters
	token <- paste0('?api_token=', api, '&api_token_secret=', secret) # Must be in the first trailing URL position

	if (locale == "US") {
		url <- "https://restapi.surveygizmo.com/v4/survey/"
	} else if (locale == "EU") {
		url <- "https://restapi.surveygizmo.eu/v4/survey/"
	} else if (locale == "CA") {
		url <- "https://restapica.surveygizmo.com/v4/survey/"
	} else {
		stop("Locale not known. Please use one of US, EU or CA")
	}

	response <- "/surveyresponse/"
	question <- "/surveyquestion/"
	pages    <- "&page="
	results  <- "&resultsperpage=100"

	#Build local parameters
	filturl  <- paste0("&filter[field][0]=status&filter[operator][0]==",
					   "&filter[value][0]=Complete")
	if (completes_only) {
		filt = filturl
	} else {
		filt <- ""
	}

	lc_base  <- paste0(url, surveyid, response, token, filt)
	lc_furl  <- paste0(url, surveyid, response, token, results, filt, pages)
	lc_qurl  <- paste0(url, surveyid, question, token)

	message("\n  Retrieving survey summary data:")
	progb <- txtProgressBar(min = 0, max = 4, style = 3)
	setTxtProgressBar(progb, 1)

	# Get base response parameters of the survey and extract N
	lc_base      <- fromJSON(txt=lc_base)
	lc_resp_cnt  <- as.integer(lc_base[['total_count']])
	setTxtProgressBar(progb, 2)

	# Calculate page number (starting with 1) based on 100 responses per call
	lc_respnum  <- ceiling(lc_resp_cnt/100)

	# Retrieve the question list from the "/surveyquestion/" call
	setTxtProgressBar(progb, 3)
	# get id and qtext of questions
	lc_qs <- get_questions(lc_qurl, default_var_name_policy)[,c('id', 'qtext')]
	close(progb)

	# Retrieve the response data with the "/surveyresponse/" call
	message(paste0("\n  Retrieving survey response data (", lc_respnum, " pages):"))
	progb <- txtProgressBar(min = 0, max = lc_respnum, style = 3)
	for(i in 1:lc_respnum){
		sg_return_url  <- paste0(lc_furl, i)
		sg_return_data <- fromJSON(txt=sg_return_url)
		sg_return_data <- as.data.frame(sg_return_data$data)
		assign(paste0("lc_survey_page", i), sg_return_data)
		setTxtProgressBar(progb, i)
	}
	close(progb)

	#Bind the returned frames
	lc_fullset <- do.call("rbind", mget(ls(pattern="lc_survey_page")))

	# Extract the column names of the responsedata returned from SG
	lc_names <- names(lc_fullset)

	# Define patterns to drop from the vector of column names
	patterns = "[[:punct:]]|[[:space:]]|question|shown|STANDARD"

	# Hidden values generally take form of: 'question(76), option(0)'
	# If a hidden value is assigned to more than one field (e.g., in
	# an Exact Target Pull action, then an '...option(1)... option(N)...'
	# format is used. SG password actions also have a qid with two option
	# fields. Here, we extract the "option 0" parameter from the questions
	# so they will merge correctly with the array of names.
	lc_names <- lapply(lc_names, gsub, patt= ", option(0)",
					   rep="", fixed=TRUE)

	# Now remove the patterns saved above.
	lc_names <- lapply(lc_names, gsub, patt= patterns, rep="")

	# Save the SG geography variables derived from respondent IP
	if(keep_geo_vars) {
		lc_names <- lapply(lc_names, gsub, patt="^variableLONG$",       rep="rsp_lng")
		lc_names <- lapply(lc_names, gsub, patt="^variableLAT$",        rep="rsp_lat")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOCOUNTRY$", rep="rsp_cntry")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOCITY$",    rep="rsp_city")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOREGION$",  rep="rsp_region")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOPOSTAL$",  rep="rsp_post")
	}


	# Now rename SG system fields, subtituting "sys_" for "variable"
	lc_names <- lapply(lc_names, gsub, patt= "variable",
					   rep="sys_")

	# Now lower case the names and convert to data.frame
	lc_names <- data.frame(id=tolower(lc_names))

	# Index to preserve the inital column sorting
	lc_names$index <- seq_len(nrow(lc_names))

	# Left join of the cleaned name table
	lc_names <- merge(lc_names, lc_qs, by="id", all.x=TRUE)
	lc_names <- lc_names[order(lc_names$index),]
	rownames(lc_names) <- NULL # reset the row.names attribute

	# Replace variable name with qtext where available
	lc_names$id   <- ifelse(is.na(lc_names$qtext),
							lc_names$id, lc_names$qtext)

	# Subset the vector of cleaned names and replace the full_set names
	lc_names <- lc_names[,'id']
	colnames(lc_fullset) <- lc_names

	# Removes 'sys_*' vars that end in a digit
	# Removes residual number-only vars as they will falsely align in rbind
	drops <- "^[[:digit:]]+$|sys_[[:digit:]]+"
	set <- lc_fullset[, -grep(drops, names(lc_fullset), perl=TRUE)]

	if(delete_sys_vars | clean) set <- set[, -grep("^sys_", names(set),
												   ignore.case=TRUE)]

	# Other variables to drop per the "clean" parameter (V4 API)
	otherdrops <- paste0("contactid|istestdata|sessionid|",
						 "language|ilinkid|sresponsecomment")

	if(clean) set <- set[, -grep(otherdrops, names(set), ignore.case=TRUE)]
	if(clean) set <- as.data.frame(lapply(set, type.convert, na.strings = "", as.is=TRUE))
	if(reset_row_names) row.names(set) <- NULL

	# Format Survey Gizmo date fields
	set[, c('datestarted', 'datesubmitted')] <- lapply(set[, c('datestarted', 'datesubmitted')],
														   as.POSIXct, format="%Y-%m-%d %H:%M")

	# Merge data from the contact object with survey returns
	if(mergecampaign) {
		campaign <- Rsurveygizmo::pullsg_campaign(surveyid, api, secret, locale=locale, small=small)
		if(sum(duplicated(campaign$contactid))){
			message("Campaign data for survey id# ", surveyid, " contains duplicate contactid records.")
			message("The most recent record will be selected and merged with the survey data. \nHowever, we strongly, ",
					"recommend that you merge campaign data separately after examining duplicate contactids by:\n",
					"(1) Re-downloading the response data for survey ", surveyid, ", choosing the option 'mergecampaign=FALSE', and\n",
					"(2) Running the 'pullsg_campaign' separately, merging records after analyzing and discarding duplicates.")
			campaign <- campaign[order(campaign[, "contactid"], -as.numeric(campaign[, "dlastsent"])), ]
			campaign <- campaign[!duplicated(campaign$contactid), ]
	}

	if(!is.null(campaign))  set <- merge(campaign, set, by="contactid", all.y=TRUE)
	if(is.null(campaign))   message('Because no email campaign(s) were located for survey "', surveyid, '", email addresses will not be merged with the survey returns.')
	}

	if(verbose) message('\n\nData retrieval for survey "', surveyid, '" is complete!\n')
	return(set)
}

#' Fetch the question information, and make up variable names in the qtext column.
#'
#' qtext (question name) is whichever is non-NA from shortname, sub_varname,
#' and the title made up from default_var_name_policy.
#'
#' Returns a data frame with all columns.
get_questions <- function(lc_qurl, default_var_name_policy) {
	# Retrieve the question list from the "/surveyquestion/" call
	lc_qs   <- jsonlite::fromJSON(txt=lc_qurl)

	lc_qs   <- as.data.frame(lc_qs$data)

	if (default_var_name_policy == "id") {
		lc_qs$qtext <- paste0("var", lc_qs$id)
	} else {
		# Extract question text for questions without a defined SG alias, stripping
		# html tags, punctuation, and spaces, keeping the first 35 characters and
		# appending the question id.
		lc_qs$qtext <- gsub("<.*?>", "", lc_qs$title$English)
		lc_qs$qtext <- gsub("[[:punct:]]", "", lc_qs$qtext)
		setTxtProgressBar(progb, 4)

		if(default_var_name_policy == "title_plus_id") {
			lc_qs$qtext <- paste0(substr(gsub("[[:space:]]", ".", lc_qs$qtext), 1, 35),
								  "_ID", lc_qs$id)
		} else {
			stopifnot(default_var_name_policy == "title")
			lc_qs$qtext <- paste0(substr(gsub("[[:space:]]", ".", lc_qs$qtext), 1, 75))
		}
		lc_qs$qtext <- trimws(lc_qs$qtext)
	}

	sub_ques <- get_sub_questions(lc_qs)
	lc_qs <- merge(lc_qs, sub_ques[,c('sub_question_id', 'sub_varname')],
				   by.x="id", by.y="sub_question_id",
				   all.x=TRUE)

	# shortname, then sub_varname, then qtext
	# TODO(dan): Any other names we can use for sub vars?
	lc_qs$qtext <- ifelse(
		!(is.na(lc_qs$shortname) | lc_qs$shortname==""),
		lc_qs$shortname,
		ifelse(!is.na(lc_qs$sub_varname), lc_qs$sub_varname, lc_qs$qtext))

	# Drop instructional messages.
	lc_qs[lc_qs$`_type` !="SurveyDecorative",]
}

#' Return a data frame with parent_id, sub_question_id, and sub_varname
#'
#' sub_varname comes from from SPSS variable name, if any.
get_sub_questions <- function(lc_qs) {
	df <- lc_qs[,c('id', 'sub_question_skus', 'varname')]
	# get only rows with 'sub_question_skus' not NULL
	df$sub_question_skus <- replace(
		df$sub_question_skus,
		sapply(df$sub_question_skus, is.null), NA)
	df <- df[!is.na(df$sub_question_skus),]
	# make varname NA for rows with 'varname' NULL or list()
	df$varname <- replace(
		df$varname,
		sapply(df$varname, function(x) { is.null(x) | length(x) == 0 }), NA)
	df2 <- as.data.frame(do.call(
		rbind,
		Map(f=cbind,
			id=df$id,
			sub_question_id=df$sub_question_skus,
			varname=df$varname)),
		row.names = FALSE)
	colnames(df2) <- c('parent_id', 'sub_question_id', 'sub_varname')
	# Make this a simple data frame, with no lists, no named columns
	df2 <- sapply(df2, function(col) unlist(unname(col)))
}

#' Get all the options for all questions with options (e.g., radio, table, checkbox).
#'
#' Return a data frame with option_id, option_type, title, title_language, value, order,
#' question_id, question_type, question_subtype.
get_question_options <- function(question_df) {
	df <- question_df[,c('id', '_type', '_subtype', 'options', 'properties')]
	# get only rows with 'options' != list()
	df$options <- replace(
		df$options,
		sapply(df$options, function(x) { is.null(x) | length(x) == 0 }), NA)
	df <- df[!is.na(df$options),]
	# Expect only SurveyQuestion:
	stopifnot(unique(df$`_type`) == c('SurveyQuestion'))

	# not dealing with properties right now, so let's assume they're not set
	stopifnot(sum(df$properties$hidden)==0)
	stopifnot(sum(df$properties$option_sort)==0)

	result <- data.frame()
	for (idx in 1:nrow(df)) {
		# the 'options' column of df is a list with 1 element.  Unlist it.
		the_options <- df[idx,c('options')][[1]]
		# remove row names so they don't collide with each other
		rownames(the_options) <- c()
		# throw away disabled options
		if ('disabled' %in% names(the_options)) {
			the_options <- the_options[!the_options$disabled,]
		}
		if (is.data.frame(the_options$title)) {
			# this is a data frame with one column, the title
			# for example, the one column may be "English"
			# so the column name may be the language?
			# so this may become more complicated if there are multiple languages?
			stopifnot(ncol(the_options$title) == 1)
			# keep the language info
			the_options$title_language <- colnames(the_options$title)
			# make it a plain column
			the_options$title <- the_options$title[[1]]
		}
		# just be triple sure and preserve the order instead of
		# relying on the option id
		the_options$order <- seq(1,length(the_options$title))
		the_options <- the_options[
			,c('id', 'title', 'title_language', 'value', 'order')]
		# rename column
		names(the_options)[1] <- 'option_id'

		the_options$question_id <- df[idx,c('id')]
		the_options$question_subtype <- df[idx,c('_subtype')]

		result <- rbind(result, the_options)
	}
	result
}
