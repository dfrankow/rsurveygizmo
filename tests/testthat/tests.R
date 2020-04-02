library("Rsurveygizmo")

context("Sub-questions")

test_that("Test get_sub_questions", {
	test_qs <- structure(
		list(
			id = c(52L, 185L, 211L, 333L, 340L, 345L, 346L),
			sub_question_skus = list(
				NULL,
				NULL,
				list(
					`0` = 329L,
					AAA = 215L
				),
				c(340L, 341L, 342L, 359L),
				NULL,
				c(346L, 353L, 350L),
				NULL
			),
			varname = list(
				list(),
				"flat_name",
				list(
					`329` = "nest.aa",
					`355` = "nest.bb"
				),
				list(
					`340` = "kk",
					`341` = "ll",
					`342` = "mm",
					`359` = "nn"
				),
				list(),
				list(
					`346` = "nest2.aaa",
					`353` = "nest2.bbb",
					`350` = "nest2.ccc"
				),
				list()
			)
		),
		row.names = c(1L, 12L, 14L, 25L, 26L, 30L, 31L),
		class = "data.frame"
	)
	subq <- get_sub_questions(test_qs)

	# TODO(dan): I might rather have a plain data frame than this one
	# with lists that have named elements.
	expect_equal(subq,
				 structure(
				 	list(
				 		parent_id = list(
				 			`0` = 211L,
				 			AAA = 211L,
				 			`340` = 333L,
				 			`341` = 333L,
				 			`342` = 333L,
				 			`359` = 333L,
				 			`346` = 345L,
				 			`353` = 345L,
				 			`350` = 345L
				 		),
				 		sub_question_id = list(
				 			`0` = 329L,
				 			AAA = 215L,
				 			`340` = 340L,
				 			`341` = 341L,
				 			`342` = 342L,
				 			`359` = 359L,
				 			`346` = 346L,
				 			`353` = 353L,
				 			`350` = 350L
				 		),
				 		sub_varname = list(
				 			`0` = "nest.aa",
				 			AAA = "nest.bb",
				 			`340` = "kk",
				 			`341` = "ll",
				 			`342` = "mm",
				 			`359` = "nn",
				 			`346` = "nest2.aaa",
				 			`353` = "nest2.bbb",
				 			`350` = "nest2.ccc"
				 		)
				 	),
				 	class = "data.frame",
				 	row.names = c(NA,-9L)
				 ))
})

test_that("Test get_question_options", {
	test_qs <-
		structure(
			list(
				id = c(52L, 185L, 211L),
				`_type` = c("SurveyAction", "SurveyQuestion", "SurveyQuestion"),
				`_subtype` = c("javascript", "radio", "table"),
				title = structure(
					list(
						English = c(
							"title1 to ignore",
							"please select one",
							"How would you rate?"
						)
					),
					row.names = c(1L, 12L, 14L),
					class = "data.frame"
				),
				shortname = c(NA, "most_important", "pre"),
				varname = list(
					list(),
					"most_important",
					list(
						`329` = "pre.word1",
						`355` = "pre.word2",
						`215` = "pre.word3"
					)
				),
				description = list(list(), list(), list()),
				properties = structure(
					list(
						hidden = c(FALSE, FALSE, FALSE),
						disabled = c(FALSE, FALSE, FALSE),
						option_sort = c(NA, FALSE, FALSE),
						subtype = c(NA_character_,
									NA_character_, NA_character_)
					),
					row.names = c(1L, 12L, 14L),
					class = "data.frame"
				),
				options = list(
					structure(
						list(),
						.Names = character(0),
						row.names = integer(0),
						class = "data.frame"
					),
					structure(
						list(
							id = c(
								10348L,
								10357L,
								10358L
							),
							`_type` = c(
								"SurveyOption",
								"SurveyOption",
								"SurveyOption"
							),
							title = structure(
								list(
									English = c(
										"blue1",
										"blue2",
										"blue3"
									)
								),
								class = "data.frame",
								row.names = c(NA, 3L)
							),
							value = c(
								"vblue1",
								"vblue2",
								"vblue3"
							),
							properties = structure(
								list(
									disabled = c(FALSE, FALSE, TRUE)
								),
								class = "data.frame",
								row.names = c(NA, 3L)
							)
						),
						class = "data.frame",
						row.names = c(NA, 3L)
					),
					structure(
						list(
							id = 10417:10421,
							`_type` = c(
								"SurveyOption",
								"SurveyOption",
								"SurveyOption",
								"SurveyOption",
								"SurveyOption"
							),
							title = structure(
								list(
									English = c(
										"Strongly approve",
										"Somewhat approve",
										"Don't know",
										"Somewhat disapprove",
										"Strongly disapprove"
									)
								),
								class = "data.frame",
								row.names = c(NA, 5L)
							),
							value = c(
								"vStrongly approve",
								"vSomewhat approve",
								"vDon't know",
								"vSomewhat disapprove",
								"vStrongly disapprove"
							),
							properties = structure(
								list(disabled = c(FALSE, FALSE, FALSE, FALSE, FALSE)),
								class = "data.frame",
								row.names = c(NA, 5L)
							)
						),
						class = "data.frame",
						row.names = c(NA, 5L)
					)
				),
				sub_question_skus = list(
					NULL,
					NULL,
					list(
						`0` = 329L,
						`1` = 355L,
						aa = 215L
					)
				)
			),
			row.names = c(1L, 12L, 14L), class = "data.frame")
	opts <- get_question_options(test_qs)
	expect_equal(
		opts,
		data.frame(
			option_id = c(10348L, 10357L, 10358L, 10417L,
						  10418L, 10419L, 10420L, 10421L),
			title = c("blue1", "blue2", "blue3", "Strongly approve",
					  "Somewhat approve", "Don't know",
					  "Somewhat disapprove", "Strongly disapprove"),
			title_language = c("English", "English", "English", "English",
							   "English", "English", "English", "English"),
			value = c("vblue1", "vblue2", "vblue3", "vStrongly approve",
					  "vSomewhat approve", "vDon't know", "vSomewhat disapprove",
					  "vStrongly disapprove"),
			order = c(1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L),
			question_id = c(185L, 185L, 185L, 211L, 211L, 211L, 211L, 211L),
			question_subtype = c("radio", "radio", "radio", "table",
								 "table", "table", "table", "table")
		)
	)
})


test_that("Test get_question_options disabled", {
	test_qs <-
		structure(
			list(
				id = 185L,
				`_type` = "SurveyQuestion",
				`_subtype` = "radio",
				title = structure(
					list(English = "please select one"),
					row.names = 12L,
					class = "data.frame"
				),
				shortname = "most_important",
				varname = list("most_important"),
				description = list(list()),
				has_showhide_deps = FALSE,
				comment = FALSE,
				properties = structure(
					list(
						disabled = FALSE,
						option_sort = FALSE,
						subtype = NA_character_
					),
					row.names = 3L,
					class = "data.frame"
				),
				options = list(structure(
					list(
						id = c(
							10348L,
							10358L,
							10719L
						),
						`_type` = c(
							"SurveyOption",
							"SurveyOption",
							"SurveyOption"
						),
						title = structure(
							list(
								English = c(
									"val1",
									"val3",
									"val3"
								)
							),
							class = "data.frame",
							row.names = c(NA, 3L)
						),
						value = c(
							"val1",
							"val3",
							"val3"
						),
						properties = structure(
							list(
								disabled = c(FALSE, TRUE, FALSE)
							),
							class = "data.frame",
							row.names = c(NA, 3L)
						)
					),
					class = "data.frame",
					row.names = c(NA, 3L)
				)),
				sub_question_skus = list(							  									 									  																						   								 										 											 													  										   																	NULL)), row.names = 12L, class = "data.frame")
	opts <- get_question_options(test_qs)
	expect_equal(nrow(opts), 2)
	# option 10358 disappeared because it was disabled
	expect_equal(opts$option_id, c(10348, 10719))

	expected_df <- data.frame(option_id = c(10348L, 10719L),
							  title = c("val1", "val3"),
							  title_language = c("English", "English"),
							  value = c("val1", "val3"),
							  order = 1:2,
							  question_id = c(185L, 185L),
							  question_subtype = c("radio", "radio"))

	# Compare data frames element-wise because the rownames are somehow different.
	# TODO(dan): expect_equal(opts, expected_df)
	# TODO(dan): Make default, plain rownames.
	# rownames(expected_df) <- c("1", "3")
	expect_true(all(opts == expected_df))
})

# TODO(dan): Test behavior when a question has both a shortname and varname (SPSS var name).
#            Right now, I think it will take shortname first.
# TODO(dan): Test behavior when a sub-question has both a shortname and varname (SPSS var name)
#            from its parent question.
#            Right now, I think it will take shortname first.
