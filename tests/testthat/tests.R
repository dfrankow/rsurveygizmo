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
