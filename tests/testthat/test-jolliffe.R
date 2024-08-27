test_that("console is used properly", {
  expect_output(jolliffe_test(PlantGrowth$weight, PlantGrowth$group))
  expect_output(
    jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
      console = FALSE
    ),
    regexp = NA
  )
})

test_that("wrong alpha value is detected", {
  expect_warning(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = 999
  ))
  expect_warning(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = "0.1"
  ))
  expect_no_warning(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = 0.1
  ))
  expect_no_warning(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = 0.01
  ))
})

test_that("alerts are shown properly", {
  expect_no_message(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = 0.1
  ))
  expect_message(
    jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
      alpha = 0.01
    ),
    regexp = "No differences"
  )
  expect_message(
    jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
      alpha = 0.2
    ),
    regexp = "All differences"
  )
})

test_that("error due to non-existent column is displayed correctly", {
  modelo <- lm(data = PlantGrowth, weight ~ group)
  expect_no_error(jolliffe_test(modelo, "group"))
  expect_error(jolliffe_test(modelo, "columna_inexistente"),
    regexp = "Column.*can't be found in"
  )
})

test_that("error due to zero d.f. for error is displayed correctly", {
  expect_error(jolliffe_test(USArrests$Murder, row.names(USArrests)),
    regexp = "equal to zero"
  )
})

test_that("error due to less than three treatments is displayed correctly", {
  expect_error(jolliffe_test(ToothGrowth$len, ToothGrowth$supp),
    regexp = "at least.*treatments"
  )
})

test_that("wrong object type or length returns error", {
  expect_error(jolliffe_test(PlantGrowth$weight[1:29], PlantGrowth$group),
    regexp = "variable lengths"
  )
  expect_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group[1:29]),
    regexp = "variable lengths"
  )
  expect_error(jolliffe_test(PlantGrowth$weight, "PlantGrowth$group"))
  expect_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    show_plot = "Yes"
  ))
  expect_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    console = "Yes"
  ))
  expect_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    abline_options = 2
  ))
})

test_that("clustering method is handled correctly", {
  expect_no_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    method = "slca"
  ))
  expect_no_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    method = "clca"
  ))
  expect_warning(
    jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
      method = "other method"
    ),
    regexp = "Invalid.*name"
  )
})

test_that("plots are properly rendered", {
  expect_no_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    col = "red"
  ))
  expect_no_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    abline_options = list(col = "orange")
  ))
  expect_no_error(jolliffe_test(PlantGrowth$weight, PlantGrowth$group,
    abline_options = list(col = "purple"), col = "green"
  ))
})

test_that("returns are correct", {
  expect_type(jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$stats,
    type = "list"
  )
  expect_type(jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$groups,
    type = "list"
  )
  expect_type(jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$groups[[1]],
    type = "integer"
  )
  expect_type(jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$parameters,
    type = "list"
  )
  expect_type(jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$parameters$n,
    type = "integer"
  )
  expect_type(jolliffe_test(mtcars$mpg, mtcars$gear)$parameters$n,
    type = "double"
  )
  expect_length(jolliffe_test(
    PlantGrowth$weight,
    PlantGrowth$group
  )$parameters, n = 6)

  if (length(
    jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$parameters
  ) == 6) {
    for (i in c(1, 4)) {
      expect_type(
        jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$parameters[[i]],
        type = "integer"
      )
    }

    for (i in c(3, 5:6)) {
      expect_type(
        jolliffe_test(
          PlantGrowth$weight,
          PlantGrowth$group
        )$parameters[[i]],
        type = "double"
      )
    }
  }

  expect_s3_class(
    jolliffe_test(PlantGrowth$weight, PlantGrowth$group)$dendrogram_data,
    "hclust"
  )
})
