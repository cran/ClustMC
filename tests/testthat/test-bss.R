test_that("console is used properly", {
  expect_output(bss_test(PlantGrowth$weight, PlantGrowth$group))
  expect_output(
    bss_test(PlantGrowth$weight, PlantGrowth$group,
      console = FALSE
    ),
    regexp = NA
  )
})

test_that("wrong alpha value is detected", {
  expect_warning(bss_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = 999
  ))
  expect_warning(bss_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = "0.1"
  ))
  expect_no_warning(bss_test(PlantGrowth$weight, PlantGrowth$group,
    alpha = 0.1
  ))
  expect_no_warning(bss_test(iris$Sepal.Length, iris$Species,
    alpha = 0.01
  ))
})

test_that("alerts are shown properly", {
  expect_no_message(bss_test(PlantGrowth$weight, PlantGrowth$group))
  expect_message(bss_test(PlantGrowth$weight, PlantGrowth$group, alpha = 0.9),
    regexp = "All differences"
  )
})

test_that("error due to non-existent column is displayed correctly", {
  model <- lm(data = PlantGrowth, weight ~ group)
  expect_no_error(bss_test(model, "group"))
  expect_error(bss_test(model, "columna_inexistente"),
    regexp = "Column.*can't be found in"
  )
})

test_that("error due to no overall significant F is displayed correctly", {
  expect_error(bss_test(PlantGrowth$weight, PlantGrowth$group, alpha = 0.001),
    regexp = "No treatments are significantly different"
  )
  model <- lm(PlantGrowth$weight ~ PlantGrowth$group)
  expect_error(bss_test(model, "PlantGrowth$group", alpha = 0.001),
    regexp = "No treatments are significantly different"
  )
})

test_that("error due to less than three treatments is displayed correctly", {
  expect_error(bss_test(mtcars$mpg, mtcars$am),
    regexp = "at least.*treatments"
  )
})

test_that("wrong object type or length returns error", {
  expect_error(bss_test(PlantGrowth$weight[1:29], PlantGrowth$group),
    regexp = "variable lengths"
  )
  expect_error(bss_test(PlantGrowth$weight, PlantGrowth$group[1:29]),
    regexp = "variable lengths"
  )
  expect_error(bss_test(PlantGrowth$weight, "PlantGrowth$group"))
  expect_error(bss_test(PlantGrowth$weight, PlantGrowth$group,
    show_plot = "Yes"
  ))
  expect_error(bss_test(PlantGrowth$weight, PlantGrowth$group,
    console = "Yes"
  ))
  expect_error(bss_test(PlantGrowth$weight, PlantGrowth$group,
    abline_options = 2
  ))
})

test_that("models work properly", {
  model <- lm(volume ~ variety + as.factor(bromate), data = bread)
  expect_no_error(bss_test(model, "variety"))
  model <- lm(Sepal.Length ~ ., data = iris)
  expect_no_error(bss_test(model, "Species"))
  model <- lm(Sepal.Length ~ Sepal.Width * Species * Petal.Length, data = iris)
  expect_no_error(bss_test(model, "Species"))
})

test_that("plots are properly rendered", {
  expect_no_error(bss_test(PlantGrowth$weight, PlantGrowth$group, col = "red"))
  expect_no_error(bss_test(PlantGrowth$weight, PlantGrowth$group,
    abline_options = list(col = "orange")
  ))
  expect_no_error(bss_test(PlantGrowth$weight, PlantGrowth$group,
    abline_options = list(col = "purple"), xlab = "group"
  ))
})

test_that("returns are correct", {
  expect_type(bss_test(PlantGrowth$weight, PlantGrowth$group)$stats,
    type = "list"
  )
  expect_type(bss_test(PlantGrowth$weight, PlantGrowth$group)$groups,
    type = "list"
  )
  expect_type(bss_test(PlantGrowth$weight, PlantGrowth$group)$groups[[1]],
    type = "integer"
  )
  expect_type(bss_test(PlantGrowth$weight, PlantGrowth$group)$parameters,
    type = "list"
  )
  expect_length(bss_test(
    PlantGrowth$weight,
    PlantGrowth$group
  )$parameters, n = 2)

  expect_s3_class(
    bss_test(PlantGrowth$weight, PlantGrowth$group)$dendrogram_data,
    "hclust"
  )
})
