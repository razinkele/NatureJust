test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
})
