test_that("class object creation works", {
  expect_is(modosin(), c('lfcMODOSIN'))
  expect_equal(lfcdata:::lfcMODOSIN$new(), modosin())
  expect_true(rlang::is_function(modosin()$get_data_TIMING_R))
  expect_true(rlang::is_function(modosin()$get_data_TIMING_SQL))
  expect_true(rlang::is_function(modosin()$get_data_by_R))
  expect_true(rlang::is_function(modosin()$get_data_by_SQL))
  expect_true(rlang::is_function(modosin()$get_data_SQL))
  expect_true(rlang::is_function(modosin()$get_data_R))
  expect_true(rlang::is_function(modosin()$get_data_SQL_tim))
  expect_true(rlang::is_function(modosin()$get_data_R_tim))
  expect_true(rlang::is_function(modosin()$avail_tables))
  expect_true(rlang::is_function(modosin()$describe_table))
  expect_true(rlang::is_function(modosin()$describe_var))

})

# nfidb to avoid calling the db so often
# nfidb <- nfi()
mod <- modosin()

test_that("get_data method works", {

  skip_on_cran()
  skip_on_travis()

  expect_s3_class(mod$get_data_R_tim('data_day'),'data.frame')
  expect_s3_class(mod$get_data_SQL_tim('data_day','2021-10-31'),'data.frame')
  expect_s3_class(mod$get_data_R('data_day'),'data.frame')
  expect_s3_class(mod$get_data_SQL('data_day','2021-10-31'),'data.frame')
  expect_s3_class(mod$get_data_TIMING_R('data_day','2021-10-31'),'difftime')
  expect_s3_class(mod$get_data_TIMING_SQL('data_day','2021-10-31'),'difftime')
  expect_s3_class(mod$get_data_by_R('data_day','2021-10-31'),'data.frame')
  expect_s3_class(mod$get_data_by_SQL('data_day','2021-10-31'),'data.frame')

  # errors
  expect_error(mod$get_data_by_R('prova_dec_bc_4_plots_nfi3'),
               Message = 'argument "date_1" is missing, with no default')

  expect_error(mod$get_data_by_R('data_day',FALSE),
               Message = "do not know how to convert")

  expect_error(mod$get_data_by_R('data_day',21),
               Message = "'origin' must be supplied")

  expect_error(mod$get_data_by_R(FALSE,21),
               Message = "'origin' must be supplied")

  expect_error(mod$get_data_by_R('non_existent_table','2021-10-31'),
               Message = "Can not connect to the database")
  }
)

test_that("avail_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(mod$avail_tables(), c('character'))
  expect_type(mod$avail_tables(),'character')
  expect_true("variables_thesaurus" %in% mod$avail_tables())
  }
)


test_that("describe_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(mod$describe_table("prova_plot_nfi2_genus"), c('lfcMODOSIN'))
  expect_output(mod$describe_table("prova_plot_nfi2_genus"))
  expect_output(mod$describe_table(c("prova_plot_nfi2_genus","prova_plots_dynamic_nfi2")))
  expect_error(mod$describe_table(c("prova_plot_nfi2_genus", "prova_ejemplo")), 'not found')
  expect_error(mod$describe_table('base_datos'), 'base_datos not found')
  expect_error(mod$describe_table(NA), 'Argument tables is not character')
  expect_error(mod$describe_table(25), 'Argument tables is not character')
  expect_error(mod$describe_table(), "el argumento \"tables\" está ausente, sin valor por omisión")
  }
)

test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(mod$describe_var("REW"),c('lfcMODOSIN'))
  expect_output(mod$describe_var("LAI"))
  expect_output(mod$describe_var(c("REW","Precipitaion","LAI")))

  # errors
    expect_error(mod$describe_var(NA),
                 Message = "Argument variables is not character")
    expect_error(mod$describe_var(21),
                 Message = "Argument variables is not character")
    expect_error(mod$describe_var(),
                 Message = 'argument "variables" is missing')
  }
)

rm(mod)


