test_that("class object creation works", {
  expect_is(siteDrought(), c('lfcsiteDrought'))
  expect_equal(lfcdata:::lfcsiteDrought$new(), siteDrought())
  expect_true(rlang::is_function(siteDrought()$get_data_TIMING_R))
  expect_true(rlang::is_function(siteDrought()$get_data_TIMING_SQL))
  expect_true(rlang::is_function(siteDrought()$get_data_by_R))
  expect_true(rlang::is_function(siteDrought()$get_data_by_SQL))
  expect_true(rlang::is_function(siteDrought()$get_data_SQL))
  expect_true(rlang::is_function(siteDrought()$get_data_R))
  expect_true(rlang::is_function(siteDrought()$get_data_SHAPE))
  expect_true(rlang::is_function(siteDrought()$timing_loop))
  expect_true(rlang::is_function(siteDrought()$avail_tables))
  expect_true(rlang::is_function(siteDrought()$describe_table))
  expect_true(rlang::is_function(siteDrought()$describe_var))

})

# nfidb to avoid calling the db so often
# nfidb <- nfi()
siteDro <- siteDrought()

test_that("get_data method works", {

  # Si las pruebas deben ejecutarse con relativa rapidez,
  # idealmente menos de un minuto en total. Utilice skip_on_cran()
  # en una prueba que sea inevitablemente de larga duración.

  skip_on_cran()
  skip_on_travis()

  expect_s3_class(siteDro$get_data_R('data_day'),'data.frame')
  expect_s3_class(siteDro$get_data_SQL('data_day','2021-10-31'),'data.frame')
  expect_s3_class(siteDro$get_data_TIMING_R('data_day','2021-10-31'),'difftime')
  expect_s3_class(siteDro$get_data_TIMING_SQL('data_day','2021-10-31'),'difftime')
  expect_s3_class(siteDro$get_data_by_R('data_day','2021-10-31'),'data.frame')
  expect_s3_class(siteDro$get_data_by_SQL('data_day','2021-10-31'),'data.frame')

  # Hacer TESTS => GET_DATA SHAPE
  # Hacer TESTS => TIMING LOOP

  # expect_s3_class(siteDro$get_data_SHAPE("xxxxx"))
  # expect_s3_class(siteDro$timing_loop("xxxxx"))


  # errors
  expect_error(siteDro$get_data_by_R('prova_dec_bc_4_plots_nfi3'),
               Message = 'argument "date_1" is missing, with no default')

  expect_error(siteDro$get_data_by_R('data_day',FALSE),
               Message = "do not know how to convert")

  expect_error(siteDro$get_data_by_R('data_day',21),
               Message = "'origin' must be supplied")

  expect_error(siteDro$get_data_by_R(FALSE,21),
               Message = "'origin' must be supplied")

  expect_error(siteDro$get_data_by_R('non_existent_table','2021-10-31'),
               Message = "Can not connect to the database")
  }
)

test_that("avail_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(siteDro$avail_tables(), c('character'))
  expect_type(siteDro$avail_tables(),'character')
  expect_true("variables_thesaurus" %in% siteDro$avail_tables())
  }
)


test_that("describe_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(siteDro$describe_table("prova_plot_nfi2_genus"), c('lfcsiteDrought'))
  expect_output(siteDro$describe_table("prova_plot_nfi2_genus"))
  expect_output(siteDro$describe_table(c("prova_plot_nfi2_genus","prova_plots_dynamic_nfi2")))
  expect_error(siteDro$describe_table(c("prova_plot_nfi2_genus", "prova_ejemplo")), 'not found')
  expect_error(siteDro$describe_table('base_datos'), 'base_datos not found')
  expect_error(siteDro$describe_table(NA), 'Argument tables is not character')
  expect_error(siteDro$describe_table(25), 'Argument tables is not character')
  expect_error(siteDro$describe_table(), "el argumento \"tables\" está ausente, sin valor por omisión")
  }
)

test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(siteDro$describe_var("REW"),c('lfcsiteDrought'))
  expect_output(siteDro$describe_var("LAI"))
  expect_output(siteDro$describe_var(c("REW","Precipitaion","LAI")))

  # errors
    expect_error(siteDro$describe_var(NA),
                 Message = "Argument variables is not character")
    expect_error(siteDro$describe_var(21),
                 Message = "Argument variables is not character")
    expect_error(siteDro$describe_var(),
                 Message = 'argument "variables" is missing')
  }
)

rm(siteDro)


