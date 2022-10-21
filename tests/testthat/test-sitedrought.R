test_that("class object creation works", {
  expect_is(siteDrought(), c('lfcsiteDrought'))
  expect_equal(lfcdata:::lfcsiteDrought$new(), siteDrought())
  expect_true(rlang::is_function(siteDrought()$get_data))
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

  expect_s3_class(siteDro$get_data('data_day'),'data.frame')


  # errors


  expect_error(siteDro$get_data(FALSE),
               Message = "Argument table_name is not character")

  expect_error(siteDro$get_data(21),
               Message = "Argument table_name is not character")
  }
)

test_that("avail_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(siteDro$avail_tables(), c('character'))
  expect_type(siteDro$avail_tables(),'character')
  expect_true("variables_thesaurus_sitedr" %in% siteDro$avail_tables())
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


