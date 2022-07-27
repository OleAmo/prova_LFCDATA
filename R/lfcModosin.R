
modosin <- function() {
  lfcMODOSIN$new()
}

lfcMODOSIN <- R6::R6Class(

  classname = "lfcMODOSIN",
  inherit = lfcObject_LH,
  cloneable = FALSE,

  public = list(

    # ................... GET_DATA R ...................
    # ..................................................

    #      .) Usamos SUPER$GET_DATA  (dplyr::tbl + dplyr::collect)
    #      .) Nos descargamos TODA la TABLA con TODAS las FECHAS
    #      .) Usamos FILTER para seleccionar UNA FECHA

    get_data = function(table_name,date){

      # check_args_for(table name) => is always validated in the super
      check_args_for(date = list(date = date))
      date_format <- as.Date(date)

      res <- private$data_cache[[glue::glue("{table_name}_{date_format}_FALSE")]] %||%
        {
          query_data_spatial <- super$get_data(table_name) %>%
            data.frame() %>%
               dplyr::filter(date == date_format)
          private$data_cache[[glue::glue("{table_name}_{date_format}_FALSE")]] <- query_data_spatial
          query_data_spatial
        }
      return(res)
    },

    # .................. AVAIL TABLES ..................
    # ..................................................


    avail_tables = function() {
      c('data_day')
    },

    # .................. DESCRIBE VAR ..................
    # ..................................................


    describe_table = function(tables){

      check_args_for(character = list(tables = tables))
      check_if_in_for(tables, self$avail_tables())

      tables_dict <- nfi_table_dictionary()
      variables_thes <- suppressMessages(super$get_data('variables_thesaurus'))

      tables %>%
        purrr::map(
          modosin_describe_table_cat,
          tables_dict = tables_dict, variables_thes = variables_thes
        )

      return(invisible(self))
    },

    # ... DESCRIVE VAR ...
    # ....................

    describe_var = function(variables) {
      check_args_for(character = list(variables = variables))

      variables_thes <- suppressMessages(super$get_data('variables_thesaurus'))
      numerical_thes <- suppressMessages(super$get_data('variables_numerical'))

      variables %>%
        purrr::map(
          modosin_describe_var_eng,
          variables_thes = variables_thes, numerical_thes = numerical_thes
        )

      invisible(self)

    },
    print = function(...) {
      cat(
        " Access to Laboratori Forestal (CREAF).\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("modosin_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("modosin_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("nfi_describe_var") %+%
          " to get the information available on the variables.\n"
      )
      invisible(self)
    }

  ),

  private = list(
    dbname ="creaf_v4"
  )
)

# ............... FUNCIONES REDIRECIONABLES ............
# ......................................................


modosin_get_data <- function(object, table_name, date) {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  check_class_for(object, 'lfcMODOSIN')
  check_args_for(date = list(date = date))
  # call to the class method
  object$get_data(table_name, date)
}

modosin_avail_tables <- function(object) {
  check_class_for(object, 'lfcMODOSIN')
  object$avail_tables()
}

modosin_describe_table <- function(object, tables) {

  check_class_for(object, 'lfcMODOSIN')
  object$describe_table(tables)
}

modosin_describe_var<- function(object, variables) {
  check_class_for(object, 'lfcMODOSIN')
  object$describe_var(variables)
}





