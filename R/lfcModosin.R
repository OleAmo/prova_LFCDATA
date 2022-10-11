#'
#'
#' @export
plotDrought <- function() {
  lfcplotDrought$new()
}

lfcplotDrought <- R6::R6Class(

  classname = "lfcplotDrought",
  inherit = lfcObject_LH,
  cloneable = FALSE,

  public = list(

    # ................... GET_DATA R ...................
    # ..................................................

    #      .) GET_DATA SIMPRE devuelve tabla (tipo SF) con GEOMETRIA
    #      .) Por lo tanto:

    #             .) NO Usamos SUPER$GET_DATA (devuelve DF sin geometría)
    #             .) CACHE siempre (SPATIAL = TRUE)
    #             .) Usamos el SF::ST_READ  (Se conecta a al BBDD y devuelve SF con geometría)
    #             .) NO Usamos FILTER para seleccionar UNA FECHA
    #                       .) Nos descargarmos TODA la TABLA
    #                       .) Con todas las FECHAS y datos
    #                       .) Pesarà poco para R y despues en la App ya la filtraremos (fecha,variable,...)

    #      .) Tablas por DEFECTO
    #                       .) Usamos (table_name = "data_day") para indicar que el defecto es "data_day"
    #                       .) Así => get_data() lo hace en f(x) de la tabla "data_day"
    #                       .) Y si queremos otra tabla usamos => get_data("municipios")


    get_data = function(table_name = "data_day") {

      check_args_for(character = list(table_name = table_name))


      res <- private$data_cache[[glue::glue("{table_name}_TRUE")]] %||%
        {
          message('Querying table from LFC database, this can take a while...')
          query_data_spatial <- sf::st_read(private$pool_conn, table_name)
          message('Done')
          private$data_cache[[glue::glue("{table_name}_TRUE")]] <- query_data_spatial
          query_data_spatial
        }

      return(res)
    },

    # .................. AVAIL TABLES ..................
    # ..................................................

    #      .) El usuario SOLO puede acceder a la tabla DATA_dAY
    #      .) El resto de tablas de la BBDD no son accesibles


    avail_tables = function() {
      c('data_day')
    },

    # ................ DESCRIBE TABLE ..................
    # ..................................................

    #      .) Para obtener VARIABLES THESAURUS
    #      .) Usamos SUPER$GET_DATA
    #      .) Ya que queremos descargar de la BBDD una tabla sin GEOMETRIA


    describe_table = function(tables){

      check_args_for(character = list(tables = tables))
      check_if_in_for(tables, self$avail_tables())

      tables_dict <- plotDrought_table_dictionary()
      variables_thes <- suppressMessages(super$get_data('variables_thesaurus'))

      tables %>%
        purrr::map(
          plotDrought_describe_table_cat,
          tables_dict = tables_dict, variables_thes = variables_thes
        )

      return(invisible(self))
    },

    # .................. DESCRIBE VAR ..................
    # ..................................................

    #      .) Para obtener VARIABLES THESAURUS / NUMERICAL
    #      .) Usamos SUPER$GET_DATA
    #      .) Ya que queremos descargar de la BBDD dos tablas sin GEOMETRIA

    describe_var = function(variables) {
      check_args_for(character = list(variables = variables))

      variables_thes <- suppressMessages(super$get_data('variables_thesaurus'))
      numerical_thes <- suppressMessages(super$get_data('variables_numerical'))

      variables %>%
        purrr::map(
          plotDrought_describe_var_eng,
          variables_thes = variables_thes, numerical_thes = numerical_thes
        )

      invisible(self)

    },
    print = function(...) {
      cat(
        " Access to Laboratori Forestal (CREAF).\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("plotDrought_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("plotDrought_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("plotDrought_describe_table") %+%
          " to get the information available on the tables.\n",
        "Use " %+% crayon::yellow$bold("plotDrought_describe_var") %+%
          " to get the information available on the variables.\n"

      )
      invisible(self)
    }

  ),

  private = list(
    dbname ="creaf_v4"
  )
)

# ............... FUNCIONES .....................
# ...............................................

#      .) Estas funciones facilitan al usuario acceder a los datos
#      .) Cada método tine una función
#      .) Todas se inicializan con un OBJETO => Es plotDrought()
#      .) CHECKS:
#               .) Solo tienen CHECK CLASS
#               .) Las variables seran Chequeadas por el método


#'
#'
#' @export
plotDrought_get_data <- function(object, table_name = "data_day") {
  check_class_for(object, 'lfcplotDrought')
  object$get_data(table_name)
}


#'
#'
#' @export
plotDrought_avail_tables <- function(object) {
  check_class_for(object, 'lfcplotDrought')
  object$avail_tables()
}


#'
#'
#' @export
plotDrought_describe_table <- function(object, tables) {
  check_class_for(object, 'lfcplotDrought')
  object$describe_table(tables)
}


#'
#'
#' @export
plotDrought_describe_var<- function(object, variables) {
  check_class_for(object, 'lfcplotDrought')
  object$describe_var(variables)
}





