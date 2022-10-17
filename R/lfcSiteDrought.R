#'
#'
#' @export
siteDrought <- function() {
  lfcsiteDrought$new()
}

lfcsiteDrought <- R6::R6Class(

  classname = "lfcsiteDrought",
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

      tables_dict <- siteDrought_table_dictionary()
      variables_thes <- suppressMessages(super$get_data('variables_thesaurus'))

      tables %>%
        purrr::map(
          siteDrought_describe_table_cat,
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
          siteDrought_describe_var_eng,
          variables_thes = variables_thes, numerical_thes = numerical_thes
        )

      invisible(self)

    },
    print = function(...) {
      cat(
        " Access to Laboratori Forestal (CREAF).\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("siteDrought_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("siteDrought_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("siteDrought_describe_table") %+%
          " to get the information available on the tables.\n",
        "Use " %+% crayon::yellow$bold("siteDrought_describe_var") %+%
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
#      .) Todas se inicializan con un OBJETO => Es siteDrought()
#      .) CHECKS:
#               .) Solo tienen CHECK CLASS
#               .) Las variables seran Chequeadas por el método
#      .) EJEMPLO de USO:
#               .) sitedr <- lfcdata::sitedrougth()
#               .) siteDrought_avail_tables(sitedr,'data_Day)


#'
#'
#' @export
siteDrought_get_data <- function(object, table_name = "data_day") {
  check_class_for(object, 'lfcsiteDrought')
  object$get_data(table_name)
}


#' @export
siteDrought_avail_tables <- function(object) {
  check_class_for(object, 'lfcsiteDrought')
  object$avail_tables()
}


#'
#'
#' @export
siteDrought_describe_table <- function(object, tables) {
  check_class_for(object, 'lfcsiteDrought')
  object$describe_table(tables)
}


#'
#'
#' @export
siteDrought_describe_var<- function(object, variables) {
  check_class_for(object, 'lfcsiteDrought')
  object$describe_var(variables)
}



#' Add together two numbers
#' @param x A number.
#' @param y A number.
#' @return A numeric vector.
#' @examples
#' add(1,1)
#' add(10,1)

add <- function(x,y){
  x+y
}


#' Remove duplicates strings
#'
#' `str_unique()` remove duplicate values, with optional control over
#' how duplicte is measured.
#'
#' `str_detect()` return a logial vector `TRUE` if `pattern` or `FALSE` if bla bla bla
#'
#' @param string   A character vector to return unique entries.
#' @param ...   Other options used to control matching behaviour between
#' duplicate strings. Passed on to [stringi::stri_opts_collator()].
#'
#' @returns A character vector.
#' @seealso
#' * [tibble()] constructs from individual columns.
#' * [enframe()] converts a named vector into a two-column tibble (names and
#'   values).
#' * [name-repair] documents the details of name repair.
#' @description
#' `str_like()` follows the conventions of the SQL `LIKE` operator:
#'
#' * Must match the entire string.
#' * `_` matches a single character (like `.`).
#' * `%` matches any number of characters (like `.*`).
#' * `\%` and `\_` match literal `%` and `_`.
#' * The match is case insensitive by default.
#' @examples
#' str_unique(c("a","b","c","a","a","f"))
#'
#' # Use ... to pass additonal arguments to stri_unique()
#' str_unique(c("motley", "mötley", "pinguino", "pingüino"))
#' str_unique(c("motley", "mötley", "pinguino", "pingüino"), strength = 1)
#' @export

str_unique <- function(string, ...) {
  ...

}




#' The length of a string
#'
#' Technically this returns the number of "code points", in a string. One
#' code point usually corresponds to one character, but not always. For example,
#' an u with a umlaut might be represented as a single character or as the
#' combination a u and an umlaut.
#'
#' @inheritParams str_detect
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @seealso [stringi::stri_length()] which this function wraps.
#' @examples
#' str_length(letters)
#' str_length(NA)
#' str_length(factor("abc"))
#' str_length(c("i", "like", "programming", NA))
#' #' @export
str_length <- function(string) {
}

# ...............  MANUAL WEB .................
# .............................................

#         .) https://r-pkgs.org/man.html#description
#         .) M'he quedat al punt 16.3













