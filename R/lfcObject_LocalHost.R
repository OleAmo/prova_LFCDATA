

lfcObject_LH <- R6::R6Class(

  classname = 'lfcObject_LH',

  public = list(
     initialize = function(){
     private$pool_conn <- private$pool_conn_create()
    },

  # ................... GET_DATA R ...................
  # .................................................

  #      .) La Consulta SQL => TODAS las FECHAS
  #      .) En R = seleccion de fecha

  get_data_R = function(table_name){
    private$data_cache[[glue::glue("{table_name}_FALSE")]] %||% {

      message('Querying table from LFC database, this can take a while...')
      query_data <- try(
        dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
      )
      message('Done')

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}_FALSE")]] <- query_data
        return(query_data)
      }
    }
  },

  # ................. GET_DATA SQL ..................
  # .................................................

  #      .) La Consulta SQL => SELECCION por FECHA
  #      .)


  get_data_SQL = function(table_name,date){
    private$data_cache[[glue::glue("{table_name}_FALSE")]] %||% {

      message('Querying table from LFC database, this can take a while...')

      date_2 <- as.Date(date, format = "%Y-%m-%d")

      sql <- glue:::glue("
        SELECT * FROM public.{table_name}
        WHERE date = '{date_2}' ")

      query_data <- try(pool::dbGetQuery(private$pool_conn,sql))

      message('Done')

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}_FALSE")]] <- query_data
        return(query_data)
      }

    }
  }

  ),
  private = list(
    dbname = NULL,
    pool_conn = NA,
    pool_conn_create = function(){
      res <- try({
        pool::dbPool(
          drv = RPostgres::Postgres(),
          dbname = private$dbname,
          host = 'localhost',
          port = 5432,
          idleTimeout = 3600,
          user = 'postgres',
          password = '12345database',
          options = "-c client_min_messages=warning"
        )

      })
      return(res)
    },

    data_cache = list(),

    finalize = function(){
      pool::poolClose(private$pool_conn)

    }

  )





)
