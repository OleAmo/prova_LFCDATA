lfcObject_LH <- R6::R6Class(
  classname = 'lfcObject_LH',
  public = list(
     initialize = function(){
     private$pool_conn <- private$pool_conn_create()
    },
  get_data = function(table_name){
    private$data_cache[[glue::glue("{table_name}")]] %||% {

      message('Querying table from LFC database, this can take a while...')
      query_data <- try(
        dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
      )
      message('Done')

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}")]] <- query_data
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
