

# ........... CHECK_ARGS_FOR ...............
# ...........................................


check_args_for_LH <- function(
    character = NULL, numerical = NULL, logical = NULL, na = NULL,
    sf = NULL, points = NULL, polygons = NULL, date = NULL
) {

  # character
  if (!rlang::is_null(character)) {
    not_complying <- character %>%
      purrr::map(rlang::is_character) %>%
      purrr::keep(.p = ~!isTRUE(.x)) %>%
      names()

    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not character\n"
      )
      stop(error_message)
    }
  }

  # numerical
  if (!rlang::is_null(numerical)) {
    not_complying <- numerical %>%
      purrr::map(rlang::is_bare_numeric) %>%
      purrr::keep(.p = ~!isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "One or more variables are not numeric\n"
      )
      stop(error_message)
    }
  }

  # dates
  if (!rlang::is_null(date)) {
    not_complying <- date %>%
      purrr::map(
        .f = function(x) {
          date_check <- try(as.Date(x))
          if (is(date_check, 'try-error')) {
            return(FALSE)
          }
          # if x is a vector, with valid and invalid dates as characters,
          # as.Date is going to return NAs for invalid ones, check for that too:
          if (any(is.na(date_check))) {
            return(FALSE)
          }
          return(TRUE)
        }
      ) %>%
      purrr::keep(.p = ~ !isTRUE(.x)) %>%
      names()

    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} ",
        "cannot be converted to date"
      )
      stop(error_message)
    }
  }




  }




# ........... DESCRIBRE TABLE ...............
# ...........................................



modosin_describe_table_cat = function(table, tables_dict, variables_thes){
  variable_names <- variables_thes %>%
    dplyr::filter(.data$var_table == table) %>%
    dplyr::pull(.data$var_id) %>%
    unique()

    table_deconstructed <- stringr::str_split(table, '_') %>%
      purrr::flatten_chr()

      cat('\n', crayon::yellow$bold(table), '\n', sep = '')
      # table descriptio
      cat(
        glue::glue("{tables_dict[table_deconstructed] %>% purrr::discard(is.na)}") %>%
          glue::glue_collapse() %>%
          crayon::green() %>%
          strwrap(width = 75),
        # '\n',
        fill = 80, sep = ''
      )

      cat('Variables in table:\n')
      cat(
        glue::glue(" - {sort(variable_names)}") %>%
          glue::glue_collapse(sep = '\n') %>%
          crayon::magenta()
      )
      cat('\n')
}

# .......... DESCRIBRE VARIABLE .............
# ...........................................


modosin_describe_var_eng <- function(variable, variables_thes, numerical_thes) {

  . <- NULL

  variables_thes %>%
    dplyr::filter(.data$var_id == variable) %>%
    dplyr::left_join(numerical_thes, by = c("var_id", "var_table")) %>%
    dplyr::group_by(.data$var_description_eng) %>%
    dplyr::group_walk(
      ~ cat(
        "\n",
        # var name
        crayon::yellow$bold(glue::glue(
          "{.x$translation_eng %>% unique()} ({.x$var_id %>% unique()})"
        )),
        "\n",
        # var description
        strwrap(crayon::green(.y$var_description_eng), width = 72),
        "\n",
        # var units
        crayon::blue$bold(
          "Units: [" %+%
            crayon::blue$italic$bold(
              glue::glue("{(.x$var_units %na% ' - ') %>% unique()}")
            ) %+%
            "]"
        ),
        "\n",
        # tables present
        "Present in the following tables:\n",
        crayon::magenta(glue::glue_collapse(
          glue::glue(" - {sort(.x$var_table)}"), sep = '\n'
        )),
        # cat options
        sep = '', fill = 80
      )
    )

  return(invisible(NULL))
}
