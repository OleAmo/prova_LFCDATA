

modosin_describe_table_cat = function(table, tables_dict, variables_thes){
  variable_names <- variables_thes %>%
    dplyr::filter(.data$var_table == table) %>%
    dplyr::pull(.data$var_id) %>%
    unique()

  cat('\n', crayon::yellow$bold(table), '\n', sep = '')
  # table description
  cat(
    glue::glue("{tables_dict[table] %>% purrr::discard(is.na)}") %>%
      glue::glue_collapse() %>%
      crayon::green() %>%
      strwrap(width = 75),
    # '\n',
    fill = 80, sep = ''
  )


}
