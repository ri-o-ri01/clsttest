check_arg_in_func <- function(func_list, func_name, tgt_arg) {
  # 指定された関数をリストから取得
  if (!func_name %in% names(func_list)) {
    stop(paste("Function", func_name, "not found in the list"))
  }

  func <- func_list[[func_name]]

  # 関数の引数リストを取得
  args <- names(formals(func))

  # 指定された引数名が含まれているかチェック
  tgt_arg %in% args
}

update_new_func_by_add_variables<- function(func, add_variables = NULL) {
  # 'add_variables' 引数を持つかどうかチェック
  if ("add_variables" %in% names(formals(func))) {
    # 'add_variables' 引数を持つ場合、新しい関数を定義
    function(x) func(x, add_variables)
  } else {
    # 'add_variables' 引数を持たない場合、元の関数をそのまま返す
    func
  }
}

exe_parallel_for_apply <- function(
    df,  groups, tgt_names, tgt_func_dict, libraries = c("tidyverse"),
    apply_type = "agg",
    no_cores = detectCores(), is_debug=F,
    add_variables = NULL
    ) {

  switch (apply_type,
    "agg" = {
      result_df_smrz <- df %>%
        group_by(across(all_of(groups))) %>%
        summarise(across(all_of(tgt_names), ~ list(.x), .unpack = TRUE), .groups = 'drop') %>%
        ungroup()
    },
    "transform" = {
      result_df_smrz <- df
    },
    "map" = {
      result_df_smrz <- df
    },

  )

  result_df_l = result_df_smrz %>%
    pivot_longer(
      cols = all_of(tgt_names),
      names_to = "key",
      values_to = "value"
    )

  f = function(row, variables){
    tgt_func_dict = variables[["tgt_func_dict"]]
    check_arg_in_func = variables[["check_arg_in_func"]]
    if(!is.null(variables[["add_variables"]])){
      add_variables = variables[["add_variables"]]
      for (key in names(add_variables)) {
        assign(key, add_variables[[key]])
      }
    }

    result_row <- map_dfc(names(tgt_func_dict), ~ {
      fd <- tgt_func_dict[[.]]

      if(check_arg_in_func(tgt_func_dict, ., "add_variables")){
        tibble(!!.x := fd(row$value[[1]], add_variables))
      }else{
        tibble(!!.x := fd(row$value[[1]]))
      }

    }) %>%
      bind_cols(row, .)
    result_row
  }
  if(is_debug) {
    browser()
  }
  variables = list(tgt_func_dict = tgt_func_dict)
  if(!is.null(add_variables)){
    variables[["add_variables"]] = add_variables
  }
  variables[["check_arg_in_func"]] = check_arg_in_func

  assign("libraries", libraries, envir = .GlobalEnv)
  assign("variables", variables, envir = .GlobalEnv)

  result_tbl_p <- apply_parallel(
    data = result_df_l,
    f = f,
    libraries = libraries,  # 使用するライブラリ
    variables =  variables ,  # 追加の変数名はなし
    no_cores = no_cores  # 使用するコアの数
  ) %>%
    select(-all_of("value"))

  # 列名を生成
  cols <- groups
  keys <- unique(result_tbl_p$key)
  values <- names(tgt_func_dict)
  for (k in keys) {
    for (v in values) {
      cols <- c(cols, paste(k, v, sep = "_"))
    }
  }

  values_fn = if(apply_type == "map") list else NULL

  result_df = result_tbl_p %>%
    pivot_wider(
      names_from = key,
      values_from = values,
      names_glue = "{key}_{.value}",
      values_fn = values_fn
    ) %>%
    select(all_of(cols))

  if(apply_type %in% c("transform", "map") ){
    result_df = result_df %>% unnest(all_of(cols))
  }

  return(result_df)
}


agg_by_groups <- function(df, groups, indices, tgt_names, func_dict,
                          use_func_order = FALSE, is_debug = FALSE,
                          use_parallel = F, no_cores = detectCores(),
                          libraries = c("tidyverse"), add_variables = NULL,
                          use_matrix_op = F
                          ) {

  if(use_parallel){
    agg_df = exe_parallel_for_apply(
      df, groups, tgt_names, func_dict, libraries, apply_type = "agg",
      no_cores = no_cores, is_debug=is_debug, add_variables=add_variables)

  }else{
    # グループごとに関数を適用
    if(is_debug) print("No Parallel")
    if(use_matrix_op){
      if(is_debug) print("use_matrix_op = T")
      stopifnot(length(tgt_names) == 1)
      types_to_apply <- df %>% select(all_of(groups)) %>%
        map(class) %>% unlist()

      wide_tibble <- df %>%
        unite(id, all_of(groups), sep="-") %>%
        select(date, id, all_of(tgt_names))  %>%
        pivot_wider(
          names_from = id,  # ワイド型の列名を指定
          values_from = tgt_names  # ワイド型の各列の値を指定
        ) %>%
        arrange(indices)

      agg_df = lapply(func_dict %>% names(), function(fname) {
        x = func_dict[[fname]]
        result_mtrx = wide_tibble %>%
          select(- all_of(indices)) %>% apply(2, x)
        mtrx_names  = names(result_mtrx)
        mtrx_values = result_mtrx
        tibble(
          fname2 = fname,
          name  = mtrx_names,
          value = mtrx_values
        ) %>% separate(name, into = groups, sep = "-")
      }) %>% bind_rows() %>%
        mutate(fname2 = paste0(tgt_names, "_", fname2)) %>%
        pivot_wider(
          names_from = fname2, values_from = value
        ) %>%
        mutate(across(names(types_to_apply), ~ type.convert(.x, as.is = TRUE))) %>%
        arrange(indices)
    }else{
      if(is_debug) print("use_matrix_op = F")
      new_func_dict <- lapply(func_dict, update_new_func_by_add_variables,
                              add_variables = add_variables)
      agg_df <- df %>%
        group_by(across(all_of(groups))) %>%
        summarise(across(all_of(tgt_names), new_func_dict, .names = "{.col}_{.fn}"), .groups = 'drop')
    }


  }

  # 関数順序の使用
  if (use_func_order) {
    new_columns <- unlist(lapply(tgt_names, function(col) paste0(col, "_", seq_along(func_dict))))
    old_columns <- agg_df %>% select(-all_of(groups)) %>% colnames()
    names(agg_df) <- c(groups, new_columns)
    columns_map <- setNames(old_columns, new_columns)
    return (list(agg_df, columns_map))
  }

  return (agg_df)
}


tf_by_groups <- function(df, groups, tgt_names, func_dict,
                         use_func_order = FALSE, is_debug = FALSE,
                         use_parallel = F, no_cores = detectCores(), libraries = c("tidyverse")
                         ) {


  if(use_parallel){
    func_dict2 <- lapply(func_dict, function(f) {
      # 新しい関数を定義
      function(x) {
        # 元の関数の返り値をリストに格納して返す
        list(f(x))
      }
    })

    input_df2 = df %>%
      group_by(across(all_of(groups))) %>%
      summarise(across(all_of(tgt_names), ~ list(.x), .unpack = TRUE), .groups = 'drop')
    tf_df = exe_parallel_for_apply(
      input_df2, groups, tgt_names, func_dict2, libraries, apply_type="transform")
  }else{
    tf_df <- df[, c(groups, tgt_names)] %>%
      group_by(across(all_of(groups))) %>%
      mutate(across(all_of(tgt_names), .fns = func_dict, .names = "{.col}_{.fn}")) %>%
      select(- all_of(tgt_names)) %>%
      ungroup()

  }

  if (use_func_order) {
    new_columns <- unlist(lapply(tgt_names, function(col) paste0(col, "_", seq_along(func_dict))))
    old_columns <- tf_df %>% select(-all_of(groups)) %>% colnames()
    names(tf_df) <- c(groups, new_columns)
    columns_map <- setNames(old_columns, new_columns)
    return(list(tf_df, columns_map))
  }

  return(tf_df)
}


map_by_groups <- function(df, groups, tgt_names, func_dict,
                          use_func_order = FALSE, is_debug = FALSE,
                          use_parallel = F, no_cores = detectCores(),
                          libraries = c("tidyverse"), add_variables = NULL
) {

  if(use_parallel){
    map_df = exe_parallel_for_apply(
      df, groups, tgt_names, func_dict, libraries,
      apply_type = "map", no_cores = no_cores, is_debug=is_debug,
      add_variables=add_variables)

  }else{
    new_func_dict <- lapply(func_dict, update_new_func_by_add_variables,
                            add_variables = add_variables)
    # グループごとに関数を適用
    map_df <- df %>% rowwise() %>%
      mutate(across(all_of(tgt_names), new_func_dict, .names = "{.col}_{.fn}")) %>%
      select(- all_of(tgt_names))
  }

  # 関数順序の使用
  if (use_func_order) {
    new_columns <- unlist(lapply(tgt_names, function(col) paste0(col, "_", seq_along(func_dict))))
    old_columns <- map_df %>% select(-all_of(groups)) %>% colnames()
    names(map_df) <- c(groups, new_columns)
    columns_map <- setNames(old_columns, new_columns)
    return (list(map_df, columns_map))
  }

  return (map_df)
}



GroupbyChainOperator <- R6Class("GroupbyChainOperator",
  public = list(
    init_df = NULL,
    init_name = "init",
    level = 0,
    level_result_dict = list(),
    level_column_dict = list(),

    initialize = function(init_df, init_name = "init") {
      if (is.null(init_df)) {
        stop("Please set init_df")
      }
      self$init_df <- init_df
      self$init_name <- init_name
      self$level <- 0
      self$level_result_dict[[as.character(self$level)]] <-
        GroupbyResult$new(init_name, "None", NULL, NULL, NULL,
                          NULL, NULL, NULL, init_df, NULL, self$level)
      self$level_column_dict[[as.character(self$level)]] <- NULL
    },

    check_update_inputs = function(tgt_names = NULL, func_dict = NULL, is_debug = F, use_parallel=F) {
      prev_level <- self$level
      input_result <- self$level_result_dict[[as.character(prev_level)]]
      input_df <- input_result$output_df
      input_name <- input_result$output_name

      if (is.null(tgt_names)) {
        tgt_names <- input_result$tgt_names
      }
      if (is.null(func_dict)) {
        func_dict <- input_result$func_dict
      }
      if (is.null(tgt_names) || is.null(func_dict)) {
        stop("Please set tgt_names and func_dict as they are not included in the previous result.")
      }
      if(is_debug & prev_level == 1) browser()
      if(use_parallel) {
        print(paste0("Execute Parallel level=", prev_level + 1))
      }else{
        print(paste0("Execute No Parallel level=", prev_level + 1))
      }

      return(list(input_df, input_name, tgt_names, func_dict))
    },

    update_level_result = function(new_df, operation_type, input_name, groups, indices, tgt_names, func_dict, funcs_name, columns_dict, output_name) {
      level <- self$level + 1
      result <- GroupbyResult$new(output_name, operation_type, input_name, groups, indices, tgt_names, func_dict, funcs_name, new_df, columns_dict, level)
      self$level_column_dict[[as.character(level)]] <- columns_dict
      self$level_result_dict[[as.character(level)]] <- result
      self$level <- level
    },

    update_by_agg = function(groups, indices = NULL, tgt_names = NULL,
                             func_dict = NULL, funcs_name = NULL,
                             output_name = NULL, is_debug = FALSE,
                             use_parallel = F, no_cores = detectCores(),
                             libraries = c("tidyverse"), add_variables = NULL,
                             use_matrix_op = F
                             ) {
      check_result <- self$check_update_inputs(tgt_names, func_dict, is_debug, use_parallel)
      input_df <- check_result[[1]]
      input_name <- check_result[[2]]
      tgt_names <- check_result[[3]]
      func_dict <- check_result[[4]]

      result <- agg_by_groups(input_df, groups, indices, tgt_names,
                              func_dict, TRUE, is_debug, use_parallel,
                              no_cores, libraries, add_variables, use_matrix_op)
      agg_df <- result[[1]]
      columns_dict <- result[[2]]

      if (is_debug) {
        print(tgt_names)
      }

      self$update_level_result(agg_df, "agg", input_name, groups, indices,
                               names(columns_dict), func_dict, funcs_name,
                               columns_dict, output_name)
      return (self)
    },

    update_by_transform = function(groups, indices = NULL, tgt_names = NULL,
                                   func_dict = NULL, funcs_name = NULL,
                                   output_name = NULL, is_debug = FALSE,
                                   use_parallel = F, no_cores = detectCores()) {
      check_result <- self$check_update_inputs(tgt_names, func_dict, is_debug, use_parallel)
      input_df <- check_result[[1]]
      input_name <- check_result[[2]]
      tgt_names <- check_result[[3]]
      func_dict <- check_result[[4]]

      result <- tf_by_groups(input_df, groups, tgt_names,
                             func_dict, TRUE, is_debug, use_parallel, no_cores)
      tf_df <- result[[1]]
      columns_dict <- result[[2]]

      if (is_debug) {
        print(tgt_names)
        print(tf_df)
      }

      self$update_level_result(tf_df, "transform", input_name, groups, indices,
                               names(columns_dict), func_dict, funcs_name,
                               columns_dict, output_name)
      return(self)
    },

    update_by_map = function(groups, indices = NULL, tgt_names = NULL,
                             func_dict = NULL, funcs_name = NULL,
                             output_name = NULL, is_debug = FALSE,
                             use_parallel = F, no_cores = detectCores(),
                             libraries = c("tidyverse"), add_variables = NULL
    ) {
      check_result <- self$check_update_inputs(tgt_names, func_dict, is_debug, use_parallel)
      input_df <- check_result[[1]]
      input_name <- check_result[[2]]
      tgt_names <- check_result[[3]]
      func_dict <- check_result[[4]]

      result <- map_by_groups(input_df, groups, tgt_names,
                              func_dict, TRUE, is_debug, use_parallel,
                              no_cores, libraries, add_variables)
      map_df <- result[[1]]
      columns_dict <- result[[2]]
      print("aaaa")
      if (is_debug) {
        print(tgt_names)
      }

      self$update_level_result(map_df, "map", input_name, groups, indices,
                               names(columns_dict), func_dict, funcs_name,
                               columns_dict, output_name)
      return (self)
    },

    get_output_df = function(level, use_func_order = TRUE) {
      result <- self$level_result_dict[[as.character(level)]]
      output_df <- result$output_df

      if (!use_func_order || level == 0) {
        return(output_df)
      } else {
        for (l in rev(seq_len(level))) {
          tgt_dict <- self$level_column_dict[[as.character(l)]]
          tgt_dict2 <- tgt_dict[grepl(paste0("(_\\d){", l, "}$"), names(tgt_dict))]
          for (old in names(tgt_dict2)) {
            new <- tgt_dict2[[old]]
            names(output_df) <- sub(old, new, names(output_df))
          }
        }
        return(output_df)
      }
    },

    merge_all_level_df = function(use_func_order = TRUE) {
      ret_df <- self$get_output_df(0, use_func_order)
      for (l in seq_len(self$level) - 1) {
        l_ret <- self$level_result_dict[[as.character(l + 1)]]
        method <- l_ret$method
        groups <- l_ret$groups
        output_df <- self$get_output_df(l + 1, use_func_order)

        if (method == "transform") {
          ret_df <- cbind(ret_df, output_df[!names(output_df) %in% groups])
        } else {
          ret_df <- merge(ret_df, output_df, by = groups, all.x = TRUE)
        }
      }
      return(ret_df)
    },

    get_log = function() {
      sapply(self$level_result_dict, function(v) capture.output(v$print()))
    },

    concat_log = function(){
      paste0("|", reduce(self$get_log(), paste, sep = "="), "|")
    },

    deep_copy = function(updated_df = NULL, updated_name = NULL){
      if(is.null(updated_name)){
        updated_name = self$concat_log()
      }
      if(!is.null(updated_df)){
        updated_obj = GroupbyChainOperator$new(updated_df, updated_name)
      }else{
        updated_obj = self$clone(deep = TRUE)
        updated_obj$init_name = updated_name
      }
      updated_obj
    }

  )

)
