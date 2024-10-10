#' Parameter Result Handler Class
#'
#' This class is designed to manage parameter sets and their corresponding results.
#' It allows for the creation, saving, loading, and deletion of parameter sets and results.
#' Results are saved in a structured directory hierarchy based on the result name and trial.
#'
#' @name ParameterResultHandler
#' @field save_path The base directory where parameter sets and results are saved.
#' @method new Initialize a new ParameterResultHandler object with a specific save path.
#' @method set_params Save a new set of parameters under a specified trial name.
#' @method save_results Save results under a specified result name and trial.
#' @method load_results Load results from a specified result name. If trial is provided, only results for that trial are loaded.
#' @method delete_params Delete a set of parameters. If trial is provided, only parameters for that trial are deleted.
#' @method delete_results Delete results under a specified result name. If trial is provided, only results for that trial are deleted.
#' @export ParameterResultHandler
ParameterResultHandler <- R6Class(
  "ParameterResultHandler",

  public = list(
    save_path = NULL,
    level = 1,
    primary_keys = NULL, # 主キー列名を格納するフィールドを追加
    sub_dir_name = NULL, 
    is_old = F, # Tのとき、levelを0にする
    origin_level = 1, 
    old_handler_list = list(),
    
    initialize = function(save_path, primary_keys = NULL, is_old = F
                          ) {
      self$is_old = is_old
      if(self$is_old){
        self$level = self$level - 1
        self$origin_level = 0
      }
      self$save_path <- save_path
      self$primary_keys <- primary_keys # 初期化時に主キーを設定
      if (!dir.exists(self$save_path)) {
        dir.create(self$save_path, recursive = TRUE)
      }
    },

    get_column_name = function() {
      # if (self$level == 0) "trial" else paste("trial", self$level, sep = "")
      if (self$level == self$origin_level) "trial" else paste("trial", self$level, sep = "")
    },
    
    get_seq_id = function() {
      # if (self$level == 0) "trial" else paste("trial", self$level, sep = "")
      if (self$level == self$origin_level) "seq_id" else paste("seq_id", self$level, sep = "")
    },

    set_params = function(trial, param_tbl) {
      
      column_name <- self$get_column_name()
      seq_id_name <- self$get_seq_id()
      param_tbl = param_tbl %>% 
        # param_tblに動的に設定した列名でtrial値を追加
        mutate(!!sym(column_name) := .env$trial) 
      
      if ("seq_id" %in% colnames(param_tbl)　) {
        stop("Error: Don't use seq_id")
      }
      # 主キーが設定されている場合、その列だけを使用して重複をチェック
      # param_tblのすべての列について重複があるかチェック
      if (any(duplicated(param_tbl))) {
        stop("Error: Duplicate entries found in param_tbl.")
      }

      file_path <- file.path(self$save_path, "param_tbl.rds")
      if (file.exists(file_path)) {
        existing_param_tbl <- readRDS(file_path)
        
        if(seq_id_name %in% colnames(existing_param_tbl)){
          max_seq_str = gsub("[0-9]", "", existing_param_tbl[[seq_id_name]]) %>% 
            unique() %>% 
            find_max_incremented_string()
          seq_str = increment_string(max_seq_str)
          param_tbl <- param_tbl %>% 
            # 連番をつける
            mutate(seq_no = row_number())
          
          # 連番と文字の組み合わせ(Ex:a1,a2,...)
          param_tbl = param_tbl %>% 
            mutate(!!sym(seq_id_name) := paste0(seq_str, seq_no)) %>% 
            select(-seq_no)
        }

        if (trial %in% existing_param_tbl[[column_name]]) {
          # 差分を保存する
          filtered_param_tbl <- param_tbl %>% select(- any_of(seq_id_name)) %>% 
            anti_join(existing_param_tbl, by = setdiff(names(param_tbl), seq_id_name))
          if(nrow(filtered_param_tbl) == 0){
            warning("Warning: The trial already exists in the existing param_tbl 
                    and I don't save that.Please set another param_tbl or load 
                    the existing param_tbl.") 
          }else{
            updated_param_tbl2 = existing_param_tbl %>% 
              bind_rows(filtered_param_tbl %>% left_join(param_tbl))
            saveRDS(updated_param_tbl2, file_path)
          }
          
        } else {
          updated_param_tbl <- bind_rows(existing_param_tbl, param_tbl)
          saveRDS(updated_param_tbl, file_path)
        }
      } else {
        param_tbl = param_tbl %>% 
          mutate(!!sym(seq_id_name) := paste0("a", row_number())) 
        saveRDS(param_tbl, file_path)
      }
    },

    load_params = function(trial = NULL, remove_seq_id = FALSE,
                           merge_with_parents = FALSE
                           ) {
      param_file_path <- file.path(self$save_path, "param_tbl.rds")
      param_tbl <- readRDS(param_file_path)
      if (!is.null(trial)) {
        column_name <-self$get_column_name()
        param_tbl = param_tbl %>%
          filter(!!sym(column_name) == !!trial)
      }
      if (remove_seq_id) {
        seq_id_columns <- grep("seq_id", names(param_tbl), value = TRUE)
        param_tbl <- param_tbl %>% select(-any_of(seq_id_columns))
      }
      
      # old_handler_listが空でなく、include_old_handlersがTRUEの場合
      if (merge_with_parents && length(self$old_handler_list) > 0) {
        for (handler in self$old_handler_list) {
          old_params <- handler$load_params(trial = trial, remove_seq_id = remove_seq_id)
          param_tbl <- param_tbl %>% 
            left_join(old_params)  # ここで結合するキーを適切に指定
        }
      }
      
      return(param_tbl)
    },

    save_results = function(trial, result_name, result_tbl
                            # , file_ext = "rds"
                            ) {
      column_name <- self$get_column_name()
      seq_id_name <- self$get_seq_id()
      # result_tblに動的に設定した列名で引数trial値を追加
      result_tbl <- result_tbl %>%
        mutate(!!sym(column_name) := .env$trial)

      param_file_path <- file.path(self$save_path, "param_tbl.rds")
      if (!file.exists(param_file_path)) {
        stop("Error: Parameter table does not exist.")
      } else {
        param_tbl <- readRDS(param_file_path) 
        
        # 主キー列とトライアル列を結合してチェック列を作成
        check_cols <- c(self$primary_keys, column_name)
        
        # result_tblにseq_id_nameが含んでいない場合、
        # seq_id_nameはresult_tblに存在しないので、取り除いておく
        if(!seq_id_name %in% colnames(result_tbl)){
          param_tbl <- param_tbl %>% 
            select(- any_of(seq_id_name))
          check_cols <- setdiff(check_cols, seq_id_name)
        }
     
        if (!trial %in% param_tbl[[column_name]]) {
          stop("Error: Specified trial does not exist in the parameter table.")
        }

        # 主キーが設定されているかチェック
        if (!is.null(self$primary_keys)) {


          # result_tblからparam_tblの主キー列のユニークな組み合わせに一致する行を抽出
          result_tbl_filtered <- result_tbl %>%
            dplyr::semi_join(param_tbl, by = check_cols)
          
        } else {
          # 主キーが設定されていない場合、すべての列を使用してチェック
          result_tbl_filtered <- result_tbl %>%
            dplyr::semi_join(param_tbl, by = setdiff(names(param_tbl), column_name))
          # print(param_tbl %>% colnames())
          # print(result_tbl %>% colnames())
          # print(result_tbl_filtered)
          # print(check_cols %>% colnames())
        }

        # 抽出した結果が空の場合エラーを出す
        if (nrow(result_tbl_filtered) == 0) {
          stop("Error: result_tbl does not contain any matching combinations from param_tbl.")
        }
      }

      result_dir_path <- file.path(self$save_path, result_name)
      if (!dir.exists(result_dir_path)) {
        dir.create(result_dir_path)
      }

      trial_dir_path <- file.path(result_dir_path, paste0(trial, ".rds"))
      saveRDS(result_tbl_filtered, trial_dir_path)
      
      # save_object(result_tbl_filtered, file.path(result_dir_path, trial), file_ext=file_ext)
      
      message("Results saved for trial: ", trial)
    },
    
    # load_resultsで呼び込むデータについても同じ処理が必要
    # それができるまでコメントアウトにしておく
    # update_params = function(with_op){
    #   param_file_path <- file.path(self$save_path, "param_tbl.rds")
    #   param_tbl = readRDS(param_file_path) %>% 
    #     with_op()
    #   saveRDS(param_tbl, param_file_path) 
    # },

    load_results = function(result_name, trial = NULL, add_params = F,
                            trials = NULL , file_ext = "rds",
                            merge_with_parents = F
                            ) {
      result_dir_path <- file.path(self$save_path, result_name)
      if (!dir.exists(result_dir_path)) {
        stop(paste0("Error: Result directory does not exist. Path: ", 
                    result_dir_path))
      }
      param_file_path <- file.path(self$save_path, "param_tbl.rds")
      if (!file.exists(param_file_path)) {
        stop("Error: param_tbl does not exist.")
      }
      # param_tbl <- readRDS(param_file_path)
      param_tbl = self$load_params(remove_seq_id = FALSE, 
                       merge_with_parents = merge_with_parents)
      # 両方が非NULLの場合のエラーチェック
      if (!is.null(trial) && !is.null(trials)) {
        stop("Error: Specify only one of 'trial' or 'trials', not both.")
      }
      
      if(file_ext == "rds"){
        obj_func = readRDS
      }else if(file_ext == "fst"){
        obj_func = read_fst
      }else{
        stop("Error: Incorrect file_ext.")
      }
      
      
      if (is.null(trial) & is.null(trials)) {
        # trialが指定されていない場合、すべての結果を読み込む
        result_list <- lapply(result_dir_path, function(dir_path) {
          # result_files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
          # lapply(result_files, readRDS)
          result_files <- list.files(dir_path, pattern = "\\." %+% file_ext %+% "$", full.names = TRUE)
          lapply(result_files, obj_func)
        }) %>% unlist(recursive = FALSE)
        ret_tbl = bind_rows(result_list)
      } 
      if(!is.null(trial) & is.null(trials)) {
        # trialが指定されている場合、そのtrialの結果を読み込む
        trial_dir_path <- result_dir_path
        # result_file <- file.path(trial_dir_path, paste0( trial, ".rds"))
        result_file <- file.path(trial_dir_path, paste0(trial, ".", file_ext))
        if (!file.exists(result_file)) {
          stop("Error: Result file for the specified trial does not exist.")
        }
        # ret_tbl = readRDS(result_file)
        ret_tbl = obj_func(result_file)
      }
      ## ユーザーの指定した複数の結果を読み込み
      if(!is.null(trials)){
        results_list <- lapply(trials, function(trial_item) {
          # result_file_path <- file.path(result_dir_path, paste0(trial_item, ".rds"))
          result_file_path <- file.path(result_dir_path, paste0(trial_item, ".", file_ext))
          if (file.exists(result_file_path)) {
            # readRDS(result_file_path)
            obj_func(result_file_path)
          } else {
            message("File does not exist:", result_file_path)
            NULL  # ファイルが存在しない場合はNULLを返す
          }
        })
        ret_tbl = results_list %>% bind_rows()
        
      }
      
      # if(self$level == 0){
      #   ret_tbl %>%
      #     filter(trial %in% param_tbl$trial)
      # }else{
      #   ret_tbl %>%
      #     filter(!!sym(self$get_column_name()) %in% param_tbl[["trial" %+% self$level]])
      # }
      ret_tbl = ret_tbl %>%
        filter(!!sym(self$get_column_name()) %in% param_tbl[[self$get_column_name()]])

      if(add_params){

    
        ret_tbl = ret_tbl %>%
          left_join(param_tbl)
      }
      ret_tbl

    },
    
    load_results_folder = function(result_name, pattern="", cols=NULL,
                                   join_type = "bind_rows", by = NULL,
                                   transform_fn = function(x){ x },
                                   add_params = F, merge_with_parents = F
                            
    ) {
      data_list = list.files(path = self$save_path %>% 
                   paste0("//",result_name  , "//"), 
                 pattern = pattern, full.names = TRUE) %>% 
        map(~ {
          data <- readRDS(.x)
          if (is.null(cols)) {
            data = data %>% select(everything()) 
          } else {
            data = data %>% select(any_of(cols))
          }
          data %>% transform_fn()
        })
      # データの結合方法を選択
      combined_data <- switch(join_type,
                              "bind_rows" = bind_rows(data_list),
                              "left_join" = reduce(data_list, left_join, by = by),
                              "inner_join" = reduce(data_list, inner_join, by = by),
                              stop("Invalid join_type specified.")
      )
      if(add_params){
        param_tbl = self$load_params(merge_with_parents = merge_with_parents)
        combined_data = combined_data %>% 
          left_join(param_tbl)
                                      
      }
      combined_data
    },
    
  load_wider_results_folder = function(result_name, pattern="",
                                        names_from = "key", values_from = "value",
                                        id_names = "id"
                                        ){
    # pivot_longer の設定を行う transform_fn を定義
    transform_fn <- function(df) {
      # force(names_from)
      # force(values_from)
      df %>% 
        select(id_names, names_from, values_from) %>% 
        pivot_wider(
          names_from = names_from, 
          values_from = values_from)
    }
    
    # load_results_folder を呼び出し、pivot_longerを適用
    combined_data <- self$load_results_folder(
      result_name = result_name,
      pattern = pattern,
      join_type = "left_join",
      by = id_names,
      transform_fn = transform_fn
    )
    
    return(combined_data)
      
      
    },

    # configデータを保存する関数
    save_config = function(trial, config_tbl) {
      # 列名を動的に設定
      column_name <- self$get_column_name()

      # result_tblに動的に設定した列名で引数trial値を追加
      config_tbl <- config_tbl %>%
        mutate(!!sym(column_name) := trial)

      config_dir_path <- file.path(self$save_path, "config")
      if (!dir.exists(config_dir_path)) {
        dir.create(config_dir_path)
      }
      config_file_path <- file.path(config_dir_path, paste0(trial, ".rds"))
      saveRDS(config_tbl, config_file_path)
      message(paste("Config saved at:", config_file_path))
    },

    # configデータを読み込む関数
    load_config = function(trial=NULL) {
      config_dir_path <- file.path(self$save_path, "config")

      if (is.null(trial)) {
        # trialがNULLの場合、configディレクトリ内の全てのRDSファイルを読み込む
        config_files <- list.files(config_dir_path, pattern = "\\.rds$", full.names = TRUE)

        if (length(config_files) == 0) {
          stop("No config files found in the directory:", config_dir_path)
        }

        # 全てのファイルを読み込み、結合し、trial列を追加
        config_list <- lapply(config_files, function(file) {
          config_data <- readRDS(file)
          trial_name <- tools::file_path_sans_ext(basename(file))
          config_data <- tibble::as_tibble(config_data)
          return(config_data)
        })
        combined_config <- bind_rows(config_list)

        return(combined_config)
      } else {
        # trialが指定されている場合、そのtrialのファイルを読み込む
        config_file_path <- file.path(config_dir_path, paste0(trial, ".rds"))

        if (file.exists(config_file_path)) {
          config_data <- readRDS(config_file_path)
          config_data <- tibble::as_tibble(config_data)
          return(config_data)
        } else {
          stop(paste("File does not exist:", config_file_path))
        }
      }
    },

    save_interm_result = function(trial, result_name, result_tbl,
                                  folder_name=NULL) {
      # intermediateフォルダのパスを作成
      intermediate_dir_path <- file.path(self$save_path, "intermediate", trial)

      # intermediateフォルダが存在しない場合は作成
      if (!dir.exists(intermediate_dir_path)) {
        dir.create(intermediate_dir_path, recursive = TRUE)
      }

      # folder_nameがNULLでない場合、その名前のフォルダをさらに作成
      if (!is.null(folder_name)) {
        intermediate_dir_path <- file.path(intermediate_dir_path, folder_name)
        if (!dir.exists(intermediate_dir_path)) {
          dir.create(intermediate_dir_path)
        }
      }

      # 結果を保存するファイルのパス
      result_file_path <- file.path(intermediate_dir_path,
                                    paste0(result_name, ".rds"))

      # 結果をRDSファイルとして保存
      saveRDS(result_tbl, result_file_path)

      message(paste("Intermediate result saved at:", result_file_path))
    },

    # intermデータを読み込む関数
    load_interm_result = function(trial=NULL, result_name=NULL, folder_name=NULL,
                                  trials = NULL, 
                                  # すべてのファイルを読み込む
                                  is_all_files = F
                                  ) {
      # 両方がNULL、または両方が非NULLの場合のエラーチェック
      if (is.null(trial) && is.null(trials)) {
        stop("Error: Either 'trial' or 'trials' must be specified.")
      }
      if (!is.null(trial) && !is.null(trials)) {
        stop("Error: Specify only one of 'trial' or 'trials', not both.")
      }
      
      # interm_dir_path <- file.path(self$save_path, "intermediate", trial)
      # 
      # if (!is.null(folder_name)) {
      #   interm_dir_path <- file.path(interm_dir_path, folder_name)
      # }
      # 
      # result_file_path <- file.path(interm_dir_path, paste0(result_name, ".rds"))
      # 
      # if (file.exists(result_file_path)) {
      #   return(readRDS(result_file_path))
      # } else {
      #   stop(paste("File does not exist:", result_file_path))
      # }
      
      # 単一の結果を読み込む場合
      if (!is.null(trial)) {
        trials = c(trial)
      }
      # 複数の結果を読み込む場合
      if (!is.null(trials)) {
        results_list <- lapply(trials, function(trial_item) {
          interm_dir_path <- file.path(self$save_path, "intermediate", trial_item)
          if (!is.null(folder_name)) {
            interm_dir_path <- file.path(interm_dir_path, folder_name)
          }
          result_file_path <- file.path(interm_dir_path, paste0(result_name, ".rds"))
          if (file.exists(result_file_path)) {
            readRDS(result_file_path)
          } else {
            if(is_all_files){
              result_file_path <- file.path(interm_dir_path, result_name)
              print("is_all_files=T")
              print(result_file_path)
              list.files(path = result_file_path, full.names = TRUE) %>% 
                map(readRDS) %>% bind_rows()
            }else{
              message("File does not exist:", result_file_path)
              NULL  # ファイルが存在しない場合はNULLを返す 
            }
          }
        })
        # if(length(results_list) == 1) results_list = results_list[[1]]
        results_list = results_list %>% bind_rows()
        return(results_list)
      }
      
    },
  
  load_interm_folder = function(interm_name, pattern="", cols=NULL,
                                 join_type = "bind_rows", by = NULL,
                                 transform_fn = function(x){ x },
                                 add_params = F, merge_with_parents = F
                                 
    ) {
    
    data_list = list.files(path = self$save_path %>% 
                             paste0("//intermediate//"), 
                           full.names = TRUE) %>% 
      grep(pattern, ., value = TRUE) %>% 
      paste0("//", interm_name, ".rds") %>% 
      map(~ {
        data <- readRDS(.x)
        if (is.null(cols)) {
          data = data %>% select(everything()) 
        } else {
          data = data %>% select(any_of(cols))
        }
        data %>% transform_fn()
      })
    
    
      # データの結合方法を選択
      combined_data <- switch(join_type,
                              "bind_rows" = bind_rows(data_list),
                              "left_join" = reduce(data_list, left_join, by = by),
                              "inner_join" = reduce(data_list, inner_join, by = by),
                              stop("Invalid join_type specified.")
      )
      if(add_params){
        param_tbl = self$load_params(merge_with_parents = merge_with_parents)
        combined_data = combined_data %>%
          left_join(param_tbl)

      }
      combined_data
    },
  
    # otherデータを保存する関数
    save_other_data = function(data_name, data_tbl, folder_name = NULL) {
      other_dir_path <- file.path(self$save_path, "other")
      if (!dir.exists(other_dir_path)) {
        dir.create(other_dir_path)
      }
      # folder_nameがNULLでない場合、その名前のフォルダをさらに作成
      if (!is.null(folder_name)) {
        other_dir_path <- file.path(other_dir_path, folder_name)
        if (!dir.exists(other_dir_path)) {
          dir.create(other_dir_path)
        }
      }
      
      data_file_path <- file.path(other_dir_path, paste0(data_name, ".rds"))
      saveRDS(data_tbl, data_file_path)
      message(paste("Other data saved at:", data_file_path))
    },


    # otherデータを読み込む関数
    load_other_data = function(data_name, folder_name = NULL) {
      other_dir_path <- file.path(self$save_path, "other")
      if (!is.null(folder_name)) {
        other_dir_path <- file.path(other_dir_path, folder_name)
      }
      data_file_path <- file.path(other_dir_path, paste0(data_name, ".rds"))

      if (file.exists(data_file_path)) {
        return(readRDS(data_file_path))
      } else {
        stop(paste("File does not exist:", data_file_path))
      }
    },

    # delete = function(){
    #   if (dir.exists(self$save_path)) {
    #     unlink(self$save_path, recursive = TRUE)
    #     message("Directory deleted: ", self$save_path)
    #   }
    # },

    delete_params = function(trial = NULL) {
      param_file_path <- file.path(self$save_path, "param_tbl.rds")
      column_name <- self$get_column_name()
      if (!file.exists(param_file_path)) {
        warning("Warning: Parameter file does not exist.")
      }

      if (is.null(trial)) {
        # trialが指定されていない場合、param_tbl.rdsファイルを削除してすべてのパラメータをクリア
        unlink(param_file_path)
        message("All parameters have been deleted.")
      } else {
        # trialが指定されている場合、対応するレコードを削除
        param_tbl <- readRDS(param_file_path)
        filtered_param_tbl <- param_tbl %>% filter(!!sym(column_name) != !!trial)

        if (nrow(filtered_param_tbl) == nrow(param_tbl)) {
          stop("Error: Specified trial does not exist in the parameter table.")
        }

        saveRDS(filtered_param_tbl, param_file_path)
        message(paste("Parameters for trial", trial, "have been deleted."))
        if(nrow(readRDS(param_file_path)) == 0){
          file.remove(param_file_path)
          message(paste(param_file_path, "have been deleted as its rows is zero."))
        } 
      }
      
    },

    delete_results = function(result_name, trial = NULL) {
      result_dir_path <- file.path(self$save_path, result_name)

      if (!dir.exists(result_dir_path)) {
        warning("Warning: Result directory does not exist.")
      }

      if (is.null(trial)) {
        # trialが指定されていない場合、result_nameに関連するすべてのオブジェクトを削除
        unlink(result_dir_path, recursive = TRUE)
        message("All results under ", result_name, " have been deleted.")
      } else {
        # trialが指定されている場合、そのtrialに関連するオブジェクトを削除
        trial_dir_path <- file.path(result_dir_path, paste0(trial, ".rds"))
        if (!file.exists(trial_dir_path)) {
          stop("Error: Specified trial directory does not exist.")
        }
        unlink(trial_dir_path, recursive = TRUE)
        message("Results for trial ", trial, " under ", result_name, " have been deleted.")
      }
    },
    
    delete_interm_result = function(trial, result_name = NULL, folder_name=NULL) {
      # Adjusted to handle cases when folder_name and result_name are both NULL
      intermediate_dir_path <- file.path(self$save_path, "intermediate", trial)
      if (is.null(folder_name) && is.null(result_name)) {
        # If folder_name and result_name are NULL, delete the trial's entire intermediate directory
        if (dir.exists(intermediate_dir_path)) {
          unlink(intermediate_dir_path, recursive = TRUE)
          message(paste("Deleted all intermediate results for trial:", trial, "at:", intermediate_dir_path))
        } else {
          warning(paste("Directory does not exist:", intermediate_dir_path))
        }
      } else {
        # Specific file removal
        if (!is.null(folder_name)) {
          intermediate_dir_path <- file.path(intermediate_dir_path, folder_name)
        }
        # result_name=NULLの場合はfolder_nameのフォルダ全体を削除
        # このとき、folder_nameは非NULLとなる。
        if(is.null(result_name)){
          unlink(intermediate_dir_path, recursive = TRUE)
          message(paste("Deleted all intermediate results for trial:", trial, "at:", intermediate_dir_path))
        }else{
          # result_name=非NULLの場合、folder_nameが非NULLの場合は上記で対応済み、
          result_file_path <- file.path(intermediate_dir_path, paste0(result_name, ".rds"))
          if (file.exists(result_file_path)) {
            file.remove(result_file_path)
            message(paste("Deleted intermediate result at:", result_file_path))
          } else {
            warning(paste("File does not exist:", result_file_path))
          }
        }
        

      }
    },

    create_subdir_obj = function(sub_dir_name, primary_keys = NULL) {
      new_level <- self$level + 1  # 新たに生成されるオブジェクトのlevel
      new_dir_name <- paste0(sub_dir_name, "_", new_level)  # 新しいフォルダ名
      new_dir_path <- file.path(self$save_path, new_dir_name)  # 新しいフォルダのパス
      
      # 新しいフォルダが存在しているかチェック
      if (dir.exists(new_dir_path)) {
        warning("Warning: Subdirectory already exists.")
      } else {
        # 新しいフォルダを作成
        dir.create(new_dir_path)
        message("New subdirectory created: ", new_dir_name)
      }
      is_old = self$is_old
      # 新しいParameterResultHandlerオブジェクトを生成
      new_handler <- ParameterResultHandler$new(new_dir_path, primary_keys, is_old)
      new_handler$level <- new_level  # levelを設定
      new_handler$sub_dir_name <- sub_dir_name
      
      if(!is_old){
        new_handler$old_handler_list = self$old_handler_list
        new_handler$old_handler_list[[self$level]] = self 
      }

      return(new_handler)
    },

    convert_rds = function(file_name, target_ext = "fst", is_delete = FALSE) {
      if(grepl(self$save_path, file_name)){
        file_path = file_name
      }else{
        file_path <- file.path(self$save_path, file_name) 
      }
      
      # 拡張子の確認
      if (tolower(tools::file_ext(file_path)) != "rds") {
        stop("The file is not an RDS file.")
      }
      
      # target_ext が "fst" 以外の場合は何もせず終了
      if (tolower(target_ext) != "fst") {
        message("Conversion to the specified target extension is not supported.")
        return(NULL)
      }
      
      # RDSファイルを読み込む
      data <- readRDS(file_path)
      
      # 新しいファイルパス
      new_file_path <- sub("\\.rds$", paste0(".", target_ext), file_path, ignore.case = TRUE)
      
      # fst形式で保存
      write_fst(data, new_file_path)
      
      # 元のファイルを削除するかどうか
      if (is_delete) {
        file.remove(file_path)
      }
      
      return(new_file_path)
    },

    convert_results_rds = function(result_name, trial = NULL, trials = NULL ,
                                        file_ext = "fst", is_delete = FALSE) {
      result_dir_path <- file.path(self$save_path, result_name)
      # 両方が非NULLの場合のエラーチェック
      if (!is.null(trial) && !is.null(trials)) {
        stop("Error: Specify only one of 'trial' or 'trials', not both.")
      }
      
      if (is.null(trial) & is.null(trials)) {
        # trialが指定されていない場合、すべての結果を読み込む
        result_files <- lapply(result_dir_path, function(dir_path) {
          list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
        }) %>% unlist(recursive = FALSE)
      } 
      if(!is.null(trial) & is.null(trials)) {
        result_file = file.path(result_dir_path, paste0( trial, ".rds"))
        if (!file.exists(result_file)) {
          stop("Error: Result file for the specified trial does not exist.")
        }
        # trialが指定されている場合、そのtrialの結果を読み込む
        result_files <- c(result_file)
      }
      ## ユーザーの指定した複数の結果を読み込み
      if(!is.null(trials)){
        result_files <- lapply(trials, function(trial_item) {
          result_file = file.path(result_dir_path, paste0(trial_item, ".rds"))
          if (!file.exists(result_file)) {
            stop("Error: Result file for the specified trial does not exist.")
          }
          result_file
        })  %>% unlist(recursive = FALSE)
      }
      
      lapply(result_files, function(file_name) {
        self$convert_rds(file_name, file_ext, is_delete) 
      })
      
    },
    get_fields = function(include_functions = FALSE) {
      # オブジェクトの名前を取得
      all_fields <- ls(self)
      
      # include_functions が FALSE の場合、関数を除外
      if (!include_functions) {
        all_fields <- all_fields[!sapply(mget(all_fields, envir = self), is.function)]
      }
      
      # フィールドをリスト形式で取得
      fields <- mget(all_fields, envir = self)
      return(fields)
    },
    
    restore_seq_ids = function(result_name){
      # file_path <- file.path(self$save_path, "param_tbl.rds")
      # seq_id_name <- self$get_seq_id()
      # existing_param_tbl <- readRDS(file_path)
      # existing_param_tbl = existing_param_tbl %>% 
      #   mutate(!!sym(seq_id_name) := paste0("a", row_number()))
      # saveRDS(existing_param_tbl, file_path)
      
      file_path <- file.path(self$save_path, "param_tbl.rds")
      existing_param_tbl <- readRDS(file_path)
      seq_id_name <- self$get_seq_id()
      result_tbl = self$load_results(result_name)
      param_tbl2 = existing_param_tbl %>% 
        select(- any_of(seq_id_name) )
      
      new_param_tbl = result_tbl %>% 
        select(colnames(param_tbl2), seq_id_name)
      saveRDS(new_param_tbl, file_path)
    },
    
    update_column_in_params = function(col_name, update_f, 
                                       trials = NULL, is_test = TRUE
                                       ){
      file_path <- file.path(self$save_path, "param_tbl.rds")
      existing_param_tbl <- readRDS(file_path)
      remain_param_tbl = tibble()
      column_name <- self$get_column_name()
      if(!is.null(trials)){
        remain_param_tbl = existing_param_tbl %>% 
          filter(!(!!sym(column_name) %in% trials))
        existing_param_tbl = existing_param_tbl %>% 
          filter(!!sym(column_name) %in% trials )
        
      }
      
      # 新しい列を作成し、指定された関数を適用
      existing_param_tbl <- existing_param_tbl %>%
        mutate(!!col_name := update_f(.))
      
      update_param_tbl = existing_param_tbl %>% 
        bind_rows(remain_param_tbl)
      if(!is_test){
        # 変更後のtibbleを保存
        saveRDS(update_param_tbl, file_path)
      }
      update_param_tbl
      
    }
  )
  


)









