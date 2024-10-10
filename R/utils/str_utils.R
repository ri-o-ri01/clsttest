increment_string <- function(x) {
  # 文字列を逆順にして処理（末尾から処理）
  chars <- unlist(strsplit(x, NULL))
  len <- length(chars)
  
  # 末尾から処理を開始
  for (i in len:1) {
    if (chars[i] == "z") {
      chars[i] <- "a"  # zの場合はaにリセット
      if (i == 1) {
        chars <- c("a", chars)  # 最初の桁がzだったら桁を増やす
      }
    } else {
      chars[i] <- intToUtf8(utf8ToInt(chars[i]) + 1)  # 次のアルファベットに進む
      break
    }
  }
  
  return(paste0(chars, collapse = ""))
}

find_max_incremented_string <- function(strings) {
  # 桁数の大きさでソートし、桁数が同じ場合は辞書順でソート
  sorted_strings <- strings[order(nchar(strings), strings, decreasing = TRUE)]
  return(sorted_strings[1])
}

my_split <- function(input_string, separator) {
  # 文字列をseparatorで分割し、結果をベクトルとして返す
  return(unlist(strsplit(input_string, separator)))
}

join_non_empty_strings <- function(...) {
  strings <- c(...)  # 可変長引数をベクトルに変換
  strings %>%
    discard(is.na) %>%
    discard(str_detect, pattern = "^\\s*$") %>%
    str_c(collapse = "-")
}

`%+%` <- function(e1, e2) {
  paste0(e1, e2)
}