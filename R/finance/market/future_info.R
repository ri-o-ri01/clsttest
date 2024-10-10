#環境はRにおいて不変のリストとして機能し、Enumのように使用できます。環境内の
#オブジェクトは参照によって渡されるため、Enumの値を変更することはできません。
FUT_TICK_SIZE_LIST<- new.env()
FUT_TICK_SIZE_LIST$TP <- 0.5
FUT_TICK_SIZE_LIST$JB <- 0.01
FUT_TICK_SIZE_LIST$NK <- 10
lockEnvironment(FUT_TICK_SIZE_LIST, bindings = TRUE) # 環境をロックして変更不可にする

FUT_MULTIPLIER <- new.env()
FUT_MULTIPLIER$TP <- 10000
# FUT_TICK_SIZE_LIST$JB <-
FUT_MULTIPLIER$NK <- 1000
lockEnvironment(FUT_MULTIPLIER, bindings = TRUE) # 環境をロックして変更不可にする

FUT_NIGHT_SESSION_START_DATE<- new.env()
FUT_NIGHT_SESSION_START_DATE$TP = as.Date("2008-06-16")
FUT_NIGHT_SESSION_START_DATE$NK = as.Date("2007-09-18")
lockEnvironment(FUT_NIGHT_SESSION_START_DATE, bindings = TRUE) # 環境をロックして変更不可にする
