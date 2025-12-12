###############################################
# JS 펀드 모니터링 메인 스크립트 (루프 버전)
# - stock_eval.R / stock_eval_us.R 필요
# - risk_module.R의 몬테카, MDD, 인출, 팩터, PCA를 모두 호출
###############################################

# 1) 필요한 패키지 전부 설치 ------------------------------------------
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr", "PerformanceAnalytics")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

# 2) 로드 --------- ctrl + alt + e
library(readr);   library(readxl)
library(openxlsx); library(rvest); library(httr)
library(dplyr);   library(ggplot2); library(scales)
library(patchwork); library(treemap); library(DT)
library(stringr); library(PerformanceAnalytics)

setwd("c:\\easy_r")

options(scipen = 999)

# ★ 리스크 + 팩터 + PCA 모듈 로드
source("risk_module.R")

update_factor_data()

count <- 1
last_mc_date <- as.Date(NA)

repeat {
  now  <- as.POSIXct(Sys.time())
  hhmm <- format(now, "%H:%M")
  wday <- as.numeric(format(now, "%u"))  # 1=월 ~ 7=일
  week_kor <- c("일", "월", "화", "수", "목", "금", "토")
  
  in_fast_range <- hhmm >= "08:40" & hhmm <= "15:30"
  
  cat("[", count, "회차]", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),
      ": 실행 시작***********************************************\n")
  
  # 현재 보유자산 평가 업데이트 -------------------------------------
  source("stock_eval.R")      # data_ko, exchange_rate 등
  source("stock_eval_us.R")   # data_en 등
  
  today <- Sys.Date()
  
  file1 <- paste0("output_stock_",    today, ".xlsx")
  file2 <- paste0("output_stock_us_", today, ".xlsx")
  output_file <- "output_sum.csv"
  
  column_name  <- "평가금"
  column_name2 <- "수익금"
  
  data1 <- read_excel(file1)
  data2 <- read_excel(file2)
  
  last_value1   <- tail(data1[[column_name]],  1)
  last_value1_2 <- tail(data1[[column_name2]], 1)
  
  last_value2   <- tail(data2[[column_name]],  1)
  last_value2_2 <- tail(data2[[column_name2]], 1)
  
  sum_value    <- round(last_value1 + last_value2, 0)
  profit_value <- round(last_value1_2 + last_value2_2, 0)
  
  yesugum <- 0
  yegum   <- 0
  sum_value <- round(sum_value + yegum, 0)
  
  result <- data.frame(Date = today, Sum = sum_value, Profit = profit_value)
  
  # output_sum.csv 갱신 ----------------------------------------------
  if (file.exists(output_file)) {
    existing_data <- read_csv(output_file,
                              col_types = cols(
                                Date   = col_date(format = ""),
                                Sum    = col_double(),
                                Profit = col_double()
                              ), 
                              show_col_types = FALSE)
    
    if (nrow(existing_data) > 0 && tail(existing_data$Date, 1) == Sys.Date()) {
      existing_data <- existing_data[-nrow(existing_data), ]
    }
    
    updated_data <- bind_rows(existing_data, result)
    
  } else {
    updated_data <- result
  }
  
  write_csv(updated_data, output_file)
  
  # 분석용 데이터 재읽기 ---------------------------------------------
  dd <- read_csv(output_file,
                 col_types = cols(
                   Date   = col_date(format = ""),
                   Sum    = col_double(),
                   Profit = col_double()
                 ))
  
  dd <- dd %>% mutate(Return = Profit / (Sum - Profit))
  
  
  
  
  # ========================================================================
  # === PerformanceAnalytics 블록 시작 =====================================
  #  - 평가금(Sum) 시계열 → 일별 수익률 → 연환산 성과/Sharpe/MDD 계산
  # ========================================================================
  
  dd_daily <- dd %>%
    group_by(Date) %>%
    summarise(Sum = last(Sum), .groups="drop") %>%
    arrange(Date)
  
  sum_xts <- xts(dd_daily$Sum, order.by = dd_daily$Date)
  ret_xts <- Return.calculate(sum_xts, method="discrete")[-1]
  # 
  # # 1) 날짜순 정렬 (혹시 순서가 꼬였을 경우를 대비)
  # dd <- dd %>% arrange(Date)
  # 
  # # 2) 평가금 시계열을 xts로 변환
  # sum_xts <- xts(dd$Sum, order.by = dd$Date)
  # 
  # # 3) 기간별(일별) 수익률 계산
  # ret_xts <- PerformanceAnalytics::Return.calculate(sum_xts, method = "discrete")
  # ret_xts <- ret_xts[-1, , drop = FALSE]  # 첫 행 NA 제거
  colnames(ret_xts) <- "JS_Fund"
  
  # 4) 성과 요약 출력
  cat("\n=========== PerformanceAnalytics 성과 요약 ===========\n")
  print(table.AnnualizedReturns(ret_xts))
  cat("\nMax Drawdown:\n")
  print(maxDrawdown(ret_xts))
  cat("Sharpe(연환산, Rf=0):\n")
  print(SharpeRatio.annualized(ret_xts, Rf = 0))
  cat("======================================================\n\n")
  
  # ========================================================================
  # === PerformanceAnalytics 블록 끝 =======================================
  # ========================================================================
  
  
  
  
  
  
  
  
  
  
  today_date <- max(dd$Date, na.rm = TRUE)
  
  # 5-1) 적립식 10년 Monte Carlo -------------------------------------
  if (is.na(last_mc_date) || last_mc_date < today_date) {
    cat("\n[리스크] 오늘 기준 몬테카를로 10년 스트레스 테스트 실행...\n")
    run_mc_from_dd(
      dd,
      years           = 10,
      monthly_contrib = 5000000,
      n_sims          = 5000
    )
    
    cat("[리스크] 미래 10년 최대낙폭(MDD) 분포 시뮬레이션 실행...\n")
    run_future_mdd_from_dd(
      dd,
      years           = 10,
      monthly_contrib = 5000000,
      n_sims          = 2000
    )
    
    cat("[리스크] 은퇴 후 30년, 연 2억 인출 시나리오(현재자산 기준) 시뮬레이션 실행...\n")
    run_mc_withdraw_from_dd(
      dd,
      years           = 30,
      annual_withdraw = 200000000,
      n_sims          = 5000,
      withdraw_freq   = "monthly"
      # initial_value 기본값(NULL) → 현재 Sum으로 시작
    )
    
    # (선택 예시) 10년 후 60억으로 은퇴했다고 가정한 시나리오도 보고 싶다면:
    # cat("[리스크] 가정: 10년 후 60억으로 은퇴, 연 2억 인출 시나리오 시뮬레이션...\n")
    # run_mc_withdraw_from_dd(
    #   dd,
    #   years           = 30,
    #   annual_withdraw = 200000000,
    #   n_sims          = 5000,
    #   withdraw_freq   = "monthly",
    #   initial_value   = 60000000000   # 60억 가정
    # )
    weights <- c(
      asset_SPY_ETC / today_tsum,
      asset_SCHD    / today_tsum,
      asset_QQQ     / today_tsum,
      asset_TQQQ    / today_tsum,
      asset_GLD     / today_tsum,
      asset_BOND    / today_tsum
    )
    
    # ★ 팩터 분석: factors_monthly.csv 가 있을 때만 실행 ----------------
    #  - 예: Date, MKT, VALUE, SIZE, MOM ... 형태의 월간 팩터 수익률 데이터
    if (file.exists("factors_monthly.csv")) {
      # ===== PCA 기반 리스크 분해 =====
      cat("[리스크] PCA 기반 리스크 분해(Principal Component Risk) 실행...\n")
      #run_pca_dashboard_from_file("asset_returns_monthly.csv", weights)
    } else {
      cat("[리스크] 팩터 데이터(factors_monthly.csv)를 찾을 수 없어 팩터 분석을 건너뜁니다.\n")
    }
    
    # ★ PCA 분석: asset_returns_monthly.csv 가 있을 때만 실행 ----------
    #  - 예: Date, SPY, SCHD, QQQ, TQQQ, GOLD, BOND 형식의 월간 수익률
    if (file.exists("asset_returns_monthly.csv")) {
      #cat("[리스크] PCA 기반 리스크 분해(Principal Component Risk) 실행...\n")
      # 자산별 장기 목표 비중 또는 현재 비중 사용 (예시 비중)
      # weights <- c(
      #   0.40,  # SPY등
      #   0.20,  # SCHD
      #   0.15,  # QQQ
      #   0.10,  # TQQQ
      #   0.10,  # GOLD
      #   0.05   # BOND
      # )
      run_pca_dashboard_from_file("asset_returns_monthly.csv", weights)
    } else {
      cat("[리스크] PCA용 자산수익률 파일(asset_returns_monthly.csv)이 없어 PCA 분석을 건너뜁니다.\n")
    }
    
    last_mc_date <- today_date
  } else {
    cat("\n[리스크] 오늘(", format(today_date),
        ") 몬테카를로는 이미 실행됨 (다음날 재실행)\n\n", sep = "")
  }
  
  
  # 이하 부분은 기존 JS 펀드 모니터링 로직 그대로 -------------------
  sum_left  <- dd$Sum / 10000000
  ret_right <- dd$Return * 100
  
  sum_range     <- range(sum_left,  na.rm = TRUE)
  return_range  <- range(ret_right, na.rm = TRUE)
  
  a <- diff(sum_range) / diff(return_range)
  b <- sum_range[1] - a * return_range[1]
  
  start_date <- format(min(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  end_date   <- format(max(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  plot_title <- paste0("JS 펀드 주식평가액 분석 (", start_date, " ~ ", end_date, ")  ",
                       format(Sys.time(), "%Y년 %m월 %d일"), 
                       "(",
                       week_kor[as.numeric(format(Sys.Date(), "%w")) + 1], 
                       ") ",
                       format(Sys.time(), "%H시 %M분"))
  
  df <- dd[1:2]
  df$Date <- as.Date(df$Date)
  last_date <- max(df$Date, na.rm = TRUE)
  
  periods <- c(1, 3, 6, 12)
  
  result_period <- data.frame(
    Period       = paste0(periods, "개월 전"),
    Target_Date  = as.Date(NA),
    Closest_Date = as.Date(NA),
    Sum          = NA,
    Diff         = NA
  )
  
  for (i in seq_along(periods)) {
    target <- seq(last_date, length = 2, by = paste0("-", periods[i], " month"))[2]
    idx <- which.min(abs(df$Date - target))
    closest_date <- df$Date[idx]
    sum_value_p <- df$Sum[idx]
    latest_sum <- df$Sum[df$Date == last_date]
    diff_value <- latest_sum - sum_value_p
    
    result_period[i, ] <- c(
      paste0(periods[i], "개월 전"),
      as.character(target),
      as.character(closest_date),
      sum_value_p,
      diff_value
    )
  }
  
  result_period$Sum  <- as.numeric(result_period$Sum)
  result_period$Diff <- as.numeric(result_period$Diff)
  
  # 구성비율 트리맵 ---------------------------------------------------
  dt_ko <- data_ko %>% 
    head(-1) %>% 
    dplyr::select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
  
  dt_en <- data_en %>% 
    head(-2) %>% 
    dplyr::select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
  
  dt_ko <- dt_ko %>% 
    mutate(한화평가금 = 평가금) %>% 
    mutate(한화매수가격 = 매수가격)
  
  dt_en <- dt_en %>% 
    mutate(한화평가금 = 평가금 * exchange_rate) %>% 
    mutate(한화매수가격 = 매수가격 * exchange_rate)
  
  dt_fn <- bind_rows(dt_ko, dt_en)
  
  dt_fn <- dt_fn %>% 
    dplyr::select(-평가금) %>% 
    arrange(desc(한화평가금))
  
  View(dt_fn)
  
  treemap(
    dt_fn,
    index = "종목명",
    vSize = "한화평가금",
    title = "구성비율 트리맵",
    palette = "Set3",
    fontsize.labels = 18,
    fontcolor.labels = "black",
    fontface.labels = 2,
    bg.labels = 0,
    overlap.labels = 0.5,
    inflate.labels = TRUE,
    align.labels = list(c("center","center"))
  )
  
  fit <- lm(sum_left ~ as.numeric(Date), data = dd)
  slope_per_day <- coef(fit)[2]
  
  get_prev_file <- function(prefix = "output_stock_", ext = "xlsx") {
    pattern <- paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.", ext, "$")
    files <- dir(pattern = pattern)
    if (length(files) == 0) return(NA)
    dates <- as.Date(sub(paste0(prefix, "(\\d{4}-\\d{2}-\\d{2})\\.", ext), "\\1", files))
    valid_idx <- which(dates < Sys.Date())
    if (length(valid_idx) == 0) return(NA)
    files[which.max(dates[valid_idx])]
  }
  
  data_prev_ko <- read_excel(get_prev_file("output_stock_"))
  data_prev_en <- read_excel(get_prev_file("output_stock_us_"))
  
  data_prev_ko <- data_prev_ko %>%
    head(-1) %>%
    dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 평가금)
  
  data_prev_en <- data_prev_en %>%
    head(-2) %>%
    mutate(한화평가금 = 평가금 * exchange_rate) %>%
    dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 한화평가금)
  
  data_prev_fn <- bind_rows(data_prev_ko, data_prev_en) %>%
    arrange(desc(전일한화평가금))
  
  join_stock_data <- function(today_df, prev_df) {
    today_df %>%
      distinct(종목번호, 보유증권사, .keep_all = TRUE) %>%
      left_join(prev_df, by = c("종목번호", "보유증권사")) %>%
      mutate(
        한화평가금 = trunc(한화평가금),
        전일한화평가금 = trunc(전일한화평가금),
        전일대비 = trunc(한화평가금 - 전일한화평가금),
        전일대비율 = if_else(
          is.na(전일한화평가금),
          NA_character_,
          sprintf("%.2f", round((한화평가금 - 전일한화평가금) / 전일한화평가금 * 100, 2))
        ),
        비중 = sprintf("%.2f", round(한화평가금 / sum(한화평가금, na.rm = TRUE) * 100, 2))
      ) %>%
      arrange(desc(한화평가금))
  }
  
  rt <- join_stock_data(dt_fn, data_prev_fn) %>%
    mutate(
      총매수금 = 한화매수가격 * 수량,
      총수익금 = 한화평가금 - 총매수금,
      총수익률 = round((총수익금 / 총매수금) * 100, 2)
    ) %>% 
    dplyr::select(-매수가격) %>% 
    dplyr::select(종목명, 보유증권사, 한화매수가격, 수량, 한화평가금, 전일한화평가금,
                  전일대비, 전일대비율, 비중, 총매수금, 총수익금, 총수익률)
  
  today_tsum <- tail(dd$Sum, 1)
  
  asset_SCHD <- rt %>% filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_QQQ  <- rt %>% filter(str_detect(종목명, "나스닥100|QQQ"),
                              !str_detect(종목명, "TQQQ")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_TQQQ <- rt %>% filter(str_detect(종목명, "TQQQ")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_GLD  <- rt %>% filter(str_detect(종목명, "금현물")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_BOND <- rt %>% filter(str_detect(종목명, "채권|국채")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  
  asset_SCHD[is.na(asset_SCHD)] <- 0
  asset_QQQ[is.na(asset_QQQ)]   <- 0
  asset_TQQQ[is.na(asset_TQQQ)] <- 0
  asset_GLD[is.na(asset_GLD)]   <- 0
  asset_BOND[is.na(asset_BOND)] <- 0
  
  asset_SPY_ETC <- today_tsum - asset_SCHD - asset_QQQ - asset_TQQQ - asset_GLD - asset_BOND
  
  asset_SCHD_ratio    <- asset_SCHD    / today_tsum * 100
  asset_QQQ_ratio     <- asset_QQQ     / today_tsum * 100
  asset_TQQQ_ratio    <- asset_TQQQ    / today_tsum * 100
  asset_GLD_ratio     <- asset_GLD     / today_tsum * 100
  asset_BOND_ratio    <- asset_BOND    / today_tsum * 100
  asset_SPY_ETC_ratio <- asset_SPY_ETC / today_tsum * 100
  
  label_text <- paste0(
    "오늘평가액 : ", comma(round(today_tsum, 0)), "원   ",
    "총수익 : ", comma(round(tail(dd$Profit, 1), 0)),"원" ,
    "(", round(tail(dd$Return, 1)*100, 2), "%)   \n",
    "전일대비 : ", comma(round(tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1], 0)),
    "원 (",
    ifelse((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) >= 0, "+", ""),
    round((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) * 100 / tail(dd$Sum, 1), 2),
    "%)" ,
    "  1일 평균 증가액 : ", comma(round(slope_per_day * 10000000, 0)), "(원/일)   \n",
    "(증분)1개월간 :", format(result_period$Diff[1], big.mark = ","), 
    "    3개월간 :", format(result_period$Diff[2], big.mark = ","), 
    "    6개월간 :", format(result_period$Diff[3], big.mark = ","), 
    "    1년간   :", format(result_period$Diff[4], big.mark = ","), "\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(최종목표%) = 40.0 : 20.0 : 15.0 : 10.0 : 10.0 : 5.0\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(현재비율%) = ", 
    format(round(asset_SPY_ETC_ratio, 1), nsmall = 1)," : ",
    format(round(asset_SCHD_ratio,    1), nsmall = 1)," : ",
    format(round(asset_QQQ_ratio,     1), nsmall = 1)," : ",
    format(round(asset_TQQQ_ratio,    1), nsmall = 1)," : ",
    format(round(asset_GLD_ratio,     1), nsmall = 1)," : ",
    format(round(asset_BOND_ratio,    1), nsmall = 1),"\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(목표억원  ) = ",
    format(round(today_tsum *  .4  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .2  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .15 / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .1  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .1  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .05 / 100000000, 1), nsmall = 1), "\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(현재억원  ) = ", 
    format(round(asset_SPY_ETC / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_SCHD    / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_QQQ     / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_TQQQ    / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_GLD     / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_BOND    / 100000000, 1), nsmall = 1)
  )
  
  p <- ggplot(dd, aes(x = Date)) +
    geom_point(aes(y = sum_left, color = Profit / 10000000), size = 5) +
    geom_line(aes(y = sum_left, group = 1), color = "gray") +
    geom_smooth(aes(y = sum_left), method = "lm", se = FALSE, color = "black") +
    geom_line(aes(y = a * ret_right + b), color = "green", linewidth = 1) +
    geom_point(aes(y = a * ret_right + b), color = "green", size = 2) +
    scale_color_gradient(low = "red", high = "blue") +
    scale_x_date(breaks = scales::breaks_pretty(n = max(1, floor(nrow(dd) * 0.1)))) +
    scale_y_continuous(
      name = "보유합계(천만원)",
      sec.axis = sec_axis(~ (. - b) / a, name = "수익률(%)")
    ) +
    labs(title = plot_title, x = paste0(exchange_rate, "원/달러"), color = "수익") +
    theme_minimal(base_size = 13) +
    theme(axis.title.y.right = element_text(color = "green"),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    coord_cartesian(ylim = c(sum_range[1], sum_range[2])) +
    annotate("text",
             x = min(dd$Date, na.rm = TRUE),
             y = max(sum_left, na.rm = TRUE),
             label = label_text,
             hjust = 0, vjust = 1, size = 5, color = "black")
  
  # calc_cagr <- function(start_date, end_date, start_value, end_value) {
  #   years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  #   (end_value / start_value)^(1 / years) - 1
  # }
  # 
  # cat("CAGR : ")
  #print(calc_cagr(dd$Date[1], tail(dd$Date, 1), dd$Sum[1], tail(dd$Sum, 1)))
  
  dd <- dd %>%
    mutate(Peak = cummax(Sum),
           DD   = ifelse(Peak > 0, Sum / Peak - 1, 0))
  
  mdd_value     <- min(dd$DD, na.rm = TRUE)
  mdd_end_idx   <- which.min(dd$DD)
  mdd_end_date  <- dd$Date[mdd_end_idx]
  mdd_start_idx <- which.max(dd$Sum[1:mdd_end_idx])
  mdd_start_date<- dd$Date[mdd_start_idx]
  mdd_start_sum <- dd$Sum[mdd_start_idx]
  mdd_end_sum   <- dd$Sum[mdd_end_idx]
  
  peak_label   <- paste0("피크\n", scales::comma(mdd_start_sum), "원\n(", format(mdd_start_date), ")")
  trough_label <- paste0("바닥\n", scales::comma(mdd_end_sum), "원\n(", format(mdd_end_date), ")")
  
  dd_points <- data.frame(
    Date = c(mdd_start_date, mdd_end_date),
    DDpct = c(0, mdd_value * 100)
  )
  
  y_peak_label   <- -2
  y_trough_label <- (mdd_value * 100) + 5
  
  p_dd <- ggplot(dd, aes(x = Date, y = DD * 100)) +
    geom_hline(yintercept = 0) +
    geom_area(alpha = 0) +
    geom_line(linewidth = 2) +
    geom_vline(xintercept = c(mdd_start_date, mdd_end_date), linetype = "dashed") +
    geom_point(data = dd_points, aes(x = Date, y = DDpct), size = 3, color = "firebrick") +
    annotate("label", x = mdd_start_date, y = y_peak_label,
             label = peak_label, label.size = 0.25,
             vjust = 1, hjust = 0.5, fill = "white") +
    annotate("label", x = mdd_end_date, y = y_trough_label,
             label = trough_label, label.size = 0.25,
             vjust = 0, hjust = 0.5, fill = "white") +
    annotate("label", x = mdd_end_date, y = (mdd_value * 100) + 5,
             label = paste0("MDD: ", scales::percent(-mdd_value, accuracy = 0.01)),
             vjust = 1, hjust = 0.5) +
    labs(title = paste0("Drawdown(", tail(dd, 1)[6] * 100, "%, ", 
                        tail(dd,1)[2] - tail(dd,1)[5], "원)"), 
         x = "날짜", 
         y = "Drawdown (%)") +
    theme_minimal(base_size = 13)
  
  combined_plot <- p / p_dd + patchwork::plot_layout(heights = c(2, 1))
  suppressMessages(print(combined_plot))
  
  print(
    paste(
      "국내주식수 :", dim(data1)[1] - 1,
      " 해외주식수 :", dim(data2)[1] - 2,
      " 환율 :", exchange_rate,"원/달러"
    )
  )
  
  print(
    datatable(
      rt,
      options = list(
        pageLength = 100,
        columnDefs = list(
          list(targets = c("전일대비율", "비중", "총수익률"), className = "dt-right")
        )
      )
    ) %>%
      formatCurrency(
        columns = c("한화평가금", "한화매수가격", "전일한화평가금", "전일대비", "총매수금", "총수익금"),
        currency = "",
        mark = ",",
        digits = 0
      ) %>%
      formatRound(columns = c("전일대비율", "비중", "총수익률"), digits = 2) %>%
      formatStyle(
        columns = c("전일대비", "총수익금"),
        color = styleInterval(
          c(-0.000001, 0.000001),
          c("red", "black", "blue")
        ),
        fontWeight = styleInterval(
          0,
          c("bold", "normal")
        )
      ) %>%
      formatStyle(
        columns = c("전일대비율", "총수익률"),
        color = styleInterval(
          c(-0.000001, 0.000001),
          c("red", "gray", "blue")
        ),
        fontWeight = styleInterval(
          0,
          c("bold", "normal")
        )
      )
  )
  
  print(tail(dd,2))
  
  
  
  ##### ===========================
  #####  리스크 엔진 실행 구간
  ##### ===========================
  
  # 현재 포트폴리오 비중 (메인 코드에서 이미 계산됨)
  weights <- c(
    SPY_ETC = asset_SPY_ETC_ratio / 100,
    SCHD    = asset_SCHD_ratio    / 100,
    QQQ     = asset_QQQ_ratio     / 100,
    TQQQ    = asset_TQQQ_ratio    / 100,
    GLD     = asset_GLD_ratio     / 100,
    BOND    = asset_BOND_ratio    / 100
  )
  
  # 목표 비중
  target_weights <- c(
    SPY_ETC = 0.40,
    SCHD    = 0.20,
    QQQ     = 0.15,
    TQQQ    = 0.10,
    GLD     = 0.10,
    BOND    = 0.05
  )
  
  current_nav <- tail(dd$Sum, 1)
  
  cat("\n\n================ 리스크 분석 시작 ================\n")
  
  # 1) Stress Test Replay
  run_stress_replay_from_file(
    asset_file     = "asset_returns_monthly.csv",
    weights        = weights,
    current_nav    = current_nav,
    monthly_contrib = 0
  )
  
  # 2) VaR / CVaR
  run_var_cvar_from_file(
    asset_file  = "asset_returns_monthly.csv",
    weights     = weights,
    current_nav = current_nav,
    alpha       = 0.95
  )
  
  # 3) DRIFT 기반 리밸런싱 신호
  run_drift_rebal_signal(
    target_weights = target_weights,
    current_weights = weights,
    threshold = 0.05
  )
  
  cat("================ 리스크 분석 종료 ================\n\n")
  
  
  
  
  cat("장중 10분 그이외는 1시간 후에 다시 실행됨(중단을 원하면 Interrupt-R 빨간버튼 클릭)",
      format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),"\n\n")
  
  View(rt)
  
  count <- count + 1
  
  if (in_fast_range & (wday >= 1 & wday <= 5)) {
    wait_min <- 10
  } else {
    wait_min <- 60
  }
  Sys.sleep(wait_min * 60)
}
