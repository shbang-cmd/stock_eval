# 1) 필요한 패키지 전부 설치 -------------------------------------------------

pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}
# 최신 패키지 설치 명령 : update.packages(ask = FALSE, checkBuilt = TRUE)

# 2) 로드 --------------------------------------------------------------------
library(readr);    library(readxl)
library(openxlsx); library(rvest);   library(httr)
library(dplyr);    library(ggplot2); library(scales)
library(patchwork);library(treemap); library(DT)
library(stringr)

setwd("c:\\easy_r")  # 워킹 디렉토리 설정(각자 환경에 맞게 수정)

options(scipen = 999)  # 지수표기(Scientific notation) 끄기
count <- 1  # 반복 횟수 카운터

repeat {
  
  # 현재 시간 ---------------------------------------------------------------
  now  <- as.POSIXct(Sys.time())
  hhmm <- format(now, "%H:%M")
  wday <- as.numeric(format(now, "%u"))  # 요일: 1=월 ~ 7=일
  week_kor <- c("일", "월", "화", "수", "목", "금", "토")
  
  # 실행 구간 판별 (국내장 시간)
  in_fast_range <- hhmm >= "08:40" & hhmm <= "15:30"
  
  # 현재 시간과 반복 횟수 출력 ---------------------------------------------
  cat("[", count, "회차]", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),
      ": 실행 시작***********************************************\n")
  
  # KRX/미국 주식 평가 파일 생성 스크립트 실행 ----------------------------
  source("stock_eval.R")
  source("stock_eval_us.R")
  
  today <- Sys.Date()
  
  file1 <- paste0("output_stock_",    today, ".xlsx")
  file2 <- paste0("output_stock_us_", today, ".xlsx")
  output_file <- "output_sum.csv"
  
  column_name  <- "평가금"
  column_name2 <- "수익금"
  
  # 3) 엑셀 읽기: readxl::read_excel 사용 -----------------------------------
  data1 <- read_excel(file1)
  data2 <- read_excel(file2)
  
  last_value1   <- tail(data1[[column_name]], 1)
  last_value1_2 <- tail(data1[[column_name2]], 1)
  
  last_value2   <- tail(data2[[column_name]], 1)
  last_value2_2 <- tail(data2[[column_name2]], 1)
  
  sum_value    <- round(last_value1 + last_value2, 0)
  profit_value <- round(last_value1_2 + last_value2_2, 0)
  
  yesugum <- 0
  yegum   <- 0
  sum_value <- round(sum_value + yegum, 0)
  
  result <- data.frame(Date = today, Sum = sum_value, Profit = profit_value)
  
  # 4) output_sum.csv 업데이트 ----------------------------------------------
  if (file.exists(output_file)) {
    # 기존 파일 읽기
    existing_data <- read_csv(
      output_file,
      col_types = cols(
        Date   = col_date(format = ""),
        Sum    = col_double(),
        Profit = col_double()
      ),
      show_col_types = FALSE
    )
    
    # 마지막 행의 Date가 오늘이면 삭제 후 다시 추가
    if (nrow(existing_data) > 0 && tail(existing_data$Date, 1) == Sys.Date()) {
      existing_data <- existing_data[-nrow(existing_data), ]
    }
    
    updated_data <- dplyr::bind_rows(existing_data, result)
    
  } else {
    updated_data <- result
  }
  
  write_csv(updated_data, output_file)
  
  # 5) 분석용 데이터 재읽기 -------------------------------------------------
  dd <- read_csv(
    output_file,
    col_types = cols(
      Date   = col_date(format = ""),
      Sum    = col_double(),
      Profit = col_double()
    )
  )
  
  # 수익률
  dd <- dd %>% mutate(Return = Profit / (Sum - Profit))
  
  sum_left  <- dd$Sum / 10000000   # 천만원 단위
  ret_right <- dd$Return * 100     # %
  
  sum_range    <- range(sum_left,  na.rm = TRUE)
  return_range <- range(ret_right, na.rm = TRUE)
  
  a <- diff(sum_range) / diff(return_range)
  b <- sum_range[1] - a * return_range[1]
  
  start_date <- format(min(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  end_date   <- format(max(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  
  plot_title <- paste0(
    "JS 펀드 주식평가액 분석 (", start_date, " ~ ", end_date, ")  ",
    format(Sys.time(), "%Y년 %m월 %d일"),
    "(",
    week_kor[as.numeric(format(Sys.Date(), "%w")) + 1],
    ") ",
    format(Sys.time(), "%H시 %M분")
  )
  
  df <- dd[1:2]      # Date, Sum
  df$Date <- as.Date(df$Date)
  
  # 마지막 날짜
  last_date <- max(df$Date, na.rm = TRUE)
  
  # 비교할 기간 벡터 (1개월, 3개월, 6개월, 12개월)
  periods <- c(1, 3, 6, 12)
  
  # 한 달 단위로 Sum값 비교 -------------------------------------------------
  result <- data.frame(
    Period       = paste0(periods, "개월 전"),
    Target_Date  = as.Date(NA),
    Closest_Date = as.Date(NA),
    Sum          = NA,
    Diff         = NA
  )
  
  for (i in seq_along(periods)) {
    # 목표 날짜 (개월 단위)
    target <- seq(last_date, length = 2,
                  by = paste0("-", periods[i], " month"))[2]
    
    # 실제 데이터 중 가장 가까운 날짜
    idx          <- which.min(abs(df$Date - target))
    closest_date <- df$Date[idx]
    sum_value_i  <- df$Sum[idx]
    
    # 마지막 날짜의 Sum
    latest_sum <- df$Sum[df$Date == last_date]
    
    # 차이 계산
    diff_value <- latest_sum - sum_value_i
    
    result[i, ] <- c(
      paste0(periods[i], "개월 전"),
      as.character(target),
      as.character(closest_date),
      sum_value_i,
      diff_value
    )
  }
  
  result$Sum  <- as.numeric(result$Sum)
  result$Diff <- as.numeric(result$Diff)
  
  # 구성비율 트리맵 데이터 준비 --------------------------------------------
  dt_ko <- data_ko %>%
    head(-1) %>%
    select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
  
  dt_en <- data_en %>%
    head(-2) %>%
    select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
  
  dt_ko <- dt_ko %>%
    mutate(한화평가금 = 평가금,
           한화매수가격 = 매수가격)
  
  dt_en <- dt_en %>%
    mutate(한화평가금 = 평가금 * exchange_rate,
           한화매수가격 = 매수가격 * exchange_rate)
  
  dt_fn <- bind_rows(dt_ko, dt_en) %>%
    select(-평가금) %>%
    arrange(desc(한화평가금))
  
  View(dt_fn)  # 한국주식과 미국주식 합친 테이블
  
  # 트리맵 -------------------------------------------------------------------
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
    inflate.labels = TRUE,                 # 작아도 표시
    align.labels = list(c("center","center")) # 중앙 정렬
  )
  
  # 날짜형 변환 후 회귀 ------------------------------------------------------
  fit <- lm(sum_left ~ as.numeric(Date), data = dd)
  slope_per_day <- coef(fit)[2]
  
  # ── 전일 데이터 파일명 탐색 함수 -----------------------------------------
  get_prev_file <- function(prefix = "output_stock_", ext = "xlsx") {
    pattern <- paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.", ext, "$")
    files   <- dir(pattern = pattern)
    if (length(files) == 0) return(NA)
    dates <- as.Date(sub(paste0(prefix, "(\\d{4}-\\d{2}-\\d{2})\\.", ext), "\\1", files))
    valid_idx <- which(dates < Sys.Date())
    if (length(valid_idx) == 0) return(NA)
    files[which.max(dates[valid_idx])]
  }
  
  # ── 전일 데이터 불러오기 -------------------------------------------------
  data_prev_ko <- read_excel(get_prev_file("output_stock_"))
  data_prev_en <- read_excel(get_prev_file("output_stock_us_"))
  
  data_prev_ko <- data_prev_ko %>%
    head(-1) %>%
    select(종목번호, 보유증권사, 전일한화평가금 = 평가금)
  
  data_prev_en <- data_prev_en %>%
    head(-2) %>%
    mutate(한화평가금 = 평가금 * exchange_rate) %>%
    select(종목번호, 보유증권사, 전일한화평가금 = 한화평가금)
  
  data_prev_fn <- bind_rows(data_prev_ko, data_prev_en) %>%
    arrange(desc(전일한화평가금))
  
  # ── 오늘 vs 전일 비교 함수 -----------------------------------------------
  join_stock_data <- function(today_df, prev_df) {
    today_df %>%
      distinct(종목번호, 보유증권사, .keep_all = TRUE) %>%
      left_join(prev_df, by = c("종목번호", "보유증권사")) %>%
      mutate(
        한화평가금      = trunc(한화평가금),
        전일한화평가금  = trunc(전일한화평가금),
        전일대비        = trunc(한화평가금 - 전일한화평가금),
        전일대비율      = if_else(
          is.na(전일한화평가금),
          NA_character_,
          sprintf("%.2f", round((한화평가금 - 전일한화평가금) /
                                  전일한화평가금 * 100, 2))
        ),
        비중 = sprintf("%.2f", round(한화평가금 /
                                     sum(한화평가금, na.rm = TRUE) * 100, 2))
      ) %>%
      arrange(desc(한화평가금))
  }
  
  # 종합 테이블 -------------------------------------------------------------
  rt <- join_stock_data(dt_fn, data_prev_fn) %>%
    mutate(
      총매수금   = 한화매수가격 * 수량,                     # 매수금(원)
      총수익금   = 한화평가금 - 총매수금,                   # 총수익금
      총수익률   = round((총수익금 / 총매수금) * 100, 2)    # 총수익률(%)
    ) %>%
    select(-매수가격) %>%
    select(
      종목명, 보유증권사, 한화매수가격, 수량,
      한화평가금, 전일한화평가금,
      전일대비, 전일대비율, 비중,
      총매수금, 총수익금, 총수익률
    )
  
  today_tsum <- tail(dd$Sum, 1)  # 오늘 한화평가금 합계
  
  # 자산군별 합계 -----------------------------------------------------------
  asset_SCHD <- rt %>%
    filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
    summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
    pull(합계)
  
  # QQQ 계열 ETF를 검색하되 TQQQ는 제외
  asset_QQQ <- rt %>%
    filter(str_detect(종목명, "나스닥100|QQQ"),
           !str_detect(종목명, "TQQQ")) %>%
    summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
    pull(합계)
  
  asset_TQQQ <- rt %>%
    filter(str_detect(종목명, "TQQQ")) %>%
    summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
    pull(합계)
  
  asset_GLD <- rt %>%
    filter(str_detect(종목명, "금현물")) %>%
    summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
    pull(합계)
  
  asset_BOND <- rt %>%
    filter(str_detect(종목명, "채권|국채")) %>%
    summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
    pull(합계)
  
  asset_SPY_ETC <- today_tsum - asset_SCHD - asset_QQQ - asset_TQQQ - asset_GLD - asset_BOND
  
  asset_SCHD_ratio    <- asset_SCHD    / today_tsum * 100
  asset_QQQ_ratio     <- asset_QQQ     / today_tsum * 100
  asset_TQQQ_ratio    <- asset_TQQQ    / today_tsum * 100
  asset_GLD_ratio     <- asset_GLD     / today_tsum * 100
  asset_BOND_ratio    <- asset_BOND    / today_tsum * 100
  asset_SPY_ETC_ratio <- asset_SPY_ETC / today_tsum * 100
  
  # ====================== 리밸런싱 권고 계산 ================================
  # 목표 비중(공격형 버전)
  target_pct <- c(
    SPY_ETC = 0.40,
    SCHD   = 0.20,
    QQQ    = 0.15,
    TQQQ   = 0.10,
    GOLD   = 0.10,
    BOND   = 0.05
  )
  
  curr_value <- c(
    SPY_ETC = asset_SPY_ETC,
    SCHD   = asset_SCHD,
    QQQ    = asset_QQQ,
    TQQQ   = asset_TQQQ,
    GOLD   = asset_GLD,
    BOND   = asset_BOND
  )
  
  target_value <- today_tsum * target_pct
  diff_value   <- round(target_value - curr_value, 0)  # +는 매수, -는 매도
  
  rebalance_table <- data.frame(
    자산군        = c("SPY등(기타주식)", "SCHD", "QQQ", "TQQQ", "금", "채권"),
    현재평가금    = round(curr_value, 0),
    목표평가금    = round(target_value, 0),
    차이금액      = diff_value,
    매매구분      = ifelse(diff_value > 0, "매수", 
                       ifelse(diff_value < 0, "매도", "유지")),
    stringsAsFactors = FALSE
  )
  
  # TQQQ 상한선 체크(예: 15% 초과 시 경고)
  tqqq_warn_flag <- as.numeric(asset_TQQQ_ratio) > 15
  
  if (tqqq_warn_flag) {
    cat("⚠ 경고: TQQQ 비중이 15%를 초과했습니다. 레버리지 축소(부분 매도)를 고려하세요.\n")
  }
  
  # 리밸런싱 권고 테이블 출력
  cat("=== 오늘 기준 리밸런싱 권고(목표비중 40/20/15/10/10/5) ===\n")
  print(
    rebalance_table %>%
      mutate(
        현재평가금 = format(현재평가금, big.mark = ","),
        목표평가금 = format(목표평가금, big.mark = ","),
        차이금액   = format(차이금액,   big.mark = ",")
      )
  )
  
  # 라벨 텍스트 -------------------------------------------------------------
  tqqq_warn_text <- if (tqqq_warn_flag) {
    "⚠ TQQQ 비중이 15%를 초과했습니다. 레버리지 축소 필요.\n"
  } else {
    ""
  }
  
  label_text <- paste0(
    tqqq_warn_text,
    "오늘평가액 : ", comma(round(today_tsum, 0)), "원   ",
    "총수익 : ", comma(round(tail(dd$Profit, 1), 0)), "원",
    "(", round(tail(dd$Return, 1) * 100, 2), "%)   \n",
    "전일대비 : ", comma(round(tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1], 0)),
    "원 (",
    ifelse((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) >= 0, "+", ""),
    round((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) * 100 / tail(dd$Sum, 1), 2),
    "%)",
    "  1일 평균 증가액 : ",
    comma(round(slope_per_day * 10000000, 0)), "(원/일)   \n",
    "(증분)1개월간 :", format(result$Diff[1], big.mark = ","), 
    "    3개월간 :", format(result$Diff[2], big.mark = ","), 
    "    6개월간 :", format(result$Diff[3], big.mark = ","), 
    "    1년간   :", format(result$Diff[4], big.mark = ","), "\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(최종목표%) = 40.0 : 20.0 : 15.0 : 10.0 : 10.0 : 5.0\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(현재비율%) = ", 
    format(round(asset_SPY_ETC_ratio, 1), nsmall = 1), " : ",
    format(round(asset_SCHD_ratio,    1), nsmall = 1), " : ",
    format(round(asset_QQQ_ratio,     1), nsmall = 1), " : ",
    format(round(asset_TQQQ_ratio,    1), nsmall = 1), " : ",
    format(round(asset_GLD_ratio,     1), nsmall = 1), " : ",
    format(round(asset_BOND_ratio,    1), nsmall = 1), "\n",
    # 수익최대화 버전 (40/20/15/10/10/5)
    "SPY등:SCHD:QQQ:TQQQ:금:채권(목표억원  ) = ",
    format(round(today_tsum *  .4  / 100000000, 1), nsmall = 1), " : ",
    format(round(today_tsum *  .2  / 100000000, 1), nsmall = 1), " : ",
    format(round(today_tsum *  .15 / 100000000, 1), nsmall = 1), " : ",
    format(round(today_tsum *  .1  / 100000000, 1), nsmall = 1), " : ",
    format(round(today_tsum *  .1  / 100000000, 1), nsmall = 1), " : ",
    format(round(today_tsum *  .05 / 100000000, 1), nsmall = 1), "\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(현재억원  ) = ", 
    format(round(asset_SPY_ETC / 100000000, 1), nsmall = 1), " : ",
    format(round(asset_SCHD    / 100000000, 1), nsmall = 1), " : ",
    format(round(asset_QQQ     / 100000000, 1), nsmall = 1), " : ",
    format(round(asset_TQQQ    / 100000000, 1), nsmall = 1), " : ",
    format(round(asset_GLD     / 100000000, 1), nsmall = 1), " : ",
    format(round(asset_BOND    / 100000000, 1), nsmall = 1)
  )
  
  # 메인 평가액 그래프 ------------------------------------------------------
  p <- ggplot(dd, aes(x = Date)) +
    geom_point(aes(y = sum_left, color = Profit / 10000000), size = 5) +
    geom_line(aes(y = sum_left, group = 1), color = "gray") +
    geom_smooth(aes(y = sum_left), method = "lm", se = FALSE, color = "black") +
    geom_line(aes(y = a * ret_right + b), color = "green", linewidth = 1) +
    geom_point(aes(y = a * ret_right + b), color = "green", size = 2) +
    scale_color_gradient(low = "red", high = "blue") +
    scale_x_date(
      breaks = scales::breaks_pretty(n = max(1, floor(nrow(dd) * 0.1)))
    ) +
    scale_y_continuous(
      name = "보유합계(천만원)",
      sec.axis = sec_axis(~ (. - b) / a, name = "수익률(%)")
    ) +
    labs(
      title = plot_title,
      x     = paste0(exchange_rate, "원/달러"),
      color = "수익"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.y.right = element_text(color = "green"),
      legend.position    = "right",
      plot.title         = element_text(hjust = 0.5, face = "bold")
    ) +
    coord_cartesian(ylim = c(sum_range[1], sum_range[2])) +
    annotate(
      "text",
      x     = min(dd$Date, na.rm = TRUE),
      y     = max(sum_left, na.rm = TRUE),
      label = label_text,
      hjust = 0, vjust = 1, size = 5, color = "black"
    )
  
  # 보조: 선형모형(날짜 → Sum) ----------------------------------------------
  model <- lm(Sum / 10000000 ~ as.numeric(Date), data = dd)
  
  # CAGR 함수 및 계산 -------------------------------------------------------
  calc_cagr <- function(start_date, end_date, start_value, end_value) {
    years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
    (end_value / start_value)^(1 / years) - 1
  }
  
  cat("CAGR : ")
  print(calc_cagr(dd$Date[1], tail(dd$Date, 1), dd$Sum[1], tail(dd$Sum, 1)))
  
  # ===== MDD 계산 ==========================================================
  dd <- dd %>%
    mutate(
      Peak = cummax(Sum),
      DD   = ifelse(Peak > 0, Sum / Peak - 1, 0)
    )
  
  mdd_value     <- min(dd$DD, na.rm = TRUE)
  mdd_end_idx   <- which.min(dd$DD)
  mdd_end_date  <- dd$Date[mdd_end_idx]
  mdd_start_idx <- which.max(dd$Sum[1:mdd_end_idx])
  mdd_start_date<- dd$Date[mdd_start_idx]
  mdd_start_sum <- dd$Sum[mdd_start_idx]
  mdd_end_sum   <- dd$Sum[mdd_end_idx]
  
  peak_label   <- paste0("피크\n",
                         scales::comma(mdd_start_sum), "원\n(",
                         format(mdd_start_date), ")")
  trough_label <- paste0("바닥\n",
                         scales::comma(mdd_end_sum), "원\n(",
                         format(mdd_end_date), ")")
  
  dd_points <- data.frame(
    Date  = c(mdd_start_date, mdd_end_date),
    DDpct = c(0, mdd_value * 100)
  )
  
  y_peak_label   <- -2
  y_trough_label <- (mdd_value * 100) + 5
  
  # 라벨용 데이터프레임 -----------------------------------------------------
  label_df_peak <- data.frame(
    Date  = mdd_start_date,
    Y     = y_peak_label,
    Label = peak_label
  )
  
  label_df_trough <- data.frame(
    Date  = mdd_end_date,
    Y     = y_trough_label,
    Label = trough_label
  )
  
  label_df_mdd <- data.frame(
    Date  = mdd_end_date,
    Y     = (mdd_value * 100) + 5,
    Label = paste0("MDD: ", scales::percent(-mdd_value, accuracy = 0.01))
  )
  
  # Drawdown 그래프 (geom_label 사용, 경고 제거) ----------------------------
  p_dd <- ggplot(dd, aes(x = Date, y = DD * 100)) +
    geom_hline(yintercept = 0) +
    geom_area(alpha = 0) +
    geom_line(linewidth = 2) +
    geom_vline(
      xintercept = c(mdd_start_date, mdd_end_date),
      linetype   = "dashed"
    ) +
    geom_point(
      data = dd_points,
      aes(x = Date, y = DDpct),
      size = 3,
      color = "firebrick"
    ) +
    # 피크 라벨
    geom_label(
      data = label_df_peak,
      aes(x = Date, y = Y, label = Label),
      label.size = 0.25,
      vjust      = 1,
      hjust      = 0.5,
      fill       = "white"
    ) +
    # 바닥 라벨
    geom_label(
      data = label_df_trough,
      aes(x = Date, y = Y, label = Label),
      label.size = 0.25,
      vjust      = 0,
      hjust      = 0.5,
      fill       = "white"
    ) +
    # MDD 값 라벨
    geom_label(
      data = label_df_mdd,
      aes(x = Date, y = Y, label = Label),
      label.size = 0.25,
      vjust      = 1,
      hjust      = 0.5
    ) +
    labs(
      title = paste0(
        "Drawdown(",
        tail(dd, 1)[6] * 100, "%, ",
        tail(dd, 1)[2] - tail(dd, 1)[5], "원)"
      ),
      x = "날짜",
      y = "Drawdown (%)"
    ) +
    theme_minimal(base_size = 13)
  
  # 메인 + MDD 결합 ---------------------------------------------------------
  combined_plot <- p / p_dd + plot_layout(heights = c(2, 1))
  suppressMessages(print(combined_plot))
  
  # 간단한 요약 출력 --------------------------------------------------------
  print(
    paste(
      "국내주식수 :", dim(data1)[1] - 1,
      " 해외주식수 :", dim(data2)[1] - 2,
      " 환율 :", exchange_rate, "원/달러"
    )
  )
  
  # DT 테이블 출력 ----------------------------------------------------------
  print(
    datatable(
      rt,
      options = list(
        pageLength = 100,
        columnDefs = list(
          list(
            targets  = c("전일대비율", "비중", "총수익률"),
            className = "dt-right"
          )
        )
      )
    ) %>%
      formatCurrency(
        columns = c(
          "한화평가금", "한화매수가격", "전일한화평가금",
          "전일대비", "총매수금", "총수익금"
        ),
        currency = "",
        mark     = ",",
        digits   = 0
      ) %>%
      formatRound(
        columns = c("전일대비율", "비중", "총수익률"),
        digits  = 2
      ) %>%
      # 전일대비 / 총수익금: 음수=빨강, 0=검정, 양수=파랑
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
      # 전일대비율 / 총수익률: 음수=빨강, 0=회색, 양수=파랑
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
  
  print(tail(dd, 2))
  cat(
    "장중 10분 그이외는 1시간 후에 다시 실행됨(중단을 원하면 Interrupt-R 빨간버튼 클릭)",
    format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"), "\n\n"
  )
  
  View(rt)
  
  # 반복 횟수 증가 ----------------------------------------------------------
  count <- count + 1
  
  # 다음 대기시간 설정 ------------------------------------------------------
  if (in_fast_range & (wday >= 1 & wday <= 5)) {  # 월 ~ 금일 때만
    wait_min <- 10     # 거래시간대: 10분 간격
  } else {
    wait_min <- 60     # 비거래시간대: 1시간 간격
  }
  Sys.sleep(wait_min * 60)
}

# 연 1회 리밸런싱(가장 추천), 매월 적립매수시 비율 유지(자동으로 쌀 때 더사게됨)
# 매년 1월 1일:
#  SPY 40
#  QQQ 20
#  SCHD 15
#  TQQQ 10
#  금 10
#  채권 5

# ✅ 포트폴리오 기대수익률(μ)
# → 약 11.5%
# ✅ 포트폴리오 연간 변동성(σ)
# → 약 17~18%

# 주 
# 1. KRX 금매수 시 ACE KRX 금현물로 변환하여 입력(KRX 금시세 조회가 여려움)
# 2. 보유증권사 구분:  xx증권 : 국내일반계좌, xx증권(미) : 미국주식
#                      xx증권IRP : IPR계좌, xx증권ISA : ISA계좌
# 3. 미국주식은 매수 시점의 환율 기록이 안되어 환차손익의 반영이 안됨
