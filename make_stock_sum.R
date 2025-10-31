# 1) 필요한 패키지 전부 설치
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

# 2) 로드
library(readr); library(readxl)
library(openxlsx); library(rvest); library(httr)
library(dplyr); library(ggplot2); library(scales)
library(patchwork)

count <- 1  # 반복 횟수 카운터

repeat {
  # 현재 시간과 반복 횟수 출력
  cat("[", count, "회차]", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"), ": 실행 시작\n")
  
  setwd("c:\\easy_r\\easy_r")  # 워킹 디렉토리를 지정한다.(개별 설정이 다를 수 있음)
  source("stock_eval.R")
  source("stock_eval_us.R")
  
  today <- Sys.Date()
  
  file1 <- paste0("output_stock_", today, ".xlsx")
  file2 <- paste0("output_stock_us_", today, ".xlsx")
  output_file <- "output_sum.csv"
  
  column_name  <- "평가금"
  column_name2 <- "수익금"
  
  # 3) 엑셀 읽기: readxl::read_excel 사용
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
  
  # 4) CSV append: Date가 꼭 Date형으로 유지되도록 col_types 지정
  # if (file.exists(output_file)) {
  #   existing_data <- read_csv(output_file,
  #                             col_types = cols(
  #                               Date   = col_date(format = ""),
  #                               Sum    = col_double(),
  #                               Profit = col_double()
  #                             ))
  #   updated_data <- bind_rows(existing_data, result)
  # } else {
  #   updated_data <- result
  # }
  
  if (file.exists(output_file)) {
    # 기존 파일 읽기
    existing_data <- read_csv(output_file,
                              col_types = cols(
                                Date   = col_date(format = ""),
                                Sum    = col_double(),
                                Profit = col_double()
                              ), 
                              show_col_types = FALSE)
    
    # ✅ 마지막 행의 Date가 오늘이면 삭제
    if (nrow(existing_data) > 0 && tail(existing_data$Date, 1) == Sys.Date()) {
      existing_data <- existing_data[-nrow(existing_data), ]
    }
    
    # 새 결과와 합치기
    updated_data <- bind_rows(existing_data, result)
    
  } else {
    updated_data <- result
  }
  
  write_csv(updated_data, output_file)
  
  # cat("성공적으로 데이터를 추가했습니다! 합산금은 : ",
  #     sum_value, "원 총수익금은 :", profit_value, "총수익률 : ",
  #     profit_value / (sum_value - profit_value), "\n")
  
  # 5) 분석용 데이터 재읽기 (Date 파싱 보장)
  dd <- read_csv(output_file,
                 col_types = cols(
                   Date   = col_date(format = ""),
                   Sum    = col_double(),
                   Profit = col_double()
                 ))
  
  # 수익률
  dd <- dd %>% mutate(Return = Profit / (Sum - Profit))
  
  sum_left  <- dd$Sum / 10000000
  ret_right <- dd$Return * 100
  
  sum_range     <- range(sum_left,  na.rm = TRUE)
  return_range  <- range(ret_right, na.rm = TRUE)
  
  a <- diff(sum_range) / diff(return_range)
  b <- sum_range[1] - a * return_range[1]
  
  start_date <- format(min(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  end_date   <- format(max(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  plot_title <- paste0("주식평가액 분석 (", start_date, " ~ ", end_date, ")  ",
                       format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분"))
  
  # Date는 숫자형으로 변환해 회귀 (안전)
  fit <- lm(sum_left ~ as.numeric(Date), data = dd)
  slope_per_day <- coef(fit)[2]
  label_text <- paste0(
    "오늘평가액 : ", comma(round(tail(dd$Sum, 1), 0)), "   ",
    "총수익 : ", comma(round(tail(dd$Profit, 1), 0)), 
    "(", round(tail(dd$Return, 1)*100, 2), "%)   ",
    "전일대비 : ", comma(round(tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1], 0)),
    " (",
    ifelse((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) >= 0, "+", ""),
    round((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) * 100 / tail(dd$Sum, 1), 2),
    "%)" ,
    "  1일 평균 증가액 : ", comma(round(slope_per_day * 10000000, 0)), "(원/일)   "
  )
  print(label_text)
  print(
    paste(
      "국내주식수 :", dim(data1)[1] - 1,
      " 해외주식수 :", dim(data2)[1] - 2
    )
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
      name = "천만원",
      sec.axis = sec_axis(~ (. - b) / a, name = "Return (%)")
    ) +
    labs(title = plot_title, x = "날짜(월)", color = "수익 (천만원)") +
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
  suppressMessages(
    print(p)
  )
  
  # 보조: 선형모형(날짜 → Sum)
  model <- lm(Sum / 10000000 ~ as.numeric(Date), data = dd)
  
  #cat("1일증가당 Sum 변화(원단위) : ", coef(model)[2] * 10000000, "\n")
  
  #cat("평가액 최고였던 날짜 : ")
  #print(dd[which.max(dd$Sum), 1:3])
  
  #cat("평가액 최저였던 날짜 : ")
  #print(dd[which.min(dd$Sum), 1:3])
  
  #cat("최대합계 - 최소합계 차이 : ")
  #print(dd[which.max(dd$Sum), 2] - dd[which.min(dd$Sum), 2])
  
  # CAGR 함수 및 계산 (Date 형 보장)
  calc_cagr <- function(start_date, end_date, start_value, end_value) {
    years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
    (end_value / start_value)^(1 / years) - 1
  }
  
  cat("CAGR : ")
  print(calc_cagr(dd$Date[1], tail(dd$Date, 1), dd$Sum[1], tail(dd$Sum, 1)))
  
  # ===== MDD =====
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
  
  #cat("MDD(최대하락폭): ", scales::percent(-mdd_value, accuracy = 0.01), "\n")
  #cat("피크→바닥 구간 : ", format(mdd_start_date), " → ", format(mdd_end_date), "\n", sep = "")
  #cat("평가금액    : ", scales::comma(mdd_start_sum), " → ", scales::comma(mdd_end_sum), " (원)\n", sep = "")
  
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
    geom_area(alpha = 0.25) +
    geom_line() +
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
    labs(title = "Drawdown", x = "날짜", y = "Drawdown (%)") +
    theme_minimal(base_size = 13)
  
  suppressMessages(
    print(p_dd)
  )
  
  combined_plot <- p / p_dd + plot_layout(heights = c(2, 1))
  suppressMessages(
    print(combined_plot)
  )
  
  print(tail(dd,2))
  cat("1시간 후에 다시 실행됩니다...(중단을 원하면 Interrupt-R 빨간버튼 클릭)",
      format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),"\n\n")
  
  # 1시간 대기
  Sys.sleep(3600)
  
  # 반복 횟수 증가
  count <- count + 1
}

# Drawdown 10%이상 떨어진날 보는 코드 : 
# dd %>% filter(abs(DD) > 0.1)