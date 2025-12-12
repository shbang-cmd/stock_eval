############################################################
# asset_returns_monthly.csv + factors_monthly.csv 동시 생성 스크립트
# - JS 펀드용 월간 자산 수익률 + 팩터 데이터 자동 생성
# - 실행 위치: c:/easy_r
############################################################

# 아래 파일들을 만들기위해 최초 1회 실행하면 됨
# asset_returns_monthly.csv — “자산군의 월간 수익률 데이터”
# factors_monthly.csv — “간단한 팩터(요인) 수익률 데이터”


# 1. 패키지 설치 & 로드 ------------------------------------
pkg <- c("quantmod", "dplyr", "readr")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

library(quantmod)
library(dplyr)
library(readr)

# 2. 워킹 디렉토리 설정 -----------------------------------
setwd("c:/easy_r")   # 필요시 경로 수정

# 3. 심볼 정의 (원하는 대로 수정 가능) ---------------------
#  - SPY  : 미국 전체 주식시장(시장 팩터)
#  - SCHD : 고배당/가치 역할
#  - QQQ  : 성장/기술주 역할
#  - TQQQ : 레버리지/모멘텀 역할
#  - GLD  : 금
#  - IEF  : 중장기 미국채 (채권 대표)
symbols <- c("SPY", "SCHD", "QQQ", "TQQQ", "GLD", "IEF")

start_date <- "2010-01-01"   # 필요시 시작일 조정

cat("▶ 야후에서 가격 데이터 다운로드 중...\n")
getSymbols(symbols, from = start_date)

# 4. 월간 수익률 계산 (asset_returns_monthly) ---------------
#   - type = "arithmetic": (P_t / P_{t-1} - 1), 직관적인 단순수익률
monthly_list <- lapply(symbols, function(sym) {
  px <- Ad(get(sym))
  ret <- monthlyReturn(px, type = "arithmetic")
  colnames(ret) <- sym
  ret
})

# 날짜 기준으로 merge
all_monthly_xts <- do.call(merge, monthly_list)

# xts → data.frame 변환
asset_returns_monthly <- data.frame(
  Date = as.Date(index(all_monthly_xts)),
  coredata(all_monthly_xts)
)

# 혹시라도 NA가 있으면 제거 (상장일 차이 등)
asset_returns_monthly <- asset_returns_monthly %>%
  tidyr::drop_na()

# CSV 저장
write_csv(asset_returns_monthly, "asset_returns_monthly.csv")
cat("✔ asset_returns_monthly.csv 생성 완료\n")

# 5. 팩터 정의 및 factors_monthly.csv 생성 ------------------
#   - 아주 단순한 팩터 구성 예시:
#     MKT   = SPY              (시장 팩터)
#     VALUE = SCHD - SPY       (가치/배당 팩터: SPY 초과수익)
#     GROWTH= QQQ  - SPY       (성장 팩터: SPY 초과수익)
#     MOM   = TQQQ - QQQ       (레버리지/모멘텀 팩터)

asset_returns_monthly <- asset_returns_monthly %>%
  mutate(
    YM = format(Date, "%Y-%m")   # 필요시 월 키로 사용
  )

factors_monthly <- asset_returns_monthly %>%
  transmute(
    Date  = Date,
    YM    = YM,
    MKT   = SPY,
    VALUE = SCHD - SPY,
    GROWTH= QQQ  - SPY,
    MOM   = TQQQ - QQQ
  )

write_csv(factors_monthly, "factors_monthly.csv")
cat("✔ factors_monthly.csv 생성 완료\n")

cat("\n=== 작업 완료 ===\n")
cat(" - asset_returns_monthly.csv\n")
cat(" - factors_monthly.csv\n")
cat(" 두 파일이 c:/easy_r 에 생성되었습니다.\n")

