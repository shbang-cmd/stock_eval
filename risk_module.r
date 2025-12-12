###############################################################################
# risk_module.R  (JS 펀드용 리스크 분석 모듈 - 확장 버전)
#
# 포함 기능:
#  0) dd로부터 "현금흐름 보정 일별 운용수익률" 계산
#  1) 포트폴리오 μ, σ 추정 (연 기대수익률, 변동성)
#  2) 적립식 몬테카를로 (10년 후 평가액 분포)
#  3) 미래 최대낙폭(MDD) 분포 시뮬레이션
#  4) 은퇴 후 인출 시나리오 Monte Carlo (initial_value로 시작자산 지정 가능)
#  5) 팩터 회귀 분석 (Factor model)
#  6) PCA 기반 리스크 분해 (Risk via PCA)
###############################################################################

library(dplyr)

###############################################################################
# 0. dd로부터 "현금흐름 보정 일별 운용수익률" 계산
###############################################################################
# dd: 최소한 아래 컬럼을 가져야 함
#   - Date  : 날짜 (Date 형)
#   - Sum   : 전체 평가금액
#   - Profit: 총수익금 (Sum - 투자원금)
#
# 개념:
#   Invested_t  = Sum_t - Profit_t            (누적 투자원금)
#   Flow_t      = Invested_t - Invested_{t-1} (t일에 외부에서 새로 들어온/나간 돈)
#   Sum_t       = (Sum_{t-1} + Flow_t) * (1 + r_t)
#   ⇒ r_t       = Sum_t / (Sum_{t-1} + Flow_t) - 1
###############################################################################
compute_daily_returns_from_dd <- function(dd) {
  dd <- dd %>% arrange(Date)
  
  if (!all(c("Sum", "Profit") %in% colnames(dd))) {
    stop("compute_daily_returns_from_dd: dd에 'Sum'과 'Profit' 컬럼이 필요합니다.")
  }
  
  dd <- dd %>%
    mutate(
      Invested      = Sum - Profit,             # 누적 투자원금
      Invested_lag  = dplyr::lag(Invested),
      Sum_lag       = dplyr::lag(Sum),
      Flow          = Invested - Invested_lag,  # t일에 새로 들어온 순 현금
      Gross_base    = Sum_lag + Flow,          # 운용 대상 자산
      DailyRet_raw  = if_else(
        !is.na(Gross_base) & Gross_base > 0,
        Sum / Gross_base - 1,
        NA_real_
      )
    )
  
  r_daily <- dd$DailyRet_raw
  r_daily <- r_daily[!is.na(r_daily)]
  if (length(r_daily) == 0) {
    stop("compute_daily_returns_from_dd: 유효한 수익률이 없습니다.")
  }
  
  # ±50% 이상은 데이터 오류/극단값 가능성이 커서 제거(원하면 주석 처리 가능)
  r_daily <- r_daily[abs(r_daily) < 0.5]
  
  if (length(r_daily) < 10) {
    warning("compute_daily_returns_from_dd: 유효한 일별 수익률이 10개 미만입니다. 추정치 신뢰도가 낮을 수 있습니다.")
  }
  
  return(r_daily)
}

###############################################################################
# 1. 포트폴리오 μ, σ 추정 (현금흐름 보정 버전)
###############################################################################
# dd: Date, Sum, Profit 포함한 데이터프레임
#
# 출력:
#   - mu_daily, sigma_daily   : 일간 기대수익률, 변동성
#   - mu_annual, sigma_annual : 연환산 기대수익률, 변동성 (252 영업일 가정)
###############################################################################
estimate_mu_sigma_from_dd <- function(dd) {
  dd <- dd %>% arrange(Date)
  
  if (nrow(dd) < 2) {
    stop("estimate_mu_sigma_from_dd: dd에 최소 2개 이상의 행이 필요합니다.")
  }
  
  r_daily <- compute_daily_returns_from_dd(dd)
  
  mu_daily    <- mean(r_daily, na.rm = TRUE)
  sigma_daily <- sd(r_daily,   na.rm = TRUE)
  
  mu_annual    <- mu_daily * 252
  sigma_annual <- sigma_daily * sqrt(252)
  
  list(
    mu_daily     = mu_daily,
    sigma_daily  = sigma_daily,
    mu_annual    = mu_annual,
    sigma_annual = sigma_annual
  )
}

###############################################################################
# 2. 적립식 몬테카 (현역기 - accumulation phase)
###############################################################################
# 입력:
#   dd              : Date, Sum, Profit 포함
#   years           : 시뮬레이션 기간(년)
#   monthly_contrib : 매월 적립금(원)
#   n_sims          : 시뮬레이션 횟수
###############################################################################
run_mc_from_dd <- function(dd,
                           years = 10,
                           monthly_contrib = 5000000,
                           n_sims = 5000) {
  est <- estimate_mu_sigma_from_dd(dd)
  
  V0    <- tail(dd$Sum, 1)        # 현재 평가액
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  dt_year <- 1 / 12   # 월 단위
  sim_terminal <- numeric(n_sims)
  set.seed(123)
  
  for (i in seq_len(n_sims)) {
    V <- V0
    for (m in 1:(years * 12)) {
      V <- V + monthly_contrib   # 매월 적립
      z <- rnorm(1)
      r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
      V <- V * exp(r)
    }
    sim_terminal[i] <- V
  }
  
  qs       <- quantile(sim_terminal, c(0.1, 0.5, 0.9))
  mean_val <- mean(sim_terminal)
  
  cat("========================================\n")
  cat(" 몬테카를로 기반", years, " 년 후 평가액 분포\n")
  cat("  (", n_sims, "회 시뮬레이션, 월 적립",
      format(monthly_contrib, big.mark = ","), "원)\n", sep = "")
  cat("----------------------------------------\n")
  cat(" 10% 분위수 (나쁜 장세) :",
      format(round(qs[1], 0), big.mark = ","), "원\n")
  cat(" 50% 분위수 (중앙값)   :",
      format(round(qs[2], 0), big.mark = ","), "원\n")
  cat(" 90% 분위수 (좋은 장세) :",
      format(round(qs[3], 0), big.mark = ","), "원\n")
  cat(" 평균값(Mean)          :",
      format(round(mean_val, 0), big.mark = ","), "원\n")
  cat(" 연 기대수익률(추정)   :",
      round(mu * 100, 2), "%,  연 변동성(추정) :", round(sigma * 100, 2), "%\n")
  cat("========================================\n\n")
  
  invisible(list(
    terminal_values = sim_terminal,
    summary = list(
      q10          = as.numeric(qs[1]),
      q50          = as.numeric(qs[2]),
      q90          = as.numeric(qs[3]),
      mean         = mean_val,
      mu_annual    = mu,
      sigma_annual = sigma
    )
  ))
}

###############################################################################
# 3. 미래 최대낙폭(MDD) 분포 시뮬레이션
###############################################################################
# run_future_mdd_from_dd:
#   - 적립 여부를 포함한 전체 경로를 여러 번 시뮬레이션
#   - 각 시뮬레이션 경로에서 최대낙폭(MDD)을 계산
#   - MDD(%) 분포의 분위수를 출력
###############################################################################
run_future_mdd_from_dd <- function(dd,
                                   years = 10,
                                   monthly_contrib = 5000000,
                                   n_sims = 2000) {
  est <- estimate_mu_sigma_from_dd(dd)
  
  V0    <- tail(dd$Sum, 1)
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  dt_year <- 1 / 12
  m_per_year <- 12
  n_steps <- years * m_per_year
  
  mdd_vec <- numeric(n_sims)
  set.seed(456)
  
  for (i in seq_len(n_sims)) {
    V <- numeric(n_steps + 1)
    V[1] <- V0
    peak <- V0
    dd_path <- numeric(n_steps + 1)
    dd_path[1] <- 0
    
    for (t in 1:n_steps) {
      # 적립
      V[t] <- V[t] + monthly_contrib
      # 수익률
      z <- rnorm(1)
      r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
      V[t + 1] <- V[t] * exp(r)
      
      # drawdown 갱신
      if (V[t + 1] > peak) {
        peak <- V[t + 1]
      }
      dd_path[t + 1] <- (V[t + 1] / peak) - 1
    }
    
    mdd_vec[i] <- min(dd_path, na.rm = TRUE)  # 가장 낮은 DD
  }
  
  mdd_percent <- mdd_vec * 100
  qs <- quantile(mdd_percent, c(0.1, 0.5, 0.9))
  
  cat("========================================\n")
  cat(" 미래", years, " 년 동안 최대낙폭(MDD) 분포 (적립 포함)\n")
  cat("  (", n_sims, "경로 시뮬레이션)\n", sep = "")
  cat("----------------------------------------\n")
  cat(" 10% 분위수(하위 10%: 비교적 크게 빠지는 경우)   :",
      round(qs[1], 2), "%\n")
  cat(" 50% 분위수(중앙값: 보통 한 번쯤 겪을 만한 MDD)  :",
      round(qs[2], 2), "%\n")
  cat(" 90% 분위수(상위 10%: 상대적으로 덜 빠지는 경우) :",
      round(qs[3], 2), "%\n")
  cat("----------------------------------------\n")
  cat(" * 음수 값이므로 -30% 라는 뜻은 피크 대비 30% 하락을 의미\n")
  cat("========================================\n\n")
  
  invisible(list(
    mdd_raw      = mdd_vec,
    mdd_percent  = mdd_percent,
    summary      = list(q10 = qs[1], q50 = qs[2], q90 = qs[3]),
    mu_annual    = mu,
    sigma_annual = sigma
  ))
}

###############################################################################
# 4. 은퇴 후 인출 시나리오 Monte Carlo (decumulation phase)
###############################################################################
# run_mc_withdraw_from_dd:
#   - 시작 시점 자산: 기본은 dd$Sum 마지막 값
#   - initial_value 인자를 주면 그 값을 시작자산으로 사용 (예: 10년 후 예상자산)
#   - 이후 적립 없음, 대신 매년/매월 고정 금액 인출
#   - 자산이 0 이하로 떨어지면 "파산" 처리
#
# 입력:
#   dd                : Date, Sum, Profit 포함
#   years             : 은퇴 후 시뮬레이션 기간 (예: 30년)
#   annual_withdraw   : 연 인출액(원)
#   n_sims            : 시뮬레이션 수
#   withdraw_freq     : "annual" 또는 "monthly"
#   initial_value     : 시작자산(원), NULL이면 tail(dd$Sum,1) 사용
###############################################################################
run_mc_withdraw_from_dd <- function(dd,
                                    years = 30,
                                    annual_withdraw = 200000000,
                                    n_sims = 5000,
                                    withdraw_freq = c("annual", "monthly"),
                                    initial_value = NULL) {
  withdraw_freq <- match.arg(withdraw_freq)
  
  est <- estimate_mu_sigma_from_dd(dd)
  
  # 시작자산: initial_value가 지정되면 그 값 사용, 아니면 dd의 마지막 Sum
  V0 <- if (is.null(initial_value)) {
    tail(dd$Sum, 1)
  } else {
    initial_value
  }
  
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  set.seed(789)
  
  ruin_flag <- logical(n_sims)
  terminal  <- numeric(n_sims)
  
  if (withdraw_freq == "annual") {
    dt_year <- 1
    n_steps <- years
    withdraw_step <- annual_withdraw
    for (i in seq_len(n_sims)) {
      V <- V0
      ruined <- FALSE
      for (t in 1:n_steps) {
        # 1년 수익
        z <- rnorm(1)
        r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
        V <- V * exp(r)
        # 인출
        V <- V - withdraw_step
        if (V <= 0) {
          ruined <- TRUE
          V <- 0
          break
        }
      }
      ruin_flag[i] <- ruined
      terminal[i]  <- V
    }
  } else if (withdraw_freq == "monthly") {
    dt_year <- 1 / 12
    n_steps <- years * 12
    withdraw_step <- annual_withdraw / 12
    for (i in seq_len(n_sims)) {
      V <- V0
      ruined <- FALSE
      for (t in 1:n_steps) {
        # 1개월 수익
        z <- rnorm(1)
        r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
        V <- V * exp(r)
        # 인출
        V <- V - withdraw_step
        if (V <= 0) {
          ruined <- TRUE
          V <- 0
          break
        }
      }
      ruin_flag[i] <- ruined
      terminal[i]  <- V
    }
  }
  
  ruin_prob <- mean(ruin_flag)
  qs <- quantile(terminal, c(0.1, 0.5, 0.9))
  mean_val <- mean(terminal)
  
  cat("========================================\n")
  cat(" 은퇴 후 인출 시나리오 Monte Carlo\n")
  cat("  기간 :", years, "년,  연 인출액:",
      format(annual_withdraw, big.mark = ","), "원\n", sep = "")
  cat("  시작자산 :", format(round(V0, 0), big.mark = ","), "원\n")
  cat("  가정: 연 기대수익률", round(mu * 100, 2),
      "%, 연 변동성", round(sigma * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat(" 파산 확률 (자산이 0 이하로 떨어질 확률) :", round(ruin_prob * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat(" 기간 종료 시 잔고 10% 분위수 :", format(round(qs[1], 0), big.mark = ","), "원\n")
  cat(" 기간 종료 시 잔고 50% 분위수 :", format(round(qs[2], 0), big.mark = ","), "원\n")
  cat(" 기간 종료 시 잔고 90% 분위수 :", format(round(qs[3], 0), big.mark = ","), "원\n")
  cat(" 평균 잔고(Mean)              :", format(round(mean_val, 0), big.mark = ","), "원\n")
  cat("========================================\n\n")
  
  invisible(list(
    terminal_values = terminal,
    ruin_flag       = ruin_flag,
    ruin_prob       = ruin_prob,
    summary         = list(
      q10  = as.numeric(qs[1]),
      q50  = as.numeric(qs[2]),
      q90  = as.numeric(qs[3]),
      mean = mean_val
    ),
    mu_annual       = mu,
    sigma_annual    = sigma
  ))
}

###############################################################################
# 5. 팩터별 분석 (Factor Regression)
###############################################################################
#   port_ret  : 포트폴리오 수익률 벡터 (월별 수익률 권장)
#   factors_df: data.frame, 열 = 각 팩터 (MKT, GOLD, RATE 등)
###############################################################################
run_factor_model <- function(port_ret, factors_df) {
  if (length(port_ret) != nrow(factors_df)) {
    stop("run_factor_model: 포트 수익률 길이와 factors_df 행 수가 같아야 합니다.")
  }
  
  dat <- data.frame(port = port_ret, factors_df)
  form <- as.formula(
    paste("port ~", paste(colnames(factors_df), collapse = " + "))
  )
  
  fit <- lm(form, data = dat)
  
  cat("========================================\n")
  cat(" 팩터 회귀모형 결과\n")
  cat(" 포트 수익률 ~", paste(colnames(factors_df), collapse = " + "), "\n")
  cat("========================================\n\n")
  print(summary(fit))
  cat("\n(해석)\n")
  cat("- 각 팩터의 계수(Estimate)가 해당 팩터에 대한 민감도(β)입니다.\n")
  cat("- 예) MKT 계수 1.2 → 시장이 1% 오르면 포트는 평균 1.2% 수익\n")
  cat("- R-squared → 팩터들로 설명되는 비율(설명력)\n\n")
  
  invisible(fit)
}

###############################################################################
# 6. PCA 기반 리스크 분해 (Risk via PCA)
###############################################################################
#   asset_returns: (날짜 x 자산) 수익률 data.frame
#   weights      : 해당 자산의 포트 비중 (합계 = 1)
###############################################################################
run_pca_risk <- function(asset_returns, weights, scale. = TRUE) {
  asset_returns <- as.data.frame(asset_returns)
  
  if (ncol(asset_returns) != length(weights)) {
    stop("run_pca_risk: 자산 열 수와 weights 길이가 같아야 합니다.")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    warning("run_pca_risk: weights 합이 1이 아니라 자동 정규화합니다.")
    weights <- weights / sum(weights)
  }
  
  ret_clean <- asset_returns[complete.cases(asset_returns), ]
  cov_mat <- cov(ret_clean)
  
  pca_res <- prcomp(ret_clean, center = TRUE, scale. = scale.)
  
  eig    <- eigen(cov_mat)
  lambda <- eig$values
  phi    <- eig$vectors
  
  pc_contrib <- numeric(length(lambda))
  for (k in seq_along(lambda)) {
    loading_k     <- sum(weights * phi[, k])
    pc_contrib[k] <- lambda[k] * loading_k^2
  }
  pc_contrib_ratio <- pc_contrib / sum(pc_contrib)
  
  cat("========================================\n")
  cat(" PCA 기반 리스크 분해 결과\n")
  cat("========================================\n\n")
  
  cat("1) 전체 자산 분산 기준 PC별 설명력\n\n")
  print(summary(pca_res))
  cat("\n")
  
  cat("2) 포트폴리오 분산 기준 PC별 기여 비율\n")
  pc_table <- data.frame(
    PC                   = paste0("PC", seq_along(lambda)),
    PortVar_Contribution = pc_contrib_ratio
  )
  print(pc_table)
  cat("\n(해석)\n")
  cat("- PortVar_Contribution가 큰 PC일수록, 해당 PC가 포트 변동성에 크게 기여\n")
  cat("- 예) PC1이 70% 이상이면, 거의 '공통 시장 리스크'가 지배적이라고 볼 수 있음\n\n")
  
  cat("3) 자산별 PC 로딩 (pca_res$rotation)\n")
  print(pca_res$rotation)
  cat("\n(해석)\n")
  cat("- 어떤 PC에서 특정 자산의 로딩이 크면, 그 자산은 해당 공통 팩터에 많이 노출\n")
  cat("- 예) PC1에서 SPY/QQQ/SCHD/TQQQ 로딩이 모두 +이면, PC1 = '미국 주식 공통 팩터'\n\n")
  
  invisible(list(
    pca        = pca_res,
    cov_mat    = cov_mat,
    pc_contrib = pc_contrib,
    pc_ratio   = pc_contrib_ratio
  ))
}

###############################################################################
# risk_module.R 끝
###############################################################################
