# 주식명과 종목번호, 매수가와 수량을 넣으면
# 자동으로 현재가를 가져와서 수익금과 수익률을 보여주는 R 코드
# 
# 입력파일(csv파일) : 맨 위에 "종목명", "종목번호", "보유증권사", "매수가격", "수량"의 필드명을 넣고
# 그 아래에 각각 데이터를 넣고 저장한다. 파일명은 input_stock.csv로 만든다. 
# 단, 종목번호는 finance.yahoo.com에서 조회가 가능한 종목번호를 넣는다. ex)삼성전자는 005930.KS
# 
# 출력파일(엑셀파일) : 종목별 평가금과 비중, 평가액을 보여주고 맨 아래에 총 평가금합계와 수익률을 보여준다.

pkg = c("quantmod", "realxl", "writexl", "dplyr", "tidyverse", "scales")
new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}
# ctrl + alt + e
library(quantmod); library(readxl); library(writexl); library(dplyr); library(tidyverse); library(scales)


options(warn=-1)  # warning 표시 안함

# 오늘의 날짜 문자열 생성
today <- format(Sys.Date(), "%Y-%m-%d") 

data <- read_csv("input_stock.csv",
                 #col_name = T,
                 locale = locale("ko", encoding = "euc-kr"),
                 na = ".")
data
output_file <- paste(paste("output_stock_", today, sep = ""), ".xlsx", sep = "") # 출력파일명 뒤에 날짜삽입

# Check its existence
if (file.exists(output_file)) {
  file.remove(output_file) # 파일이 이미 존재하면 지운다.
}

# 수익금 계산을 위한 빈 벡터 생성
tickername <- NA
security <- NA
current_price <- NA
amount <- NA
profits <- NA

# 주식 정보를 순회하면서 수익금 계산
for (i in 1:nrow(data)) {
  tickername[i] <- as.character(data$종목명[i])
  symbol <- as.character(data$종목번호[i])
  security[i] <- as.character(data$보유증권사[i])
  purchase_price <- data$매수가격[i]
  quantity <- data$수량[i]
  
  # 현재 주식 가격 가져오기
  getSymbols(symbol, src = "yahoo", from = Sys.Date()-7, to = Sys.Date())
  current_price[i] <- as.numeric(last(get(symbol)[,4])) # symbol 종목의 open, high, low, close 가격에서 4번째 위치한 종가를 가져온다.
  
  amount[i] <- current_price[i] * quantity  # 종목별 평가액
  
  # 수익금 계산
  profits[i] <- (current_price[i] - purchase_price) * quantity
}

# 데이터 프레임에 수익금 추가
data$종목명 <- tickername
data$보유증권사 <- security
data$현재가 <- current_price
data$평가금 <- amount

total_sum <- sum(amount) # 평가액 합산
total_profit <- sum(profits) # 총 수익금 계산

stock_ratio <- NA
stock_profit_ratio <- NA

for (i in 1:nrow(data)) {
  stock_ratio[i] <- (data$평가금[i] / total_sum)
  stock_profit_ratio[i] <- (profits[i] / (data$평가금[i] - profits[i]))
}

data$비중 <- stock_ratio
data$수익금 <- profits
data$수익률 <- stock_profit_ratio

data <- data %>% arrange(desc(평가금))

# 오늘의 날짜로 시작하는 행을 추가하고 총 수익금 입력
summary_row <- data.frame(종목명 = paste("(", today, "합계", ")"), 종목번호 = NA, 보유증권사 = NA, 매수가격 = NA, 수량 = NA, 현재가 = NA, 평가금 = total_sum, 비중 = sum(stock_ratio), 수익금 = total_profit, 수익률 = total_profit / (total_sum - total_profit))
data <- rbind(data, summary_row)

# 결과를 엑셀 파일로 저장
write_xlsx(data, output_file)

cat(nrow(data)-1, "개 종목의 수익금 계산이 완료되었습니다. 결과는", output_file, "에 저장되었습니다.")

data_ko <- data
View(data_ko)

# 아래 통계는 콘솔과 plots창에 표시됨
# 증권사별 평가액
new_data <- data %>% 
  group_by(보유증권사) %>% 
  summarize(sec_tot = sum(평가금)) %>% 
  arrange(desc(sec_tot))
new_data <- new_data %>% filter(!is.na(보유증권사))  # NA 제거

ggplot(data = new_data, aes(x = reorder(보유증권사, -sec_tot), y = sec_tot/1000000)) + 
  labs(x = "증권사", y = "보유액합계(백만원)") +
  geom_text(aes(label=sec_tot/1000000), vjust = -0.1) +
  geom_col()

# 종목별 평가액
new_data <- data %>% 
  group_by(종목명) %>% 
  summarize(종목평가합산 = sum(평가금), 합산수량 = sum(수량), 수익금합산 = sum(수익금)) %>% 
  arrange(desc(종목평가합산))
new_data <- new_data[-1,]    # 첫번째 행 제거
new_data$rate = new_data$종목평가합산 / sum(new_data$종목평가합산)
new_data_to_display <- new_data %>% 
  mutate(종목평가합산 = comma(종목평가합산)) %>% 
  mutate(합산수량 = comma(합산수량)) %>% 
  mutate(수익금합산 = comma(수익금합산))
print(new_data_to_display, n=50)

ggplot(new_data, aes(x = reorder(종목명, -종목평가합산), y = 종목평가합산/1000000, fill=수익금합산/종목평가합산)) + 
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  labs(x = "종목", y = "종목별 합계(백만원)") +
  geom_col() +
  geom_text(aes(label = paste0(round(종목평가합산/sum(종목평가합산)*100, 2), "%") ), vjust = -0.02) +
  scale_fill_gradient2(low = "red", 
                       high = "blue", 
                       midpoint = 0)

