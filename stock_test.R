## 데이터 불러 오기

krx_idx <- read.csv("KRX.csv", fileEncoding = "euc-kr") # 각 컬럼을 날짜형식으로 바꿔주자. 
KOSDAQ_idx <- read.csv("KOSDAQ.csv", fileEncoding = "euc-kr") # 각 컬럼을 날짜형식으로 바꿔주자. 
KOSPI_idx <- read.csv("KOSPI.csv", fileEncoding = "euc-kr")




## KRX ----
sample_set <- krx_idx

krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[1,]

krx_idx_2 <- krx_idx_2[-1,]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

krx_idx_2 <- krx_idx_2 %>% 
  drop_na()



sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       `KRX 자동차`, `KRX 반도체`, `KRX 헬스케어`, `KRX 은행`,
                       `KRX 에너지화학`, `KRX 철강`, `KRX 방송통신`, `KRX 건설` ,`KRX 증권` ,`KRX 기계장비`, `KRX 보험`, `KRX 운송`,
                       `KRX 경기소비재`, `KRX 필수소비재`, `KRX IT소프트웨어`, `KRX IT하드웨어`, `KRX 유틸리티`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)



sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)



## KOSDAQ ----
# krx_idx <- read.csv("KOSDAQ.csv", fileEncoding = "euc-kr") # 각 컬럼을 날짜형식으로 바꿔주자. 

## 코스닥 시리즈 1~3 ---- 
sample_set <- KOSDAQ_idx[1:3,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[1,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

krx_idx_2 <- krx_idx_2 %>%
  drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       KOSDAQ, `KOSDAQ 150`, `KOSDAQ IT`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)

## 코스닥 시리즈 4~7 ---- 
sample_set <- KOSDAQ_idx[4:7,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[2,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

krx_idx_2 <- krx_idx_2 %>%
  drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       `KOSDAQ 우량기업부`, `KOSDAQ 벤처기업부`, `KOSDAQ 중견기업부`, `KOSDAQ 기술성장기업부`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)


## 코스닥 시리즈 8~10 ---- 
sample_set <- KOSDAQ_idx[8:10,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[2,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2 %>% 
# drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       `코스닥 대형주`, `코스닥 중형주`, `코스닥 소형주`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)


## KOSPI ----

krx_idx <- read.csv("KOSPI.csv", fileEncoding = "euc-kr")


## 코스피 시리즈 1~5 ---- 
sample_set <- KOSPI_idx[1:5,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[1,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

krx_idx_2 <- krx_idx_2 %>%
  drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       KOSPI, `KOSPI 200`, `V-KOSPI 200`, `KOSPI 100`, `KOSPI 50`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)

## 코스피 시리즈 6~8 ---- 
sample_set <- KOSPI_idx[6:8,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[2,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

krx_idx_2 <- krx_idx_2 %>%
  drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       대형주, 중형주, 소형주) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)


## 코스피 시리즈 9~13 ---- 
sample_set <- krx_idx[9:13,1:7]
sample_set <- KOSPI_idx[9:13,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[1,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2 %>% 
# drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       `F-KOSPI 200`, `F-KOSPI 200 인버스지수`, `KOSPI 200 레버리지지수`, `KOSPI 200 커버드콜 지수`, `KOSPI 200 프로텍티브풋 지수`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)

## 코스피 시리즈 14~16 ---- 
sample_set <- KOSPI_idx[14:16,]


krx_idx_2 <-  sample_set %>% 
  t()

colnames(krx_idx_2) <- krx_idx_2[2,]

krx_idx_2 <- krx_idx_2[-c(1:2),]


krx_idx_2 <- cbind("Date" = rownames(krx_idx_2),krx_idx_2)

rownames(krx_idx_2) <- NULL

krx_idx_2 <- krx_idx_2 %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2[,-c(2:5)]


date_month <- krx_idx_2$Date %>% 
  str_remove_all("X") %>% 
  str_replace_all("\\.\\.","-")

date_month <- paste0(date_month, "-01") 


date_month <- date_month %>% 
  ymd()

krx_idx_2$Date <- date_month


krx_idx_2[,-1] <- sapply(krx_idx_2[,-1], as.numeric) %>% 
  as_tibble()

# krx_idx_2 <- krx_idx_2 %>% 
# drop_na()

sample_df_22 <- krx_idx_2

sample_df_22 <- gather(data = sample_df_22, key="종목",value= "주가",
                       `KOSPI 200`, `KOSPI 100`, `KOSPI 50`) %>% 
  arrange(Date)

sample_df_22$종목 <- as_factor(sample_df_22$종목)

#### ggplot 전처리

sample_df_22 <- sample_df_22 %>% 
  mutate(year = year(Date),
         month = month(Date))


sample_df_22_barplot <- sample_df_22 %>% 
  group_by(year, 종목) %>% 
  summarise(주가평균 = mean(주가))

## 
sample_df_22_barplot %>% 
  ggplot(aes( x = year, y = 주가평균, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()



asdf <- sample_df_22 %>% 
  ggplot(aes( x = Date, y = 주가, group = 종목, colour = 종목)) +
  theme_bw() +
  geom_line()


ggplotly(asdf)
