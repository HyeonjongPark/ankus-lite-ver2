
sgsp = wfood %>% filter(companyname == "세계식품(주)")

sgsp %>% as.data.table()
colSums(is.na(sgsp))
sgsp$itemseq %>% unique() %>% length()
sgsp$itemname %>% unique() %>% length()

sgsp$custclass %>% unique()


# custclass가 결측인 경우(15개) 제외
sgsp = sgsp %>% filter(is.na(custclass) == FALSE) 


sgsp



# 업종별 빈도
sgsp$custclass %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "custclass", y = "빈도") + ggtitle("업종별 빈도") +
  theme(title = element_text(size = 15))

ggsave("./visualization/세계식품업종별빈도.jpg")

# 계절별 판매량
sgsp %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = season, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
  theme(title = element_text(size = 15))


# 아이템별 판매량
sgsp %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = item, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
  theme(title = element_text(size = 15))


# 공휴일 유무에 따른 판매량
sgsp %>% filter(invoicedate <= "2018-12-30") %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))



sgsp$yearmonth = substr(sgsp$invoicedate, 1, 7)
sgsp$month = substr(sgsp$invoicedate, 6, 7)

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
sgsp$weekday = factor(weekdays((sgsp$invoicedate)), levels=day_levels, ordered=TRUE)

sgsp_plus = sgsp %>% filter(qty > 0)
sgsp_minus = sgsp %>% filter(qty < 0)


## 아몬드류로 한정
sgsp$item %>% table()
sgsp_amond = sgsp_plus %>% filter(item == "아몬드류")
sgsp_amond %>% as.data.table()


## 아몬드 - 편의점

sgsp_amond_con = sgsp_amond %>% filter(custclass == "편의점")

sgsp_amond_con %>% group_by(yearmonth) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = yearmonth, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드편의점1.jpg")

sgsp_amond_con %>% group_by(month) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = month, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드편의점2.jpg")

sgsp_amond_con %>% group_by(weekday) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = weekday, y = qty_sum)) + geom_bar(stat = "identity")

ggsave("./visualization/아몬드편의점3.jpg")



## 아몬드 - 대리점
sgsp_amond_daeri = sgsp_amond %>% filter(custclass == "대리점")

sgsp_amond_daeri %>% group_by(yearmonth) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = yearmonth, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드대리점1.jpg")

sgsp_amond_daeri %>% group_by(month) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = month, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드대리점2.jpg")

sgsp_amond_daeri %>% group_by(weekday) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = weekday, y = qty_sum)) + geom_bar(stat = "identity")

ggsave("./visualization/아몬드대리점3.jpg")





## 아몬드 - 할인점
sgsp_amond_sale = sgsp_amond %>% filter(custclass == "할인점")

sgsp_amond_sale %>% group_by(yearmonth) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = yearmonth, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드할인점1.jpg")

sgsp_amond_sale %>% group_by(month) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = month, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드할인점2.jpg")

sgsp_amond_sale %>% group_by(weekday) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = weekday, y = qty_sum)) + geom_bar(stat = "identity")

ggsave("./visualization/아몬드할인점3.jpg")


## 아몬드 - 온라인


sgsp_amond_online = sgsp_amond %>% filter(custclass == "온라인")

sgsp_amond_online %>% group_by(yearmonth) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = yearmonth, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드온라인1.jpg")

sgsp_amond_online %>% group_by(month) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = month, y = qty_sum, group = 1)) + geom_line(color = "blue")

ggsave("./visualization/아몬드온라인2.jpg")

sgsp_amond_online %>% group_by(weekday) %>% summarise(qty_sum = sum(qty)) %>% 
  ggplot(aes(x = weekday, y = qty_sum)) + geom_bar(stat = "identity")

ggsave("./visualization/아몬드온라인3.jpg")





