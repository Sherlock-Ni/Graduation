
# Load packages  ============================

library(tidyverse)
library(haven)
library(patchwork)
library(RColorBrewer)
library(ggthemes) # set color as tableau
library(DescTools) # use to calculate pseudo R2
library(AER)
library(stargazer)
#library(safeBinaryRegression)
#library(logistf)  #用于消除probit回归时出现的概率趋于0或1
#source("ivreg2.R") #工具变量检验的另一种形式

# Load data  ===================================

master <- read_dta("master.dta", encoding = "utf-8")

master$total_income[is.na(master$total_income)] <- 0 
master$total_debt[is.na(master$total_debt)] <- 0  

ind <- read_dta("ind.dta", encoding = "utf-8")

ind <- ind %>% 
  filter(hhead == 1)

hh <- read_dta("hh.dta", encoding = "utf-8")

ind_a1 <- read_dta("ind.dta", encoding = "utf-8")

# Clean data  ====================================

m19 <- master %>% 
  mutate(id = paste(hhid, pline, sep = "_"),
         t_income = ifelse(total_income == 0, 0, 
                           ifelse(total_income > 0, log(total_income), -log(abs(total_income)))),
         t_asset = ifelse(total_asset > 0, log(total_asset), 0),
         nasset = total_asset-total_debt, 
         net_asset = ifelse(nasset == 0, 0, ifelse(nasset<0, -log(abs(nasset)), log(nasset)))) %>% 
  select(id, hhid, prov, rural,total_income,t_income, total_asset, t_asset, nasset, net_asset,fina_asset,house_asset)

m19$house_asset <- ifelse(is.na(m19$house_asset) == T, 0, m19$house_asset)


d19 <- ind %>% 
  mutate(id = paste(hhid, pline, sep = "_"),
         male = ifelse(a2003 == 1, 1, 0),
         age = 2019-ifelse(is.na(a2005) == T, 2019, a2005),
         edu = ifelse(is.na(a2012) == T, 1, a2012),
         work = ifelse(is.na(a3132b) == T, 0, ifelse(a3132b == 1, 1, 0)),
         marriage = ifelse(is.na(a2024) == T, 0, ifelse(a2024 == 2, 1, 0)),
         industry = ifelse(is.na(a3132f) == T, 0, a3132f),
         yanglao = ifelse(is.na(f1001a) == T, 0,ifelse(f1001a == 7788,0,ifelse(f1001a == ".d",0,ifelse(f1001a == ".r",0,ifelse(f1001a == ".e",0,ifelse(f1001a == ".n",0,1)))))),
         yiliao = ifelse(is.na(f2001a) == T, 0,ifelse(f2001a == 7788,0,ifelse(f2001a == ".d",0,ifelse(f2001a == ".r",0,ifelse(f2001a == ".e",0,ifelse(f2001a == ".n",0,1)))))),
         shiye = ifelse(is.na(f3001) == T, 0,ifelse(f3001 == 7788,0,ifelse(f3001 == ".d",0,ifelse(f3001 == ".r",0,ifelse(f3001 == ".e",0,ifelse(f3001 == ".n",0,1)))))),
         shangye = ifelse(is.na(f6001a) == T, 0,ifelse(f6001a == 7788,0,ifelse(f6001a == ".d",0,ifelse(f6001a == ".r",0,ifelse(f6001a == ".e",0,ifelse(f6001a == ".n",0,1)))))),
         insurance = yanglao + yiliao + shiye + shangye,
         health = ifelse(is.na(a2025b) == T, 3, ifelse(a2025b == 1, 5, ifelse(a2025b == 2, 4, 
                                                                              ifelse(a2025b == 4, 2, ifelse(a2025b == 5, 1, 3)))))
  ) %>% 
  select(id, hhid, male, age, marriage, edu, health, work, industry, insurance)

d19$insurance <- ifelse(d19$insurance > 0, 1, 0)



ind_a <- ind_a1 %>% 
  filter(hhead == 0) %>% 
  group_by(hhid, a2001) %>% 
  mutate(edu1 = ifelse(is.na(a2012) == T, 1, a2012))%>% 
  summarize(edu_p = max(edu1)) %>%
  filter(a2001 == 3) %>% 
  select(hhid,edu_p)


d19 <-left_join(d19, ind_a, by = "hhid")



h19 <- hh %>% 
  mutate(finance_inf = ifelse(is.na(h3101) == T, 1, ifelse(h3101 == 1, 5, ifelse(h3101 == 2, 4, 
                                                                                 ifelse(h3101 == 4, 2, ifelse(h3101 == 5, 1, 3))))),
         risk_aver = h3104,
         finknow_int = ifelse(is.na(h3105) == T, 0, ifelse(h3105 == 2, 1, 0)),
         finknow_inf = ifelse(is.na(h3106) == T, 0, ifelse(h3106 == 3, 1, 0)),
         finknow_risk = ifelse(is.na(h3112) == T, 0, ifelse(h3112 == 2, 1, 0)),
         finknow = finknow_inf + finknow_int + finknow_risk,
         ind_business = ifelse(is.na(b2000b) == T, 0, ifelse(b2000b == 1, 1, 0)),
         house = ifelse(is.na(c2001) == T, 0, ifelse(c2001 == 1, 1, 0)),
         numhouse = ifelse(is.na(c2002) == T, 1, c2002),
         cash = ifelse(is.na(k1101_imp) == T, 0, k1101_imp),
         current_dep = ifelse(is.na(d1105_imp) == T, 0, d1105_imp),
         time_dep = ifelse(is.na(d2104_imp) == T, 0, d2104_imp),
         inter_fin = ifelse(is.na(d7106d) == T, 0, ifelse(d7106d == 1, 1, 0)),
         inter_fin_a = ifelse(is.na(d7106hb_imp) == T, 0, d7106hb_imp),
         fin_planning = ifelse(is.na(d7109) == T, 0, ifelse(d7109 == 1, 1, 0)),
         fin_planning_a = ifelse(is.na(d7110a_imp) == T, 0, d7110a_imp),
         stock = ifelse(is.na(d3101) == T, 0, ifelse(d3101 == 1, 1, 0)),
         stock_a = ifelse(is.na(d3103_imp) == T, 0, d3103_imp),
         fund = ifelse(is.na(d5102) == T, 0, ifelse(d5102 == 1, 1, 0)),
         fund_1 = ifelse(is.na(d5107_1_imp) == T, 0, d5107_1_imp),
         fund_2 = ifelse(is.na(d5107_2_imp) == T, 0, d5107_2_imp),
         fund_3 = ifelse(is.na(d5107_3_imp) == T, 0, d5107_3_imp),
         fund_4 = ifelse(is.na(d5107_4_imp) == T, 0, d5107_4_imp),
         fund_5 = ifelse(is.na(d5107_5_imp) == T, 0, d5107_5_imp),
         fund_6 = ifelse(is.na(d5107_6_imp) == T, 0, d5107_6_imp),
         fund_7 = ifelse(is.na(d5107_7_imp) == T, 0, d5107_7_imp),
         fund_7777 = ifelse(is.na(d5107_7777_imp) == T, 0, d5107_7777_imp),
         fund_a = fund_1 + fund_2 + fund_3 + fund_4 + fund_5 + fund_6 + fund_7 + fund_7777,
         bond = ifelse(is.na(d7113_1_mc) == T, 0, ifelse(d7113_1_mc == 1, 1, 0)),
         bond_a = ifelse(is.na(d4103_imp) == T, 0, d4103_imp),
         derivative = ifelse(is.na(d7113_2_mc) == T, 0, ifelse(d7113_2_mc == 1, 1, 0)),
         der_1 = ifelse(is.na(d6100a) == T, 0, d6100a), 
         der_2 = ifelse(is.na(d6100ait) == T, 0, d6100ait),
         der_a = der_1 + der_2,
         golden = ifelse(is.na(d7113_3_mc) == T, 0, ifelse(d7113_3_mc == 1, 1, 0)),
         golden_a = ifelse(is.na(d9103_imp) == T, 0, d9103_imp),
         nonrmb = ifelse(is.na(d7113_4_mc) == T, 0, ifelse(d7113_4_mc == 1, 1, 0)),
         nonrmb_a = ifelse(is.na(d8104_imp) == T, 0, d8104_imp),
         riskasset = stock_a + bond_a + fund_a + inter_fin_a + fin_planning_a + 
           der_a + golden_a + nonrmb_a
  ) %>% 
  select(hhid, risk_aver, finknow, finknow_int, finknow_inf, finknow_risk, finance_inf, ind_business,
         house, numhouse,cash, current_dep, time_dep, stock, stock_a, bond, bond_a, fund, fund_a, inter_fin, inter_fin_a,
         fin_planning, fin_planning_a, derivative, der_a, golden, golden_a, nonrmb, nonrmb_a,riskasset)

df <- d19 %>% left_join(m19,by = "id") %>% 
  filter(prov %in% c("上海市","浙江省","江苏省","安徽省")) %>% 
  mutate(hhid = hhid.x) %>% 
  select(id, hhid, prov,male, age, marriage, edu, edu_p, health, work, industry, insurance,
         rural, total_income,t_income, total_asset, t_asset, nasset,net_asset,fina_asset, house_asset)

df <- left_join(df, h19, by = "hhid") %>% 
  mutate(pro_stock = ifelse(fina_asset == 0, 0, round((stock_a/fina_asset), 4)),
         fin_market = ifelse((stock+bond+fund+inter_fin+fin_planning+derivative+golden+
                                nonrmb)>0, 1, 0),
         pro_fin_market = ifelse(fina_asset == 0, 0, round((riskasset/fina_asset),4)),
         t_income2 = t_income^2,
         age2 = age^2) %>% 
  filter(age > 17)

df <- subset(df,df$risk_aver < 6) 

df$edu_p <- ifelse(is.na(df$edu_p) == T, df$edu, df$edu_p) 





write.csv(df,"df.csv") #全局使用的主数据框（长三角地区）


# Gross data(全国数据)


df_gross <- d19 %>% left_join(m19,by = "id") %>% 
  mutate(hhid = hhid.x) %>% 
  select(id, hhid, prov,male, age, marriage, edu, edu_p, health, work, industry, insurance,
         rural, total_income,t_income, total_asset, t_asset, nasset,net_asset,fina_asset, house_asset)

df_gross <- left_join(df_gross, h19, by = "hhid") %>% 
  mutate(pro_stock = ifelse(fina_asset == 0, 0, round((stock_a/fina_asset), 4)),
         fin_market = ifelse((stock+bond+fund+inter_fin+fin_planning+derivative+golden+
                                nonrmb)>0, 1, 0),
         pro_fin_market = ifelse(fina_asset == 0, 0, round((riskasset/fina_asset),4)),
         t_income2 = t_income^2,
         age2 = age^2) %>% 
  filter(age > 17)

df_gross <- subset(df_gross,df_gross$risk_aver < 6) 

df_gross$edu_p <- ifelse(is.na(df_gross$edu_p) == T, df_gross$edu, df_gross$edu_p) 




write.csv(df_gross,"df_gross.csv") # 包括全国的数据，数据已经处理过








# Draw The Picture of Risk Aversion  ===========================================


risk_aver_v <- df %>% 
  group_by(prov, risk_aver) %>% 
  summarize(count = n()) %>% 
  mutate(prop = round((count/sum(count))*100, 2))

risk_aver_v <- risk_aver_v %>% 
  mutate(risk_app = ifelse(risk_aver == 1, "风险偏好",
                           ifelse(risk_aver == 2, "较风险偏好",
                                  ifelse(risk_aver == 3, "风险中性",
                                         ifelse(risk_aver == 4, "较风险厌恶","风险厌恶")))))

risk_aver_v$risk_app <- factor(risk_aver_v$risk_app, levels = c("风险偏好","较风险偏好","风险中性",
                                                                "较风险厌恶","风险厌恶"))

risk_aver_p <- ggplot(data = risk_aver_v) + 
  geom_bar(mapping = aes(x = risk_app, y = prop,fill = prov), 
           stat = "identity", position = "dodge") + 
  labs(x = "风险态度", y = "各省份占比(%)", fill = "省份") + 
  scale_fill_brewer(palette = "Paired")

write.csv(risk_aver_v, "risk_aver_v.csv") #各省市的风险厌恶状况






# Analysize the condition of financial knowledge  ====================================
# Three problems discuss respectively

h19_a <- hh %>% 
  mutate(
    risk_aver_a = h3104,
    finknow_int_a = ifelse(is.na(h3105) == T, 0, ifelse(h3105 == 2, 1, ifelse(h3105 == 4, 2, 0))),
    finknow_inf_a = ifelse(is.na(h3106) == T, 0, ifelse(h3106 == 3, 1, ifelse(h3106 == 4, 2, 0))),
    finknow_risk_a = ifelse(is.na(h3112) == T, 0, ifelse(h3112 == 2, 1, ifelse(h3112 == 3, 2, 
                                                                               ifelse(h3112 == 4, 2,
                                                                                      ifelse(h3112 == 5, 2,0)))))
  ) %>% 
  select(hhid, risk_aver_a, finknow_int_a, finknow_inf_a, finknow_risk_a)


df_a <- d19 %>% left_join(m19,by = "id") %>% 
  filter(prov %in% c("上海市","浙江省","江苏省","安徽省")) %>% 
  mutate(hhid = hhid.x) %>% 
  select(id, hhid, prov,male, age, marriage, edu, work, industry, insurance,
         rural, t_income, total_asset, t_asset, net_asset,fina_asset)


df_a <- df_a %>% 
  left_join(h19_a, by = "hhid") %>% 
  filter(age > 17) %>% 
  subset(risk_aver_a < 6)


df_a1 <- df_a  %>%
  select(prov, finknow_int_a, finknow_inf_a, finknow_risk_a)


write.csv(df_a, "df_a.csv") # df_a文件中将金融知识的回答情况结果分为了正确、错误与不知道，与原有的df不同
write.csv(df_a1, "df_a1.csv") #从df_a数据中挑选出所需变量



finknow_v_int <- df_a1 %>% 
  group_by(prov, finknow_int_a) %>% 
  summarize(count_int = n()) %>% 
  mutate(prop_int = round((count_int/sum(count_int))*100,2),
         finknow_int_a1 = ifelse(finknow_int_a == 0, "错误",
                                 ifelse(finknow_int_a == 1, "正确","不知道")))  

finknow_v_inf <- df_a1 %>% 
  group_by(prov, finknow_inf_a) %>% 
  summarize(count_inf = n()) %>% 
  mutate(prop_inf = round((count_inf/sum(count_inf))*100,2),
         finknow_inf_a1 = ifelse(finknow_inf_a == 0, "错误",
                                 ifelse(finknow_inf_a == 1, "正确","不知道")))

finknow_v_risk <- df_a1 %>% 
  group_by(prov, finknow_risk_a) %>% 
  summarize(count_risk = n()) %>% 
  mutate(prop_risk = round((count_risk/sum(count_risk))*100,2),
         finknow_risk_a1 = ifelse(finknow_risk_a == 0, "错误",
                                  ifelse(finknow_risk_a == 1, "正确","不知道")))

finknow_v <- cbind(finknow_v_int,finknow_v_inf,finknow_v_risk) 
finknow_v <- finknow_v[c(-6,-11)]
names(finknow_v)[1] <- "prov"


write.csv(finknow_v, "finknow_v.csv") #各省市金融知识回答情况（三个金融知识分别讨论）





# Discuss the number of truth

df_knowlegde_truth <- df %>% 
  group_by(prov, finknow) %>% 
  summarize(count = n()) %>% 
  mutate(prop = round((count/sum(count))*100, 2))

write.csv(df_knowlegde_truth, "the number of answer right.csv") #各省金融知识答对题数分布






# The condition of finance information ============================

df_fininf <- df %>% 
  group_by(prov, finance_inf) %>% 
  summarize(count =n()) %>% 
  mutate(prop = round((count/sum(count))*100, 2))

df_fininf <- df_fininf %>% 
  mutate(fininf_att = ifelse(finance_inf == 1, "从不关注",
                             ifelse(finance_inf == 2, "很少关注", 
                                    ifelse(finance_inf == 3, "一般", 
                                           ifelse(finance_inf == 4, "很关注", "非常关注")))))


write.csv(df_fininf, "df_fininf.csv") #各省市对金融信息的关注度







# The condition of allocation  ===========================================


df_allo <- df %>% 
  mutate(pro_stock = round((stock_a/total_asset)*100, 2),
         pro_bond = round((bond_a/total_asset)*100, 2),
         pro_fund = round((fund_a/total_asset)*100, 2),
         pro_int_fin = round((inter_fin_a/total_asset)*100, 2),
         pro_fin_planning = round((fin_planning_a/total_asset)*100, 2),
         pro_derivate = round((der_a/total_asset)*100, 2),
         pro_golden = round((golden_a/total_asset)*100, 2),
         pro_nonrmb = round((nonrmb_a/total_asset)*100,2),
         pro_cash = round((cash/total_asset)*100,2),
         pro_current_dep = round((current_dep/total_asset)*100,2),
         pro_time_dep = round((time_dep/total_asset)*100,2),
         pro_house = round((house_asset/total_asset)*100,2)
  ) %>% 
  select(prov, pro_cash, pro_current_dep, pro_time_dep, pro_house, pro_stock, pro_bond, pro_fund, 
         pro_int_fin, pro_fin_planning, pro_derivate, pro_golden, pro_nonrmb)




asset_allo <- df_allo %>% 
  group_by(prov) %>% 
  summarize(m_cash = round(mean(pro_cash, na.rm = T), 2),
            m_current_dep = round(mean(pro_current_dep, na.rm = T), 2),
            m_time_dep = round(mean(pro_time_dep, na.rm = T), 2),
            m_stock = round(mean(pro_stock, na.rm = T), 2), 
            m_bond = round(mean(pro_bond, na.rm = T), 2),
            m_fund = round(mean(pro_fund, na.rm = T), 2),
            m_int_fin = round(mean(pro_int_fin, na.rm = T), 2),
            m_fin_planning = round(mean(pro_fin_planning, na.rm = T), 2),
            m_derivate = round(mean(pro_derivate, na.rm = T), 2),
            m_golden = round(mean(pro_golden, na.rm = T), 2),
            m_nonrmb = round(mean(pro_nonrmb, na.rm = T), 2),
            m_house = round(mean(pro_house, na.rm = T), 2)
  )

write.csv(asset_allo, "condition_allocation.csv") # 各省家庭资产配置情况


p1 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_cash), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[1]) + 
  labs(x = "省份", y = "现金占比(%)") + 
  scale_y_continuous(breaks = seq(0,2,0.25))

p2 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_current_dep), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[2]) + 
  labs(x = "省份", y = "活期存款占比(%)")

p3 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_time_dep), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[3]) + 
  labs(x = "省份", y = "定期存款占比(%)")

p4 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_stock), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[4]) + 
  labs(x = "省份", y = "股票占比(%)")

p5 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_bond), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[5]) + 
  labs(x = "省份", y = "债券占比(%)")

p6 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_fund), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[6]) + 
  labs(x = "省份", y = "基金占比(%)")

p7 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_int_fin), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[7]) + 
  labs(x = "省份", y = "互联网理财占比(%)")

p8 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_fin_planning), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[8]) + 
  labs(x = "省份", y = "金融理财占比(%)")

p9 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_derivate), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[9]) + 
  labs(x = "省份", y = "衍生品占比(%)")

p10 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_golden), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[10]) + 
  labs(x = "省份", y = "黄金占比(%)")

p11 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_nonrmb), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[11]) + 
  labs(x = "省份", y = "非人民币占比(%)")

p12 <- ggplot(data = asset_allo) + 
  geom_bar(mapping = aes(x = prov, y = m_house), stat = "identity",
           fill = brewer.pal(11,"RdYlGn")[1]) + 
  labs(x = "省份", y = "房屋资产占比(%)")


(( p1 | p2 | p3 ) / ( p4 | p5 | p6 ) / ( p7 | p8 | p9 ) / ( p10 | p11 | p12 ))

ggsave("condition_allocation.png")



# Table about the allocation


df_allo_gross <- df_gross %>% 
  mutate(pro_stock = round((stock_a/total_asset)*100, 2),
         pro_bond = round((bond_a/total_asset)*100, 2),
         pro_fund = round((fund_a/total_asset)*100, 2),
         pro_int_fin = round((inter_fin_a/total_asset)*100, 2),
         pro_fin_planning = round((fin_planning_a/total_asset)*100, 2),
         pro_derivate = round((der_a/total_asset)*100, 2),
         pro_golden = round((golden_a/total_asset)*100, 2),
         pro_nonrmb = round((nonrmb_a/total_asset)*100,2),
         pro_cash = round((cash/total_asset)*100,2),
         pro_current_dep = round((current_dep/total_asset)*100,2),
         pro_time_dep = round((time_dep/total_asset)*100,2),
         pro_house = round((house_asset/total_asset)*100,2)
  ) %>% 
  select(prov, pro_cash, pro_current_dep, pro_time_dep, pro_house, pro_stock, pro_bond, pro_fund, 
         pro_int_fin, pro_fin_planning, pro_derivate, pro_golden, pro_nonrmb)




asset_allo_gross <- df_allo_gross %>% 
  summarize(m_cash = round(mean(pro_cash, na.rm = T), 2),
            m_current_dep = round(mean(pro_current_dep, na.rm = T), 2),
            m_time_dep = round(mean(pro_time_dep, na.rm = T), 2),
            m_stock = round(mean(pro_stock, na.rm = T), 2), 
            m_bond = round(mean(pro_bond, na.rm = T), 2),
            m_fund = round(mean(pro_fund, na.rm = T), 2),
            m_int_fin = round(mean(pro_int_fin, na.rm = T), 2),
            m_fin_planning = round(mean(pro_fin_planning, na.rm = T), 2),
            m_derivate = round(mean(pro_derivate, na.rm = T), 2),
            m_golden = round(mean(pro_golden, na.rm = T), 2),
            m_nonrmb = round(mean(pro_nonrmb, na.rm = T), 2),
            m_house = round(mean(pro_house, na.rm = T), 2)
  )

asset_allo_gross <- cbind(prov = c("全国"),asset_allo_gross) 

asset_allo_table <- rbind(asset_allo_gross,asset_allo)

write.csv(asset_allo_table,"condition_allocation_g.csv") #长三角地区资产配置情况的table（包括全国平均数据）







#  Probit  =================================================  

attach(df)


probit_stock_sim <- glm(stock~finknow+age+age2+male+marriage+edu+health+work+t_income+
                          t_income2+net_asset+rural+insurance+house+ind_business,
                        data = df,family = binomial(link = "probit"))

probit_stock <- glm(stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                      t_income2+net_asset+rural+insurance+house+ind_business,
                    data = df,family = binomial(link = "probit"))

probit_fin_sim <- glm(fin_market~finknow+age+age2+male+marriage+edu+health+work+t_income+
                        t_income2+net_asset+rural+insurance+house+ind_business,
                      data = df,family = binomial(link = "probit"))

probit_fin <- glm(fin_market~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                    t_income2+net_asset+rural+insurance+house+ind_business,
                  data = df,family = binomial(link = "probit"))



ivprobit_stock <- ivreg(stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                          t_income2+net_asset+rural+insurance+house+ind_business | 
                          finknow+risk_aver+age+age2+male+marriage+edu_p+health+work+t_income+
                          t_income2+net_asset+rural+insurance+house+ind_business, data = df)


ivprobit_fin <- ivreg(fin_market~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                        t_income2+net_asset+rural+insurance+house+ind_business | 
                        finknow+risk_aver+age+age2+male+marriage+edu_p+health+work+t_income+
                        t_income2+net_asset+rural+insurance+house+ind_business, data = df)

PseudoR2(probit_stock)
PseudoR2(probit_fin)
PseudoR2(probit_stock_sim)
PseudoR2(probit_fin_sim)


#summary.ivreg is  diagnostics test for IV. Can see help documentation (ivreg)
#These encompass an F test of the first stage regression for weak instruments, 
#a Wu-Hausman test for endogeneity, and a Sargan test of overidentifying restrictions 
#(only if there are more instruments than regressors)
# 弱工具变量与DWH均为F test

summary(ivprobit_stock, diagnostics = T)                                         
summary(ivprobit_fin, diagnostics = T)



# 
#ivreg2(stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
#        t_income2+net_asset+rural+insurance+house+ind_business,
#       endog = "finknow", iv = "edu_p", data = as.data.frame(df))



#  Tobit  =====================================================


tobit_stock_sim <- tobit(pro_stock~finknow+age+age2+male+marriage+edu+health+work+t_income+
                           t_income2+net_asset+rural+insurance+house+ind_business,
                         data = df)

tobit_stock <- tobit(pro_stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                       t_income2+net_asset+rural+insurance+house+ind_business,
                     data = df)


tobit_fin_sim <- tobit(pro_fin_market~finknow+age+age2+male+marriage+edu+health+work+t_income+
                         t_income2+net_asset+rural+insurance+house+ind_business,
                       data = df)

tobit_fin <- tobit(pro_fin_market~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                     t_income2+net_asset+rural+insurance+house+ind_business,
                   data = df)


ivtobit_stock <- ivreg(pro_stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                         t_income2+net_asset+rural+insurance+house+ind_business | 
                         finknow+risk_aver+age+age2+male+marriage+edu_p+health+work+t_income+
                         t_income2+net_asset+rural+insurance+house+ind_business, data = df)


ivtobit_fin <- ivreg(pro_fin_market~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                       t_income2+net_asset+rural+insurance+house+ind_business | 
                       finknow+risk_aver+age+age2+male+marriage+edu_p+health+work+t_income+
                       t_income2+net_asset+rural+insurance+house+ind_business, data = df)



summary(ivtobit_fin, diagnostics = T)
summary(ivtobit_stock, diagnostics = T)



## About how to calculate pseudo R2 for tobit in R can be found
## https://stats.stackexchange.com/questions/196930/how-to-get-r-squared-goodness-of-fit-for-tobit-model-in-r
## https://community.rstudio.com/t/pseudo-r2-for-tobit-regression/68922


pseudoR2_tobit_stock <- 1 - as.vector(logLik(tobit_stock)/logLik(update(tobit_stock, . ~ 1)))
pseudoR2_tobit_fin <- 1 - as.vector(logLik(tobit_fin)/logLik(update(tobit_fin, . ~ 1)))
pseudoR2_tobit_stock_sim <- 1 - as.vector(logLik(tobit_stock_sim)/logLik(update(tobit_stock_sim, . ~ 1)))
pseudoR2_tobit_fin_sim <- 1 - as.vector(logLik(tobit_fin_sim)/logLik(update(tobit_fin_sim, . ~ 1)))




#  Robust  =====================================

## Without finance industry  (rofin <- robust finance)==============================

df_rofin <- df %>% 
  filter(industry != 9)

attach(df_rofin)



probit_stock_rofin <- glm(stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                            t_income2+net_asset+rural+insurance+house+ind_business,
                          data = df_rofin,family = binomial(link = "probit"))

probit_fin_rofin <- glm(fin_market~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                          t_income2+net_asset+rural+insurance+house+ind_business,
                        data = df_rofin,family = binomial(link = "probit"))


PseudoR2(probit_stock_rofin)
PseudoR2(probit_fin_rofin)



tobit_stock_rofin <- tobit(pro_stock~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                             t_income2+net_asset+rural+insurance+house+ind_business,
                           data = df_rofin)


tobit_fin_rofin <- tobit(pro_fin_market~finknow+risk_aver+age+age2+male+marriage+edu+health+work+t_income+
                           t_income2+net_asset+rural+insurance+house+ind_business,
                         data = df_rofin)

pseudoR2_tobit_stock_rofin <- 1 - as.vector(logLik(tobit_stock_rofin)/logLik(update(tobit_stock_rofin, . ~ 1)))
pseudoR2_tobit_fin_rofin <- 1 - as.vector(logLik(tobit_fin_rofin)/logLik(update(tobit_fin_rofin, . ~ 1)))



## Every problem ==============================================
### Problem about interest  =============================
attach(df)


probit_stock_ropint <- glm(stock~risk_aver+finknow_int+age+age2+male+marriage+edu+health+work+
                             t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                           data = df,family = binomial(link = "probit"))

probit_fin_ropint <- glm(fin_market~risk_aver+finknow_int+age+age2+male+marriage+edu+health+work+
                           t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                         data = df,family = binomial(link = "probit"))


PseudoR2(probit_stock_ropint)
PseudoR2(probit_fin_ropint)



tobit_stock_ropint <- tobit(pro_stock~risk_aver+finknow_int+age+age2+male+marriage+edu+health+work+
                              t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                            data = df)


tobit_fin_ropint <- tobit(pro_fin_market~risk_aver+finknow_int+age+age2+male+marriage+edu+health+work+
                            t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                          data = df)

pseudoR2_tobit_stock_ropint <- 1 - as.vector(logLik(tobit_stock_ropint)/logLik(update(tobit_stock_ropint, . ~ 1)))
pseudoR2_tobit_fin_ropint <- 1 - as.vector(logLik(tobit_fin_ropint)/logLik(update(tobit_fin_ropint, . ~ 1)))



###  Problem about inflation  ===================================


probit_stock_ropinf <- glm(stock~risk_aver+finknow_inf+age+age2+male+marriage+edu+health+work+
                             t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                           data = df,family = binomial(link = "probit"))

probit_fin_ropinf <- glm(fin_market~risk_aver+finknow_inf+age+age2+male+marriage+edu+health+work+
                           t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                         data = df,family = binomial(link = "probit"))


PseudoR2(probit_stock_ropinf)
PseudoR2(probit_fin_ropinf)



tobit_stock_ropinf <- tobit(pro_stock~risk_aver+finknow_inf+age+age2+male+marriage+edu+health+work+
                              t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                            data = df)


tobit_fin_ropinf <- tobit(pro_fin_market~risk_aver+finknow_inf+age+age2+male+marriage+edu+health+work+
                            t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                          data = df)

pseudoR2_tobit_stock_ropinf <- 1 - as.vector(logLik(tobit_stock_ropinf)/logLik(update(tobit_stock_ropinf, . ~ 1)))
pseudoR2_tobit_fin_ropinf <- 1 - as.vector(logLik(tobit_fin_ropinf)/logLik(update(tobit_fin_ropinf, . ~ 1)))



###  Problem about risk  ===================================


probit_stock_roprisk <- glm(stock~risk_aver+finknow_risk+age+age2+male+marriage+edu+health+work+
                              t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                            data = df,family = binomial(link = "probit"))

probit_fin_roprisk <- glm(fin_market~risk_aver+finknow_risk+age+age2+male+marriage+edu+health+work+
                            t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                          data = df,family = binomial(link = "probit"))


PseudoR2(probit_stock_roprisk)
PseudoR2(probit_fin_roprisk)



tobit_stock_roprisk <- tobit(pro_stock~risk_aver+finknow_risk+age+age2+male+marriage+edu+health+work+
                               t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                             data = df)


tobit_fin_roprisk <- tobit(pro_fin_market~risk_aver+finknow_risk+age+age2+male+marriage+edu+health+work+
                             t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                           data = df)

pseudoR2_tobit_stock_roprisk <- 1 - as.vector(logLik(tobit_stock_roprisk)/logLik(update(tobit_stock_roprisk, . ~ 1)))
pseudoR2_tobit_fin_roprisk <- 1 - as.vector(logLik(tobit_fin_roprisk)/logLik(update(tobit_fin_roprisk, . ~ 1)))



## Robust all finance knowledge problems ================

probit_stock_rofinknow_all <- glm(stock~risk_aver+finknow_int+finknow_inf+finknow_risk+age+age2+male+marriage+edu+health+work+
                                    t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                                  data = df,family = binomial(link = "probit"))


probit_fin_rofinknow_all <- glm(fin_market~risk_aver+finknow_int+finknow_inf+finknow_risk+age+age2+male+marriage+edu+health+work+
                                    t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                                  data = df,family = binomial(link = "probit"))



PseudoR2(probit_stock_rofinknow_all)
PseudoR2(probit_fin_rofinknow_all)




tobit_stock_rofinknow_all <- tobit(pro_stock~risk_aver+finknow_int+finknow_inf+finknow_risk+age+age2+male+marriage+edu+health+work+
                                     t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                             data = df)


tobit_fin_rofinknow_all <- tobit(pro_fin_market~risk_aver+finknow_int+finknow_inf+finknow_risk+age+age2+male+marriage+edu+health+work+
                             t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                           data = df)

pseudoR2_tobit_stock_roprisk <- 1 - as.vector(logLik(tobit_stock_rofinknow_all)/logLik(update(tobit_stock_rofinknow_all, . ~ 1)))
pseudoR2_tobit_fin_roprisk <- 1 - as.vector(logLik(tobit_fin_rofinknow_all)/logLik(update(tobit_fin_rofinknow_all, . ~ 1)))




##  finance information  ===================================


probit_stock_rofininf <- glm(stock~risk_aver+finance_inf+age+age2+male+marriage+edu+health+work+
                               t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                             data = df,family = binomial(link = "probit"))

probit_fin_rofininf <- glm(fin_market~risk_aver+finance_inf+age+age2+male+marriage+edu+health+work+
                             t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                           data = df,family = binomial(link = "probit"))


PseudoR2(probit_stock_rofininf)
PseudoR2(probit_fin_rofininf)



tobit_stock_rofininf <- tobit(pro_stock~risk_aver+finance_inf+age+age2+male+marriage+edu+health+work+
                                t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                              data = df)


tobit_fin_rofininf <- tobit(pro_fin_market~risk_aver+finance_inf+age+age2+male+marriage+edu+health+work+
                              t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                            data = df)

pseudoR2_tobit_stock_rofininf <- 1 - as.vector(logLik(tobit_stock_rofininf)/logLik(update(tobit_stock_rofininf, . ~ 1)))
pseudoR2_tobit_fin_rofininf <- 1 - as.vector(logLik(tobit_fin_rofininf)/logLik(update(tobit_fin_rofininf, . ~ 1)))



# 定义主观与客观金融素养

probit_stock_rofininf_a <- glm(stock~risk_aver+finance_inf+finknow+age+age2+male+marriage+edu+health+work+
                                 t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                               data = df,family = binomial(link = "probit"))

probit_fin_rofininf_a <- glm(fin_market~risk_aver+finance_inf+finknow+age+age2+male+marriage+edu+health+work+
                               t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                             data = df,family = binomial(link = "probit"))


PseudoR2(probit_stock_rofininf_a)
PseudoR2(probit_fin_rofininf_a)



tobit_stock_rofininf_a <- tobit(pro_stock~risk_aver+finance_inf+finknow+age+age2+male+marriage+edu+health+work+
                                  t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                                data = df)


tobit_fin_rofininf_a <- tobit(pro_fin_market~risk_aver+finance_inf+finknow+age+age2+male+marriage+edu+health+work+
                                t_income+t_income2+net_asset+rural+insurance+house+ind_business,
                              data = df)

pseudoR2_tobit_stock_rofininf_a <- 1 - as.vector(logLik(tobit_stock_rofininf_a)/logLik(update(tobit_stock_rofininf_a, . ~ 1)))
pseudoR2_tobit_fin_rofininf_a <- 1 - as.vector(logLik(tobit_fin_rofininf_a)/logLik(update(tobit_fin_rofininf_a, . ~ 1)))






#  Gather the result of all regression  ====================================

##  probit  ====================

stargazer(probit_stock_sim ,probit_stock,ivprobit_stock,probit_fin_sim,probit_fin,ivprobit_fin,
          title = "金融素养、风险厌恶水平对股票市场参与和金融市场参与的影响",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场"),
          covariate.labels = c("金融素养","风险厌恶水平","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "Probitb.html")


stargazer(probit_stock_sim ,probit_stock,ivprobit_stock,probit_fin_sim,probit_fin,ivprobit_fin,
          title = "金融素养、风险厌恶水平对股票市场参与和金融市场参与的影响",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场"),
          covariate.labels = c("金融素养","风险厌恶水平","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "Probitb.doc")


##  tobit  =======================

stargazer(tobit_stock_sim,tobit_stock,ivtobit_stock,tobit_fin_sim,tobit_fin,ivtobit_fin,
          title = "金融素养、风险厌恶水平对家庭资产选择影响",align = T,
          dep.var.labels = c("股票资产占比","风险资产占比"),
          covariate.labels = c("金融素养","风险厌恶水平","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "Tobitb.html")


stargazer(tobit_stock_sim,tobit_stock,ivtobit_stock,tobit_fin_sim,tobit_fin,ivtobit_fin,
          title = "金融素养、风险厌恶水平对家庭资产选择影响",align = T,
          dep.var.labels = c("股票资产占比","风险资产占比"),
          covariate.labels = c("金融素养","风险厌恶水平","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "Tobitb.doc")


##  Without finance industry  (rofin <- robust finance)==============================

stargazer(probit_stock_rofin,probit_fin_rofin,tobit_stock_rofin,tobit_fin_rofin,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(去除有金融从业人员的家庭)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","股票资产占比","风险资产占比"),
          covariate.labels =c("金融素养","风险厌恶水平","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                              "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(去除有金融从业人员的家庭)b.html")

stargazer(probit_stock_rofin,probit_fin_rofin,tobit_stock_rofin,tobit_fin_rofin,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(去除有金融从业人员的家庭)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","股票资产占比","风险资产占比"),
          covariate.labels = c("金融素养","风险厌恶水平","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(去除有金融从业人员的家庭)b.doc")




##  finance information  ===================================


stargazer(probit_stock_rofininf,probit_fin_rofininf,probit_stock_rofininf_a,probit_fin_rofininf_a,
          tobit_stock_rofininf,tobit_fin_rofininf,tobit_stock_rofininf_a,tobit_fin_rofininf_a,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(对经济、金融信息的关注程度)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","参与股票市场","参与金融市场",
                             "股票资产占比","风险资产占比","股票资产占比","风险资产占比"),
          covariate.labels = c("风险厌恶水平","对经济、金融信息的关注程度","金融素养","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(对经济、金融信息的关注程度)b.html")


stargazer(probit_stock_rofininf,probit_fin_rofininf,probit_stock_rofininf_a,probit_fin_rofininf_a,
          tobit_stock_rofininf,tobit_fin_rofininf,tobit_stock_rofininf_a,tobit_fin_rofininf_a,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(对经济、金融信息的关注程度)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","参与股票市场","参与金融市场",
                             "股票资产占比","风险资产占比","股票资产占比","风险资产占比"),
          covariate.labels = c("风险厌恶水平","对经济、金融信息的关注程度","金融素养","年龄", "年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(对经济、金融信息的关注程度)b.doc")


##  problem  =============================================


stargazer(probit_stock_ropint,probit_fin_ropint,tobit_stock_ropint,tobit_fin_ropint,
          probit_stock_ropinf,probit_fin_ropinf,tobit_stock_ropinf,tobit_fin_ropinf,
          probit_stock_roprisk,probit_fin_roprisk,tobit_stock_roprisk,tobit_fin_roprisk,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(利率、通货膨胀以及风险问题)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","股票资产占比","风险资产占比",
                             "参与股票市场","参与金融市场","股票资产占比","风险资产占比",
                             "参与股票市场","参与金融市场","股票资产占比","风险资产占比"),
          covariate.labels = c("风险厌恶水平","利率问题回答正确", "通货膨胀问题回答正确", "风险问题回答正确","年龄","年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(利率、通货膨胀以及风险问题)b.html")


stargazer(probit_stock_ropint,probit_fin_ropint,tobit_stock_ropint,tobit_fin_ropint,
          probit_stock_ropinf,probit_fin_ropinf,tobit_stock_ropinf,tobit_fin_ropinf,
          probit_stock_roprisk,probit_fin_roprisk,tobit_stock_roprisk,tobit_fin_roprisk,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(利率、通货膨胀以及风险问题)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","股票资产占比","风险资产占比",
                             "参与股票市场","参与金融市场","股票资产占比","风险资产占比",
                             "参与股票市场","参与金融市场","股票资产占比","风险资产占比"),
          covariate.labels = c("风险厌恶水平","利率问题回答正确", "通货膨胀问题回答正确", "风险问题回答正确","年龄","年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(利率、通货膨胀以及风险问题)b.doc")



stargazer(probit_stock_rofinknow_all,probit_fin_rofinknow_all,tobit_stock_rofinknow_all,tobit_fin_rofinknow_all,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(利率、通货膨胀以及风险问题)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","股票资产占比","风险资产占比"),
          covariate.labels = c("风险厌恶水平","利率问题回答正确", "通货膨胀问题回答正确", "风险问题回答正确","年龄","年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(利率、通货膨胀以及风险问题b).html")




stargazer(probit_stock_rofinknow_all,probit_fin_rofinknow_all,tobit_stock_rofinknow_all,tobit_fin_rofinknow_all,
          title = "金融素养、风险厌恶水平对家庭资产选择影响(利率、通货膨胀以及风险问题)",align = T,
          dep.var.labels = c("参与股票市场","参与金融市场","股票资产占比","风险资产占比"),
          covariate.labels = c("风险厌恶水平","利率问题回答正确", "通货膨胀问题回答正确", "风险问题回答正确","年龄","年龄平方","男性","已婚","受教育水平","健康状况",
                               "有工作","总收入(Ln)","总收入平方(Ln)","净资产(Ln)","农村","保险","房子", "个体工商业", "常数项"),
          omit.stat = c("LL","aic","ser"), no.space = T, type = "html", out = "稳健性检验(利率、通货膨胀以及风险问题b).doc")

## Stastics  ===============================

df_s <- df %>% 
  select(risk_aver, finknow, stock, fin_market, pro_stock, pro_fin_market, male, age, marriage,edu, health, work, total_income, 
         nasset, house, insurance, ind_business, rural)


df_s$total_income <- round((df_s$total_income / 10000), 2)
df_s$nasset <- round((df_s$nasset / 10000), 2)
df_s <- as.data.frame(df_s)   #描述性统计只能识别data.frame，为转化之前可能时tibble


stargazer(df_s,align = T,
          covariate.labels = c("风险厌恶水平","金融素养","参与股票市场","参与金融市场","股票资产占比",
                               "风险资产占比","男性","年龄","已婚","受教育水平","健康状况","是否工作","总收入",
                               "净资产","是否自有住房","保险","个体工商业","农村"),
          summary.stat = c("n","mean","sd","min","max"),digits = 3,type = "html",out = "statiticsb.html")

stargazer(df_s,align = T,
          covariate.labels = c("风险厌恶水平","金融知识","参与股票市场","参与金融市场","股票资产占比",
                               "风险资产占比","男性","年龄","已婚","受教育水平","健康状况","是否工作","总收入",
                               "净资产","是否自有住房","保险","个体工商业","农村"),
          summary.stat = c("n","mean","sd","min","max"),digits = 3,type = "html",out = "statiticsb.doc")



# Additional test or analysis =========================

# 答对三题的人中不同年龄段的差异

df_age_allright <- df %>% 
  mutate(age_t = ifelse(age<30, "<30", ifelse(age<50, "30~50", ifelse(age<65, "50~65", ">65")))) %>% 
  filter(finknow == 3) %>% 
  group_by(prov , age_t) %>% 
  summarize(count = n()) %>% 
  mutate(pro_test = round((count/sum(count))*100, 2))



df_age_allright$age_t <- factor(df_age_allright$age_t, levels = c("<30","30~50","50~65",">65"))


p_age_allright<- ggplot(data = df_age_allright) + 
  geom_bar(mapping = aes(x = prov, y = pro_test, fill = age_t), stat = "identity",
           position = "dodge") + 
  labs(x = "省份", y = "各年龄段占比(%)", fill = "年龄段") + 
  scale_fill_tableau() + 
  scale_y_continuous(breaks = seq(0,60,10))

#ggsave("age gap between knowledge all right.png")




# 答对三题的人中不同性别的差异

df_knowledge_allright <- df %>% 
  filter(finknow == 3) %>% 
  mutate(gender = ifelse(male == 1, "Male", "Female")) %>% 
  group_by(prov, gender) %>% 
  summarize(count = n()) %>% 
  mutate(prop = round((count/sum(count))*100, 2))


p_knowledge_all<- ggplot(data = df_knowledge_allright) + 
  geom_bar(mapping = aes(x = prov, y = prop, fill = gender), 
           stat = "identity", position = "dodge") + 
  labs(x = "省份", y = "男女比重(%)", fill = "Gender") + 
  geom_text(aes(label = prop, x = prov ,y = prop + 1, group = gender, fontface = "italic"), 
            position = position_dodge(0.9), vjust = 0)

ggsave("gender gap between konwledge all right.png")



# 答对三题的女性中已婚与未婚的差异


df_marriage_allright <- df %>% 
  filter(finknow == 3, male == 0) %>% 
  mutate(marri = ifelse(marriage == 0, "Single", "Marriage")) %>% 
  group_by(prov, marri) %>% 
  summarize(count = n()) %>% 
  mutate(prop = round((count/sum(count))*100, 2))


p_marriage_allright<- ggplot(data = df_test) + 
  geom_bar(mapping = aes(x = prov, y = prop, fill = marri),
           stat = "identity", position = "dodge",width = 0.8) + 
  labs(x = "省份", y = "已婚与未婚女性占比(%)", fill = "Marital Status") + 
  geom_text(aes(label = prop, x = prov, y = prop + 1, group = marri, fontface = "italic"),
            position = position_dodge(0.9), vjust = 0, hjust = 0.5)


ggsave("marital status gap between knowledge all right.png")




# 回答不知道的人的性别差异（不考虑放此图）
df_a2 <- df_a %>% 
  mutate(finknow_int_dk = ifelse(finknow_int_a == 2, 1, 0),
         finknow_inf_dk = ifelse(finknow_inf_a == 2, 1, 0),
         finknow_risk_dk = ifelse(finknow_risk_a == 2, 1, 0),
         finknow_dk = ifelse((finknow_inf_dk+finknow_int_dk+finknow_risk_dk)>0 , 1, 0),
         gender = ifelse(male == 1, "Male", "Female")) %>% 
  filter(finknow_dk == 1) %>% 
  group_by(prov, gender) %>% 
  summarize(count = n()) %>%
  mutate(prop = round((count/sum(count))*100, 2))



p_knowledge_dk<- ggplot(data = df_a2) + 
  geom_bar(mapping = aes(x = prov, y = prop, fill = gender), 
           stat = "identity", position = "dodge") + 
  labs(x = "省份", y = "男女比重(%)") + 
  geom_text(aes(label = prop, x = prov ,y = prop + 1, group = gender, fontface = "italic"), 
            position = position_dodge(0.9), vjust = 0)



# logistf package  ========================

df_test <- df %>% mutate(income = total_income/10000)





attach(df)


probit_stock_test <- glm(stock~risk_aver+finknow+age+male+marriage+edu+work+income
                         + net_asset+rural+insurance+house+ind_business, data = df_test,
                         family = binomial(link = "probit"))


summary(probit_stock_test)


1-as.vector(probit_stock_test$loglik/(update(probit_stock_test, . ~ 1))$loglik)[1]





probit_stock_test <- glm(stock~risk_aver+finknow+age+male+marriage+edu+work+
                           t_income+net_asset+rural+house+ind_business,
                         data = df,family = binomial(link = "probit"))


PseudoR2(probit_stock_test)



master <- master %>% mutate(net_asset = total_asset-total_debt)








df_test <- df %>% 
  group_by(total_income) %>% 
  summarize(count = n())
write.csv(df_test, "df_test.csv")




myvar <- c("pro_stock", "pro_fin_market")
stat.desc(df[myvar], basic = F, norm = T, p=0.95)


hist(df$pro_fin_market)
lines(density(df$pro_stock))


plot(density(df$pro_fin_market))



