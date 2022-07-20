---
title: "U.S. Stocks vs. Cryptocurrencies"
subtitle: "RiskAndReturn--Finance"
author: "Cheryl Isabella"
output:
  html_document:
    toc: yes
    toc_float: yes
    toc_depth: 2
    theme: paper
    df_print: paged
  word_document:
    toc: yes
    toc_depth: '2'
---
# **Overview**
This project aims to evaluate the Risk and Return for U.S. stocks and cryptocurrencies. 

# **Data**

The excel file “datacase1.xls” contains daily prices (end of the day) for a variety of US stocks and stock indices. The stock price data are end of the day closing prices. Daily prices of some cryptocurrencies are included in a second data file (Datacase2.xls) and started to trade much later (source: finance:yahoo:com). Assume a risk-free rate (annualized) equal to 3%. The data file contains the following series:

- US stock indices: Dow, S&P500, NASDAQ, Russel 2000.
- Individual US stocks: General Electric, Bank of America, Coca Cola, Intel, Apple inc,
Mc Donalds Corp, Procter and Gamble, American Airlines, Caterpillar, Wallmart.4
- Cryptocurrencies: Bitcoin, Ethereum, Ripple.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = F, message = F)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
```

## Importing and Cleaning the Data
```{r data, echo = FALSE}
#import
df1 <- read_xlsx("C:/Users/Isabella/Documents/UM/Year 1/Finance EBC1048/Projects/Data_case_1.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
df2 <- read_xlsx("C:/Users/Isabella/Documents/UM/Year 1/Finance EBC1048/Projects/Data_case_2.xlsx", col_types = c("date", "numeric", "date", "numeric", "numeric", "numeric"))


#stock dataset (removing empty row, renaming column, rearranging data)
df1_2 <- df1[-1,]
df1_3 <- df1_2 %>%
  rename(date = Days) %>%
  pivot_longer(2:15, names_to = "name", values_to = "price") %>%
  na.omit()

#cryptocurrency dataset (removing empty row, renaming columns, removing duplicate data)
df2_2 <- df2[-1,]
df2_3 <- df2_2 %>% 
  rename(date = Days...3,
         bitcoin = Cryptocurrencies,
         etherium = ...5,
         ripple = ...6) %>%
  select(-Days...1, -SP_500) %>%
  pivot_longer(2:4, names_to = "name", values_to = "price") %>%
  na.omit()

#joining the datasets
df_sc <- rbind(df1_3, df2_3) %>%
  arrange(date)

#show clean datasets
df_sc
```

## a) Creating new Columns
```{r measures}
#creating the daily returns
df_sc_return <- df_sc %>%
  group_by(name) %>%
  mutate(daily_return = log(lead(price)/price)) %>%
  na.omit 

#creating other variables per stock
measures <- df_sc_return %>%
  group_by(name) %>%
  summarise(avg_annual_return = mean(daily_return) * 252,
            annual_risk = sd(daily_return) * sqrt(252),
            min_return = min(daily_return),
            max_return = max(daily_return),
            sharpe_ratio = (avg_annual_return-0.03)/annual_risk) %>%
  mutate(type = ifelse(name %in% c("bitcoin", "etherium", "ripple"),"crypto", ifelse(name %in% c("Russel_2000", "SP_500", "nasdaq", "Dow"), "index" , "stock")))

measures
```


# **Risk and Return**

# b) Riskiness
```{r annual risk}
measures %>%
  group_by(type) %>%
  summarise(Avg_Annual_Risk = mean(annual_risk)) %>%
  arrange(desc(Avg_Annual_Risk))
```

**The relative riskiness of the stocks, stock indices and the cryptos based on the
annualized standard deviation: **

- We can evaluate the riskiness of an investment based on its standard deviation, also known as volatility. A higher volatility means that the return deviates more from the average return (distribution is more spread out). Hence, the investment is riskier.

- The average annual volatility of the cryptocurrencies in our data is about 145%. Since this is the highest value, investments in cryptocurrencies are a lot riskier than investments in stocks or indices. Individual stocks have an average annual volatility of 36% and indices a volatility of 21%. Since indices have the lowest average volatility, they bear the least risk. Very interesting to observe, is the high difference (of more than 100%) between the riskiness of stocks/indices and cryptocurrencies.

# c) Sharpe Ratios
```{r sharpe ratios}
measures %>%
  group_by(type) %>%
  summarise(Avg_Sharpe_Ratio = mean(sharpe_ratio)) %>%
  arrange(desc(Avg_Sharpe_Ratio))
```

**Comparison of the Sharpe ratios of the portfolios vs. the Sharpe ratios of the individual
US stocks, and the Sharpe ratios of the cryptos relative to the other investments: **

- The Sharpe ratio measures the ratio of reward to volatility and is used to evaluate the return of an investment compared to its risk. The higher the ratio, the higher the reward per unit of volatility.

- The portfolios/indices have, in comparison, a higher Sharpe ratio than the individual US stocks. Hence, the portfolios would be the optimal investment to combine with the risk-free investment.

- The average Sharpe ratio of the cryptocurrencies is a lot higher than the average ratio of the indices and stocks.  As we have seen before, investments in cryptocurrencies are riskier. However, Investors are compensated for taking on this additional risk. Still, the Sharpe ratio of cryptocurrencies indicates that the risk premium per unit  of volatility is higher than for stocks and indices.

# d) Investment Decision
```{r sharpe ratio decision}
measures %>%
  select(type, name, sharpe_ratio) %>%
  arrange(desc(sharpe_ratio))
```

**Result:**

- We would invest in the cryptocurrency Ethereum since it has the highest Sharpe ratio. This means this investment would give the highest return per unit of volatility.

# e) Stock Indices
```{r indices}
#filtering for stock indices
indices <- filter(measures, type == "index")

#plot
ggplot(indices, aes(annual_risk, avg_annual_return)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Stock Indices: Risk vs. Return", x = "Volatility (sd)", y = "Average Annual Return")
```

**Results:**

- An investment with higher volatility must offer a higher return to compensate investors for the additional risk. The excess return gives the risk premium investors get.

- On average, this corresponds with our data. The higher volatility/risk, the higher the average annual return. The only exception is the index SP 500. Its volatility is slightly higher than the volatility of the Dow index; still, the average return is lower.

# f) Individual Stocks
```{r stocks}
#filtering for individual stocks
stocks <- filter(measures, type == "stock")

#plot
ggplot(stocks, aes(annual_risk, avg_annual_return)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Individual Stocks: Risk vs. Return", x = "Volatility (sd)", y = "Average Annual Return")
```

**Results:**

- The assumption that a more volatile investment should have a higher return does not hold for the individual stocks. The plot shows no clear relationship between volatility and return.

- We can also observe that the volatility for individual stocks is higher than for the indices. In general, individual stocks are typically more volatile than indices/portfolios. Furthermore, larger stocks have lower volatility overall.

# g) 2-Stock Portfolio
- Caterpillar (A)
- Walmart (B)
```{r portfolio}
#weights
A_w <- c(rep(1:99)/100)
B_w <- c(rep(99:1)/100)
weights <- as.data.frame(cbind(A_w, B_w))

#Return and Risk of both stocks
A_return <- as.numeric(measures %>% filter(name == "Caterpillar") %>% select(avg_annual_return))
B_return <- as.numeric(measures %>% filter(name == "Walmart") %>% select(avg_annual_return))
A_risk <- as.numeric(measures %>% filter(name == "Caterpillar") %>% select(annual_risk))
B_risk <- as.numeric(measures %>% filter(name == "Walmart") %>% select(annual_risk))

#correlation between returns
A_r <- df_sc_return %>% filter(name == "Caterpillar")
B_r <- df_sc_return %>% filter(name == "Walmart") 
A_B_r <- cbind(A_r[,4], B_r[,4])

cor(A_B_r)

#dataframe
portfolio <- mutate(weights, Return = A_w*A_return + B_w*B_return,
                    Volatility = sqrt(A_w^2*A_risk^2 + B_w^2*B_risk^2 + 2*A_w*B_w*0.3052991*A_risk*B_risk))
portfolio

#plot
ggplot(portfolio, aes(Volatility, Return)) +
  geom_point(size = 0.5, color = "blue") +
  ggtitle("Efficient Frontier: 2-Stock Portfolio")
```

**Results:**

- The efficiency frontier is the set of optimal portfolios with the highest possible return for a given level of volatility. Portfolios below the efficiency frontier are called inefficient portfolios because there is another portfolio that is better in terms of return and volatility. The efficient portfolio is also the portfolio with the highest Sharpe ratio.

- To answer the question, the portfolio with the lowest risk, which is still an efficient frontier, is the one furthest on the left on the graph: 35% Caterpillar and 65% Walmart

# h) Portfolios

Correlation matrix between returns:
```{r, portfolios, echo=FALSE}
#Correlation matrix between returns
a_r <- df_sc_return %>% filter(name == "General_electric") %>% rename(General_electric = daily_return)
b_r <- df_sc_return %>% filter(name == "Walmart") %>% rename(Walmart = daily_return)
c_r <- df_sc_return %>% filter(name == "Coke") %>% rename(Coke = daily_return)
d_r <- df_sc_return %>% filter(name == "ProcterGamble") %>% rename(ProcterGamble = daily_return)
e_r <- df_sc_return %>% filter(name == "Caterpillar") %>% rename(Caterpillar = daily_return)
f_r <- df_sc_return %>% filter(name == "Mc_Donalds") %>% rename(Mc_Donalds = daily_return)
A_B_r <- cbind(a_r[,4], b_r[,4], c_r[,4], d_r[,4], e_r[,4], f_r[,4])
c_coef <- cor(A_B_r)
c_coef
```

Dataframe of portfolio metrics:
```{r, echo=FALSE}
#Average correlation per portfolio
C_2 <- c_coef[2]
C_3 <- (c_coef[2] + c_coef[3] + c_coef[9]) / 3
C_4 <- (c_coef[2] + c_coef[3] + c_coef[4] + c_coef[9] + c_coef[10] + c_coef[16]) / 6
C_5 <- (c_coef[2] + c_coef[3] + c_coef[4] + c_coef[5] + c_coef[9] + c_coef[10] + c_coef[11] + c_coef[16] + c_coef[17] + c_coef[23]) / 10
C_6 <- (c_coef[2] + c_coef[3] + c_coef[4] + c_coef[5] + c_coef[6] + c_coef[9] + c_coef[10] + c_coef[11] + c_coef[12] + c_coef[16] + c_coef[17] + c_coef[18] + c_coef[23] + c_coef[24]) / 14


#Risk for each individual stock
a_risk <- as.numeric(measures %>% filter(name == "General_electric") %>% select(annual_risk))
b_risk <- as.numeric(measures %>% filter(name == "Walmart") %>% select(annual_risk))
c_risk <- as.numeric(measures %>% filter(name == "Coke") %>% select(annual_risk))
d_risk <- as.numeric(measures %>% filter(name == "ProcterGamble") %>% select(annual_risk))
e_risk <- as.numeric(measures %>% filter(name == "Caterpillar") %>% select(annual_risk))
f_risk <- as.numeric(measures %>% filter(name == "Mc_Donalds") %>% select(annual_risk))

#Average risk per portfolio
R_2 <- (a_risk + b_risk) / 2
R_3 <- (a_risk + b_risk + c_risk) / 3
R_4 <- (a_risk + b_risk + c_risk + d_risk) / 4
R_5 <- (a_risk + b_risk + c_risk + d_risk + e_risk) / 5
R_6 <- (a_risk + b_risk + c_risk + d_risk + e_risk + f_risk) / 6

#dataframe
n <- c(rep(2:6))
volatility <- c(R_2, R_3, R_4, R_5, R_6)
correlation <- c(C_2, C_3, C_4, C_5, C_6)
n_col_corr <- as.data.frame(cbind(n, volatility, correlation))
portfolios <- mutate(n_col_corr, portfolio_volatility = sqrt((1/n)*volatility^2 + (1-1/n)*(correlation*volatility^2)))
portfolios

```

Plot of Equally Weighted Portfolio: Volatility vs. Number of Stocks
```{r}
#plot
ggplot(portfolios, aes(n, portfolio_volatility)) +
  geom_point(size = 2, color = "blue") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = F) +
  geom_hline(yintercept = 0.173, linetype = "dotted") +
  labs(title = "Equally Weighted Portfolio: Volatility vs. Number of Stocks", x = "Number of Stocks") +
  scale_y_continuous("Portfolio Volatility", limits = c(0.15,0.26))
```


**Results:**

- The graph shows a relationship between the volatility of the portfolios and the number of stocks. The more stocks are added to the portfolio, the lower the volatility gets. Furthermore, the relationship flattens out. The effect of adding one stock when there are two stocks in the portfolio is higher than when there are already five stocks in the portfolio. By adding more stocks to the portfolio, we eliminate the diversifiable risk. At one point, the volatility cannot get lower since the correlated market risk/common risk cannot be diversified.

- The curvature in the graph is due to the correlation between the stocks. The first portfolio contains the stocks of General Electric and Walmart. The correlation matrix shows that they have a relatively high correlation coefficient of 0.4, which gives the correlated market risk. The correlated market risk is displayed by the area underneath the curve, which cannot be eliminated.




