library(tidyverse)
library(lubridate)
library(tidyquant)
library(svglite)
#----STOCK DATA

stock = as.data.frame(tq_get("^GSPC", get = "stock.prices", from = "1982-04-23"))
head(stock)

#----CHECK FOR PROBLEMS

stock = stock[!duplicated(stock$date),]
sum(is.na(stock))

#----TURN INTO SEPEREAT DATA FRAMES BASED ON TIME

  #DAILY RETURN#

#daily returns
day = merge(stock, 
            stock %>% tq_transmute(select = adjusted, 
                                   mutate_fun = periodReturn, 
                                   period = "daily", 
                                   col_rename = "day_ror"), by = "date")

#removes first value because its an NA
day = day[-1,]

  #WEEKLY RETURN#

#weekly returns
week = merge(stock, 
             stock %>% tq_transmute(select = adjusted, 
                                    mutate_fun = periodReturn, 
                                    period = "weekly", 
                                    col_rename = "week_ror"), by = "date")

#removes first and last week because they maybe incomplete
week = week[-c(1,nrow(week)),]

  #MONTHLY RETURNS

#Monthly returns
month = merge(stock, 
              stock %>% tq_transmute(select = adjusted, 
                                     mutate_fun = periodReturn, 
                                     period = "monthly", 
                                     col_rename = "month_ror"), by = "date")

#removes first and last rows because they are incomplete months
month = month[-c(1,nrow(month)),]

  #QUARTERLY RETURNS

#Quarterly returns
quarter = merge(stock, 
                stock %>% tq_transmute(select = adjusted, 
                                       mutate_fun = periodReturn, 
                                       period = "quarterly", 
                                       col_rename = "quarter_ror"), by = "date")

#removes first and last rows because they maybe incomplete
quarter = quarter[-c(1,nrow(quarter)),]

  #YEARLY RETURNS

#Yearly returns
year = merge(stock, 
             stock %>% tq_transmute(select = adjusted, 
                                    mutate_fun = periodReturn, 
                                    period = "yearly", 
                                    col_rename = "year_ror"), by = "date")

#removes first and last rows because they are incomplete years
year = year[-c(1,nrow(year)),]


#----SAVE AS SVG

res <- 144

#day
svglite("../src/daily_return.svg", width = 1080/res, height = 720/res)

hist(day$day_ror, 
     breaks= "Scott", 
     main = "Distribution of Daily Stock Returns", 
     xlab = "Daily Returns",
     col = rgb(1,0,0,1/4))

hist(rnorm(nrow(day), mean(day$day_ror), sd(day$day_ror)),
     breaks = "Scott",
     add=TRUE, 
     col=rgb(0,0,1,1/4))

legend("topleft", legend=c("Actual", "Predicted"),
       inset=.02,
       col=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),
       fill=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),cex=0.8)

dev.off()

#week
svglite("../src/week_return.svg", width = 1080/res, height = 720/res)

hist(week$week_ror, 
     breaks = "Scott",
     main = "Distribution of Weekly Stock Returns", 
     xlab = "Weekly Returns",
     col = rgb(1,0,0,1/4))

hist(rnorm(nrow(week), mean(week$week_ror), sd(week$week_ror)),
     breaks = "Scott",
     add=TRUE, 
     col=rgb(0,0,1,1/4))

legend("topleft", legend=c("Actual", "Predicted"),
       inset=.02,
       col=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),
       fill=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),cex=0.8)

dev.off()

#Month
svglite("../src/month_return.svg", width = 1080/res, height = 720/res)

hist(month$month_ror, 
     breaks = "Scott",
     main = "Distribution of Monthly Stock Returns", 
     xlab = "Monthly Returns",
     col = rgb(1,0,0,1/4))

hist(rnorm(nrow(month),mean(month$month_ror),sd(month$month_ror)),
     breaks = "Scott",
     add=TRUE,
     col=rgb(0,0,1,1/4))

legend("topleft", legend=c("Actual", "Predicted"),
       inset=.02,
       col=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),
       fill=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),cex=0.8)

dev.off()

#Quarter
svglite("../src/quarter_return.svg", width = 1080/res, height = 720/res)

hist(quarter$quarter_ror, 
     breaks = "Scott",
     main = "Distribution of Quarterly Stock Returns", 
     xlab = "Quarterly Returns",
     col = rgb(1,0,0,1/4))

hist(rnorm(nrow(month),mean(month$month_ror),sd(month$month_ror)),
     breaks = "Scott",
     add=TRUE,
     col=rgb(0,0,1,1/4))

legend("topleft", legend=c("Actual", "Predicted"),
       inset=.02,
       col=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),
       fill=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),cex=0.8)

dev.off()

#Year
svglite("../src/year_return.svg", width = 1080/res, height = 720/res)

hist(year$year_ror, 
     breaks = "Scott",
     main = "Distribution of Yearly Stock Returns", 
     xlab = "Yearly Returns", 
     col = rgb(1,0,0,1/4))

hist(rnorm(nrow(year),mean(year$year_ror),sd(year$year_ror)),
     breaks = "Scott",
     add=TRUE, 
     col=rgb(0,0,1,1/4))

legend("topleft", legend=c("Actual", "Predicted"),
       inset=.02,
       col=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),
       fill=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)),cex=0.8)

dev.off()

#----PREDICT VALUES IF THEY HAVE A CONSTANT RETURN BASED ON THE MEAN

  #DAILY

n_day = nrow(day)

pred_day = vector(mode = "double", length = n_day)

for(i in 1:n_day){
  pred_day[i] = day$adjusted[1]*(1+mean(day$day_ror))^(i-1)
}

  #WEEKLY

n_week = nrow(week)

pred_week = vector(mode = "double", length = n_week)

for(i in 1:n_week){
  pred_week[i] = week$adjusted[1]*(1+mean(week$week_ror))^(i-1)
}

  #MONTHLY

n_mon = nrow(month)

pred_mon = vector(mode = "double", length = n_mon)

for(i in 1:n_mon){
  pred_mon[i] = month$adjusted[1]*(1+mean(month$month_ror))^(i-1)
}

  #QUARTERLY

n_quar = nrow(quarter)

pred_quar = vector(mode = "double", length = n_quar)

for(i in 1:n_quar){
  pred_quar[i] = quarter$adjusted[1]*(1+mean(quarter$quarter_ror))^(i-1)
}

  #YEARLY

n_year = nrow(year)

pred_year = vector(mode = "double", length = n_year)

for(i in 1:n_year){
  pred_year[i] = year$adjusted[1]*(1+mean(year$year_ror))^(i-1)
}

#----SAVE PLOTS OF PREDICTED VALUES

colors = c("Adjusted" = "black", "Predicted" = "orange")
nam = c("date","adjusted","predicted")

  #DAILY

df_day = cbind.data.frame(day$date,day$adjusted,pred_day)
colnames(df_day) = nam

days = ggplot(data = df_day, aes(x = date))+
  geom_line(aes(y = adjusted, color = "Adjusted"))+
  geom_line(aes(y = predicted, color = "Predicted"))+
  ggtitle("S&P 500 Daily Price from 1982-2022")+
  xlab("")+
  ylab("Price")+
  labs(color = "")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = colors)+
  scale_x_date(date_labels = "%Y")+
  scale_y_continuous(labels=scales::dollar_format())

ggsave(file="../src/daily_pred.svg", plot=days)

  #WEEKLY

df_week = cbind.data.frame(week$date,week$adjusted,pred_week)
colnames(df_week) = nam

weeks = ggplot(data = df_week, aes(x = date))+
  geom_line(aes(y = adjusted, color = "Adjusted"))+
  geom_line(aes(y = predicted, color = "Predicted"))+
  ggtitle("S&P 500 Weekly Price from 1982-2022")+
  xlab("")+
  ylab("Price")+
  labs(color = "")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = colors)+
  scale_x_date(date_labels = "%Y")+
  scale_y_continuous(labels=scales::dollar_format())

ggsave(file="../src/weekly_pred.svg", plot=weeks)

  #MONTHLY

df_mon = cbind.data.frame(month$date,month$adjusted,pred_mon)
colnames(df_mon) = nam

months = ggplot(data = df_mon, aes(x = date))+
  geom_point(aes(y = adjusted,color = "Adjusted"))+
  geom_point(aes(y = predicted, color = "Predicted"))+
  ggtitle("S&P 500 Monthly Price from 1982-2022")+
  xlab("")+
  ylab("Stock Price")+
  labs(color = "")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = colors)+
  scale_x_date(date_labels = "%Y")+
  scale_y_continuous(labels=scales::dollar_format())

ggsave(file="../src/monthly_pred.svg", plot=months)

  #QUARTERLY

df_quar = cbind.data.frame(quarter$date,quarter$adjusted,pred_quar)
colnames(df_quar) = nam

quart = ggplot(data = df_quar, aes(x = date))+
  geom_point(aes(y = adjusted,color = "Adjusted"))+
  geom_point(aes(y = predicted, color = "Predicted"))+
  ggtitle("S&P 500 Quarterly Price from 1982-2022")+
  xlab("")+
  ylab("Stock Price")+
  labs(color = "")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = colors)+
  scale_x_date(date_labels = "%Y")+
  scale_y_continuous(labels=scales::dollar_format())

ggsave(file="../src/quarterly_pred.svg", plot=quart)

  #YEARLY

df_year = cbind.data.frame(year$date,year$adjusted,pred_year)
colnames(df_year) = nam

years = ggplot(data = df_year, aes(x = date))+
  geom_point(aes(y = adjusted,color = "Adjusted"))+
  geom_point(aes(y = predicted, color = "Predicted"))+
  ggtitle("S&P 500 Yearly Price from 1982-2021")+
  xlab("")+
  ylab("Stock Price")+
  labs(color = "")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = colors)+
  scale_x_date(date_labels = "%Y")+
  scale_y_continuous(labels=scales::dollar_format())

ggsave(file="../src/yearly_pred.svg", plot=years)






