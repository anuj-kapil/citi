##### Installing Packages ###########
#install.packages('rsdmx')

##### Load Packages ###########
library(rsdmx)
library(ggthemes)
library(forecast)
library(lubridate)
library(data.table)
# http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.18+19+20.3+1+2.1599.20+30.M/all?startTime=2017-10&endTime=2018-09


sdmx_lf <- readSDMX(providerId = "ABS", resource = "data", flowRef = "LF",
                    key = list(0, c(18), c(3),1599, c(10, 20, 30), 'M'),
                    start = 1982, end = 2018,
                    dsd = FALSE)

# Labels TRUE associates the definition with the data
system.time({
  df_lf <- as.data.frame(sdmx_lf, labels = FALSE)
})
setDT(df_lf)
#df_lf[,.N, by = TSEST]

df_lf[,obsTime := as.Date(paste(obsTime, "01", sep = "-"))]
df_lf[,TSEST := as.factor(TSEST)]
df_lf[,unique(TSEST)]
# TSEST Trend Series: 30 Seasonal: 20
dt_lf <- df_lf[TSEST == 30, 8:9]
dt_lf[,IND_R := 30]
setcolorder(dt_lf, c("IND_R", "obsTime", "obsValue"))

#ggplot(df_lf,aes(x=obsTime, y=obsValue, group = TSEST))+
#  geom_line(aes(color = TSEST))
listIndustries <- c(20, 41, 42, 43, 44, 45, 46)

sdmx <- readSDMX(providerId = "ABS", resource = "data", flowRef = "RT",
                 key = list(0, 2, listIndustries , c(10,20,30), 'M'),
                 start = 1980, end = 2018,
                 dsd = FALSE)

system.time({
  df <- as.data.frame(sdmx, labels = FALSE)
})
setDT(df)


dt_out <- lapply(listIndustries, function(x){
  # TSEST Trend Series: 30 Seasonal: 20
  dt <- df[IND_R == 46 & TSEST == 30, 7:8]
  dt[,IND_R := 20]
  dt[,obsTime := as.Date(paste(obsTime, "01", sep = "-"))]
  setcolorder(dt, c("IND_R", "obsTime", "obsValue"))
  
  df_combined <- rbindlist(list(dt,dt_lf))
  df_combined_cast <- dcast(df_combined, obsTime ~ IND_R, fill = NA, value.var = "obsValue")
  setDT(df_combined_cast)
  names(df_combined_cast) <- c("obsTime" ,"C20","C30" )
  
  # Enable below code to chech the correlation
  
  mydata <- na.omit(df_combined_cast[,2:3]) # listwise deletion of missing
  mydata <- scale(mydata)
  df_trend_cast_core <- cor(mydata, use = "pairwise.complete.obs")
  df_trend_cast_core
  # Total 98.86%  98.92%
  # 41 98.53% 98.59%
  # 42 98.61% 98.68%
  # 43 98.74% 98.88%
  # 44 97.30% 97.68%
  # 45 98.88% 98.95%
  # 46 97.64% 97.72%
  
  setDT(df_combined_cast)
  #df_combined_cast[,C20Growth:= (C20-shift(C20, type = "lag", n = 1))/shift(C20, type = "lag", n = 1)]
  #df_combined_cast[,C30Growth:= (C30-shift(C30, type = "lag", n = 1))/shift(C30, type = "lag", n = 1)]
  
  # Visually fit a linear regression
  
  # ggplot(df_combined_cast,aes(x=C20, y=C30))+
  #   geom_point(color = '#60636a')+
  #   geom_smooth(method = "lm", color = 'Orange')+
  #   labs(title="Linear Regression\n", size = 15)+
  #   theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))+
  #   scale_x_continuous(name="Retail Trade", labels = scales::comma)+
  #   scale_y_continuous(name="Labour Force", labels = scales::comma)+
  #   scale_fill_tableau()+
  #   theme(axis.text.x = element_text(size=8),
  #         axis.text.y = element_text(size=8))
  
  library(highcharter)
  
  data(mpg, package = "ggplot2")
  setDT(mpg)
  hchart(mpg, "point", hcaes(displ, hwy, group = drv), regression = TRUE) %>% 
    hc_colors(c("#d35400", "#2980b9", "#2ecc71")) %>% 
    hc_add_dependency("plugins/highcharts-regression.js")
  
  title_txt <- paste0("Labor Force - ",names(choiceVec)[choiceVec == input$industry], " Industry Correlation\n")
  hchart(df_combined_cast, 'point', hcaes(x = 'C20', y = 'C30'), backgroundColor = "transparent") %>%
    hc_title(text = title_txt) %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Revenue ($ million)")) %>%
    hc_legend(labelFormatter = JS("function () {
                                  switch (this.name) {
                                  case '10':
                                  val = 'Original';
                                  break;
                                  case '20':
                                  val = 'Seasonal';
                                  break;
                                  case '30':
                                  val = 'Trend';
                                  }
                                  return val;
}"))%>%
      hc_tooltip(shared = TRUE, formatter = JS("function () {
                                               var d = new Date(this.x)
                                               var month = new Array(12);
                                               month[0] = 'January';
                                               month[1] = 'February';
                                               month[2] = 'March';
                                               month[3] = 'April';
                                               month[4] = 'May';
                                               month[5] = 'June';
                                               month[6] = 'July';
                                               month[7] = 'August';
                                               month[8] = 'September';
                                               month[9] = 'October';
                                               month[10] = 'November';
                                               month[11] = 'December';
                                               var n = month[d.getUTCMonth()];
                                               var o = new Date(this.x).getUTCFullYear()
                                               
                                               function seriesNameLoookup(caseval) {
                                               switch (caseval) {
                                               case '10':
                                               val = 'Original';
                                               break;
                                               case '20':
                                               val = 'Seasonal';
                                               break;
                                               case '30':
                                               val = 'Trend';
                                               }
                                               return val;
                                               }
                                               
                                               arrLen = this.points.length;
                                               text = '<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />';
                                               text = text + '<b>' + n + ' '+ o  + '</b><br>'
                                               for (i = 0; i < arrLen; i++) {
                                               text += '<span style=\"color:'+ this.points[i].point.color +'\"><p>\u25CF <p></span>' + seriesNameLoookup(this.points[i].point.series.name) + ': <b>' + this.points[i].y + '</b><br>'
                                               }
                                               return text;
      }"))%>%
      hc_add_theme(hc_theme_custom)
  
  linear_model <- lm(C20~C30, data = df_combined_cast )
  df_combined_cast$C20Pred <- predict(linear_model, df_combined_cast[,3])
  df_combined_cast[,C20PredGrowth:= (C20Pred-shift(C20Pred, type = "lag", n = 1))/shift(C20Pred, type = "lag", n = 1)]
  
  df_combined_cast[,C20PredGrowthR:= round(C20PredGrowth*100, 2)]
  
  #   x <- runif(400, -1,1)
  #   y <- x*rnorm(400, mean = .25, sd = .5)
  #   
  #   sample_dt <- data.table(vx = x, vy = y)
  #   ggplot(data = sample_dt, aes(x=vx, y=vy))+
  #     geom_point(color = '#60636a')+
  #     geom_smooth(method = "lm", color = 'Orange')+
  #     labs(title="Linear Regression\n", size = 15)+
  #     theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))+
  #     scale_x_continuous(name="Retail Trade Growth", labels = scales::comma)+
  #     scale_y_continuous(name="Labour Force Growth", labels = scales::comma)+
  #     scale_fill_tableau()+
  #     theme(axis.text.x = element_text(size=8),
  #           axis.text.y = element_text(size=8))
  # 
  # 
  # linear_model_s <- lm(vx~vy, data = sample_dt )
  # sample_dt$vxPred <- predict(linear_model_s, sample_dt[,2])
  # 
  # min_max_accuracy <- mean(apply(sample_dt[, c("vx","vxPred"), with = FALSE],1, min)/apply(sample_dt[, c("vx","vxPred"), with = FALSE],1, max), na.rm = T)
  # mape <- mean(abs((sample_dt$vxPred - sample_dt$vx))/sample_dt$vx, na.rm = T)
  # 
  
  #min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
  
  #min_max_accuracy <- mean(apply(df_combined_cast[, c("C20","C20Pred"), with = FALSE],1, min)/apply(df_combined_cast[, c("C20","C20Pred"), with = FALSE],1, max), na.rm = T)
  #mape <- mean(abs((df_combined_cast$C20Pred - df_combined_cast$C20))/df_combined_cast$C20, na.rm = T)
  
  #summary(linear_model)
  #options(scipen = 999)
  #geom_point()
  rowNum <- nrow(df_combined_cast)
  
  fit <- auto.arima(head(df_combined_cast$C20, rowNum-5), seasonal = F)
  fcast <- forecast(fit, h=5)
  # test <- as.data.table(fcast)
  # str(test)
  # 
  # summary(fcast)
  # fcast$lower[,2]
  dt_new <- df_combined_cast
  dt_new[(rowNum-4):rowNum,C20_Adjusted := (fcast$upper[,1] * .8)+(C20Pred * .2) ]
  dt_new[(rowNum-4):rowNum,C20_F := fcast$mean[1]]
  dt_new[(rowNum-4):rowNum,C20_F_80_U := fcast$upper[1,1]]
  dt_new[(rowNum-4):rowNum,C20_F_80_L := fcast$lower[1,1]]
  dt_new[(rowNum-4):rowNum,C20_F_95_U := fcast$upper[1,2]]
  dt_new[(rowNum-4):rowNum,C20_F_95_L := fcast$lower[1,2]]
  dt_new[, IND_R := x]
  dt_new[, TSEST := 20]
  return(dt_new)
  # d <- max(df_combined_cast$obsTime) %m+% months(1)
  # 
  # df_combined_cast$C20_F <- NA
  # df_combined_cast$C20_F_80_U <- NA
  # df_combined_cast$C20_F_80_L <- NA
  # df_combined_cast$C20_F_95_U <- NA
  # df_combined_cast$C20_F_95_L <- NA
  # 
  # #names(df_combined_cast)%like%'C20'
  # 
  # dt_extra <- setNames(data.table(matrix(nrow = 12, ncol = 11)), names(df_combined_cast)) 
  # dt_extra[, names(dt_extra) := lapply(.SD, as.numeric)]
  # dt_extra[,obsTime := seq(as.Date(d), length=12, by="1 month")]
  # dt_extra[,C20 := fcast$upper[,1]]
  # dt_extra[,C20_F := fcast$mean]
  # dt_extra[,C20_F_80_U := fcast$upper[,1]]
  # dt_extra[,C20_F_80_L := fcast$lower[,1]]
  # dt_extra[,C20_F_95_U := fcast$upper[,2]]
  # dt_extra[,C20_F_95_L := fcast$lower[,2]]
  # cols <- names(dt_extra)[names(dt_extra) != 'obsTime']
  # dt_extra[ , (cols) := lapply(.SD, as.numeric, -1), .SDcols = cols]
  # df_combined_cast[, (cols) := lapply(.SD, as.numeric, -1), .SDcols = cols]
  # dt_new <- rbindlist(list(df_combined_cast, dt_extra))
  
})

dt_out <- rbindlist(dt_out)
setwd("/Users/anuj/Documents/UTS/Citi")
fwrite(dt_out, 'RT_Monthly_Forecast.csv', sep = ",")
# mape <- mean(abs((df_combined_cast$C20Pred - df_combined_cast$C20))/df_combined_cast$C20, na.rm = T)
# 
# summary(linear_model)
# options(scipen = 999)
# geom_point()
