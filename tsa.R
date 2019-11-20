## ----setup, include=FALSE-----------------------------------------------------
rm(list=ls())
library(knitr)
library(memisc)
library(pander)  # Use GitHub version 0.6.0 addressing memisc issue - this version is now on CRAN
library(rmdformats)
opts_chunk$set( 
  echo = TRUE,
  # eval = FALSE,  # Uncomment to remove R output
  # cache = TRUE,  # Uncomment after changes to code
  prompt = FALSE,
  warning = FALSE,
  message = FALSE)
opts_knit$set(width=80)
options(max.print="80")
options(scipen=6)

## ----install.packages, eval=FALSE---------------------------------------------
# required_packages <- c('broom', 'car', 'ggplot2', 'haven',
#                        'ISOweek', 'lubridate', 'MASS', 'pander',
#                        'readxl', 'reshape2', 'TSA', 'zoo')
# install.packages(required_packages)

## ----library------------------------------------------------------------------
# Packages required
required_packages <- c('broom', 'car', 'ggplot2', 'haven', 
                       'ISOweek', 'lubridate', 'MASS', 'pander',  
                       'readxl', 'reshape2', 'TSA', 'zoo')
for(i in seq(along = required_packages))
  library(required_packages[i], character.only = TRUE)

# Function to create Stata weekly date
stataweekdate <- function(year, week){
  (year - 1960) * 52 + week - 1
}

# Function to create Stata year and week numbers
statawofd <- function(date){
  if(!is.Date(date)) stop('date should be a Date.')
  dateposix <- as.POSIXlt(date)
  dayofyear <- dateposix$yday
  week <- floor(dayofyear/7) + 1
  week[week %in% 53] <- 52
  year <- dateposix$year + 1900
  list(year=year, week=week)
}

# Function to tidy glm regression output
glmtidy <- function(x, caption=''){ 
  pander(tidy(x, exponentiate=TRUE, conf.int=TRUE),
              caption=caption)
}

# Function to tidy glm regression statistics
glmstats <- function(x){
  pander(glance(x))
}

## -----------------------------------------------------------------------------
salmo <- read_excel('TSA practice1_nov2016.xls', 'dis1')
summary(salmo)

## -----------------------------------------------------------------------------
measles <- read_excel('TSA practice1_nov2016.xls', 'dis2')
summary(measles)

## -----------------------------------------------------------------------------
ari <- read_excel('TSA practice1_nov2016.xls', 'dis3')
summary(ari)

## ----viewsalmo, eval=FALSE----------------------------------------------------
## View(salmo)

## -----------------------------------------------------------------------------
salmo$date <- with(salmo, stataweekdate(year, week))
head(salmo$date, 10)

## -----------------------------------------------------------------------------
salmoz <- zooreg(salmo$cases, start=c(1981, 1), frequency=52)
plot(salmoz, ylab='Cases', main='Salmonella data')

## -----------------------------------------------------------------------------
autoplot(salmoz) +
  scale_x_continuous(breaks=1980:1990, minor_breaks=1) +
  labs(x='Index', y='Cases', title='Salmonella data')

## -----------------------------------------------------------------------------
ariz <- zooreg(ari[, 3:5], start=c(2003, 1), frequency=52)
plot(ariz, main='Acute respiratory infection')

## -----------------------------------------------------------------------------
puumala <- read_dta('puumala.dta')
head(puumala)

## -----------------------------------------------------------------------------
summary(puumala)

## -----------------------------------------------------------------------------
puumala$date_Date <- as.Date(puumala$date_str, '%d/%m/%Y')
head(puumala$date_Date, 10)

## -----------------------------------------------------------------------------
puumala$date_isowk <- ISOweek(puumala$date_Date)
puumala$isodate <- ISOweek2date(paste(puumala$date_isowk,
                                      '-1', sep=''))
head(puumala)

## -----------------------------------------------------------------------------
puumala$case <- 1
puumala2 <- aggregate(case ~ isodate, sum, data=puumala)
head(puumala2)

## -----------------------------------------------------------------------------
measles2 <- as.vector(t(measles[-1]))
measlez <- zooreg(measles2, start=c(1928, 1), frequency=12)
plot(measlez, ylab='Cases', main='Measles in New York')

## -----------------------------------------------------------------------------
mort <- read_dta('mortality.dta')
head(mort)

## -----------------------------------------------------------------------------
community <- c("Andalucía", "Aragón", "Asturias", "Baleares",
               "Canarias", "Cantabria", "Castilla y León",
               "Castilla-La Mancha", "Cataluña",
               "Comunidad Valenciana", "Extremadura",
               "Galicia", "Madrid", "Murcia", "Navarra",
               "País Vasco", "La Rioja")
mort$community2 <- factor(mort$community, labels=community)
summary(mort)

## -----------------------------------------------------------------------------
mortagg <- aggregate(cases ~ week + year, sum, data=mort)
head(mortagg)

## -----------------------------------------------------------------------------
mortz <- zooreg(mortagg, start=c(2000, 1), frequency=52)
plot(mortz$cases, ylab='Cases', main='Mortality')

## -----------------------------------------------------------------------------
MA5a <- rollapply(mortz$cases, width=5, FUN=mean,
                  align='center')
MA5b <- rollapply(mortz$cases, width=5, FUN=mean,
                  align='right')
MA5c <- rollapply(mortz$cases, width=6,
                  FUN=function(x) mean(x[-6]),
                  align='right')
mortzma <- merge(cases=mortz$cases, MA5a, MA5b, MA5c)
head(mortzma)

## ----ma, fig.height=8---------------------------------------------------------
plot(mortzma, plot.type='single', lty=1:4,
     ylab='Cases', main='Moving averages')
grid()
legend('topright', c('Cases', 'MA5a', 'MA5b', 'MA5c'), lty=1:4,
       title='Legend')

## -----------------------------------------------------------------------------
scatter.smooth(mortz$cases, ylab='Mortality',
               main='Loess smoothing')

## -----------------------------------------------------------------------------
MA25 <- rollapply(mortz$cases, width=25, FUN=mean,
                  align='right')
MA51 <- rollapply(mortz$cases, width=51, FUN=mean,
                  align='right')
MA103 <- rollapply(mortz$cases, width=103, FUN=mean,
                  align='right')
mortzma2 <- merge(cases=mortz$cases, MA25, MA51, MA103)

## ----ma2, fig.height=8--------------------------------------------------------
plot(mortzma2, plot.type='single', lty=1:4,
     ylab='Cases', main='Moving averages')
grid()
legend('topright', c('Cases', 'MA25', 'MA51', 'MA103'), lty=1:3,
       title='Legend')

## -----------------------------------------------------------------------------
mortz$Date <- with(mortagg, stataweekdate(year, week))
linregmodel <- lm(cases ~ Date, data=mortz)
names(linregmodel)
summary(linregmodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Mortality with fitted trend')
lines(as.vector(time(mortz)), fitted(linregmodel),
      xlab='Index', col='green', lwd=2)
grid()
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
nbmodel <- glm.nb(cases ~ Date, data=mortz)
glmtidy(nbmodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Mortality with fitted trend')
lines(as.vector(time(mortz)), fitted(nbmodel),
      xlab='Index', col='green', lwd=2)
grid()
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
mortz$pop <- 
  rep(c(39953520, 40688520, 41423520, 42196231, 42859172,    
        43662613,  44360521, 45236004, 45983169, 46367550), 
      each=52)

## -----------------------------------------------------------------------------
mortpoismodel <- glm(cases ~ offset(log(pop)) + Date, data=mortz, 
                     family='poisson')
glmtidy(mortpoismodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Mortality with fitted trend')
lines(as.vector(time(mortz)), fitted(mortpoismodel),
      xlab='Index', col='green', lwd=2)
grid()
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
salmo$lcases <- log(salmo$cases)
salmoz2 <- zooreg(salmo, start=c(1981, 1), frequency=52)

## -----------------------------------------------------------------------------
plot(salmoz2$lcases, ylab='Cases (natural log scale)', 
     main='Log Salmonella data')

## -----------------------------------------------------------------------------
logmodel <- lm(lcases ~ date, data=salmoz2)
summary(logmodel)

## -----------------------------------------------------------------------------
salmoz2$ltrend[!is.na(salmoz2$lcases)] <- fitted(logmodel)
salmoz2$ltrend <- na.approx(salmoz2$ltrend)
plot(salmoz2$lcases, ylab='Log Salmonella cases',
     main='Log Salmonella data with fitted trend')
lines(salmoz2$ltrend,
      xlab='Index', col='green', lwd=2)
grid()
legend('bottomright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)


## -----------------------------------------------------------------------------
salmoz2$trend <- exp(salmoz2$ltrend)

## -----------------------------------------------------------------------------
plot(salmoz2$cases, ylab='Salmonella cases',
     main='Salmonella data with fitted trend')
lines(salmoz2$trend,
      xlab='Index', col='green', lwd=2)
grid()
legend('topleft', c('Data', 'Model'), col=c('black', 'green'), lwd=2)


## -----------------------------------------------------------------------------
salmopoismodel <- glm(cases ~ date, data=salmoz2, family='poisson')
summary(salmopoismodel)

## -----------------------------------------------------------------------------
salmoz2$trend2[!is.na(salmoz2$cases)] <- fitted(salmopoismodel)
salmoz2$trend2 <- na.approx(salmoz2$trend2)
plot(salmoz2$cases, ylab='Salmonella cases',
     main='Salmonella data with fitted trend')
lines(salmoz2$trend2,
      xlab='Index', col='green', lwd=2)
grid()
legend('topleft', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## ----periodogram, fig.width=6-------------------------------------------------
mortper <- periodogram(mortz$cases)

## -----------------------------------------------------------------------------
with(mortper, 
     1 / head(freq[order(-spec)], 3))

## ----periodgram2, fig.width=8-------------------------------------------------
with(mortper, 
     plot(1/freq, log(spec), type='l',
          xlim=c(0, 160),
          xlab='Period', ylab='Log(density)'))

## -----------------------------------------------------------------------------
mortz$sin52 <- sin(2 * pi * mortz$Date / 52)
sinemodel <- lm(cases ~ sin52, data=mortz)
summary(sinemodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Regression model: one sine term')
lines(as.vector(time(mortz)), fitted(sinemodel),
      xlab='Index', col='green', lwd=2)
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
mortz$cos52 <- cos(2 * pi * mortz$Date / 52)
sinecosmodel <- lm(cases ~ sin52 + cos52, data=mortz)
summary(sinecosmodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Regression model: sine, cosine terms')
lines(as.vector(time(mortz)), fitted(sinecosmodel),
      xlab='Index', col='green', lwd=2)
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
sinecostrendmodel <- lm(cases ~ sin52 + cos52 + Date,
                        data=mortz)
summary(sinecostrendmodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Regression model: trend, sine, cosine terms')
lines(as.vector(time(mortz)), fitted(sinecostrendmodel),
      xlab='Index', col='green', lwd=2)
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
mortz$sin26 <- sin(2 * pi * mortz$Date / 26)
mortz$cos26 <- cos(2 * pi * mortz$Date / 26)
sine2cos2trendmodel <- lm(cases ~ sin52 + cos52 + sin26 + cos26 + Date,
                          data=mortz)
summary(sine2cos2trendmodel)

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Regression model with trend plus 2 sine and cosine terms')
lines(as.vector(time(mortz)), fitted(sine2cos2trendmodel),
      col='green', lwd=2)
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
drop1(sine2cos2trendmodel, test='F')

## -----------------------------------------------------------------------------
mean1 <- aggregate(cases ~ week, mean, data=mortagg)
mean2 <- mean(mortagg$cases)

## -----------------------------------------------------------------------------
with(mean1, barplot(cases, names.arg=week,
		    xlab='Week', ylab='Average cases', 
		    main='Average cases by week number', col='blue'))
abline(h=mean2, col='green', lwd=2)
legend('bottomright', legend='Overall mean',
       lwd=2, col='green', bg='white')

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality', main='Mortality with fitted trend')
lines(as.vector(time(mortz)), fitted(linregmodel),
      col='green', lwd=2)
grid()
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
plot(as.vector(time(mortz)), residuals(linregmodel), type='l',
     xlab='Index', ylab='Residuals')

## -----------------------------------------------------------------------------
mean_resid_by_week <- aggregate(list(x=residuals(linregmodel)),
                                list(week=mortagg$week), mean)
with(mean_resid_by_week, barplot(x, names.arg=week, 
				 xlab='Week number', ylab='Mean of residuals', 
				 col='blue'))

## -----------------------------------------------------------------------------
residtimemodel <-
  lm(residuals(linregmodel) ~ 0 + factor(mortz$week))
season <- fitted(residtimemodel)
plot(as.vector(time(mortz)), season,
     main='Note seasonality remains after estimated trend removed',
     ylab='Residuals', xlab='Index')

## -----------------------------------------------------------------------------
plot(mortz$cases, ylab='Mortality',
     main='Regression model with trend plus 2 sine and cosine terms')
lines(as.vector(time(mortz)), fitted(sine2cos2trendmodel),
      col='green', lwd=2)
legend('topright', c('Data', 'Model'), col=c('black', 'green'), lwd=2)

## -----------------------------------------------------------------------------
periodogram(residuals(sine2cos2trendmodel))

## -----------------------------------------------------------------------------
res <- residuals(sine2cos2trendmodel)
hist(res, probability=TRUE,
     main='Histogram of model residuals plus normal curve',
     xlab='Residuals', breaks=22)
curve(dnorm(x, mean=mean(res), sd=sd(res)), add=TRUE,
      col='green', lwd=2)

## ----qqnorm, fig.width=6------------------------------------------------------
qqnorm(res)
qqline(res)

## -----------------------------------------------------------------------------
shapiro.test(res)

## ----residpred, fig.width=6---------------------------------------------------
fit <- fitted(sine2cos2trendmodel)
plot(fit, res, ylab='Residuals', xlab='Predicted values',
     main='Plot of residuals against predicted values')

## -----------------------------------------------------------------------------
meanvarplot <- function(x, groupnum=50, ...){
  x <- as.vector(x)
  x <- x[order(x)] 
  group <- cut(1:length(x), breaks=groupnum, 
               labels=0:(groupnum-1),  
               include.lowest=TRUE,right=FALSE)
  datf <- data.frame(group, x)
  mcases <- aggregate(x ~ group, mean, data=datf)$x
  vcases <- aggregate(x ~ group, sd, data=datf)$x 
  plot(mcases, vcases, ...)
  invisible(list(datf, mcases, vcases))
}

## ----meanvarplot1, fig.width=6------------------------------------------------
meanvarplot(mortz$cases,
            xlab='Means of groups', ylab='Variances of groups (sd plotted)',
            main='Mean-variance plot of data')

## ----meanvarplot2, fig.width=6------------------------------------------------
meanvarplot(res,
            xlab='Means of groups', ylab='Variances of groups (sd plotted)',
            main='Main-variance plot of residuals')

## -----------------------------------------------------------------------------
mortz <- merge(mortz, res)
residtimemodel2 <-
  lm(res ~ Date, data=mortz)
plot(as.vector(time(mortz)), fitted(residtimemodel),
     main='',
     xlab='Index', ylab='Predictions')

## -----------------------------------------------------------------------------
mortacf<- stats::acf(mortz$cases, lag.max=50, ci.type='ma',
                    main='Autocorrelation plot of cases')
legend('topright', legend=c('+/- 95% CI'), col='blue', lty=2, lwd=1)
head(as.vector(mortacf$acf), 5)

## -----------------------------------------------------------------------------
mortpacf <- pacf(mortz$cases, lag.max=50,
                      main='Partial autocorrelation plot of cases')
legend('topright', legend=c('+/- 95% CI'), col='blue', lty=2, lwd=1)
head(as.vector(mortpacf$acf), 5)

## -----------------------------------------------------------------------------
Box.test(mortz$cases, type='Ljung-Box')

## -----------------------------------------------------------------------------
resACF <- acf(res, lag.max=50, ci.type='ma', 
              main='Autocorrelation plot')
legend('topright', legend=c('+/- 95% CI'), col='blue', lty=2, lwd=1)
head(as.vector(resACF$acf), 5)

## -----------------------------------------------------------------------------
resPACF <- pacf(res, lag.max=50,
                main='Partial autocorrelation plot of cases')
legend('topright', legend=c('+/- 95% CI'), col='blue', lty=2, lwd=1)
head(as.vector(resPACF$acf), 5)

## -----------------------------------------------------------------------------
plot(mortz$cases,
     ylab='Weekly total cases', main='Mortality')
abline(v=2008 + 26 / 52, lty=2)
text(2008 + 26/52, 7000, 'Week 26 2008', pos=4)

## ----historic, fig.height=8---------------------------------------------------
historic <- 
  with(mortz, year < 2008 & (24 <= week & week <= 28))
plot(mortz$cases, 
      ylab='Weekly total cases', 
      main='Mortality')
points(as.vector(time(mortz)[historic]), 
       mortz$cases[historic],
       lwd=2, col='red')
points(as.vector(time(mortz))[442], mortz$cases[442],
       lwd=3, col='green')

## -----------------------------------------------------------------------------
mu <- mean(mortz$cases[historic])
mu

## -----------------------------------------------------------------------------
trendmodel <- lm(cases ~ Date, data=mortz[historic])
summary(trendmodel)

## -----------------------------------------------------------------------------
nullmodel <- lm(cases ~ 1, data=mortz[historic])
summary(nullmodel)

## -----------------------------------------------------------------------------
# 95% confidence interval
ci <- predict(nullmodel, newdata=data.frame(1), interval='confidence')
ci
# Standard deviation of the residuals
std <- summary(nullmodel)$sigma
std
# 95% prediction interval
predint <- predict(nullmodel, newdata=data.frame(1),
                   interval='prediction')
predint
# 95% tolerance interval
tolint <- c(fit=mu, lwr=mu-1.96*std, upr=mu+1.96*std)
tolint

## ----predint, fig.height=8----------------------------------------------------
plot(mortz$cases, 
     ylab='Weekly total cases', main='Mortality')
points(as.vector(time(mortz)[historic]), 
       mortz$cases[historic],
       lwd=2, col='red')
points(as.vector(time(mortz))[442],
       mortz$cases[442],
       lwd=2, col='green')
points(2008.5, mu, col='darkorange', lwd=2, pch=19)
arrows(2008.5, predint[2], 2008.5, predint[3],
         code=3, angle=90, col='darkorange', lwd=2)

## ----qqnorm2, fig.width=6-----------------------------------------------------
nullres <- residuals(nullmodel)
qqnorm(nullres)
qqline(nullres)
shapiro.test(nullres)
plot(as.vector(time(mortz))[historic], nullres,
     xlab='Index', ylab='Residuals')

## ----rm, eval=FALSE-----------------------------------------------------------
## rm(mu, ci, std, predint, tolint, historic, nullres)

## ----predintplot--------------------------------------------------------------
predintplot <- function(tsdata, tsvar, num_periods, width,
                        downweight=FALSE, 
                        add=FALSE, ...) { 
  if (width %% 2 == 0) stop('num_periods should be an odd number.')
  results <- data.frame(expected = rep(NA, num_periods),  
                        lower_pi = rep(NA, num_periods),  
                        upper_pi = rep(NA, num_periods)) 
  tsvar <- tsdata[, tsvar]
  startpt <- length(tsvar) - num_periods + 1
  endpt <- length(tsvar)
  cycles <- as.numeric(cycle(tsvar))
  oneside <- (width-1)/2
  for (j in startpt:endpt){  
    h <- cycles %in% cycles[(j-oneside):(j+oneside)]
    h[startpt:endpt] <- FALSE
    nullmodel <- lm(tsvar[h] ~ 1)
    if(downweight){ 
      stdres <- residuals(nullmodel) / sigma(nullmodel)
      wts <- ifelse(stdres > 2, stdres^-2, 1)
      nullmodel <- lm(tsvar[h] ~ 1, weights=wts)
    }
    results[j-startpt+1, ] <- predict(nullmodel,
                                      newdata=data.frame(1),
                                      interval='prediction')
  }
  if(!add){  
    plot(tsvar[startpt:endpt],
         type='l',
         ylim=c(min(c(results$lower_pi, tsvar), na.rm=TRUE),
                max(c(results$upper_pi, tsvar), na.rm=TRUE)),
         ...)
    legend('topleft',   
           legend = c('Expected',   
                      'Lower bound 95% prediction interval',   
                      'Upper bound 95% prediction interval'),  
           lwd=c(2, 1, 1),  
           lty=c(1, 2, 2),  
           col=c('red', 'green', 'orange')) 
  }
  lines(as.vector(time(tsvar))[startpt:endpt], results$lower_pi,
        col='green', lty=2)
  lines(as.vector(time(tsvar))[startpt:endpt], results$expected,
        col='red', lwd=2)
  lines(as.vector(time(tsvar))[startpt:endpt], results$upper_pi,
        col='orange', lty=2)
  results$observed <- as.vector(tsvar)[startpt:endpt]
  invisible(results)
}

## ----mortpred, fig.height=8---------------------------------------------------
mortpred <- predintplot(mortz, 'cases', 52, 5, 
                        ylab='Weekly total cases',
                        main='Prediction intervals')
mortpred[mortpred$observed > mortpred$upper_pi, ]

## ----mortpred2, fig.height=8--------------------------------------------------
predintplot(mortz, 'cases', 52, 5,  
            ylab='Weekly total cases', 
            main='Prediction intervals')
mortpred2 <- predintplot(mortz, 'cases', 52, 5,
                         downweight=TRUE, add=TRUE)
mortpred2[mortpred2$observed > mortpred2$upper_pi, ]

## -----------------------------------------------------------------------------
aragon <- read_dta('aragon.dta')

## ----aragonz, fig.height=8----------------------------------------------------
aragonz <- zooreg(aragon, start=c(2000, 1), frequency=52)
plot(aragonz[, c('tmax', 'cases')],
     main='Mean maximum temperature and mortality in Aragon')

## -----------------------------------------------------------------------------
aragonz$sinvar <- sin(2 * pi * aragonz$date / 52)
aragonz$cosvar <- cos(2 * pi * aragonz$date / 52)

## -----------------------------------------------------------------------------
aragmodel1 <- lm(cases ~ tmax + sinvar + cosvar + date,
               data=aragonz)
summary(aragmodel1)

## -----------------------------------------------------------------------------
plot(aragonz$cases, ylab='Cases',
     main='Cases with fitted trend')
lines(as.vector(time(aragonz)), fitted(aragmodel1),
      col='green', lwd=2)

## -----------------------------------------------------------------------------
aragonz$winter <- with(aragonz, week >= 49 | week <= 8)

## -----------------------------------------------------------------------------
aragmodel2 <- lm(cases ~ tmax + sinvar + cosvar + date + winter,
               data=aragonz)
summary(aragmodel2)

## -----------------------------------------------------------------------------
aragmodel3 <- lm(cases ~ sinvar + cosvar + date + winter * tmax,
               data=aragonz)
summary(aragmodel3)

## ----mtable1, echo=FALSE------------------------------------------------------
pander(mtable('Model 2' = aragmodel2,  
              'Model 3' = aragmodel3, 
              summary.stats=c('R-squared', 'adj. R-squared', 'AIC', 'BIC', 'N')), 
       caption='Aragon models 2 and 3: estimate (SE)')

## -----------------------------------------------------------------------------
L1.tmax <- stats::lag(aragonz$tmax, -1)
aragonz <- merge(aragonz, L1.tmax)
head(aragonz)

## -----------------------------------------------------------------------------
aragmodel4 <- lm(cases ~ sinvar + cosvar + date +  
                   winter*tmax + L1.tmax, 
                 data=aragonz)
summary(aragmodel4)

## -----------------------------------------------------------------------------
aragonz.winter <- aragonz[aragonz$winter %in% 1]
aragonz.notwinter <- aragonz[!aragonz$winter %in% 1]

## ----aragmodel5, results='hide'-----------------------------------------------
aragmodel5 <- lm(cases ~ tmax + sinvar + cosvar + date + L1.tmax,
                  data=aragonz.winter)
# includes 2 weeks' temperatures
summary(aragmodel5)

## ----aragmodel6, results='hide'-----------------------------------------------
aragmodel6 <- lm(cases ~ sinvar + cosvar + date + L1.tmax,
                  data=aragonz.winter)
# includes only last week’s temperature
summary(aragmodel6)

## ----aragmodel7, results='hide'-----------------------------------------------
aragmodel7 <- lm(cases ~ tmax + sinvar + cosvar + date + L1.tmax,
                  data=aragonz.notwinter)
# includes 2 week's temperatures
summary(aragmodel7)

## ----aragmodel8, results='hide'-----------------------------------------------
aragmodel8 <- lm(cases ~ tmax + sinvar + cosvar + date + L1.tmax,
                  data=aragonz.notwinter)
# includes only last week’s temperature
summary(aragmodel8)

## ----aragmodel5to8, echo=FALSE------------------------------------------------
pander(
  mtable(
  'Model 5' = aragmodel5,
  'Model 6' = aragmodel6,
  'Model 7' = aragmodel7,
  'Model 8' = aragmodel8,
  summary.stats=c('R-squared', 'adj. R-squared', 'AIC', 'BIC', 'N')), 
  caption='Aragon models 5, 6, 7 and 8: estimate (SE)')

## -----------------------------------------------------------------------------
rota <- read_dta('rotavirus.dta')
rota$case <- 1
rota$year <- statawofd(rota$date)$year
rota$week <- statawofd(rota$date)$week
head(rota)

## -----------------------------------------------------------------------------
rota2 <- aggregate(case ~ agegrp + week + year, data=rota, sum)
rota3 <- dcast(rota2, year + week ~ agegrp, value.var='case', sum)
rota3$cases <- rowSums(rota3[, 3:7])
names(rota3) <- c('year', 'week', 'cases <1 year', 'cases 1-4 years',
                  'cases 5-14 years', ' cases 15-64 years', 
                  'cases 65+ years', 'cases')
head(rota3)
rotaz <- zooreg(rota3, start=c(2009, 23), frequency=52)

## ----rotaz, fig.height=8------------------------------------------------------
plot(rotaz[, 3:8], main='Rotavirus cases\n(note differing y scales)')

## -----------------------------------------------------------------------------
rotaz$vaccine <- 0
window(rotaz$vaccine, start=c(2013, 27)) <- 1
rotaz$Date <- with(rota3, stataweekdate(year, week))
rotaz$sin52 <- sin(2 * pi * rotaz$Date / 52)
rotaz$cos52 <- cos(2 * pi * rotaz$Date / 52)

## -----------------------------------------------------------------------------
rotamodel1 <- glm(`cases <1 year` ~ Date, 
                  data=rotaz[rotaz$vaccine %in% 0, ],
                  family='poisson')
glmtidy(rotamodel1, 'Rotavirus model 1')
glmstats(rotamodel1)

## -----------------------------------------------------------------------------
rotamodel2 <- glm(`cases <1 year` ~ Date, 
                  data=rotaz[rotaz$vaccine %in% 1, ],
                  family='poisson')
glmtidy(rotamodel2, 'Rotavirus model 2')
glmstats(rotamodel2)

## ----rotamodel1and2, echo=FALSE-----------------------------------------------
pander(mtable('Rotavirus model 1'=rotamodel1, 
              'Rotavirus model 2'=rotamodel2,  
              getSummary=getSummary_expcoef, 
              summary.stats=c('AIC', 'BIC', 'N')),
       caption='Rotavirus models 1 and 2: estimate (SE)')

## -----------------------------------------------------------------------------
rotamodel3 <- glm(`cases <1 year` ~ Date + sin52 + cos52 + vaccine, 
                  data=rotaz, family='poisson')
glmtidy(rotamodel3, 'Rotavirus model 3')
glmstats(rotamodel3)

## -----------------------------------------------------------------------------
plot(rotaz[, 'cases <1 year'],
     main='Rotavirus cases <1 year: model 3',
     ylab='Cases')
lines(as.vector(time(rotaz)), fitted(rotamodel3),
      col='green', lwd=2)

## -----------------------------------------------------------------------------
rotamodel4 <- glm(`cases <1 year` ~ Date * vaccine + sin52 + cos52,  
                  data=rotaz, family='poisson')
glmtidy(rotamodel4, 'Rotavirus model 4')
glmstats(rotamodel4)

## -----------------------------------------------------------------------------
rotamodel5 <- glm(`cases <1 year` ~ vaccine * (sin52 + cos52 + Date),
                  data=rotaz, family='poisson')
glmtidy(rotamodel5, 'Rotavirus model 5')
glmstats(rotamodel5)

## -----------------------------------------------------------------------------
linearHypothesis(rotamodel5,
                 c('vaccine:sin52', 'vaccine:cos52'))
