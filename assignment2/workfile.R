# C:\Users\abogn\CEU\Study\da2\assignment1\da2_assignment1

rm(list=ls())


# library(ggpubr)
# library(AER)
library(tidyverse)
library(lspline)
library(fixest)
library(modelsummary)
library(kableExtra)
library(mfx)

hotels_europe_price <- read_csv("https://osf.io/p6tyr/download")
hotels_europe_features <- read_csv("https://osf.io/utwjs/download")
data <- left_join(hotels_europe_price, hotels_europe_features, by = "hotel_id")
rm(hotels_europe_price,hotels_europe_features)


hotels <- data %>% 
  filter( !is.na( stars )) %>%
  filter( city_actual=="Barcelona" ) %>%
  filter( !is.na( distance )) %>%
  filter( !is.na( rating )) %>%
  filter( year == 2017, month == 12 , weekend == 0)  %>% 
  filter( accommodation_type  == "Hotel")%>% 
  filter( price  <= 4500)


table(hotels$nnights)
# Y: rating X1: distance X2: stars 
# X3: log price X4: offer X5: nnights


hotels %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()

datasummary_skim( hotels )


P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
datasummary( (`High or Low Rating (1/0)` = highly_rated ) + 
               (`Avg. rating` = rating ) + 
               (`Price` = price ) + 
               (`Ln Price` = logprice )+
               (`Distance from center` = distance) + 
               (`Stars` = stars) + 
               (`Number of nights` = nnights) ~
               Mean + Median + SD + Min + Max + P05 + P95 + N , 
             data = hotels ,
             title = 'Descriptive statistics') %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"))


hotels <- hotels %>% select( rating , distance , stars , price , nnights , rating_reviewcount) 

# correlation

numeric_hotels <- keep( hotels , is.numeric )
cT <- round( cor( numeric_hotels , use = "complete.obs") , 2 )
cT[ upper.tri( cT ) ] <- NA
melted_cormat <- melt( cT , na.rm = TRUE)


# histograms - rating

table(hotels$highly_rated)

ggplot( hotels , aes(x = highly_rated)) +
  geom_histogram( binwidth = 0.1, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()

# only few hotels below 3 and 3 at 5.00, almost normal around 4
table(hotels$distance)

ggplot( hotels , aes(x = distance)) +
  geom_histogram( binwidth = 0.1, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()

# maybe exlude above 4, onyl 2

table(hotels$stars)
ggplot( hotels , aes(x = stars)) +
  geom_histogram( binwidth = 0.5, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()

table(hotels$price)
ggplot( hotels , aes(x = price)) +
  geom_histogram( binwidth = 50, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()

table(hotels$nnights)
ggplot( hotels , aes(x = nnights)) +
  geom_histogram( binwidth = 3, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()



table(hotels$rating_reviewcount)
ggplot( hotels , aes(x = rating_reviewcount)) +
  geom_histogram( binwidth = 50, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()


# robustness check with low-high rating
# price as log
# New columns

hotels <- hotels %>% 
  mutate( logprice = log( price ),
          rating_cutoff = 0 +
            1 * as.numeric( hotels$rating_reviewcount >= 100 ) +
            1 * as.numeric( hotels$rating_reviewcount >= 1000 ) )


hotels$highly_rated <- ifelse(hotels$rating >= 4, 1, 0)

# Lowess

chck_sp <- function( x_var , x_lab ){
  ggplot( hotels , aes(x = x_var, y = highly_rated)) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))+
    labs(x = x_lab, y = "High-Low Rating") +
    theme_bw()
}


chck_sp( hotels$distance , "distance" )

# no correlated? up until 1 mile increae 2miles decrease spline

chck_sp( hotels$distance_alter , "distance_alter" )

# multiple

chck_sp( hotels$stars , "stars" )

#very clear linear 

chck_sp( hotels$nnights , "number of nights" )

# no correlation

chck_sp( hotels$logprice , "logprice" ) 

# spline 5, 5.6 or poly, not include 4500 price


# Regressions

reg1 <- feols( highly_rated ~ stars + distance , data = hotels , vcov = "hetero")
reg1

# reg2: NO controls, use piecewise linear spline(P.L.S) with a knot at 18
reg2 <- feols( highly_rated ~ stars + lspline( distance , c( 1 , 2 ) ) , 
               data = hotels , vcov = "hetero")
reg2

# Compare the two results:
etable( reg1 , reg2 )


lpm <- feols( highly_rated ~ stars + lspline( distance , c( 1 , 2 ) ) 
               + lspline( logprice , c( 5 , 5.6 ) )
               , data = hotels , vcov = "hetero")
lpm





logit <- glm( highly_rated ~ stars + lspline( distance , c( 1 , 2 ) ) 
             + lspline( logprice , c( 5 , 5.6 ) )
             , data = hotels , family='binomial')

summary(logit)
logit_marg <- logitmfx(formula = highly_rated ~ stars + lspline( distance , c( 1 , 2 ) ) 
                       + lspline( logprice , c( 5 , 5.6 ) )
                       , data = hotels , atmean=FALSE)

probit <- glm(highly_rated ~ stars + lspline( distance , c( 1 , 2 ) ) 
              + lspline( logprice , c( 5 , 5.6 ) )
              , data = hotels , family=binomial(link="probit"))


probit_marg <- probitmfx(formula = highly_rated ~ stars + lspline( distance , c( 1 , 2 ) ) 
                         + lspline( logprice , c( 5 , 5.6 ) )
                         , data = hotels , atmean=FALSE)



cm <- c('(Intercept)' = 'Constant')
msummary(list(lpm, logit, logit_marg, probit, probit_marg),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*'
)




pred_lpm <- stats::predict(lpm)

find("predict")
pred_probit <- predict(lpm)
pred_logit <- predict(lpm)
hotels$pred_probit<- predict.glm(probit, type="response") 
hotels$pred_logit <- predict.glm(logit, type="response")

g5<-ggplot(data = hotels) +
  geom_point(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.4,  shape=16) +
  geom_point(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.4,  shape=16) +
  #geom_line(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.3) +
  #geom_line(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.3) +
  geom_line(aes(x=pred_lpm, y=pred_lpm,    color="45 degree line"), size=0.4) +
  labs(x = "Predicted probability of getting high rating", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(name = "", values=c("green", "red", "blue")) +
  theme_classic()+
  theme(legend.position=c(0.55,0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 10))
g5


ggplot( hotels , aes(x = pred_lpm)) +
  geom_histogram( binwidth = 0.1, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Avg. Ratings") +
  theme_bw()
