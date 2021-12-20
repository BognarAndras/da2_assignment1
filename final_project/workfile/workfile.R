rm(list = ls () )
library(data.table)
library(lspline)
library(fixest)
library(modelsummary)
library(reshape2)
library(tidyverse)
setwd("C:/Users/abogn/CEU/Study/da2/assignment1/da2_assignment1/da2_assignment1/final_project/")

life_exp_dt <- read.csv("clean/LIFE_EXP-GLOBAL_DATAFLOW_2015-2018-clean.csv")
life_exp_wb_dt <- read.csv("clean/World_Bank_Life_Expectancy.csv")
educ_dt <- read.csv("clean/Table-10-Education-EN-clean.csv")
soc_dt <- read.csv("clean/Table-12-SocProt-and-Econ-EN-clean.csv")
econ_dt <- read.csv("clean/Table-15-SocProt-and-Econ-EN-clean.csv")
mort_dt <- read.csv("clean/Table-2-Child-Mortality-EN-clean.csv")
health_dt <- read.csv("clean/Table-3-MNH-EN-2.csv")
pop_dt <- read.csv("clean/Population_2018.csv")

# all(econ_dt$ï..Country == health_dt$ï..Country)

econ_dt <- data.table(econ_dt)
econ_dt <- econ_dt[, .('Country' = ï..Country , 'Soc_Prot' = On.social.protection ,
                       'Soc_Budget_Gov' = From_GDP) ]

life_exp_dt <- data.table(life_exp_dt)
life_exp_dt <- life_exp_dt[, .('Country' = ï..Country , Life_exp ) ]

life_exp_wb_dt <- data.table(life_exp_wb_dt)
life_exp_wb_dt <- life_exp_wb_dt[, .('Country' = ï..Country.Name , 'Life_exp' = X2018_or_latest ) ]

educ_dt <- data.table(educ_dt)
educ_dt <- educ_dt[, .('Country' = ï..Country , 'Male_Pre_Prim' = male..before.primary ,
                       'Female_Pre_Prim' = female..before.primary ,
                       'Male_Prim' = male.primary , 
                       'Female_Prim' = female.primary ,
                       'Male_Low_Sec' = male.Lower.secondary ,
                       'Female_Low_Sec' = female.Lower.secondary ,
                       'Male_Hi_Sec' = male.Upper.secondary ,
                       'Female_Hi_Sec' = female.Upper.secondary ,
                       'Male_Prim_Co' = male.Primary.Comp ,
                       'Female_Prim_Co' = female.Primary.Comp ,
                       'Male_Low_Sec_Co' = male.Low.Sec.Comp ,
                       'Female_Low_Sec_Co' = female.Low.Sec.Comp ,
                       'Male_Hi_Sec_Co' = male.Up.Sec.Comp ,
                       'Female_Hi_Sec_Co' = female.Up.Sec.Comp ) ]

health_dt <- data.table(health_dt)
health_dt <- health_dt[, .('Country' = ï..Country , 'Birth_Hosp' = Insitutional.Delivery) ]

mort_dt <- data.table(mort_dt)
mort_dt <- mort_dt[, .('Country' = ï..Country , 'Under_5_Mort' = X2018) ]

soc_dt <- data.table(soc_dt)
soc_dt <- soc_dt[, .('Country' = ï..Country , 'GDPP' = GDP.per.capita ,
                 'Mother_Cov' = Mothers.benefit , 'Child_Cov' = Childrens.benefit ) ]

pop_dt <- data.table(pop_dt)
pop_dt <- pop_dt[, .('Country' = ï..Geographic.area , 'Population_t' = Population_thousands ) ]


# educ_dt[!(Country %in% life_exp_dt$Country ) , .(Country , Female_Prim)]
# All missing except Cook Islands, Dominica, Liechtenstein , Marshall Islands
# Nauru , San 
# https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=DM-LI-MH-NR-SM
# drop missing

life_exp_only_wb <- life_exp_wb_dt[!(Country %in% life_exp_dt$Country)]
life_exp_dt <- rbind(life_exp_dt , life_exp_only_wb)

temp_dt <- Reduce(merge,list(educ_dt , econ_dt , health_dt , mort_dt , soc_dt , pop_dt))
main_dt <-  merge( life_exp_dt , temp_dt , by = 'Country')


rm( list = c('temp_dt' , 'life_exp_dt' , 'educ_dt' , 'econ_dt' , 'pop_dt' , 
            'health_dt' , 'mort_dt' , 'soc_dt' , 'life_exp_only_wb' , 'life_exp_wb_dt'))

main_dt[,2:24] <- lapply(main_dt[,2:24] , function(x){as.numeric(x)})

table(main_dt$Population_t)

library(ggplot2)



ggplot(main_dt) +
  geom_histogram( aes( x= Male_Prim) )

main_dt <- main_dt[!is.na(Female_Prim)] # 42 countries w/0 X1
main_dt <- main_dt[!is.na(Under_5_Mort)] # 3 countries w/0 y
# 152 = N
table(main_cleared$Under_5_Mort)

main_dt <- main_dt[!is.na(Birth_Hosp)]
main_dt <- main_dt[!is.na(GDPP)] # 131
main_dt_soc <- main_dt[!is.na(Soc_Budget_Gov)]

# Other variables

main_cleared$gender_gap_Prim <- main_cleared$Female_Prim - main_cleared$Male_Prim
table(main_cleared$gender_gap_Prim)
ifelse(main_cleared$Female_Prim < 50 & main_cleared$Female_Low_Sec < 50 & main_cleared$Female_Hi_Sec < 50
       , '0 - Low - Low - Low' ,
       ifelse(main_cleared$Female_Prim < 100 & main_cleared$Female_Low_Sec < 50 , ) )
# EDA

main_num <- keep( main_dt , is.numeric )
cT <- round( cor( main_num , use = "complete.obs") , 2 )
# create a lower triangular matrix
cT[ upper.tri( cT ) ] <- NA
# Put it into a tibble format
melted_cormat <- melt( cT , na.rm = TRUE)
# Now we can create a heat-map
ggplot( data = melted_cormat, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_bw()+ 
  theme( axis.text.x = element_text(angle = 45, vjust = 1, 
                                    size = 10, hjust = 1))+
  labs(y="",x="")+
  coord_fixed()

rm( cT , numeric_df )

data.table(melted_cormat)[Var2 == 'Male_Prim'][order(value)] # 0.85
data.table(melted_cormat)[Var1 == 'Under_5_Mort'][order(value)]
sum(is.na(main_cleared$Soc_Budget_Gov))
# Biggest neg corr Life_exp Female_Prim_Co Male_Prim_Co Birth_Hosp 
# Biggest pos corr Female_Pre_Prim Female_Prim  

# Histograms

ggplot(main_cleared) +
  geom_histogram( aes( x= GDPP) )



main_cleared %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Possible log: Birth_Hosp, female prim, gdpp, male prim, mother cov

Missing <- name <- function(x) {sum(is.na(x))}
Unique <- name <- function(x) {Unique(is.na(x))}
datasummary(  Female_Prim + Male_Prim + Under_5_Mort + GDPP 
              + Life_exp + Birth_Hosp + Child_Cov + Mother_Cov + Female_Prim_Co
 ~ Missing  , data = main_fully_cleared)

  # X 


datasummary_skim( main_dt )

sum(is.na(main_cleared$Child_Cov))


g1 <- ggplot( main_cleared , aes(x = Female_Prim)) +
  geom_histogram( binwidth = 2, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Out of school rate female') +
  theme_bw()

g2 <-  ggplot( main_cleared , aes(x = Male_Prim)) +
  geom_histogram( binwidth = 2, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Out of school rate male') +
  theme_bw()



ggplot( main_cleared , aes(x = log(Female_Prim))) +
  geom_histogram(fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Out of school rate male') +
  theme_bw()

ggplot( main_cleared , aes(x = GDPP)) +
  geom_histogram( binwidth = 5000, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'GDPP') +
  theme_bw()

ggplot( main_cleared , aes(x = log(Birth_Hosp))) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'GDPP') +
  theme_bw()

ggplot( main_cleared , aes(x = Under_5_Mort)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Mortality') +
  theme_bw()

ggplot( main_cleared , aes(x = log((Under_5_Mort*1000)))) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'log Mortality') +
  theme_bw()

colnames(main_cleared)

min(main_cleared$Under_5_Mort)


ggplot( main_cleared , aes(x = Soc_Budget_Gov)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Soc_Budget_Gov') +
  theme_bw()

# Loess

chck_sp <- function( x_var , x_lab ){
  ggplot( main_dt , aes(x = x_var, y = Under_5_Mort)) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(x = x_lab, y = "Under 5 Mortality in thousand") +
    theme_bw()
}

p1 <- chck_sp(main_dt$Female_Prim , "Percantage of girls out of Primary school") +
  scale_x_continuous(breaks = seq(0,75,by=3))


# spline 0 to 3 to 45, only 5 countries above 45, only 0 to 3
main_cleared[Female_Prim > 40 , .(Country , Female_Prim , Under_5_Mort)]


p2 <- chck_sp(main_dt$Male_Prim , "Percantage of boys out of Primary school") +
  scale_x_continuous(breaks = seq(0,65,by=5))
# spline 0 to 20 to 35 too small, no, 
library(ggpubr)

ggarrange(g1, g2,
          hjust = -0.6,
          ncol = 2, nrow = 1)

sum(main_cleared$Male_Prim[main_cleared$Male_Prim < 20])
sum(main_cleared$Female_Prim[main_cleared$Male_Prim < 20])

chck_sp(main_cleared$GDPP , "GDPP")

chck_sp(log(main_cleared$GDPP) , "Log GDPP")
# spline 0 to 8 too small, no

chck_sp(main_cleared$Birth_Hosp , "Birth in Hospitals %") + scale_x_continuous(breaks = seq(0,100,by=5))
# Spline 66.67

chck_sp(main_cleared$Soc_Budget_Gov , "% of budget on Social")
# 1.25

reg1 <- feols( Under_5_Mort ~ Female_Prim , data = main_dt , vcov = "hetero")
reg1
reg12 <- feols( Under_5_Mort ~ Male_Prim , data = main_dt , vcov = "hetero")

# reg2: NO controls, use piecewise linear spline(P.L.S) with a knot at 18
reg2 <- feols( Under_5_Mort ~ lspline( Female_Prim , 3 ) , data = main_cleared , vcov = "hetero" )
reg2


feols( Under_5_Mort ~ Female_Pre_Prim + Female_Prim + Female_Low_Sec + Female_Hi_Sec , data = main_dt , vcov = "hetero" )
# Compare the two results:
etable( reg1 , reg2 )

reg3 <- feols( Under_5_Mort ~ lspline( Female_Prim , 3 ) +
                 Male_Prim  + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )
reg3

reg4 <- feols( Under_5_Mort ~ lspline(Female_Prim , 3) +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) , data = main_dt , vcov = "hetero" )
reg4

reg41 <- feols( Under_5_Mort ~ Female_Prim  +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) + Child_Cov + Female_Prim*log(GDPP) , data = main_cleared , vcov = "hetero" )
reg41

reg42 <- feols( Under_5_Mort ~ lspline(Female_Prim , 3)  +  
                  Male_Prim + log(GDPP) +
                  lspline( Birth_Hosp , 66.67 ) + Child_Cov + Female_Prim*log(GDPP) , data = main_cleared , vcov = "hetero" )
reg42

reg43 <- feols( Under_5_Mort ~ lspline(Female_Prim , 3) +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) + Child_Cov , weights = main_cleared$Population_t , data = main_cleared , vcov = "hetero" )
reg43

etable( reg4 , reg43)

# Simple mult

reg44 <- feols( Under_5_Mort ~ Female_Prim  +  
                  Male_Prim + log(GDPP) +
                  lspline( Birth_Hosp , 66.67 ) 
                , data = main_dt , vcov = "hetero" )

# Interaction

reg45 <- feols( Under_5_Mort ~ Female_Prim   +  
                  Male_Prim + log(GDPP) +
                  lspline( Birth_Hosp , 66.67 )  + Female_Prim*log(GDPP) 
                , data = main_fully_cleared , vcov = "hetero" )

# Weight

reg46 <- feols( Under_5_Mort ~ Female_Prim  +  
                  Male_Prim + log(GDPP) +
                  lspline( Birth_Hosp , 66.67 ) 
                , weights = main_dt$Population_t , data = main_dt , vcov = "hetero" )

# Simple spline

reg47 <- feols( Under_5_Mort ~ lspline(  Female_Prim , 3 ) +  
                  Male_Prim + log(GDPP) +
                  lspline( Birth_Hosp , 66.67 )  
                 , data = main_fully_cleared , vcov = "hetero" )

etable( reg44 , reg45 , reg46 , reg47 )

# Weighted splin

reg48 <- feols( Under_5_Mort ~ lspline(  Female_Prim , 3 ) +  
                  Male_Prim + log(GDPP) +
                  lspline( Birth_Hosp , 66.67 )  
                , weights = main_fully_cleared$Population_t , data = main_fully_cleared , vcov = "hetero" )

reg48

reg5<- feols( Under_5_Mort ~ Female_Prim_Co +  
                Male_Prim_Co + log(GDPP) +
                lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )
reg5


reg6 <- feols( Under_5_Mort ~ Female_Prim +  
                 Male_Prim  + Mother_Cov + Child_Cov + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )
reg6

reg7<- feols( Under_5_Mort ~ Female_Pre_Prim +  
                Male_Pre_Prim + log(GDPP) + Child_Cov  +
                lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )

reg8<- feols( Under_5_Mort ~ Female_Low_Sec +  
                Male_Low_Sec + log(GDPP) + Child_Cov  +
                lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )


reg9<- feols( Under_5_Mort ~ Female_Hi_Sec +  
                Male_Hi_Sec + log(GDPP) + Child_Cov  +
                lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )

etable(reg7 , reg8 , reg9)


reg10<- feols( Under_5_Mort ~ Female_Low_Sec_Co +  
                Male_Low_Sec_Co + log(GDPP) +
                lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )


reg11<- feols( Under_5_Mort ~ Female_Hi_Sec_Co +  
                Male_Hi_Sec_Co + log(GDPP) +
                lspline( Birth_Hosp , 66.67 ) , data = main_cleared , vcov = "hetero" )

etable(reg5 , reg10 , reg11)

feols( Under_5_Mort ~ Male_Prim + Female_Prim , data = main_cleared , vcov = "hetero")

checker <- main_cleared[, .(Under_5_Mort , Female_Prim , Male_Prim)]
feols( Under_5_Mort ~ Female_Prim_Co + Male_Prim_Co , data = main_cleared , vcov = "hetero")
feols( Under_5_Mort ~ Female_Hi_Sec_Co + Male_Hi_Sec_Co , data = main_cleared , vcov = "hetero")
main_cleared$Fema