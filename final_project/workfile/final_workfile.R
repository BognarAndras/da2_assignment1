rm(list = ls () )
library(data.table)
library(lspline)
library(fixest)
library(modelsummary)
library(reshape2)
library(tidyverse)

# Tables used in final analysis

educ_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/Table-10-Education-EN-clean.csv")
mort_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/Table-2-Child-Mortality-EN-clean.csv")
health_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/Table-3-MNH-EN-2.csv")
pop_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/Population_2018.csv")
soc_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/Table-12-SocProt-and-Econ-EN-clean.csv")

# Tables used only in data exploration

life_exp_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/LIFE_EXP-GLOBAL_DATAFLOW_2015-2018-clean.csv")
life_exp_wb_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/World_Bank_Life_Expectancy.csv")
econ_dt <- read.csv("https://raw.githubusercontent.com/BognarAndras/da2_assignment1/main/final_project/clean/Table-15-SocProt-and-Econ-EN-clean.csv")


# Data cleaning, naming columns

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

mort_dt <- data.table(mort_dt)
mort_dt <- mort_dt[, .('Country' = ï..Country , 'Under_5_Mort' = X2018) ]


health_dt <- data.table(health_dt)
health_dt <- health_dt[, .('Country' = ï..Country , 'Birth_Hosp' = Insitutional.Delivery) ]

pop_dt <- data.table(pop_dt)
pop_dt <- pop_dt[, .('Country' = ï..Geographic.area , 'Population_t' = Population_thousands ) ]

soc_dt <- data.table(soc_dt)
soc_dt <- soc_dt[, .('Country' = ï..Country , 'GDPP' = GDP.per.capita ,
                     'Mother_Cov' = Mothers.benefit , 'Child_Cov' = Childrens.benefit ) ]

# Appendix tables

life_exp_dt <- data.table(life_exp_dt)
life_exp_dt <- life_exp_dt[, .('Country' = ï..Country , Life_exp ) ]

life_exp_wb_dt <- data.table(life_exp_wb_dt)
life_exp_wb_dt <- life_exp_wb_dt[, .('Country' = ï..Country.Name , 'Life_exp' = X2018_or_latest ) ]

econ_dt <- data.table(econ_dt)
econ_dt <- econ_dt[, .('Country' = ï..Country , 'Soc_Prot' = On.social.protection ,
                       'Soc_Budget_Gov' = From_GDP) ]

# Data cleaning , merging, drop missing , turn numeric

# Merging 5 main tables, all have Country columns

main_dt <- Reduce(merge,list(educ_dt , mort_dt , health_dt , pop_dt , soc_dt))

# Missing life expectancy data added from World Bank

life_exp_only_wb <- life_exp_wb_dt[!(Country %in% life_exp_dt$Country)]
life_exp_dt <- rbind(life_exp_dt , life_exp_only_wb)

# Adding other 2 potential tables to appendix

appendix_dt <-  merge( life_exp_dt , main_dt , by = 'Country')
appendix_dt <-  merge( econ_dt , appendix_dt , by = 'Country')

rm( list = c( 'life_exp_dt' , 'educ_dt' , 'econ_dt' , 'pop_dt' , 
             'health_dt' , 'mort_dt' , 'soc_dt' , 'life_exp_only_wb' ,
             'life_exp_wb_dt' ))

# Only keep cols used in final analysis. Also reorder columns

main_dt <- main_dt[ , .(Country , Under_5_Mort , Female_Prim , Male_Prim ,
                        GDPP , Birth_Hosp , Population_t , 
                        Female_Low_Sec , Male_Low_Sec , Female_Prim_Co , Male_Prim_Co )]


# Turn all columns numeric except Country. This also replaces .csv missing values
# with NAs

main_dt[,2:11] <- lapply(main_dt[,2:11] , function(x){as.numeric(x)})

# Different missing values removed for robustness check tables.

robust_secondary_dt <- main_dt[!is.na(Female_Low_Sec) & !is.na(Male_Low_Sec)
                               & !is.na(Under_5_Mort) 
                               & !is.na(Birth_Hosp) & !is.na(GDPP) ]

# N = 113

robust_completion_dt <- main_dt[!is.na(Female_Prim_Co) & !is.na(Male_Prim_Co)
                               & !is.na(Under_5_Mort) 
                               & !is.na(Birth_Hosp) & !is.na(GDPP) ]

# N = 97

# Remove missing observations from main variables. N = 202

main_dt <- main_dt[!is.na(Female_Prim) & !is.na(Male_Prim)
                   & !is.na(Under_5_Mort) 
                   & !is.na(Birth_Hosp) & !is.na(GDPP) ]

# 131 observations remains 

# EDA - Appendix

# Data Summary

datasummary_skim( main_dt )

# Relatively high variation on main variables
# Mortality, Education, GDPP are right skewed
# Hospitalization is left skewed, less variation


Missing <- name <- function(x) {sum(is.na(x))}
P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
Unique <- name <- function(x) {Unique(is.na(x))}
datasummary(  Under_5_Mort + Female_Prim + Male_Prim  + GDPP 
              + Birth_Hosp + Population_t
              ~ N + Missing + Mean + Median + SD + Min + Max + P05 + P95 , data = main_dt)

# Distributions


main_dt %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Check potential log

g1 <- ggplot( main_dt , aes(x = Female_Prim)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Out of school rate female') +
  theme_bw()

g2 <- ggplot( main_dt , aes(x = Male_Prim)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Out of school rate male') +
  theme_bw()

g3 <- ggplot( main_dt , aes(x = Under_5_Mort)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Child Mortality Under 5 years (out of 1000 births)') +
  theme_bw()

g4 <- ggplot( main_dt , aes(x = log(GDPP))) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'Log GDP per capita (thousand USD)') +
  theme_bw()

library(ggpubr)

ggarrange(g1, g2, g3,  g4,
          hjust = -0.6,
          ncol = 4, nrow = 1)

ggplot( main_dt , aes(x = Birth_Hosp)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = 'GDP per capita (thousand USD)') +
  theme_bw()

main_logged <- main_dt
main_logged$logy <- log(main_logged$Under_5_Mort) 



# Correlation - Appendix

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

rm( cT , main_num )


# Loess

chck_sp <- function( x_var , x_lab ){
  ggplot( main_dt , aes(x = x_var, y = Under_5_Mort)) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(x = x_lab, y = "Under 5 Mortality in thousand") +
    theme_bw()
}

chck_sp(main_dt$Female_Prim , "Percantage of girls out of Primary school") +
  scale_x_continuous(breaks = seq(0,75,by=3))

main_dt[Female_Prim < 3 , .(Country, Female_Prim , Under_5_Mort)]
main_dt[Female_Prim > 45 , .(Country, Female_Prim , Under_5_Mort , Population_t)]
main_dt[ Country %in% c("Eritrea" , "Liberia" ) ]
# spline 0 to 3 (to 45), only 5 countries above 45, only 0 to 3
main_cleared[Female_Prim > 40 , .(Country , Female_Prim , Under_5_Mort)]


chck_sp(main_dt$Male_Prim , "Percantage of boys out of Primary school") +
  scale_x_continuous(breaks = seq(0,65,by=5))
main_dt[Male_Prim > 45 , .(Country, Male_Prim , Under_5_Mort , Population_t)]

chck_sp(log(main_dt$GDPP) , "Log GDPP")
# spline 11 maybe

chck_sp(main_dt$Birth_Hosp , "Birth in Hospitals %") + scale_x_continuous(breaks = seq(0,100,by=5))
# Spline 66.67


# Logged loess

chck_sp_ln <- function( x_var , x_lab ){
  ggplot( main_dt , aes(x = x_var, y = log(Under_5_Mort))) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(x = x_lab, y = "Log Under 5 Mortality in thousand") +
    theme_bw()
}

chck_sp_ln(main_dt$Female_Prim , "Percantage of girls out of Primary school") +
  scale_x_continuous(breaks = seq(0,75,by=3))


chck_sp_ln(main_dt$Male_Prim , "Percantage of boys out of Primary school") +
  scale_x_continuous(breaks = seq(0,65,by=5))

chck_sp_ln(log(main_dt$GDPP) , "Log GDPP")
chck_sp_ln(main_dt$Birth_Hosp , "Birth in Hospitals %") + scale_x_continuous(breaks = seq(0,100,by=5))



# Models


reg1 <- feols( Under_5_Mort ~ Female_Prim , data = main_dt , vcov = "hetero")

reg2 <- feols( Under_5_Mort ~ Male_Prim , data = main_dt , vcov = "hetero")

reg3 <- feols( Under_5_Mort ~ Female_Prim +  
                 Male_Prim , data = main_dt , vcov = "hetero" )

reg4 <- feols( Under_5_Mort ~ lspline(Female_Prim , 3) +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) , data = main_dt , vcov = "hetero" )


reg5 <- feols( Under_5_Mort ~ lspline(Female_Prim , 3) +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 66.67 ) , weights =  main_dt$Population_t ,
               data = main_dt , vcov = "hetero" )

etable( reg1 , reg2 , reg3 , reg4 , reg5  )


 # Log Y

reg11 <- feols( log( Under_5_Mort ) ~ Female_Prim , data = main_dt , vcov = "hetero")

reg21 <- feols( log( Under_5_Mort ) ~ Male_Prim , data = main_dt , vcov = "hetero")

reg31 <- feols( log( Under_5_Mort ) ~ Female_Prim +  
                 Male_Prim , data = main_dt , vcov = "hetero" )

reg41 <- feols( log( Under_5_Mort ) ~ lspline(Female_Prim , 3)  +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 85 ) , data = main_dt , vcov = "hetero" )


reg51 <- feols( log( Under_5_Mort ) ~ lspline(Female_Prim , 3) +  
                 Male_Prim + log(GDPP) +
                 lspline( Birth_Hosp , 85 ) , weights =  main_dt$Population_t ,
               data = main_dt , vcov = "hetero" )

etable( reg11 , reg21 , reg31 , reg41 , reg51  )

etable(reg3 , reg31 , reg4 , reg41)

