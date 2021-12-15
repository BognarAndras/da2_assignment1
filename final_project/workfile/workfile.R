
library(data.table)

life_exp_dt <- read.csv("clean/LIFE_EXP-GLOBAL_DATAFLOW_2015-2018-clean.csv")
educ_dt <- read.csv("clean/Table-10-Education-EN-clean.csv")
soc_dt <- read.csv("clean/Table-12-SocProt-and-Econ-EN-clean.csv")
econ_dt <- read.csv("clean/Table-15-SocProt-and-Econ-EN-clean.csv")
mort_dt <- read.csv("clean/Table-2-Child-Mortality-EN-clean.csv")
health_dt <- read.csv("clean/Table-3-MNH-EN-2.csv")

all(econ_dt$ï..Country == health_dt$ï..Country)

econ_dt <- data.table(econ_dt)
econ_dt <- econ_dt[, .('Country' = ï..Country , 'Soc_Prot' = On.social.protection) ]

life_exp_dt <- data.table(life_exp_dt)
life_exp_dt <- life_exp_dt[, .('Country' = ï..Country , Life_exp ) ]

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

educ_dt[!(Country %in% life_exp_dt$Country ) , .(Country , Female_Prim)]
# All missing except Cook Islands, Dominica, Liechtenstein , Marshall Islands
# Nauru , San 
# https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=DM-LI-MH-NR-SM
# drop missing
life_exp_dt <- life_exp_dt[(Country %in% educ_dt$Country )  ]

temp_dt <- Reduce(merge,list(educ_dt , econ_dt , health_dt , mort_dt , soc_dt))
main_dt <-  merge( life_exp_dt , temp_dt , by = 'Country')

rm( list = c('temp_dt' , 'life_exp_dt' , 'educ_dt' , 'econ_dt' , 
            'health_dt' , 'mort_dt' , 'soc_dt') )

main_dt$Male_Low_Sec <- as.numeric(main_dt$Male_Low_Sec)
main_dt2 <- main_dt
main_dt2[,2:22] <- lapply(main_dt[,2:22] , function(x){as.numeric(x)})

table(main_dt2$Female_Prim)

library(ggplot2)

ggplot(main_dt2) +
  geom_histogram( aes( x= Female_Prim) )

