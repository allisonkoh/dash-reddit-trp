# Reconfiguring HM data 
pacman::p_load(tidyverse,gridExtra,treemapify,ggfittext)

hmc.df <- read_csv("/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/hmdf_c.csv")

hms.df <- read_csv("/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/hmdf_s.csv")

con.df <- read_csv("/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/hmdf_con.csv")

hmc1.df <- hmc.df %>% 
  complete(year = 2008:2020, month) 
hmc1.df[is.na(hmc1.df)] <- 0

hms1.df <- hms.df %>% 
  complete(year = 2008:2020, month) 
hms1.df[is.na(hms1.df)] <- 0

con1.df <- con.df %>% 
  complete(year = 2008:2020, month) 
con.df1[is.na(con1.df)] <- 0

hmc1.df$month_num = hmc1.df$month
hms1.df$month_num = hms1.df$month
con1.df$month_num = con1.df$month

hmc1.df <- hmc1.df %>% 
  mutate(month=case_when(
    month_num=="01" ~ "January",
    month_num=="02" ~ "February",
    month_num=="03" ~ "March",
    month_num=="04" ~ "April",
    month_num=="05" ~ "May",
    month_num=="06" ~ "June",
    month_num=="07" ~ "July",
    month_num=="08" ~ "August",
    month_num=="09" ~ "September",
    month_num=="10" ~ "October",
    month_num=="11" ~ "November",
    month_num=="12" ~ "December"
  ))

hms1.df <- hms1.df %>% 
  mutate(month=case_when(
    month_num=="01" ~ "January",
    month_num=="02" ~ "February",
    month_num=="03" ~ "March",
    month_num=="04" ~ "April",
    month_num=="05" ~ "May",
    month_num=="06" ~ "June",
    month_num=="07" ~ "July",
    month_num=="08" ~ "August",
    month_num=="09" ~ "September",
    month_num=="10" ~ "October",
    month_num=="11" ~ "November",
    month_num=="12" ~ "December"
  ))

con1.df <- con1.df %>% 
  mutate(month=case_when(
    month_num=="01" ~ "January",
    month_num=="02" ~ "February",
    month_num=="03" ~ "March",
    month_num=="04" ~ "April",
    month_num=="05" ~ "May",
    month_num=="06" ~ "June",
    month_num=="07" ~ "July",
    month_num=="08" ~ "August",
    month_num=="09" ~ "September",
    month_num=="10" ~ "October",
    month_num=="11" ~ "November",
    month_num=="12" ~ "December"
  ))


hmc1.df$year_num = hmc1.df$year
hms1.df$year_num = hms1.df$year
con1.df$year_num = con1.df$year

hmc1.df <- hmc1.df %>% 
  mutate(year=case_when(
    year_num==2008 ~ ".2008.",
    year_num==2009 ~ ".2009.",
    year_num==2010 ~ ".2010.",
    year_num==2011 ~ ".2011.",
    year_num==2012 ~ ".2012.",
    year_num==2013 ~ ".2013.",
    year_num==2014 ~ ".2014.",
    year_num==2015 ~ ".2015.",
    year_num==2016 ~ ".2016.",
    year_num==2017 ~ ".2017.",
    year_num==2018 ~ ".2018.",
    year_num==2019 ~ ".2019.",
    year_num==2020 ~ ".2020."
  ))

hms1.df <- hms1.df %>% 
  mutate(year=case_when(
    year_num==2008 ~ ".2008.",
    year_num==2009 ~ ".2009.",
    year_num==2010 ~ ".2010.",
    year_num==2011 ~ ".2011.",
    year_num==2012 ~ ".2012.",
    year_num==2013 ~ ".2013.",
    year_num==2014 ~ ".2014.",
    year_num==2015 ~ ".2015.",
    year_num==2016 ~ ".2016.",
    year_num==2017 ~ ".2017.",
    year_num==2018 ~ ".2018.",
    year_num==2019 ~ ".2019.",
    year_num==2020 ~ ".2020."
  ))

con1.df <- con1.df %>% 
  mutate(year=case_when(
    year_num==2008 ~ ".2008.",
    year_num==2009 ~ ".2009.",
    year_num==2010 ~ ".2010.",
    year_num==2011 ~ ".2011.",
    year_num==2012 ~ ".2012.",
    year_num==2013 ~ ".2013.",
    year_num==2014 ~ ".2014.",
    year_num==2015 ~ ".2015.",
    year_num==2016 ~ ".2016.",
    year_num==2017 ~ ".2017.",
    year_num==2018 ~ ".2018.",
    year_num==2019 ~ ".2019.",
    year_num==2020 ~ ".2020."
  ))


con1.df$n1 <- ifelse(!is.na(con1.df$n),1,0)

write_csv(hmc1.df,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/hmdf_c1.csv")
write_csv(hms1.df,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/hmdf_s1.csv")


## Ungrouped 
condf <- read_csv("/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/condf.csv")

by_con <- condf %>% group_by(month,year) %>% summarize(n=n())

by_con1 <- by_con %>% 
  complete(year = 2008:2020, month) 
by_con1[is.na(by_con1)] <- 0

by_con1 <- by_con1 %>% 
  mutate(month1=case_when(
    month=="01" ~ "January",
    month=="02" ~ "February",
    month=="03" ~ "March",
    month=="04" ~ "April",
    month=="05" ~ "May",
    month=="06" ~ "June",
    month=="07" ~ "July",
    month=="08" ~ "August",
    month=="09" ~ "September",
    month=="10" ~ "October",
    month=="11" ~ "November",
    month=="12" ~ "December"
  ))

by_con1$ym <- paste(by_con1$year,by_con1$month,sep="")
by_con1 <- by_con1[order(by_con1$ym),]

write_csv(by_con1, "/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/condf.csv")
View(by_con1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# Creating grouped data for pie charts 
df <- read_csv("/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/dash-input1.csv")
df1 <- read_csv("/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/topics5.csv")

df$id <- df$id2

df2 <- merge(df,df1,by="id",all=TRUE)

df3 <- df2[!is.na(df2$subreddit), ]
df3 <- df3[!duplicated(df3$id), ]

df3 <- df3 %>% 
  mutate(topic3 = case_when(
    Dominant_Topic==0 ~ "1",
    Dominant_Topic==1 ~ "2",
    Dominant_Topic==2 ~ "3",
    Dominant_Topic==3 ~ "3",
    Dominant_Topic==4 ~ "1"
  ))

write_csv(df3,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/topics-input.csv")

trp.df <- subset(df3,subreddit=="TheRedPill")
mgtow.df <- subset(df3,subreddit=="MGTOW")
mr.df <- subset(df3,subreddit=="MensRights")
con.df <- subset(df3,post_type=="con")

trp1.df <- data.frame(table(trp.df$Dominant_Topic))
mgtow1.df <- data.frame(table(mgtow.df$Dominant_Topic))
mr1.df <- data.frame(table(mr.df$Dominant_Topic))
con1.df <- data.frame(table(con.df$Dominant_Topic))

write_csv(trp1.df,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/trp-input.csv")
write_csv(mgtow1.df,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/mgtow-input.csv")
write_csv(mr1.df,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/mr-input.csv")
write_csv(con1.df,"/Users/allisonwun-huikoh/Documents/GitHub/rsub-redpill-analysis/dash-app/data/con-input.csv")

View(mr1.df)
