library(dplyr)

df <- read.csv('./reviews_cleaned.csv', stringsAsFactors=F)
head(df, 5)
df_cal <- df %>% select(., Date)
head(df_cal, 5)
#         Date
# 1 2018-04-01
# 2 2017-11-02
# 3 2017-04-21
# 4 2016-10-27
# 5 2016-01-19
class(df_cal$Date) # [1] "character"
df_cal$Date <- as.Date(df_cal$Date)
class(df_cal$Date) # [1] "Date"

df_cal <- df_cal %>% group_by(., Date) %>% summarise(., count = n())
head(df_cal, 5)
# A tibble: 5 x 2
#   Date       count
#   <date>     <int>
# 1 2014-04-03     4
# 2 2014-04-04     4
# 3 2014-04-05     5
# 4 2014-04-06     1
# 5 2014-04-07     3

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
calendarHeat(df_cal$Date, df_cal$count, varname="MovingCount")

df_cal[df_cal$count == max(df_cal$count), ]
# Date       count
#   <date>     <int>
# 1 2014-09-03   162
df_cal %>% filter(., grepl('2014', Date, fixed = TRUE)) %>% top_n(3)
# Selecting by count
# A tibble: 3 x 2
# Date       count
# <date>     <int>
# 1 2014-06-02    20
# 2 2014-08-04    26
# 3 2014-09-03   162
df_cal %>% filter(., !grepl('2014', Date, fixed = TRUE)) %>% top_n(3)
# A tibble: 3 x 2
#   Date       count
#   <date>     <int>
# 1 2017-06-06    16
# 2 2017-06-26    21
# 3 2018-07-02    17
'2015-01-01' > '2014-12-31' # [1] TRUE

r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
g2r <- c("#B5E384", "#FFFFBD", "#FFAE63", "#D61818") #green to red

w2r <- c("#FFFFBD", "#FFAE63", "#D61818") #white to red


# my_g2r<- c("#99CCFF", "#B5E384", "#FFFFBD", "#FFAE63", "#FFAE63", "#D61818", "#D61818")
my_g2r<- c("#CCFFCC", "#FFAE63", "#FFAE63", "#D61818", "#D61818")

df_cal_edit <- df_cal %>% 
  filter(., count <= 15) %>% 
  mutate(., count = ifelse(Date > '2015-04-01' & Date <= '2015-09-20', count*2.3, count)) %>% 
  mutate(., count = ifelse(Date > '2016-04-01' & Date <= '2016-09-20', count*2.2, count)) %>% 
  mutate(., count = ifelse(Date > '2017-03-05' & Date <= '2017-09-20', count*2, count)) %>% 
  mutate(., count = ifelse(Date > '2018-04-05', count*2.3, count))
calendarHeat(df_cal_edit$Date, df_cal_edit$count, varname="Moving Count", color = 'my_g2r') # 'r2b'

