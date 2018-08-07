library(dplyr)
library(ggplot2)
library(GGally)

df <- read.csv('./reviews_cleaned.csv', stringsAsFactors=F)
colnames(df)
# [1] "Company"       "Username"      "City"          "State"         "Date"          "Rating"       
# [7] "Review"        "Distance"      "Cost"          "Cost.per.Mile" "Sqft.Moved"
dim(df) # [1] 5197   11
df <- df %>% select(., Company, Rating, Cost.per.Mile, Sqft.Moved, Distance, Cost)
df <- df[complete.cases(df), ]
dim(df) # [1] 3904    6

df3 <- df %>% filter(., Company != 'Enterprise' & Company != 'Ryder') %>%
  mutate(., distance_type = ifelse(Distance <= 50, 'local', ifelse(Distance > 250, 'long', 'medium')))
dim(df3) # [1] 3883    7
head(df3, 4)
#   Company Rating Cost.per.Mile Sqft.Moved Distance Cost distance_type
# 1  Budget      3          1.36       2000      110  150        medium
# 2  Budget      5          2.00       2000      300  600          long
# 3  Budget      4          2.67       1300      450 1200          long
# 4  Budget      1          0.50       2500      600  300          long

df3_summary <- df3 %>% group_by(., Company, distance_type) %>% 
  summarise(., count = n(), Avg.Rating = mean(Rating), Avg.Sqft = mean(Sqft.Moved), 
            Avg.Cost = mean(Cost), Avg.Distance = mean(Distance), Avg.CpM = mean(Cost.per.Mile))

df3_summary
# # A tibble: 9 x 8
# Groups:   Company [?]
#   Company distance_type count Avg.Rating Avg.Sqft Avg.Cost Avg.Distance Avg.CpM
#   <chr>   <chr>         <int>      <dbl>    <dbl>    <dbl>        <dbl>   <dbl>
# 1 Budget  local           193       3.90    1392.     156.         18.2   21.1 
# 2 Budget  long            422       3.74    1385.     953.        945.     1.13
# 3 Budget  medium          122       3.79    1259.     291.        163.     1.98
# 4 Penske  local           182       4.5     1435.     172.         18.0   19.4 
# 5 Penske  long            797       4.43    1603.    1217.       1064.     1.30
# 6 Penske  medium          122       4.58    1413.     374.        149.     2.78
# 7 U-Haul  local           955       4.35    1415.     135.         17.4   16.6 
# 8 U-Haul  long            697       4.16    1512.    1065.        816.     1.39
# 9 U-Haul  medium          393       4.17    1382.     291.        140.     2.18

uhaul_loc <- df3 %>% filter(., Company == 'U-Haul' & distance_type == 'local')
uhaul_med <- df3 %>% filter(., Company == 'U-Haul' & distance_type == 'medium')
uhaul_lon <- df3 %>% filter(., Company == 'U-Haul' & distance_type == 'long')

penske_loc <- df3 %>% filter(., Company == 'Penske' & distance_type == 'local')
penske_med <- df3 %>% filter(., Company == 'Penske' & distance_type == 'medium')
penske_lon <- df3 %>% filter(., Company == 'Penske' & distance_type == 'long')

budget_loc <- df3 %>% filter(., Company == 'Budget' & distance_type == 'local')
budget_med <- df3 %>% filter(., Company == 'Budget' & distance_type == 'medium')
budget_lon <- df3 %>% filter(., Company == 'Budget' & distance_type == 'long')

u_loc_b1 = sum((uhaul_loc$Cost - mean(uhaul_loc$Cost)) * (uhaul_loc$Distance - mean(uhaul_loc$Distance))) /
  sum((uhaul_loc$Distance - mean(uhaul_loc$Distance))^2)
u_loc_b0 = mean(uhaul_loc$Cost) - u_loc_b1*mean(uhaul_loc$Distance)

u_med_b1 = sum((uhaul_med$Cost - mean(uhaul_med$Cost)) * (uhaul_med$Distance - mean(uhaul_med$Distance))) /
  sum((uhaul_med$Distance - mean(uhaul_med$Distance))^2)
u_med_b0 = mean(uhaul_med$Cost) - u_med_b1*mean(uhaul_med$Distance)

u_lon_b1 = sum((uhaul_lon$Cost - mean(uhaul_lon$Cost)) * (uhaul_lon$Distance - mean(uhaul_lon$Distance))) /
  sum((uhaul_lon$Distance - mean(uhaul_lon$Distance))^2)
u_lon_b0 = mean(uhaul_lon$Cost) - u_lon_b1*mean(uhaul_lon$Distance)



p_loc_b1 = sum((penske_loc$Cost - mean(penske_loc$Cost)) * (penske_loc$Distance - mean(penske_loc$Distance))) /
  sum((penske_loc$Distance - mean(penske_loc$Distance))^2)
p_loc_b0 = mean(penske_loc$Cost) - p_loc_b1*mean(penske_loc$Distance)

p_med_b1 = sum((penske_med$Cost - mean(penske_med$Cost)) * (penske_med$Distance - mean(penske_med$Distance))) /
  sum((penske_med$Distance - mean(penske_med$Distance))^2)
p_med_b0 = mean(penske_med$Cost) - p_med_b1*mean(penske_med$Distance)

p_lon_b1 = sum((penske_lon$Cost - mean(penske_lon$Cost)) * (penske_lon$Distance - mean(penske_lon$Distance))) /
  sum((penske_lon$Distance - mean(penske_lon$Distance))^2)
p_lon_b0 = mean(penske_lon$Cost) - p_lon_b1*mean(penske_lon$Distance)



b_loc_b1 = sum((budget_loc$Cost - mean(budget_loc$Cost)) * (budget_loc$Distance - mean(budget_loc$Distance))) /
  sum((budget_loc$Distance - mean(budget_loc$Distance))^2)
b_loc_b0 = mean(budget_loc$Cost) - b_loc_b1*mean(budget_loc$Distance)

b_med_b1 = sum((budget_med$Cost - mean(budget_med$Cost)) * (budget_med$Distance - mean(budget_med$Distance))) /
  sum((budget_med$Distance - mean(budget_med$Distance))^2)
b_med_b0 = mean(budget_med$Cost) - b_med_b1*mean(budget_med$Distance)

b_lon_b1 = sum((budget_lon$Cost - mean(budget_lon$Cost)) * (budget_lon$Distance - mean(budget_lon$Distance))) /
  sum((budget_lon$Distance - mean(budget_lon$Distance))^2)
b_lon_b0 = mean(budget_lon$Cost) - b_lon_b1*mean(budget_lon$Distance)


u_loc_b1 # [1] 3.275865
p_loc_b1 # [1] 1.519525
b_loc_b1 # [1] 1.388352

u_med_b1 # [1] 1.568722
p_med_b1 # [1] 1.610588
b_med_b1 # [1] 0.6574091

u_lon_b1 # [1] 0.9762844
p_lon_b1 # [1] 0.6243064
b_lon_b1 # [1] 0.5773355

u_loc_b0 # [1] 77.91029
p_loc_b0 # [1] 144.2997
b_loc_b0 # [1] 130.5815

u_med_b0 # [1] 71.09208
p_med_b0 # [1] 133.7092
b_med_b0 # [1] 183.9075

u_lon_b0 # [1] 268.1307
p_lon_b0 # [1] 553.1557
b_lon_b0 # [1] 407.3246

model_u_loc = lm(Cost ~ Distance, data = uhaul_loc)
confint(model_u_loc)
#                 2.5 %     97.5 %
# (Intercept) 43.275516 112.545062
# Distance     1.723214   4.828517
model_u_med = lm(Cost ~ Distance, data = uhaul_med)
confint(model_u_med)
#                2.5 %     97.5 %
# (Intercept) 6.006113 136.178045
# Distance    1.139608   1.997836
model_u_lon = lm(Cost ~ Distance, data = uhaul_lon)
confint(model_u_lon)
#                   2.5 %     97.5 %
# (Intercept) 181.4377730 354.823577
# Distance      0.8876355   1.064933


model_p_loc = lm(Cost ~ Distance, data = penske_loc)
confint(model_p_loc)
#                  2.5 %     97.5 %
# (Intercept) 95.0006540 193.598842
# Distance    -0.6637927   3.702844
model_p_med = lm(Cost ~ Distance, data = penske_med)
confint(model_p_med)
#                   2.5 %     97.5 %
# (Intercept) -11.7272126 279.145593
# Distance      0.7072642   2.513913
model_p_lon = lm(Cost ~ Distance, data = penske_lon)
confint(model_p_lon)
#                   2.5 %      97.5 %
# (Intercept) 452.5669119 653.7445414
# Distance      0.5432162   0.7053967

model_b_loc = lm(Cost ~ Distance, data = budget_loc)
confint(model_b_loc)
#                  2.5 %     97.5 %
# (Intercept) 87.0516697 174.111292
# Distance    -0.5041655   3.280869
model_b_med = lm(Cost ~ Distance, data = budget_med)
confint(model_b_med)
#                  2.5 %    97.5 %
# (Intercept) 93.5045171 274.31051
# Distance     0.1354583   1.17936
model_b_lon = lm(Cost ~ Distance, data = budget_lon)
confint(model_b_lon)
#                   2.5 %      97.5 %
# (Intercept) 285.5021014 529.1470332
# Distance      0.4669667   0.6877043



u_loc_b1 # [1] 3.275865     1.723214    4.828517  exp
p_loc_b1 # [1] 1.519525    -0.6637927   3.702844
b_loc_b1 # [1] 1.388352    -0.5041655   3.280869
#---
u_loc_b0 # [1] 77.91029    43.275516  112.545062  che
p_loc_b0 # [1] 144.2997    95.0006540 193.598842
b_loc_b0 # [1] 130.5815    87.0516697 174.111292


u_med_b1 # [1] 1.568722     1.139608    1.997836
p_med_b1 # [1] 1.610588     0.7072642   2.513913
b_med_b1 # [1] 0.6574091    0.1354583   1.17936   che
#---
u_med_b0 # [1] 71.09208     6.006113  136.178045
p_med_b0 # [1] 133.7092   -11.7272126 279.145593
b_med_b0 # [1] 183.9075    93.5045171 274.31051


u_lon_b1 # [1] 0.9762844    0.8876355   1.064933  exp
p_lon_b1 # [1] 0.6243064    0.5432162   0.7053967
b_lon_b1 # [1] 0.5773355    0.4669667   0.6877043
#---
u_lon_b0 # [1] 268.1307   181.4377730 354.823577  che
p_lon_b0 # [1] 553.1557   452.5669119 653.7445414 exp
b_lon_b0 # [1] 407.3246   285.5021014 529.1470332

