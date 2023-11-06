# intro ----
# Clear workspace
rm(list = ls())

# Load packages
pacman::p_load(eurostat, rsdmx, lubridate, sf, scales, ggrepel,
               ggthemes, readxl, conflicted, gghighlight, wesanderson,
               gfonts, RColorBrewer,
               plotly, xts, Benchmarking, viridis, patchwork, ggpattern)

library(tidyverse)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

# Load data
exp <- read.csv("exp.csv", header= T) 
par <- read.csv("par.csv", header= T) 

# Load eurostat datasets
mk <- get_eurostat("apro_mk_colm")
ap <- get_eurostat("apri_ap_ina")
pr_g <- get_eurostat("nrg_pc_203")
pr_e <- get_eurostat("nrg_pc_205")
ppi <- get_eurostat("sts_inpp_a")
CPI <- get_eurostat("prc_hicp_aind")

# Extract variables
mk_col <- mk %>% filter(dairyprod == 'D1110D', unit == 'THS_T')
mk_col_IE <- mk_col %>% filter(geo == 'IE')
mk_col_DE <- mk_col %>% filter(geo == 'DE')
mk_col_both <- mk_col %>% filter(geo %in% c('DE', 'IE'))

# PPI correction
ppi1 <- ppi %>% filter(unit == 'I15', geo == 'EU27_2020', nace_r2 == 'B')
ppi2 <- ppi %>% filter(unit == 'I15', geo == 'EU27_2020', nace_r2 == 'C2829')
ppi3 <- ppi %>% filter(unit == 'I15', geo == 'EU27_2020', nace_r2 == 'C2013')

ppi1$year <- year(ppi1$time)
ppi2$year <- year(ppi2$time)
ppi3$year <- year(ppi3$time)

# Function to adjust prices based on PPI data
adjust_prices <- function(price_column, year_column, ppi_data) {
   price_column / subset(ppi_data[,'values'], ppi_data[,'year'] == year_column) *
      subset(ppi_data[,'values'], ppi_data[,'year'] == 2022)
}

# List of variables to be adjusted and their corresponding PPI data
vars_to_adjust <- list(
   c("investment_precipitation", "yinvestment_precipitation", "ppi2"),
   c("investment_HTC", "yinvestment_HTC", "ppi2"),
   c("p_sodium_hydroxide", "yp_sodium_hydroxide", "ppi3"),
   c("p_magnesium_sulphate", "yp_magnesium_sulphate", "ppi3"),
   c("p_dipotassium_phosphate", "yp_dipotassium_phosphate", "ppi3"),
   c("p_ammonium_chloride", "yp_ammonium_chloride", "ppi3"),
   c("p_hypochloric_acid", "yp_hypochloric_acid", "ppi3"),
   c("p_magnesium_oxide", "yp_magnesium_oxide", "ppi3"),
   c("p_magnesium_chloride", "yp_magnesium_chloride", "ppi3"),
   c("p_phosphoric_acid", "yp_phosphoric_acid", "ppi3"),
   c("p_oxalate", "yp_oxalate", "ppi3"),
   c("p_sulfuric_acid", "yp_sulfuric_acid", "ppi3")
)

# Iterate through the variables in the vars_to_adjust list and adjust their prices
# using the adjust_prices function
for (vars in vars_to_adjust) {
   var <- vars[1]
   year_var <- vars[2]
   ppi_data <- get(vars[3])
   
   par[[var]] <- adjust_prices(par[[var]], par[[year_var]], ppi_data)
}

# Selection of electricity price
pr_e <- pr_e %>%
   mutate(year = year(time), month = month(time)) 
p_e <- pr_e %>%
   filter(nrg_cons == 'MWH500-1999', currency == 'EUR', geo == 'EU27_2020', tax == 'X_TAX', year == 2022, month == 1) %>%
   pull(values)

# Selection of gas price
pr_g <- pr_g %>%
mutate(year = year(time), month = month(time)) 
p_g <- pr_g %>%
   filter(nrg_cons == 'GJ10000-99999', currency == 'EUR', geo == 'EU27_2020', tax == 'X_TAX', year == 2022, unit == 'KWH', month == 1) %>%
   pull(values)

# Heat requirement calculations in kJ per kg sludge
cp_w <- 4.18
cp_ts <- 1.6
dry_content <- 0.12
reaction_energy <- 500
delta_pressure <- 2.4 * 10^6 / 1000
volume <- 1
compression_energy <- volume * delta_pressure

exp$gas <- exp$HTC * ((exp$HTC_temp - 30) * (cp_w * (1 - dry_content) + cp_ts * dry_content) +
                         reaction_energy + compression_energy) / 3600

# Price Phosphorus triple superphosphate 100 kg in Germany (2021 most recent)
P_prock <- ap %>%
   mutate(year = year(time)) %>%
   filter(prod_inp == '20312200', currency == 'EUR', geo == 'DE', year == 2021) %>%
   pull(values)

# Expenditure per kg sludge calculation
exp$input_ps <- (
   exp$gas * p_g +
      exp$electricity * p_e +
      exp$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
      )
# model ----
#both capacity in t, milk to sludge in kg/t 
exp$yearly_cap <- 1e5
#size consideration more than 50 plants in europe with 
#more than 100 thousand tons of milk production

# Initialize data
dat <- exp

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp)*m)


# Calculate investment costs
exp$In <- exp$HTC * par$investment_HTC[, 1] + exp$precipitation * par$investment_precipitation[, 1]

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * (dat$recovery_ratio /100) * (par$P_per_sludge / 1000)

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

# Calculate revenue (triplesuperphosphate has a phosphate content of 48%) price/kg *kg
dat$R <- dat$rec_p * 0.46 * (P_prock / 100)

# Calculate net revenue
dat$nr <- dat$R - dat$C

# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + par$discount_rate * 0.01) ** -dat$year

# Calculate sum of discounted cash flows
exp$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp$npv <- exp$dcf - exp$In
#exp$npv <- exp$npv$values

# LCOP Levelized cost of phosphorus ----

#discounted the quantities
dat$dq <- (dat$sludge_monthly * (dat$recovery_ratio /100) * (par$P_per_sludge / 1000) *  (1 + par$discount_rate * 0.01) ** -dat$year)
#discount the costs 
dat$dcq <-  (dat$sludge_monthly * dat$input_ps) * ((1 + par$discount_rate * 0.01) ** -dat$year) 
#sum discounted costs and Investment
exp$sumdcq <- aggregate(dat$dcq, by = list(dat$index), FUN = sum)$x + exp$In
#sum discounted quantities
exp$sumdq <- aggregate(dat$dq, by = list(dat$index), FUN = sum)$x

exp$LCOP <- (exp$sumdcq /exp$sumdq)
#%>%   pull(values)
exp$mr<- (exp$recovery_ratio /100) * (par$P_per_sludge / 1000) * (0.46 * (P_prock / 100))
exp$mp <- exp$mr - exp$input_ps

# calculations sensitivity ----
#extract for sensitivity
ap <- ap %>%
mutate(year = year(time), month = month(time)) 
p_price <-  ap %>% 
   filter(prod_inp == '20312200', currency == 'EUR', geo =='DE')%>%
   rename(phos_price = values)

e_price <- filter(pr_e, nrg_cons == 'MWH500-1999', currency == 'EUR',
                  geo == 'EU27_2020',tax == 'X_TAX', month == '1') %>%
   rename(elec_price = values)

pr_g <- pr_g %>%
   filter(nrg_cons == 'GJ10000-99999', currency == 'EUR', geo =='EU27_2020',
          tax == 'X_TAX', unit == 'KWH', month == '1') %>%
   rename(gas_price = values)


CPI   <- CPI %>%
   filter(coicop == 'CP00', unit == 'INX_A_AVG', geo =='EU') %>% 
   rename(infl_index = values)

CPI$year <- year(CPI$time)

#check if key is uniqe
CPI %>%
   count(year) %>%
   filter(n > 1)
price <- full_join(p_price, e_price, by = "year")
price <- full_join(price,   pr_g, by = "year")
price <- full_join(price,   CPI, by = "year")

price <- price %>% select(year, phos_price, elec_price, gas_price, infl_index)

price$phos_real <- with(price, phos_price / infl_index * infl_index[[1]])
price$elec_real <- with(price, elec_price / infl_index * infl_index[[1]])
price$gas_real <- with(price, gas_price /   infl_index * infl_index[[1]])

high_p <- quantile(price$phos_real, .9,na.rm =T)
low_p  <- quantile(price$phos_real, .1,na.rm =T)

# price for a kg of pure 
0.46 * (P_prock / 100)
0.46 * (high_p / 100)
0.46 * (low_p  / 100)

high_elec <- quantile(price$elec_real, .9,na.rm =T)
high_gas <- quantile(price$gas_real,  .9,na.rm =T)


# S1 ----
#double size of dairy processor

rm(list = c("dat")) 
exp1 <- exp
#expenditure per kg sludge
exp1$input_ps <- (
      exp1$gas * p_g +
      exp1$electricity * p_e +
      exp1$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp1$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp1$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp1$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp1$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp1$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp1$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp1$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp1$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp1$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 

exp1$yearly_cap <- 2*1e5

# Initialize data
dat <- exp1

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp1))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp1)*m)


# Calculate investment costs
exp1$In <- exp1$HTC * par$investment_HTC[, 1] + exp1$precipitation * par$investment_precipitation[, 1]

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * par$P_per_sludge / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

# Calculate revenue (triplesuperphosphate has a phosphate content of 48%)
dat$R <- dat$rec_p * 0.46 * P_prock / 100

# Calculate net revenue
dat$nr <- dat$R - dat$C

# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + par$discount_rate * 0.01) ** -dat$year

# Calculate sum of discounted cash flows
exp1$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp1$npv1 <- exp1$dcf - exp1$In
#exp1$npv1 <- exp1$npv$values


# S2 ----
#milk to sludge ratio
rm(list = c("dat")) 
exp2 <- exp
#expenditure per kg sludge
exp2$input_ps <- (
      exp2$gas * p_g +
      exp2$electricity * p_e +
      exp2$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp2$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp2$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp2$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp2$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp2$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp2$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp2$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp2$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp2$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
      )

#both capacity in t, milk to sludge in kg/t 
exp2$yearly_cap <- 1e5

# Initialize data
dat <- exp2

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp2))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp2)*m)


# Calculate investment costs
exp2$In <- exp2$HTC * par$investment_HTC[, 1] + exp2$precipitation * par$investment_precipitation[, 1]
#creates monthly sludge quantities in kg
#change here ######
milk_to_sludge <- 18
# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * par$P_per_sludge / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

# Calculate revenue (triplesuperphosphate has a phosphate content of 48%)
dat$R <- dat$rec_p * 0.46 * P_prock / 100

# Calculate net revenue
dat$nr <- dat$R - dat$C

# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + par$discount_rate * 0.01) ** -dat$year

# Calculate sum of discounted cash flows
exp2$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp2$npv2 <- exp2$dcf - exp2$In
#exp2$npv2 <- exp2$npv$values

# S3 ----
#double discount rate
rm(list = c("dat")) 
exp3 <- exp
#expenditure per kg sludge
exp3$input_ps <- (
      exp3$gas * p_g +
      exp3$electricity * p_e +
      exp3$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp3$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp3$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp3$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp3$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp3$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp3$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp3$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp3$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp3$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 
exp3$yearly_cap <- 1e5

# Initialize data
dat <- exp3

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp3))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp3)*m)

# Calculate investment costs
exp3$In <- exp3$HTC * par$investment_HTC[, 1] + exp3$precipitation * par$investment_precipitation[, 1]

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * par$P_per_sludge / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

# Calculate revenue (triplesuperphosphate has a phosphate content of 48%)
dat$R <- dat$rec_p * 0.46 * P_prock / 100

# Calculate net revenue
dat$nr <- dat$R - dat$C
# change here
############# interest rate doubled
# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + 0.06) ** -dat$year

# Calculate sum of discounted cash flows
exp3$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp3$npv3 <- exp3$dcf - exp3$In
#exp3$npv3 <- exp3$npv$values

# S4 ----
#include revenue 
rm(list = c("dat")) 
exp4 <- exp
#expenditure per kg sludge
exp4$input_ps <- (
   exp4$gas * p_g +
      exp4$electricity * p_e +
      exp4$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp4$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp4$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp4$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp4$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp4$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp4$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp4$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp4$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp4$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 
exp4$yearly_cap <- 1e5

# Initialize data
dat <- exp4

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp4))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp4)*m)

# Calculate investment costs
exp4$In <- exp4$HTC * par$investment_HTC[, 1] + exp4$precipitation * par$investment_precipitation[, 1]
#creates monthly sludge quantities in kg

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * par$P_per_sludge / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

#revenues triplesuperphosphate has a phosphate content of 48 %
# change here
dat$R <-  dat$rec_p * 0.46 * P_prock /100 + dat$sludge_monthly * par$disposal/1000
#triplesuperphosphate has a phosphate content of 48 %

# Calculate net revenue
dat$nr <- dat$R - dat$C
# change here
############# interest rate doubled
# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + 0.06) ** -dat$year

# Calculate sum of discounted cash flows
exp4$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp4$npv4 <- exp4$dcf - exp4$In
#exp4$npv4 <- exp4$npv$values

# S5 ----
#high phosphorus price 
rm(list = c("dat")) 
exp5 <- exp
#expenditure per kg sludge
exp5$input_ps <- (
   exp5$gas * p_g +
      exp5$electricity * p_e +
      exp5$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp5$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp5$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp5$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp5$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp5$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp5$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp5$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp5$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp5$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 
exp5$yearly_cap <- 1e5

# Initialize data
dat <- exp5

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp5))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp5)*m)


# Calculate investment costs
exp5$In <- exp5$HTC * par$investment_HTC[, 1] + exp5$precipitation * par$investment_precipitation[, 1]
#creates monthly sludge quantities in kg

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

#revenues triplesuperphosphate has a phosphate content of 48 %
# change here
dat$R <-  dat$rec_p * 0.46 * high_p  /100
#triplesuperphosphate has a phosphate content of 48 %


# Calculate net revenue
dat$nr <- dat$R - dat$C
# change here
############# interest rate doubled
# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + 0.06) ** -dat$year

# Calculate sum of discounted cash flows
exp5$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp5$npv5 <- exp5$dcf - exp5$In
#exp5$npv5 <- exp5$npv$values
# S6 ----
#low phosphorus price 
rm(list = c("dat")) 
exp6 <- exp
#expenditure per kg sludge
exp6$input_ps <- (
   exp6$gas * p_g +
      exp6$electricity * p_e +
      exp6$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp6$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp6$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp6$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp6$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp6$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp6$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp6$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 
exp6$yearly_cap <- 1e5

# Initialize data
dat <- exp6

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp6))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp6)*m)


# Calculate investment costs
exp6$In <- exp6$HTC * par$investment_HTC[, 1] + exp6$precipitation * par$investment_precipitation[, 1]
#creates monthly sludge quantities in kg

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * low_p / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

#revenues triplesuperphosphate has a phosphate content of 48 %
# change here
dat$R <-  dat$rec_p * 0.46 * low_p /100 
#triplesuperphosphate has a phosphate content of 48 %

# Calculate net revenue
dat$nr <- dat$R - dat$C
# change here
############# interest rate doubled
# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + 0.06) ** -dat$year

# Calculate sum of discounted cash flows
exp6$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp6$npv6 <- exp6$dcf - exp6$In
#exp6$npv6 <- exp6$npv$values
# S7 ----
#high energy price
rm(list = c("dat")) 
exp7 <- exp
#expenditure per kg sludge
exp7$input_ps <- (
      exp7$gas * high_gas +
      exp7$electricity * high_elec +
      exp7$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp7$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp7$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp7$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp7$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp7$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp7$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp7$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp7$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp7$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 
exp7$yearly_cap <- 1e5

# Initialize data
dat <- exp7

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp7))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp7)*m)

# Calculate investment costs
exp7$In <- exp7$HTC * par$investment_HTC[, 1] + exp7$precipitation * par$investment_precipitation[, 1]
#creates monthly sludge quantities in kg

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * par$P_per_sludge / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

#revenues triplesuperphosphate has a phosphate content of 48 %
# change here
dat$R <-  dat$rec_p * 0.46 * P_prock /100 
#triplesuperphosphate has a phosphate content of 48 %

# Calculate net revenue
dat$nr <- dat$R - dat$C
# change here
############# interest rate doubled
# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + 0.06) ** -dat$year

# Calculate sum of discounted cash flows
exp7$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp7$npv7 <- exp7$dcf - exp7$In
#exp7$npv7 <- exp7$npv$values
# S8 ----
#high energy and phosphorus price crisis scenario
rm(list = c("dat")) 
exp8 <- exp
#expenditure per kg sludge
exp8$input_ps <- (
   exp8$gas * high_gas +
      exp8$electricity * high_elec +
      exp8$sodium_hydroxide * par$p_sodium_hydroxide[1,] / 1e6 +
      exp8$magnesium_sulphate * par$p_magnesium_sulphate[1,] / 1e6 +
      exp8$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] / 1e6 +
      exp8$ammonium_chloride * par$p_ammonium_chloride[1,] / 1e6 +
      exp8$hypochloric_acid * par$p_hypochloric_acid[1,] / 1e6 +
      exp8$magnesium_oxide * par$p_magnesium_oxide[1,] / 1e6 +
      exp8$magnesium_chloride * par$p_magnesium_chloride[1,] / 1e6 +
      exp8$phosphoric_acid * par$p_phosphoric_acid[1,] / 1e6 +
      exp8$oxalate * par$p_oxalate[1,] / 1e6 + 
      exp8$sulfuric_acid * par$p_sulfuric_acid[1,] / 1e6
)
#both capacity in t, milk to sludge in kg/t 
exp8$yearly_cap <- 1e5

# Initialize data
dat <- exp8

# Add monthly production and month index to the dataset
m <- 12
dat <- dat[rep(1:nrow(dat), each = m), ]
dat$prod_monthly <- dat$yearly_cap / m
dat$i <- rep(1:m, nrow(exp8))

# Duration of investment
n <- 15

# Add years to the dataset
dat <- dat[rep(1:nrow(dat), each = n), ]
dat$year <- rep(1:n, nrow(exp8)*m)

# Calculate investment costs
exp8$In <- exp8$HTC * par$investment_HTC[, 1] + exp8$precipitation * par$investment_precipitation[, 1]
#creates monthly sludge quantities in kg

# Calculate monthly sludge quantities in kg
dat$sludge_monthly <- dat$prod_monthly * par$milk_to_sludge

# Calculate recovered phosphorus in kg
dat$rec_p <- dat$sludge_monthly * dat$recovery_ratio /100 * par$P_per_sludge / 1000

# Calculate total cost per month
dat$C <- dat$sludge_monthly * dat$input_ps

#revenues triplesuperphosphate has a phosphate content of 48 %
# change here
dat$R <-  dat$rec_p * 0.46 * high_p  /100 
#triplesuperphosphate has a phosphate content of 48 %

# Calculate net revenue
dat$nr <- dat$R - dat$C
# change here
############# interest rate doubled
# Calculate discounted net revenue
dat$dnr <- dat$nr * (1 + 0.06) ** -dat$year

# Calculate sum of discounted cash flows
exp8$dcf <- aggregate(dat$dnr, by = list(dat$index), FUN = sum)$x

# Calculate net present value
exp8$npv8 <- exp8$dcf - exp8$In
#exp8$npv8 <- exp8$npv$values

exp <- exp %>%
cbind(exp1$npv1,
      exp2$npv2,
      exp3$npv3,
      exp4$npv4,
      exp5$npv5,
      exp6$npv6,
      exp7$npv7,
      exp8$npv8)
# get the column names of the new columns
new_col_names <- sub(".*\\$", "", names(exp)[(ncol(exp)-7):ncol(exp)])

# rename the new columns with their respective names
names(exp)[(ncol(exp)-7):ncol(exp)] <- new_col_names
# efficiency ----
store <- aggregate(dat$sludge_monthly,by=list(dat$index), FUN=sum)
exp$Ysludge <- store$x

exp$Ygas <- exp$gas * exp$Ysludge
exp$Yelec <- exp$electricity * exp$Ysludge
exp$Yenergy <- exp$Ygas*p_g + exp$Yelec * p_e
exp$Yexp_chem <-    exp$Ysludge *  
   (exp$sodium_hydroxide      * par$p_sodium_hydroxide[1,]      /(10**6) +
       exp$magnesium_sulphate    * par$p_magnesium_sulphate[1,]    /(10**6) +
       exp$dipotassium_phosphate * par$p_dipotassium_phosphate[1,] /(10**6) +
       exp$ammonium_chloride     * par$p_ammonium_chloride[1,]     /(10**6) +
       exp$hypochloric_acid      * par$p_hypochloric_acid[1,]      /(10**6) +
       exp$magnesium_oxide       * par$p_magnesium_oxide[1,]       /(10**6) +
       exp$magnesium_chloride    * par$p_magnesium_chloride[1,]    /(10**6) +
       exp$phosphoric_acid       * par$p_phosphoric_acid[1,]       /(10**6) + 
       exp$oxalate               * par$p_oxalate[1,]               /(10**6) + 
       exp$sulfuric_acid         * par$p_sulfuric_acid[1,]         /(10**6))
exp$Yinv <- exp$In
exp$Yphos <- exp$recovery_ratio * exp$Ysludge * par$P_per_sludge / 1000

xMat <- cbind(exp$Yenergy, exp$Yexp_chem, exp$Yinv)


yVec <- exp$Yphos
deaCrsIn <- dea( xMat, yVec, RTS = "crs" )

exp$effcrs <- eff(deaCrsIn)

PEERS <- deaCrsIn %>%
   peers()
   
V_peers <- PEERS[,1]

V_peers <- unique(V_peers)

peer.exp <- exp %>%
   filter(!is.na(match(index, V_peers)))
peer.exp$group <- 1:nrow(peer.exp)

facet.exp <- pivot_longer(peer.exp, cols=c(25,31:38), names_to = "sce", values_to="npv_l")

input.exp <- pivot_longer(exp, cols=c(42:44), names_to ="input", values_to="value")

options(scipen = 999)
huhn  <-c(max(exp$sodium_hydroxide      ),
         max(exp$magnesium_sulphate),
         max(exp$dipotassium_phosphate),
         max(exp$ammonium_chloride),
         max(exp$hypochloric_acid),
         max(exp$magnesium_oxide),
         max(exp$magnesium_chloride),
         max(exp$phosphoric_acid),
         max(exp$oxalate),
         max(exp$sulfuric_acid)) 
as.numeric(huhn * dat$sludge_monthly[1] * 12  / 1e6)
