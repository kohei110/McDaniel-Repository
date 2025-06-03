library(readxl)      
library(fpp3)        
library(patchwork)   
library(tsibble)   # for index_var()
library(ggplot2)
library(feasts)

setwd("~/Workspace/McDaniel-Repository/535/group work")


df <- read_excel(
  "UK_Comp_US_GBRPROINDMISMEI.xlsx",
  col_types = c("date", "numeric", "numeric")  # first col = date
) |>
  rename(Date = 1) 

df <- df |>
  mutate(Date = yearmonth(as_date(Date)))   # 1948 Jan → 1948 Jan (no time)

uk_df  <- df |>                      # UK df
  select(Date, GBRPROINDMISMEI)

us_df  <- df |>                      # US df
  select(Date, USAPROINDMISMEI)

uk_ts  <- df |>                      # UK tisible
  select(Date, GBRPROINDMISMEI) |>
  as_tsibble(index = Date)

us_ts  <- df |>                      # US tisible
  select(Date, USAPROINDMISMEI) |>
  as_tsibble(index = Date)


#############################
# Regression - models
#############################

uk_models <- uk_ts |>
  model(
    lin   = TSLM(GBRPROINDMISMEI ~ trend() + season()),
    quad  = TSLM(GBRPROINDMISMEI ~ trend() + I(trend()^2) + season()),
    cubic = TSLM(GBRPROINDMISMEI ~ trend() + I(trend()^2) + I(trend()^3) + season())
  )
report(uk_models)

uk_models %>%
  accuracy() %>%
  arrange(MAPE)



us_models <- us_ts |>
  model(
    lin   = TSLM(USAPROINDMISMEI ~ trend() + season()),
    quad  = TSLM(USAPROINDMISMEI ~ trend() + I(trend()^2) + season()),
    cubic = TSLM(USAPROINDMISMEI ~ trend() + I(trend()^2) + I(trend()^3) + season())
  )

report(us_models)

us_models %>%
  accuracy() %>%
  arrange(MAPE)

#############################
# Simple Fitting
#############################

uk_aug <- augment(uk_models)   # gives Date, .model, .fitted

# plot data (black) + three trend curves (ggplot picks colours)
ggplot() +
  geom_line(data = uk_ts,  aes(Date, GBRPROINDMISMEI), colour = "black") +
  geom_line(data = uk_aug, aes(Date, .fitted, colour = .model)) +
  labs(title = "UK Industrial Production linear / quad / cubic fits",
       y = "Index value", x = "Year", colour = "Fit")


us_aug <- augment(us_models)   # gives Date, .model, .fitted

# plot data (black) + three trend curves (ggplot picks colours)
ggplot() +
  geom_line(data = us_ts,  aes(Date, USAPROINDMISMEI), colour = "black") +
  geom_line(data = us_aug, aes(Date, .fitted, colour = .model)) +
  labs(title = "US Industrial Production linear / quad / cubic fits",
       y = "Index value", x = "Year", colour = "Fit")

#############################
# US and UK Residuals
#############################


augment(uk_models) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "UK - Residuals")

augment(us_models) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "US - Residuals")


uk_models %>%
  accuracy() %>%
  arrange(MAPE)

us_models %>%
  accuracy() %>%
  arrange(MAPE)


#############################
# US and UK - ACF and ggresidual()
#############################


uk_pred <- augment(uk_models) 
uk_models_tslm_tsb <- uk_pred |>
  filter(.model == "cubic") |>        
  transmute(Date, y = GBRPROINDMISMEI) |> 
  model(
    cubic = TSLM(y ~ trend() + season()
                 + I(trend()^2) + I(trend()^3))
  )

uk_models_tslm_tsb |> gg_tsresiduals()

us_pred <- augment(us_models) 
us_models_tslm_tsb <- us_pred |>
  filter(.model == "cubic") |>        
  transmute(Date, y = USAPROINDMISMEI) |> 
  model(
    cubic = TSLM(y ~ trend() + season()
                 + I(trend()^2) + I(trend()^3))
  )

us_models_tslm_tsb |> gg_tsresiduals()

#############################
# UK - Decompose
#############################

# 1 ─ Decompose ----------------------------------------------------------------
uk_dec <- uk_ts |>                              # your monthly tsibble
  model(classical = classical_decomposition(GBRPROINDMISMEI,
                                            type = "additive")) |>
  components()                                  # returns x, trend, seasonal, remainder

autoplot(uk_dec)                                # equivalent to autoplot(decompose())

# 2 ─ Detrend and/or deseason ---------------------------------------------------
uk_deseason <- uk_dec |>                        # remove seasonal component
  mutate(y_deseason = GBRPROINDMISMEI - seasonal)

uk_detrend  <- uk_deseason |>                   # now remove the trend
  mutate(y_detrend = y_deseason - trend)

# 3 ─ (Optional) Re-decompose the cleaned series -------------------------------
uk_detrend_dec <- uk_detrend |>
  select(Date, y_detrend) |>                    # keep a clean 2-column tsibble
  model(classical = classical_decomposition(y_detrend)) |>
  components()

autoplot(uk_detrend_dec) +
  ggtitle("Decomposition after detrending & deseasonalising")


uk_clean <- uk_dec |>
  mutate(y_detrend = GBRPROINDMISMEI - seasonal - trend) |>
  select(Date, y_detrend) |>
  drop_na()



# 2. KPSS
uk_clean |> features(y_detrend, unitroot_kpss)

# 3. Visual ACF/PACF
uk_clean |> gg_tsdisplay(y_detrend, plot_type = "partial")

#############################
# US - Decompose
#############################

# 1 ─ Decompose ----------------------------------------------------------------
us_dec <- us_ts |>                              
  model(classical = classical_decomposition(USAPROINDMISMEI,
                                            type = "additive")) |>
  components()                                  

autoplot(us_dec)                                

# 2 ─ Detrend and/or deseason ---------------------------------------------------
us_deseason <- us_dec |>                        # remove seasonal component
  mutate(y_deseason = USAPROINDMISMEI - seasonal)

us_detrend  <- us_deseason |>                   # now remove the trend
  mutate(y_detrend = y_deseason - trend)

# 3 ─ (Optional) Re-decompose the cleaned series -------------------------------
us_detrend_dec <- us_detrend |>
  select(Date, y_detrend) |>                    # keep a clean 2-column tsibble
  model(classical = classical_decomposition(y_detrend)) |>
  components()

autoplot(us_detrend_dec) +
  ggtitle("Decomposition after detrending & deseasonalising")


us_clean <- us_dec |>
  mutate(y_detrend = USAPROINDMISMEI - seasonal - trend) |>
  select(Date, y_detrend) |>
  drop_na()


# 2. KPSS
us_clean |> features(y_detrend, unitroot_kpss)

# 3. Visual ACF/PACF
us_clean |> gg_tsdisplay(y_detrend, plot_type = "partial")


#############################
# UK Forecast
#############################

## 1.  Train / test split ----------------------------------------------------
train_uk <- uk_ts |> filter_index("2015 Jan" ~ "2021 Dec") 
test_uk  <- uk_ts |> filter_index("2021 Jan" ~ .)

## 2.  Fit ETS (auto by default) --------------------------------------------
ets_fit_uk <- train_uk |>
  model(ETS(GBRPROINDMISMEI))     

## 3.  Check hold-out accuracy ----------------------------------------------
report(ets_fit_uk)                      # components + information criteria

fc_test_uk <- ets_fit_uk |>           # your trained models
  forecast(new_data = test_uk)   # 1-step-ahead per test obs

accuracy(fc_test_uk, test_uk) |>      # compare fc vs. actual
  arrange(RMSE)

## 2015~2021
# # A tibble: 1 × 10
# .model               .type     ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
# <chr>                <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 ETS(GBRPROINDMISMEI) Test  -0.972  2.50  2.13 -1.01  2.10   NaN   NaN 0.819

## 2015~2019
# # A tibble: 1 × 10
# .model               .type    ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
# <chr>                <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 ETS(GBRPROINDMISMEI) Test  -8.58  8.89  8.58 -8.55  8.55   NaN   NaN 0.819

## 2000~2019
# # A tibble: 1 × 10
# .model               .type    ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
# <chr>                <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 ETS(GBRPROINDMISMEI) Test  -10.6  11.0  10.6 -10.6  10.6   NaN   NaN 0.820

## 4.  Re-fit to ALL observed data (up to 2024 Mar) --------------------------
final_fit_uk <- uk_ts |>                # use full history now
  filter_index(~ "2024 Mar") |>
  model(ETS(GBRPROINDMISMEI))

## 5.  Forecast 2024 Apr → 2028 Jan -----------------------------------------
h <- length(seq(yearmonth("2024 Apr"), yearmonth("2026 Jan"), by = 1))

fc_uk <- final_fit_uk |> forecast(h = h)

## 6.  Visualise -------------------------------------------------------------
history_2019_on_uk <- uk_ts |>                     # keep only the bit you want
  filter_index("2019 Jan" ~ "2024 Mar")

autoplot(fc_uk, history_2019_on_uk) +              # fc_uk already ends 2028 Jan
  labs(title = "UK Industrial Production: ETS(ANN) forecast",
       subtitle = "History 2019-2024 • Forecast 2024-2028",
       y = "Index")


#############################
# US Forecast
#############################

## 1.  Train / test split ----------------------------------------------------
train_us <- us_ts |> filter_index("2015 Jan" ~ "2021 Dec") 
test_us  <- us_ts |> filter_index("2021 Jan" ~ .)

## 2.  Fit ETS (auto by default) --------------------------------------------
ets_fit_us <- train_us |>
  model(ETS(USAPROINDMISMEI))       

## 3.  Check hold-out accuracy ----------------------------------------------
report(ets_fit_us)                      # components + information criteria

fc_test_us <- ets_fit_us |>           # your trained models
  forecast(new_data = test_us)   # 1-step-ahead per test obs

accuracy(fc_test_us, test_us) |>      # compare fc vs. actual
  arrange(RMSE)

## 2015~2021

# A tibble: 1 × 10
# .model               .type    ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
# <chr>                <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 ETS(USAPROINDMISMEI) Test  0.729  1.98  1.75 0.690  1.74   NaN   NaN 0.842

## 2015~2019
# # A tibble: 1 × 10
# .model               .type     ME  RMSE   MAE    MPE  MAPE  MASE RMSSE  ACF1
# <chr>                <chr>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 ETS(USAPROINDMISMEI) Test  -0.261  1.85  1.43 -0.293  1.44   NaN   NaN 0.842


## 2000-2019
# A tibble: 1 × 10
# .model               .type    ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
# <chr>                <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 ETS(USAPROINDMISMEI) Test  0.148  1.95  1.61 0.111  1.62   NaN   NaN 0.850



## 4.  Re-fit to ALL observed data (up to 2024 Mar) --------------------------
final_fit_us <- us_ts |>                # use full history now
  filter_index(~ "2024 Mar") |>
  model(ETS(USAPROINDMISMEI))
  
## 5.  Forecast 2024 Apr → 2028 Jan -----------------------------------------
h <- length(seq(yearmonth("2024 Apr"), yearmonth("2026 Jan"), by = 1))

fc_us <- final_fit_us |> forecast(h = h)

## 6.  Visualise -------------------------------------------------------------
history_2019_on_us <- us_ts |>                     # keep only the bit you want
  filter_index("2019 Jan" ~ "2024 Mar")

autoplot(fc_us, history_2019_on_us) +              # fc_us already ends 2028 Jan
  labs(title = "us Industrial Production: ETS(ANN) forecast",
       subtitle = "History 2019-2024 • Forecast 2024-2028",
       y = "Index")





#############################################################
#  Forecast *with the 2012-2021 models*  (UK & US together) #
#############################################################
library(dplyr)
library(tidyr)
library(ggplot2)

mk_tbl <- function(fit, h, country, dist_col){
  fit |>
    forecast(h = h) |>
    hilo(level = c(80, 95)) |>
    unpack_hilo(names_sep = "_") |>        # .lower_80 … .upper_95
    select(-all_of(dist_col)) |>           # <-- removes <dist>
    mutate(country = country) |>
    as_tibble()
}

fc_uk_tbl <- mk_tbl(final_fit_uk, h, "UK", "GBRPROINDMISMEI")
fc_us_tbl <- mk_tbl(final_fit_us, h, "US", "USAPROINDMISMEI")

# --- history -------------------------------------------------------
hist_uk_tbl <- history_2019_on_uk |>
  transmute(Date,
            value = GBRPROINDMISMEI,
            country = "UK") |>
  as_tibble()                       # <- lose tsibble-ness

hist_us_tbl <- history_2019_on_us |>
  transmute(Date,
            value = USAPROINDMISMEI,
            country = "US") |>
  as_tibble()

# now they bind cleanly
hist_all <- bind_rows(hist_uk_tbl, hist_us_tbl)
str(hist_all)
# same idea already used for fc_*_tbl
fc_all   <- bind_rows(fc_uk_tbl,  fc_us_tbl)

str(fc_all)

# --- 1. unpack the 80 % interval ---------------------------------
fc_all2 <- fc_all %>%                              # keep original intact
  unpack_hilo(`80%`, names_sep = "_")              # → 80%_lower / 80%_upper

# --- 2. plot history + forecast ----------------------------------
ggplot() +
  geom_line(data = hist_all,                       # past values
            aes(Date, value, colour = country),
            linewidth = 0.6) +
  geom_line(data = fc_all2,                        # forecast means
            aes(Date, .mean, colour = country),
            linewidth = 0.9) +
  geom_ribbon(data = fc_all2,                      # 80 % fan
              aes(Date,
                  ymin = `80%_lower`,
                  ymax = `80%_upper`,
                  fill = country),
              alpha = 0.15, colour = NA) +
  labs(title    = "UK vs US Industrial Production (ETS-ANN forecasts)",
       subtitle = "History 2019-2024  |  Forecast 2024-2028",
       y        = "Index (2015 = 100)",
       colour   = "Country",
       fill     = "Country") +
  scale_colour_manual(values = c(UK = "#1b9e77", US = "#d95f02")) +
  scale_fill_manual(values   = c(UK = "#1b9e77", US = "#d95f02")) +
  theme_minimal()


##########################################################################
# Additional analysis with different sector
##########################################################################


#### US Energy https://fred.stlouisfed.org/series/IPB50089S


data <- read.csv("IPB50089S.csv")

# 3. Inspect structure
glimpse(data)

str(Amtrak)
