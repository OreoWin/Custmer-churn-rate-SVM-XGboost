library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(naniar)
library(GGally)

data_path <- "WA_Fn-UseC_-Telco-Customer-Churn.csv"
telco_raw <- readr::read_csv(data_path, show_col_types = FALSE)
telco <- janitor::clean_names(telco_raw)

#data cleaning 
naniar::miss_var_summary(telco) #no NA
dup_count <- sum(duplicated(telco$customer_id)) #no duplicates 
dup_count

telco$total_charges <- suppressWarnings(as.numeric(round(telco$total_charges)))
telco$senior_citizen <- factor(ifelse(telco$senior_citizen == 1, "Yes", "No"))

#distribution 
tbl_churn <- as.data.frame(table(telco$churn))
colnames(tbl_churn) <- c("churn","n")
tbl_churn$prop <- tbl_churn$n / sum(tbl_churn$n)
tbl_churn[order(-tbl_churn$prop), ]

num_long <- data.frame(
name = rep(c("tenure","monthly_charges","total_charges"),
times = c(nrow(telco), nrow(telco), nrow(telco))),
value = c(telco$tenure, telco$monthly_charges, telco$total_charges)
) #for making 3 histogram in one time
ggplot(num_long, aes(x = value)) +
geom_histogram(bins = 40) +
facet_wrap(~ name, scales = "free") +
labs(title = "Numeric Distributions", x = NULL, y = "Count")

plot_bar <- function(df, var, top_n = 12) {
vec <- df[[var]]
tab <- sort(table(vec), decreasing = TRUE)
if (length(tab) > top_n) tab <- tab[1]
dd <- data.frame(level = names(tab), n = as.numeric(tab), row.names = NULL)
ggplot(dd, aes(x = reorder(level, n), y = n)) +
geom_col() +
coord_flip() +
labs(x = var, y = "Count", title = paste("Top levels:", var))
}

plot_bar(telco, "contract")
plot_bar(telco, "internet_service")
plot_bar(telco, "payment_method")
plot_bar(telco, "multiple_lines")
plot_bar(telco, "online_security")

#monthly charges & churn
ggplot(telco, aes(x = churn, y = monthly_charges, fill = churn)) +
geom_violin(trim = FALSE) +
geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
guides(fill = "none") +
labs(title = "Monthly Charges vs Churn", x = NULL, y = NULL)

#tenure & churn
ggplot(telco, aes(x = churn, y = tenure, fill = churn)) +
geom_violin(trim = FALSE) +
geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
guides(fill = "none") +
labs(title = "Tenure vs Churn", x = NULL, y = NULL)



bin_churn <- function(df, var, bins = 20) {
x <- df[[var]]
b <- cut(x, breaks = bins, include.lowest = TRUE)
churn_yes <- df$churn == "Yes"
tab_n <- tapply(rep(1, length(b)), b, sum)
tab_c <- tapply(churn_yes, b, mean, na.rm = TRUE)
dd <- data.frame(bin = names(tab_n), n = as.numeric(tab_n), churn_rate = as.numeric(tab_c))
dd$bin <- factor(dd$bin, levels = dd$bin)
ggplot(dd, aes(x = bin, y = churn_rate)) +
geom_col() +
coord_flip() +
scale_y_continuous(labels = scales::percent) +
labs(x = var, y = "Churn Rate", title = paste("Churn rate vs", var))
}

bin_churn(telco, "monthly_charges")
bin_churn(telco, "tenure")

dd_num <- telco[, c("tenure","monthly_charges","total_charges")]
GGally::ggcorr(dd_num, label = TRUE)

#Churn heatmap by contract & tenure band

tenure_band <- cut(telco$tenure,
                   breaks = c(0, 6, 12, 24, 36, 60, Inf),
                   labels = c("<=6","7-12","13-24","25-36","37-60","60+"),
                   include.lowest = TRUE, right = TRUE)

df_hm <- data.frame(contract = telco$contract,
                    tenure_band = tenure_band,
                    churn = telco$churn)

agg_rate <- aggregate(as.numeric(df_hm$churn == "Yes"),
                      by = list(contract = df_hm$contract,
                                tenure_band = df_hm$tenure_band),
                      FUN = mean, na.rm = TRUE)
colnames(agg_rate)[3] = "churn_rate"

agg_n <- aggregate(rep(1, nrow(df_hm)),
                   by = list(contract = df_hm$contract,
                             tenure_band = df_hm$tenure_band),
                   FUN = sum)
colnames(agg_n)[3] = "n"

hm <- merge(agg_rate, agg_n, by = c("contract","tenure_band"), all.x = TRUE)

hm$contract <- factor(hm$contract, levels = c("Month-to-month","One year","Two year"))
hm$tenure_band <- factor(hm$tenure_band, levels = c("<=6","7-12","13-24","25-36","37-60","60+"))

# --- Plot ---
library(scales)
ggplot(hm, aes(x = contract, y = tenure_band, fill = churn_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(percent(churn_rate, accuracy = 0.1),
                               "\n(n=", n, ")")),
            size = 3) +
  scale_fill_viridis_c(labels = percent_format(accuracy = 1)) +
  labs(title = "Churn rate by Contract × Tenure band",
       x = "Contract Type", y = "Tenure Band (months)", fill = "Churn Rate") +
  theme_minimal()
