drop if Year == 2023
xtset ID Year

gen ln_total_assets = ln(total_assets)


// Baseline model with ROA
xtabond2 GDP_Growth L.GDP_Growth ROA share_non_interest_inc CPI_pct_change Population equity_ratio ln_total_assets loan_per_gdp former_east i.Year, gmm(L.ROA, lag(2 10)) iv(share_non_interest_inc CPI_pct_change Population equity_ratio ln_total_assets loan_per_gdp former_east i.Year) twostep robust small

// Baseline model with ROE
xtabond2 GDP_Growth L.GDP_Growth ROE share_non_interest_inc CPI_pct_change Population equity_ratio ln_total_assets loan_per_gdp former_east i.Year, gmm(L.ROE, lag(2 10)) iv(share_non_interest_inc CPI_pct_change Population equity_ratio ln_total_assets loan_per_gdp former_east i.Year) twostep robust small


// Interaction terms model ROA
xtabond2 GDP_Growth L.GDP_Growth ROA share_non_interest_inc CPI_pct_change ln_total_assets Population GDP_per_capita equity_ratio loan_per_gdp interaction_GDP_ROA former_east i.Year, gmm(L.ROA, lag(2 10)) iv(share_non_interest_inc CPI_pct_change ln_total_assets Population equity_ratio loan_per_gdp GDP_per_capita interaction_GDP_ROA former_east i.Year) twostep robust small

// Interaction terms ROE
xtabond2 GDP_Growth L.GDP_Growth ROE share_non_interest_inc CPI_pct_change ln_total_assets Population GDP_per_capita equity_ratio loan_per_gdp interaction_GDP_ROE former_east i.Year, gmm(L.ROE, lag(2 10)) iv(share_non_interest_inc CPI_pct_change ln_total_assets Population equity_ratio loan_per_gdp GDP_per_capita interaction_GDP_ROE former_east i.Year) twostep robust small

// Z score
xtabond2 GDP_Growth L.GDP_Growth Z_score share_non_interest_inc CPI_pct_change Population equity_ratio loan_per_gdp former_east i.Year, gmm(L.Z_score, lag(2 10)) iv(share_non_interest_inc CPI_pct_change Population equity_ratio loan_per_gdp former_east i.Year) twostep robust small