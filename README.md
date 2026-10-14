# Living vs. Deceased Organ Donation

Is living donation a structural mechanism, or compensation for weak deceased donation capacity?

An analysis of WHO Global Observatory on Donation and Transplantation (GODT) data, 2000–2024, covering kidney, liver and lung transplants across up to 194 countries.

## Part A: Motivation

### 1. Use of AI-type tools

When writing parts of the data-cleaning and reshaping code, I sometimes knew the result I wanted but did not know the most efficient syntax, so I used AI to suggest options, then checked them against the documentation and adapted them. This was especially helpful for `pivot_longer(..., names_pattern = ...)`, which made it much easier to reshape variables such as `dd_kidney_tx` and `ld_liver_tx` into a tidy format. Instead of separating the donor type and organ manually in several steps, this approach allowed me to extract both directly from the column names in one go.

During code review and debugging, AI helped me understand error messages in R, suggested cleaner or more efficient alternatives, and introduced me to functions I had not used before. For example, `janitor::clean_names()` gave me a simple one-line way to standardise all column names, instead of renaming many variables by hand. Similarly, `sapply(..., function(x) ...)` in the zero-vs-NA section helped me quickly generate a diagnostic summary across all numeric columns, rather than checking zeros and missing values one variable at a time. I also used `dplyr::recode()` for a known typo in a country name; this was not the only possible way to fix it, but it made the correction more readable and easier to follow in the final script. In all of these cases, I made sure I understood what the function was doing before keeping it in the code.

For grammar review of the final written sections, I ran my own text through AI to help catch awkward phrasing and improve clarity.

### 2. Motivation and development of the idea

The topic emerged from a straightforward observation: in some countries virtually all kidney transplants rely on living donors, while in others the share approaches zero. I wanted to understand whether this reflects just a preference for living donation, or a structural default — countries with weak deceased donation infrastructure are forced to rely on living donors. That distinction (mechanism vs. compensation) drove the entire analytical framework.

I initially explored whether organ donation legislation (such as the shift from opt-in to opt-out consent systems) had a measurable effect on countries' LD share over time. However, most of the key reforms either predated the dataset (Spain's opt-out dates to 1979, Iran's compensated donation law to 1988) or lacked sufficient post-reform observations to identify any effect. The most promising case, the Netherlands' automatic donor registration law enacted in 2018 and effective from 2020, proved unworkable for a different reason: the reform coincided exactly with COVID-19, making it impossible to disentangle the policy effect from the pandemic shock. The Netherlands showed a sharp DD drop in 2020–21, but whether this reflected COVID disruption or public backlash against the new law cannot be determined from the data alone. This observation, however, prompted me to look more carefully at COVID itself.

A time-trend analysis for all 194 countries proved infeasible due to missing data, so I selected 12 countries with complete records instead. The Iran–Turkey case study emerged from direct engagement with the data — the large divergence between two countries with nearly identical starting conditions in 2003.

**On liver donation:** across 70 countries with sufficient liver data, the same regional divide holds: Japan (90%), Republic of Korea (77%), and Kazakhstan (87%) lead liver LD share, with high-volume Asian programmes compensating for scarce deceased-donor liver supply. Western European countries with mature DD infrastructure (Spain, Portugal, Italy, Croatia) report liver LD shares below 5%.

**On data quality:** Portugal, France, Germany, Spain, Italy, and Belgium each show 3–13 years where reported DD + LD liver totals do not match the reported total liver transplants. These countries are excluded from liver LD share analysis. Kidney data for the same countries is internally consistent and retained in all kidney models.

### 3. Links to course material

**Summarising and comparing distributions.** The country typology and all group-level descriptives rely on sample means to characterise and compare distributions across countries and regions.

**Bivariate analysis and correlation.** The cross-sectional scatter plot and Pearson correlation (r = −0.765, p < 0.001) between deceased donor capacity and LD kidney share.

**Observational data and confounding.** The analysis relies on observational cross-sectional and longitudinal data: the OLS regression between deceased donor capacity and LD kidney share establishes a robust cross-sectional association (r = −0.765, β = −0.028, R² = 0.585), but does not claim causal identification — unobserved country-level factors (institutional capacity, cultural attitudes toward deceased donation, income level) plausibly confound the relationship. The Iran–Turkey case study addresses this partially by holding geography, religion, and historical starting point approximately constant, isolating the role of deliberate policy investment in DD infrastructure as a candidate mechanism.

## Part B: Content

### Data and Preparation

The raw data were restructured into three normalised tables: a country-year panel, country-level aggregates, and an organ-donor-type breakdown. Arithmetic consistency checks (Total = DD + LD) flagged mismatch rates of 0.26% for kidney and 9.17% for liver; inconsistent rows were excluded as they couldn't be corrected without more data. The final analytical sample comprises 91 countries for kidney analysis (≥10 clean years) and 70 for liver (≥8 clean years).

### Country Typology

Based on average LD kidney share, countries were classified into three groups: Living-Dominant (LD share ≥60%, n=38), Mixed (30–60%, n=23), and Deceased-Dominant (<30%, n=30). Living-Dominant countries are concentrated outside Western Europe and predominantly have DD PMP below 5. Deceased-Dominant countries are predominantly European and high-income, though several Latin American and other systems also fall in this group; most (80%) have DD PMP above 10, and this typology structures all subsequent analysis.

### Main Findings

1. **Strong inverse correlation between living and deceased donation.** Cross-sectional analysis across 91 countries yields a Pearson correlation of r = −0.761 (p < 0.001) between average deceased-donor rate (pmp) and average LD kidney share. OLS regression confirms the association: β = −0.028 (SE = 0.003, R² = 0.585), meaning each additional deceased donor per million population is associated with a 2.82 percentage point decrease in LD kidney share — a robust substitution pattern at the cross-sectional level.

2. **Thirty-eight countries are Living-Dominant for kidney transplantation.** Using average LD share ≥ 60% as the classification threshold, 38 of 91 countries fall into the Living-Dominant typology. Nearly all are located outside Western Europe, and many operate with zero or near-zero deceased donation infrastructure (including Georgia, Pakistan, Nigeria, Jordan, and Egypt, each reporting 100% LD share across all observed years).

3. **Top deceased-donor systems achieve 20–40 DD pmp; living donors fill the gap elsewhere.** Spain (39.4 pmp), the United States (30.0 pmp), and Portugal (27.3 pmp) lead global deceased-donor capacity. Countries with fewer than 5 DD pmp consistently compensate with LD shares above 80%, confirming that living donation functions as a structural substitute where deceased-donor infrastructure is absent or underdeveloped.

4. **Iran underwent a rare transition from living-dominant to mixed typology.** Iran's LD kidney share fell from approximately 89% in 2003–2005 to 36% by 2019, as its deceased-donor programme expanded from 1.3 to 13.0 pmp over the same period. Within-country Pearson correlation for Iran is r = −0.97 (p < 0.001), consistent with a systemic substitution transition driven by deliberate policy investment in DD.

5. **Turkey and Republic of Korea show persistent high LD share despite some deceased-donor capacity.** Both countries have above 60% average LD kidney share while maintaining moderate DD programmes (Türkiye: 4.0 pmp, Korea: 7.1 pmp). Turkey's LD share has trended upward from ~70% in 2000 to over 90% in 2022–2024, suggesting that cultural and institutional factors sustain living donation independently of deceased-donor availability.

6. **Iran–Turkey divergence identifies a structural substitution threshold.** Both countries entered the observation period in near-identical positions (~80% LD share, ~2 DD pmp in 2003). Iran's deceased-donor programme expanded to 13 pmp by 2019, and LD share fell to 36% (r = −0.97). Turkey's DD capacity stalled below 8 pmp throughout the period, and LD share rose to over 90% (r = +0.12, p = 0.48). This controlled natural comparison suggests a structural substitution threshold of approximately 8–10 DD pmp: below this level, expanded deceased donation supplements rather than displaces living donation, and system inertia reinforces the existing LD-dominant equilibrium.

7. **COVID-19 as a natural experiment in donation system resilience.** Across 80 countries with complete pre-, during-, and post-COVID data, 39 (49%) experienced a meaningful decline in deceased-donor capacity (average drop of 4.2 pmp), yet living donation compensated in only 14 of these cases — predominantly middle-income systems in Latin America and Southeast Asia. High-capacity European systems saw both DD and LD fall simultaneously, suggesting that mature deceased-donor programmes do not trigger living donation as a buffer during acute shocks. By 2022–23, 41 of 80 countries had still not recovered to pre-pandemic DD levels, while living donation proved comparatively stable throughout. This asymmetry reinforces the structural substitution argument: living donation functions not only as a developmental substitute where DD infrastructure is absent, but also as a more resilient component under conditions of systemic disruption.

## Code

`final_code.R` runs the full pipeline in three parts:

1. **Data cleaning** — standardises column names and missing-value markers, coerces numeric columns, flags arithmetic mismatches between reported totals and their DD/LD components, and builds country-level completeness summaries used to select the analytical sample.
2. **Analysis & visualisation** — builds the country-year analytical dataset, classifies countries into the LD typology, runs the correlation and OLS regression, and produces the scatter, lollipop, time-trend, regional and bivariate plots.
3. **COVID natural experiment & case study** — compares pre-COVID / COVID / post-COVID periods across countries, and builds the Iran–Turkey dual-panel comparison.

The script expects `GODT_downloads.xlsx` (the raw WHO GODT export) in `~/Downloads/` and writes its intermediate CSVs and final plots to the working directory.

## References

- WHO Global Observatory on Donation and Transplantation (2024). *Global Data on Donation and Transplantation 2000–2024.* World Health Organization / ONT. Primary data source for all analyses.
- Wickham, H. (2014). Tidy Data. *Journal of Statistical Software*, 59(10), 1–23. https://doi.org/10.18637/jss.v059.i10 — framework for data restructuring, applied throughout the analytical dataset construction.
- Sharif, A., et al. (2025). *20 years Iranian experience of organ donation: Shifting from compensated living unrelated kidney transplantation.* Used to validate the Iran case study and contextualise the 2010 structural breakpoint.
- Sert, G., Guven, T., & Gorkey, S. (2013). When organ donation from living donors serves as the main source of organ procurement: a critical examination of the ethical and legal challenges to Turkey's recent efforts to overcome organ shortage. *Transplantation Proceedings*, 45(6), 2102–2105. Used to interpret Turkey as a stalled transition, explaining why legislative reforms failed to increase cadaveric donation despite institutional efforts.
</content>
