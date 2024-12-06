# WildBirds_2022
Data files and code for the wild bird/bacteria 2020-2022 project.

**Data Files**
1. [land_cover.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/land_cover.xlsx) contains proportion land cover values and FRAGSTATS values in a 4.5 km radius around each farm.
2. [sal_live.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/sal_live.xlsx) contains each sample ID, whether they were positive (1) or negative (0) for Salmonella, whether they were moist (1) or dry (0), and whether different livestock were present on-farm (1), present in the landscape (within 250 m) (2) or absent (0) the day of sampling.
3. [farm_size.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/farm_size.xlsx) includes farm size, livestock, and ecoregion for use in the NMDS figure.
4. [birds_100m_filtered.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/birds_100m_filtered.xlsx) includes all birds detected furing point counts within 100m and has filtered out suspected "double-counts". Point counts were 10 minutes long, divided into 2 3-minute and 1 4-minute period; if the same species and number of individuals were detected during two or more periods, these observations were merged in this data file to avoid double-counts.
5. [home-range.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/home_range.xlsx) includes the home range for all avaialble species and the source.

**Script Files**
1. [birds_script.R](https://github.com/SofieV123/WildBirds_2022/blob/main/birds_script.R) contains the R code for the bird community analysis and associated figure.
2. [bacterial_script.R](https://github.com/SofieV123/WildBirds_2022/blob/main/bacterial_script.R) contains the R code for the Salmonella + landscape/livestock analysis and associated figures.

**Grower Survey**
1. [survey_qualtrics](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_qualtrics.docx) and [survey_print](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_print.docx) are the online and print versions of the survey, respectively, that were distributed to growers.
2. [survey_response.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_response.xlsx) contains the full survey responses, with factor levels and coded responses from open-ended questiosn shown.
3. [prevent_eff.txt](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/prevent_eff.txt) and [prevent_freq.txt](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/prevent_freq.txt) are subsets of the larger [survey_response.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/survey_response.xlsx) used to generate figures.
4. [survey_script.R](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/survey_script.R) contains the R code used to analyze survey data and make associated figures.
