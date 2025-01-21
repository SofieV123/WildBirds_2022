# WildBirds_2022
Data files and code for the wild bird/bacteria 2020-2022 project.

**Data Files**
1. [land_cover.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/land_cover.xlsx) contains proportion land cover values and FRAGSTATS values in a 4.5 km radius around each farm.
2. [sal_live.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/sal_live.xlsx) contains each sample ID, whether they were positive (1) or negative (0) for Salmonella, whether they were moist (1) or dry (0), and whether different livestock were present on-farm (1), present in the landscape (within 250 m) (2) or absent (0) the day of sampling.
3. [farm_size.xlsx] (https://github.com/SofieV123/WildBirds_2022/blob/main/farm_size.xlsx) contains each farm ID, the size in acres, the ecoregion, and whether different livestock were present on-farm (1), present in the landscape (within 250 m) (2) or absent (0) the day of sampling.
4. [birds_100m_filtered](https://github.com/SofieV123/WildBirds_2022/blob/main/birds_100m_filtered.xlsx) contains all birds identified (visual or sound ID; "V/S") within 100m point-counts. This list of birds was filtered to avoid double-counting, so that individuals counted during subintervals ("Period") were not counted twice. "Habitat" identified which individuals were observed in the crop fields.
5. [home_range.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/home_range.xlsx) contains the known home range, gathered from [Birds of the World](https://birdsoftheworld.org/bow/home), of each species identified during point counts.
6. [poopy_bids.txt](https://github.com/SofieV123/WildBirds_2022/blob/main/poopy_birds.txt) identified which bird species were identified molecularly from foliage-collected bird feces.

**Scripts**
1. [bacterial_script.R](https://github.com/SofieV123/WildBirds_2022/blob/main/bacterial_script.R) contains the R code for Salmonella prevalence models and associated figures.
2. [birds_script](https://github.com/SofieV123/WildBirds_2022/blob/main/birds_script.R) contains the R code for bird community analysis and associated figures. [birds_nmds.txt](https://github.com/SofieV123/WildBirds_2022/blob/main/bird_nmds.txt) is a reformatted version of [birds_100m_filtered](https://github.com/SofieV123/WildBirds_2022/blob/main/birds_100m_filtered.xlsx) for the purposes of making Figure 2. 

**Grower Survey**
1. [survey_qualtrics](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_qualtrics.docx) and [survey_print](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_print.docx) are the online and print versions of the survey, respectively, that were distributed to growers.
2. [survey_response.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_response.xlsx) contains the full survey responses, with factor levels and coded responses from open-ended questiosn shown.
3. [survey_script.R](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/survey_script.R) contains the R code for grower survey analysis and associated figures. [prevent_eff.txt](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/prevent_eff.txt) and [prevent_freq.txt](https://github.com/SofieV123/WildBirds_2022/blob/main/Grower%20Survey/prevent_freq.txt) are reformatted subsets of [survey_response.xlsx](https://github.com/SofieV123/WildBirds_2022/blob/main/survey_response.xlsx) for the purposes of making Figure 5.
