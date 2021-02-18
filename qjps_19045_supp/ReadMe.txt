Replication File for "Trumping Hate on Twitter? Online Hate Speech in the 2016 Election Campaign and its Aftermath" 


** Citation **

Siegel, Nikitin, Barbera, Sterling, Pullen, Bonneau, Nagler, and Tucker. 2020. "Trumping Hate on Twitter? Online Hate Speech in the 2016 Election Campaign and its Aftermath.” The Quarterly Journal of Political Science.

@article{Siegel2020,
author = {Siegel, Alexandra A., Nikitin, Evgenii, Barbera, Pablo, Sterling, Joanna, Pullen, Bethany, Bonneau, Richard, Nagler, Jonathan, and Tucker, Joshua A.},
journal = {The Quarterly Journal of Political Science},
title = {{Trumping Hate on Twitter? Online Hate Speech in the 2016 Election Campaign and its Aftermath}},
volume = {(forthcoming)},
year = {2020}
}


** Notes **

Set the working directory to the folder where ReadMe.txt is located.

Analysis run using R version 3.5.1 and Python version 3.7.7

R package source files are stored in R_package_source folder 

Python modules are stored in python_modules folder


** Data Subfolders and Datasets **

 (in the "data" directory)
 
1. dictionary_data_clinton/

-- The datasets in this folder are used for the disaggregated dictionary-based analysis of the Clinton data. They include daily measures of hate speech or white nationalist language in all tweets mentioning Hillary Clinton from June 2015 to June 2017. Each CSV in the folder is a different category of speech (anti_asian_clinton.csv, anti_black_clinton.csv, anti_immigrant_clinton.csv, anti_latino_clinton.csv, anti_muslim_clinton.csv, anti_semitic_clinton.csv, homophobic_clinton.csv, misogynistic_clinton.csv, white_nationalist_clinton.csv). 
		
2. dictionary_data_trump/

-- The datasets in this folder are used for the disaggregated dictionary-based analysis of the Trump data. They include daily measures of hate speech or white nationalist language in all tweets mentioning Donald Trump from June 2015 to June 2017. Each CSV in the folder is a different category of speech (anti_asian_trump.csv, anti_black_trump.csv, anti_immigrant_trump.csv, anti_latino_trump.csv, anti_muslim_trump.csv, anti_semitic_trump.csv, homophobic_trump.csv, misogynistic_trump.csv, white_nationalist_trump.csv). 

3. dictionary_data_random/

-- The datasets in this folder are used for the disaggregated dictionary-based analysis of the Random Sample data. They include daily measures of hate speech or white nationalist language in all tweets produced by our sample of 500,000 American Twitter users from June 2015 to June 2017. Each CSV in the folder is a different category of speech (anti_asian_random.csv, anti_black_random.csv, anti_immigrant_random.csv, anti_latino_random.csv, anti_muslim_random.csv, anti_semitic_random.csv, homophobic_random.csv, misogynistic_random.csv, white_nationalist_random.csv). 

4. dictionary_data_clinton_agg.csv

-- This dataset is used for the aggregated dictionary-based analysis of the Clinton data. It includes aggregated daily measures of hate speech in all tweets mentioning Hillary Clinton from June 2015 to June 2017. 

5. dictionary_data_trump_agg.csv

-- This dataset is used for the aggregated dictionary-based analysis of the Trump data. It includes aggregated daily measures of hate speech in all tweets mentioning Donald Trump from June 2015 to June 2017. 

6. dictionary_data_random_agg.csv

-- This dataset is used for the aggregated dictionary-based analysis of the Random Sample data. It includes aggregated daily measures of hate speech in all tweets produced by our sample of 500,000 American Twitter users from June 2015 to June 2017. 

7. non_dictionary_data.csv

-- This dataset is used for the non-dictionary-based robustness analysis of the Clinton, Trump, and Random Sample datasets. It includes daily measures of probability that the text of the tweets in the Clinton, Trump, and Random Sample datasets is found on alt-right subreddits from June 2015 to June 2017. 

8. human_coded_data.csv

-- This dataset includes more detailed human coding of the 5400 tweets for which we asked coders questions about the targets of hate speech on Figure8. 

9. non_dictionary_validation_data/

--The datasets in this folder are used to validate the reddit-based non-dictionary analysis. They show the probabilities that content belongs to particular subreddits or that subreddits belong to larger groups of subreddits, as well as the probabilities that content contains hate speech. 

** R files **

 (in the "code" directory)
If R files are run, they will generate the figures in the paper and online appendix and save them into the "figures" directory. They also  generate the latex tables found in the online appendix. 

1. dictionary_analysis.R
-- Creates plots of raw data, loess plots, descriptive statistics, and Interrupted Time Series plots and regression tables for dictionary-based analysis. 
-- Output: Figures 3-6, A1-A37 & Tables A1-A24  

2. non_dictionary_analysis.R
-- Creates loess plots and Interrupted Time Series regression tables for non-dictionary based robustness analysis 
-- Output: Figure 7 & Tables A28-A30 

3. human_coding_analysis.R
-- Creates plot of human-coded targets of hate speech
-- Output: Figure 2

** Python files **

(In the "code" directory)
If Python files are run, they will generate the figures of the paper and save them into the "figures" directory. 

1. non_dictionary_validation.ipynb
-- Creates plots validating non-dictionary analysis
-- Output: Figures A40-A43


