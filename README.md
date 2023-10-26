# euroleague-evolution
Project for the course of Actuarial Data Science at HEC Lausanne

This project aim to transform a dataset from kaggle (https://www.kaggle.com/datasets/efehandanisman/euroleague-play-by-play-data-20072020) containing data play-by-play,  of all the games of the euroleague that happened between 2007 and 2020, into usable data to perform analysis. Furthermore, the code contains a graphical analysis of the evolution of the number of three-pointers attempted, and an analysis of the hack-a-player strategy. The written report is in our test_quarto script.

## Requirements 

Important: all the following libraries are important: tidyverse, ggplot2, hexbin, viridis, ggrepel, gridExtra

## How do the scripts work together?

Main_Wrangle performs the data cleaning/wrangling of the initial dataset and creates datframes per team per game, as well as dataframes per team per season, and calling functions created in CreationDataDrame and Ranking. As the data wrangling/cleaning, EDA and graphical analysis for individual statistics did not take as much effort, we put all of them in playerdf_script and called this script at the end of Main_Wrangle.
first_graphs is our script containg all the plots that were used for our presentation/report, and some others that were interesting but would have made the report way too big. This script usually calls functions from the Functions script, as they are about 80% similar to other graphs

