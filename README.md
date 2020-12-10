# Final: Pathways Shiny App 

https://lauren-a-kohls.shinyapps.io/lkohls_Final/?_ga=2.194057197.1733218520.1607570580-1865899214.1606852322 

## Non-Profits in Afforable Housing
Across the country, non-profit organizations have dedicated themselves to developing and redfining affordable and low income housing. Such community level and larger national nonprofit housing organizations have produced aproximately 30% of all units in the social housing sector (Bratt, 2007). As more non-profit organizaitions emerge in this industry, it becomes increasingly more important for them to capatalize on the data that they have to drive both their impact and generate revenue. A study conducted by EveryAction noted that while 90% of non-profit workers surveyed indicated that their organizations collect data, only 50%  were aware of how the data was shaping their orgazational operations. 

## Overview: 
This Project is a data visualization tool geared towards service coordinators utilizing the Family Metrics Social Services Software. The tool allows service coordinators that may not have a background in data analysis to easily visualize the information that they have available. The shiny app cleans and tidys data, creates graphs, and gives a picture of the property, or properties, that an individual is working in. It is not meant as an external reporting tool, but moreso as an internal gage to assist community coordinators in understanding the dynamics of the communities in which they work. This can be especially important in applying for grants and funding, and in insuring that community development goals are both focused, and informed. The app additionally takes property addresses oand pulls 5 year American Survey Data for each property and retuns a downloadable table for the user. This can be utilized to understand how the community may differ from its local surroundings. 


## The Data

This shiny app requires two datasets to function appropriately. The first, labeled "Data.csv", the second, labeled "addresses.csv". 

The "Data.csv" data set is a download of data from the Family Metrics system. The dummy data set in this repo mimmicking Family Metrics output. The set offers individual level dummy data for members of 4 fake affordable housing communities. It contains a variety of demographic data, including age, race, income, education and feelings of safet within the building and broader community. 

The addresses dataset is a dataset of property names and addresses. This dataset is then geocoded and utilized to pull variables from the 5 year Ammerican Community Survey Data for the property's specific census tract. The census tract data is downloadable, and therefore can be saved by the user. 


