
# Victorian LGA Crime analysis

### About App:

This app is created to have a insight of crimes and their rates per 100K in different Local government areas of Victoria,Australia. The data used to build this app can be found [here](https://discover.data.vic.gov.au/dataset/crime-by-location-data-table).

### Data Description:

Contains Criminal incidents and rate per 100,000 by principal offence and LGA - October 2007 to September 2017
The data has the following components:

1. `year` -> From 2008 to 2017
2. `Local Government Area` -> Different LGA's of victoria
3. `Offence Division` -> Describes the type of crime reported
4. `Offence Subdivision` -> Different type of crime associated to corresponding Offence Division.
5. `Offence Subgroup` -> A little more info about the exact crime incident.
6. `Incidents Recorded` -> The number of crime reported for each offence division/subdivision/subgroup base on each year and LGA.
7. `Rate per 100,000 population` -> Rates per 100,000 population are calculated for offences, criminal incidents, alleged offender incidents, victim reports and family incidents. Criminal incident rate = (Criminal incident count/ERP count) *100,000



### How to use the app:

1. Select or type in the LGA you would like to compare in the sidebar of the main dashboard view.
3. Select the `year` range to compare from the input slider on sidebar.
4. The `maps` tab displays a leaflet map that focuses on victoria showing the number of crimes in each area. (Slider's doesn't function in this; Will implement it later.)

#### References

1. "Explanatory notes | Crime Statistics Agency Victoria", Crimestatistics.vic.gov.au, 2019. [Online]. [Link](https://                    www.crimestatistics.vic.gov.au/about-the-data/explanatory-notes) [Accessed: 28- Oct- 2019]
2.  Dr.James Baglin, 2019, Chapter 7,8,9,10. [RMIT University,Melbourne]
3. "Customizing dashboard appearance", Rstudio.github.io, 2019. [Online]. [Link](https://rstudio.github.io/shinydashboard/               appearance.html) [Accessed: 28- Oct- 2019] 
4. "Leaflet for R - Using Basemaps", Rstudio.github.io, 2019. [Online]. [Link](https://rstudio.github.io/leaflet/basemaps.html)       [Accessed: 28- Oct- 2019]
5. "StackOverFlow: https://stackoverflow.com
