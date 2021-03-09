# efficient_intercepts

At [MPG Ranch](https://www.mpgranch.com/), we count and measure plants every year to learn how the plant community is changing. In particular, we are interested in changes that result from our restoration and management of the land, and how these changes affect organisms that use plants for food or shelter.

Every five years, we survey plants across the ranch at 582 locations. At each location, we collect data from 200 points. Surveying 116,400 points requires a significant investment of time and money, and it also leads to a large human presence on the landscape, causing disturbance to wildlife. This investment and disturbance is worth it if need all the data to learn about changes in the plant community, but we've never tested whether we are collecting *too much* data. Can we answer our questions just as well with less data? The analysis and reporting included here investigate the efficiency of our survey methods and suggest alterations to our protocol. 

In this analysis, data from the 2016 ranch wide survey are used. An interactive graphic of the 2016 data can be viewed on the MPG Ranch [Livemap](https://livemap.mpgranch.com/flora?center=46.701042376591104&center=-113.98378260433674&layers=flora&timelineBounds=1455759297&timelineBounds=1479513936&zoom=13). 

Our existing survey [protocol](https://docs.google.com/document/d/16XrPWP4kLGzU5aZmp7F0rF4aNdhWfNzuXJqCp_ShbMY/edit?usp=sharing) will likely be revised as a result of this analysis. 

This project was started on a Google Colaboratory [Notebook](https://colab.research.google.com/drive/1pm-39QOTpfvI5zGb4KtJzOggfWwCnky9?usp=sharing), but was moved to speed up processing, allow production of a report in Markdown, and to improve readability for readers who are not familiar with Colaboratory functions. Users who wish to execute functions on the Colaboratory Notebook must have API keys to access data from the Google Cloud. 