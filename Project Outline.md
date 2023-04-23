Dataset URL : https://www.kaggle.com/datasets/lashagoch/life-expectancy-who-updated

## Project Outline/Needed Tasks
**Setup**
- Training Set : Take averages of predictors over years 2000-2014 for each country. (178 observations by 19 predictors)
  - Only have 19 predictors because we want to remove the developing vs. developed predictors (2 predictors in dataset)
- Testing Set: Data from 2015 (178 observations by 19 predictors)


### Questions
- **Prediction** : Given a set of health-related predictors, what is a country's average life expectancy?
  - Set up training and testing set 
  - F-test
  - Model selection
  - Output our predictions
- **Classification** : Given a set of health related predictors, is a countries GDP per capita above or below the world average?
  - Set up training and testing set
  - Find world average GDP per capita to serve as our threshold (under = 0, over  = 1) --> 10,154 (https://data.worldbank.org/indicator/NY.GDP.PCAP.CD)


### Outline
- **Setup/Background/Motivations**
  - Background
  - Introduce Questions
  - Big picture – Strategy to answer questions. 
- **Results**
  - In depth answer to questions
  - Methods and Process (how we got to answer)
  - Justification (why we are confident in the answer) - Includes insights, graphs, data.
  - Unexpected results 
- **Problems**
  - Potential error due to data
  - Potential error due to our approach
  - Examine and discuss the weaknesses and strengths of our approach.
- **Summary**
  - Final summary and answer to our questions
  - Insights and further questions for ongoing study
