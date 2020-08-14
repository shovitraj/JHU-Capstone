Coursera: JHU Data Science Specialization- Capstone Project

Swift Keyboard
========================================================
author: Shovit Bhari
date: 2020-08-14
autosize: false
transition : rotate
#css: style.css


Introduction
========================================================
- This word prediction app is a final assignment of the tenth course (Data Science Capstone) in Coursea Data Science Specialization. 
- This course focuses on analyzing a large corpus of text documents to discover the structure in the data and how words are put together to build a predictive model. 
- N-Gram linguistics model was used to build a smart keyboard that predicts the next word based on input of the words. 



Roadmap to the Model 
========================================================
- Getting and cleaning the data:  
-- All the porvided corpus was combined into one      
-- 25% of the corpus was selected for training a model    
- Exploratory Data Analysis:     
-- Frequency of words and their pairs were calculated  
- Modeling:  
-- Quanteda package was used to tokenize the corpus   
-- 1 to 7-gram model was built for word prediction   

    
Algorithm and Prediction
==========================================================
- To improve efficiency, word pairs that appear less than 5 times in the corpus were removed    
- Katz's back-off model was used to predict the next word    
- The model iterates from 7-gram to 1-gram to find matches in the last (n-1) words
- It starts from 7-gram, backs off to 6-gram if there is no prediction. 
- It continues till, it back-off to 1-gram. 
- When the user input is null, the most frequent word 'the' is returned when number of prediction by default is 1

The Shiny App
========================================================
Here is a link to the application which provides all the necessary instructions.
![Shiny](Shiny.png)   

[GitHub](https://github.com/shovitraj/JHU-Capstone)


References
========================================================
- **Coursera**     
-- [Data Science Specialization by JHU](https://www.coursera.org/specializations/jhu-data-science?utm_source=gg&utm_medium=sem&utm_content=03-DataScience-JHU-US&campaignid=313639147&adgroupid=28743916747&device=c&keyword=coursera%20data%20science%20course&matchtype=b&network=g&devicemodel=&adpostion=&creativeid=265008066846&hide_mobile_promo&gclid=Cj0KCQjwvb75BRD1ARIsAP6LcqsTKmauI5eL5VZUC5l5VuP3QZ0mnYGEohf7MGyvfnt1H94GrHkr6dUaAhhVEALw_wcB)    
-- [Course 10: Capstone Final Project](https://www.coursera.org/learn/data-science-project/peer/EI1l4/final-project-submission)

- **Wikipedia**   
-- [Katz's back-off model](https://en.wikipedia.org/wiki/Katz%27s_back-off_model)   
-- [N-gram](https://en.wikipedia.org/wiki/N-gram)
- **Other links**   
-- [Quanteda package](https://quanteda.io/)  
-- [NLP Stanford](https://nlp.stanford.edu/~wcmac/papers/20050421-smoothing-tutorial.pdf)  
-- [N-gram models Cornell](http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/smoothing+backoff.pdf)

