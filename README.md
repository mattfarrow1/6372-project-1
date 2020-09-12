# Applied Stats: Project 1

## Objective 1

Display the ability to build regression models using the skills and discussions from Unit 1 and 2 with the purpose of identifying key relationships, interpreting those relationships, and making good predictions.   

## Objective 2

As mentioned before, the advantages of regression is the interpretation of the model produced. It also works very well when the number of predictors is large and the sample size is small. Disadvantages to regression is the fact that not all data sets are going to yield “linear” relationships. If the relationship is quite complex, it will be difficult to fit a good model without a lot of clever engineering of new predictor variables.  

Nonparameteric regression models are models which do not assume the residuals follow any distribution and do not assume any sort of “linear” structure on what the trend between the response and the predictors could be. For Objective 2, I want you to use the ISLR text book and read up on one nonparametric technique. I want you to select from k-nearest neighbors’ regression or regression trees. Splines could also be a topic but personally I think k-nn or trees would be much easier to digest for a short turn around project such as this.
 
After [reading up on this topic](http://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf), I want you to use a set of predictors used from your previous linear regression models and fit your nonparametric model to the data, keeping in mind what the particular model calls for in terms of what type of predictors can be used and parameters that control for model complexity. The ISLR book provides code at the end of each chapter and I will also try provide a small script for you to have so you’re not starting from scratch. In your write up after the regression models, I want you to include a small section that includes the following:

1. A brief description of your nonparametric model’s strategy to make a prediction. Include Pros and Cons.
2. Provide any additional details that you feel might be necessary to report.
3. Report the test ASE using this nonparametric model so we can see how well it does compared to regression.
