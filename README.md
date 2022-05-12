# crashes
A made-to-learn predictive pca-regression model done up in R

## run it
https://jturcott.shinyapps.io/AccidentPrediction/?_ga=2.17246716.432899035.1652222669-465208690.1652222669

## original data
https://www.kaggle.com/datasets/benoit72/uk-accidents-10-years-history-with-many-variables

## done for
https://www.gmu.edu (CDS303)

# FAQIAM

- How much memory did I need to use more of the original variables that I eventually did
  - Was there a way to save the pca/fits out to file that didn't have to include quite so much of the original data? 
- Is GLM the right regression for this?
- Why was the orignal data so inconsistent about the details of the same intersection
- Is the month really the most impactful variable?
- Why didn't they mention the make/model of the car involved?
- Why can't I `v <- as.factor( v, c('a','b','c','m') )` huh?
