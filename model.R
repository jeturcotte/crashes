
  # source of data at:
  # https://www.kaggle.com/datasets/benoit72/uk-accidents-10-years-history-with-many-variables

library(tidymodels)
library(ranger)
library(caret)
library(dplyr)
library(fastDummies)
set.seed( 7041 )

crash <- read.csv( 'cds.303.final.processed.crash.data.csv', stringsAsFactors = TRUE )

  # now... how about looking to see which factors may be the most telling
  # when it comes to any particular severity of accident

  # and we can live with reducing our question to ... is this an intersection
  # where you stop or you don't stop
crash <- crash[ crash$junction != 'other', ]
crash$junction <- as.factor( as.character( crash$junction ) )
  # there are only 19 u-class roads and they're often similar in size to c-class
levels( crash$first.road ) <- c( 'a', 'b', 'c', 'm', 'c' )
levels( crash$second.road ) <- c( 'a', 'b', 'c', 'm', 'c' )
crash$first.road <- as.factor( as.character( crash$first.road ) )
crash$second.road <- as.factor( as.character( crash$second.road ) )
  # and since the months need ot be understood as distinct
crash$month <- as.factor( crash$month )

  # and now a roughly equal and pragmatically smaller set
crash <- as.data.frame( crash %>% group_by( second.road, junction ) %>% slice_sample( n = 3200 ) )

crash.dummies <- dummy_cols( 
  crash, 
  select_columns = c(
    'month',
    'urbanity',
    'first.road',
    'second.road',
    'junction',
    'control',
    'light',
    'surface',
    'precip',
    'vehicle',
    'action'
  ),
  remove_selected_columns = TRUE,
  ignore_na = TRUE
)

  # now ... an I improve on this, as per the assignment's entire purpose??
  
  # and now we gotta make room as the following
  # fits keep running into problems with memory
  # which means we should reduce our columns as well
  # so... a little analysis?

selection = sort( sample( nrow( crash.dummies ), nrow( crash.dummies ) * 0.85 ) )
training <- crash.dummies[ selection, ]
testing <- crash.dummies[ -selection, ]
crash.lr <- lm( 
  fatal.cpm + serious.cpm + slight.cpm ~ 
      # there was a notable cycle in accidents by time
    month_1 + 
    month_2 +
    month_3 +
    month_4 +
    month_5 +
    month_6 +
    month_7 +
    month_8 +
    month_9 +
    month_10 +
    month_11 +
    month_12 +
      # interia, yo
    speed.limit + 
      # urban areas are tricker than not
    urbanity_urban +
      # can't really ignore the classes of both roads
    first.road_a +
    first.road_b +
    first.road_c +
    first.road_m +
    second.road_a +
    second.road_b +
    second.road_c +
    second.road_m +
      # but we can whittle the question down as to
      # whether they're coming to a full stop or not
      # aka, not merge and not roundabout
    junction_stop,# +
      # and we can see if being a personal car or not
      # is important
    #veh.type_car,
  data = training
)
summary(crash.lr)

  # interestingly, this regression appears to consider the kind of intersection
  # not to be significant ... let's look at PCA then.
crash.pca <- prcomp(
  ~ month_1 +
    month_2 +
    month_3 +
    month_4 +
    month_5 +
    month_6 +
    month_7 +
    month_8 +
    month_9 +
    month_10 +
    month_11 +
    month_12 +
    speed.limit +
    urbanity_urban +
    first.road_a +
    first.road_b +
    first.road_c +
    first.road_m +
    second.road_a +
    second.road_b +
    second.road_c +
    second.road_m +
    junction_stop,
  data = training,
  scale = TRUE
)
summary(crash.pca)
  # so 74 percent of the variable can be trapped by the first 13 components
  # and 83% through 15

  # save this out for use later
saveRDS( crash.pca, 'crashes.pca.rds' )

  # lets slice off 7 (86% var) for our model, eh?
retraining <- data.frame( 
  slight.cpm = training$slight.cpm,
  serious.cpm = training$serious.cpm,
  fatal.cpm = training$fatal.cpm,
  crash.pca$x[,1:15]
) 

  # now... for simplicity(if not brevity)'s sake, lets have a model per target
crash.study <- trainControl( method = "cv", number = 10 )
set.seed(7041)
fatal.fit <- train(
  fatal.cpm ~ .,
  data = retraining,
  method = 'glm',
  trControl = crash.study
)
set.seed(7041)
serious.fit <- train(
  serious.cpm ~ .,
  data = retraining,
  method = 'glm',
  trControl = crash.study
)
set.seed(7041)
slight.fit <- train(
  slight.cpm ~ .,
  data = retraining,
  method = 'glm',
  trControl = crash.study
)

  # in retrospect, for the purposes of having sufficient emergency personnel and
  # equipment, a series and a fatal accident aren't very different and we could
  # have saved ourselves a little complexity accordingly... 

serious.data <- data.frame(
  fatal.cpm = testing$fatal.cpm,
  serious.cpm = testing$serious.cpm,
  slight.cpm = testing$slight.cpm,
  predict( crash.pca, testing )[,1:15]
)

  # alas, serious.data became a misnomer after some iterations
  # on the following code, but oh well
fatal.lr <- glm( fatal.cpm ~ . - slight.cpm - serious.cpm, data = serious.data )
serious.lr <- glm( serious.cpm ~ . - fatal.cpm - slight.cpm, data = serious.data )
slight.lr <- glm( slight.cpm ~ . - fatal.cpm - serious.cpm, data = serious.data )

  # now lets build a table we can LOOK at
predictions <- rbind(
  data.frame(
    severity = 'slight',
    actual = testing$slight.cpm,
    predicted = predict( slight.lr, serious.data )
  ),
  data.frame(
    severity = 'serious',
    actual = testing$serious.cpm,
    predicted = predict( serious.lr, serious.data )
  ),
  data.frame(
    severity = 'fatal',
    actual = testing$fatal.cpm,
    predicted = predict( fatal.lr, serious.data )
  )
)

  # lets assign some arbitrary-ish values to these
predictions$error <- as.factor(
  ifelse( abs( predictions$actual - predictions$predicted ) < 0.25,
    'good', 
    ifelse( abs( predictions$actual - predictions$predicted ) < 0.5,
      'okay',
      'bad'
    )
  )
)

  # and widen this a bit for good graphing for our poster
  # also the numbers are quite good!
perc.good = round( 
  sum( predictions$error == 'good' ) / length( predictions$error ), 
  digits = 4
) * 100
perc.okay = round( 
  sum( predictions$error == 'okay' ) / length( predictions$error ), 
  digits = 4
) * 100
ggplot( predictions, aes( x = actual, y = predicted, color = severity, shape = error ) ) +
  geom_abline( intercept = 0, slope = 1, alpha = 0.1, color = 'green' ) +
  geom_abline( intercept = -0.25, slope = 1, alpha = 0.25, color = 'blue', linetype = 'dashed' ) +
  geom_abline( intercept = 0.25, slope = 1, alpha = 0.25, color = 'blue', linetype = 'dashed' ) +
  geom_abline( intercept = -0.5, slope = 1, alpha = 0.5, color = 'red', linetype = 'dotted' ) +
  geom_abline( intercept = 0.5, slope = 1, alpha = 0.5, color = 'red', linetype = 'dotted' ) +
  geom_point( aes( 
    size = abs( actual - predicted ),
    alpha = 0.01 + abs( actual - predicted ) / max( abs( actual - predicted ) )
  ) ) +
  scale_shape_manual( values = c( 'good' = 16, 'okay' = 1, 'bad' = 4 ) ) +
  theme_minimal() +
  guides( alpha = 'none', size = 'none' ) +
  theme( 
    legend.position = c( 0.85, 0.2 ),
    legend.box = 'horizontal',
    legend.box.background = element_rect( 
      color = alpha( 'black', 0.1 ), 
      fill = alpha( 'white', 0.65 )
    ),
    legend.text = element_text( size = 14 )
  ) +
  ggtitle( paste0( 
    'Predicted vehicles involved in crashes, per month, by Intersection (',
    perc.good, '% good and ',
    perc.okay, '% okay predictions)'
  ) )

  # okay, this is good enough I think for a class
  # (someone pay me to do this better! hehehe)
  # let's save it dammit
saveRDS( fatal.lr, 'crashes.fatal.rds' )
saveRDS( serious.lr, 'crashes.serious.rds' )
saveRDS( slight.lr, 'crashes.slight.rds' )
