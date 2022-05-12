
# source of data at:
# https://www.kaggle.com/datasets/benoit72/uk-accidents-10-years-history-with-many-variables

library(fastDummies)
library(shiny)
library(shinythemes)
library(ggplot2)

prefactor <- function( v, s ){
  # because I can't do v <- as.factor( v, levels=s )
  # (dies a little inside)
  v <- as.factor( v )
  levels( v ) <- c( levels( v ), setdiff( s, levels( v ) ) )
  v
}

# lets get the predictors we made back in .final.model.R
fatal <- readRDS( 'crashes.fatal.rds' )
serious <- readRDS( 'crashes.serious.rds' )
slight <- readRDS( 'crashes.slight.rds')
pca <- readRDS( 'crashes.pca.rds' )

ui <- fluidPage(
  theme = shinytheme( "lumen" ),
  titlePanel( "Accident Prediction by the Intersection Type, Per Month" ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "ftype",
        label = strong("First Road"),
        choices = c(
          'Class M'='m',
          'Class A'='a',
          'Class B'='b',
          'Class C'='c'
        )
      ),
      selectInput(
        inputId = "fspeed",
        label = strong("First Road Speed"),
        choices = c(
          '20mph'=20,
          '30mph'=30,
          '40mph'=40,
          '50mph'=50,
          '60mph'=60,
          '70mph'=70
        )
      ),
      selectInput(
        inputId = "fstop",
        label = strong("First Type"),
        choices = c(
          'Complete Stop'='stop',
          'Merger'='merge'
        )
      ),
      selectInput(
        inputId = "stype",
        label = strong("Second Road"),
        choices = c(
          'Class M'='m',
          'Class A'='a',
          'Class B'='b',
          'Class C'='c'
        )
      ),
      selectInput(
        inputId = "sspeed",
        label = strong("Second Road Speed"),
        choices = c(
          '20mph'=20,
          '30mph'=30,
          '40mph'=40,
          '50mph'=50,
          '60mph'=60,
          '70mph'=70
        )
      ),
      selectInput(
        inputId = "sstop",
        label = strong("Second Type"),
        choices = c(
          'Complete Stop'='stop',
          'Merger'='merge'
        )
      ),
      selectInput(
        inputId = "urban",
        label = strong("Urbanity"),
        choices = c(
          'Urban'='urban',
          'Rural'='rural',
          'Other'='other'
        )
      )
    ),
    mainPanel(
      plotOutput(
        outputId = 'chart',
        height = '400px'
      ),
      textOutput(
        outputId = "desc"
      ),
      h4(
        'analysis and model by ',
        strong('J.E. Turcotte'),
        ' at ',
        a('https://github.com/jeturcotte/crashes')
      ),
      h5(
        'Submitted as final project for CDS303 at ',
        a('https://www.gmu.edu')
      ),
      h5(
        'data found at ',
        a('https://www.kaggle.com/datasets/benoit72/uk-accidents-10-years-history-with-many-variables'),
      )
    )
  )
)

server <- function( input, output, session ) {
  
  data <- reactive({
    
      # had to do this to help isolate why it was breaking
    ftype <- input$ftype
    fspeed <- as.numeric(input$fspeed)
    fstop <- input$fstop
    stype <- input$stype
    sspeed <- as.numeric(input$sspeed)
    sstop <- input$sstop
    urban <- input$urban
    
      # build a baseline table
    intersection <- as.data.frame(
      expand.grid(
        1:12,
        c( ftype, stype )
      )
    )
    colnames( intersection ) <- c('month','first.road')
    
      # because the model expected it?
      # seems like there's gotta be a better way
    intersection$fatal.cpm = 0.0
    intersection$serious.cpm = 0.0
    intersection$slight.cpm = 0.0
    
      # the months are more factor than numerical
    intersection$month <- as.factor( intersection$month )
    
      # now we gotta simulate the pre-dummy structure
    intersection$first.road <- prefactor( intersection$first.road, c( 'a', 'b', 'c', 'm') )
    intersection$second.road <- prefactor(
      ifelse( ftype == intersection$first.road, stype, ftype ),
      c( 'a', 'b', 'c', 'm')
    )
    
      # same
    intersection$junction <- prefactor(
      ifelse( ftype == intersection$first.road, fstop, sstop ),
      c( 'merge', 'stop' )
    )
    
      # same (turns out these need to be as.numeric from input)
    intersection$speed.limit <- ifelse(
      ftype == intersection$first.road, fspeed, sspeed
    )
    
      # more of the same
    intersection$urbanity <- urban
    intersection$urbanity <- prefactor(
      intersection$urbanity,
      c( 'urban', 'rural', 'other' )
    )
    
      # remember some things
    intersection$month.orig <- intersection$month
    intersection$first.road.orig <- ftype
    intersection$second.road.orig <- stype
    intersection$first.speed.orig <- fspeed
    intersection$second.speed.orig <- sspeed
    intersection$first.stop.orig <- fstop
    intersection$second.stop.orig <- sstop
    intersection$urban.orig <- urban
    
      # dummy it up
    intersection <- dummy_cols( 
      intersection, 
      select_columns = c(
        'month',
        'urbanity',
        'first.road',
        'second.road',
        'junction'
      ),
      remove_selected_columns = TRUE,
      ignore_na = TRUE
    )
    
      # now turn that pca into a prediction baseline
    intersection <- cbind(
      intersection,
      predict( pca, intersection )[,1:15]
    )
    
    #cat( paste( intersection[1,1:32]),'\n')
    
      # now finally let's get our guesses
    predictions <- rbind(
      data.frame(
        month = intersection$month.orig,
        first.road = intersection$first.road.orig,
        second.road = intersection$second.road.orig,
        first.speed = intersection$first.speed.orig,
        second.speed = intersection$second.speed.orig,
        first.stop = intersection$first.stop.orig,
        second.stop = intersection$second.stop.orig,
        urbanity = intersection$urban.orig,
        severity = 'slight',
        predicted = abs(predict( slight, intersection ))
      ),
      data.frame(
        month = intersection$month.orig,
        first.road = intersection$first.road.orig,
        second.road = intersection$second.road.orig,
        first.speed = intersection$first.speed.orig,
        second.speed = intersection$second.speed.orig,
        first.stop = intersection$first.stop.orig,
        second.stop = intersection$second.stop.orig,
        urbanity = intersection$urban.orig,
        severity = 'serious',
        predicted = abs(predict( serious, intersection ))
      ),
      data.frame(
        month = intersection$month.orig,
        first.road = intersection$first.road.orig,
        second.road = intersection$second.road.orig,
        first.speed = intersection$first.speed.orig,
        second.speed = intersection$second.speed.orig,
        first.stop = intersection$first.stop.orig,
        second.stop = intersection$second.stop.orig,
        urbanity = intersection$urban.orig,
        severity = 'fatal',
        predicted = abs(predict( fatal, intersection ))
      )
    )
    
    rm(intersection)
    predictions
  })
  
  output$chart <- renderPlot({
    pred <- data()
    ggplot( pred, aes( 
      fill=severity, 
      y=predicted, 
      x=month
    ) ) +
    geom_bar(
      position = "stack",
      stat = "identity"
    ) +
    theme_minimal()
  })
  
  output$desc <- renderText({
    preds <- data()
    total.slight <- sum( preds[preds$severity=='slight', 'predicted'] )
    total.serious <- sum( preds[preds$severity=='serious', 'predicted'] )
    total.fatal <- sum( preds[preds$severity=='fatal', 'predicted'] )
    
    first.road <- preds[1,'first.road']
    second.road <- preds[1,'second.road']
    first.speed <- preds[1,'first.speed']
    second.speed <- preds[1,'second.speed']
    first.stop <- preds[1,'first.stop']
    second.stop <- preds[1,'second.stop']
    urbanity <- preds[1,'urbanity']
    
    text <- paste0(
        'For ',
        ifelse(
          urbanity == 'other',
          'an unclassified ',
          ifelse(
            urbanity == 'rural',
            'a rural ',
            'an urban '
          )
        ),
        first.speed, 'mph ',
        toupper(first.road),
        '-Class road that has a ',
        first.stop,
        '-vs-',
        second.stop,
        ' with a ',
        second.speed,
        'mph ',
        toupper(second.road),
        '-Class road, you might expect roughly ',
        ceiling(total.slight),
        ' minor accidents along with approxinmately ',
        round(total.serious, digits=3),
        ' serious and ',
        round(total.fatal, digits=4),
        ' fatal car wrecks over the course of a typical year.'
      )
    
    text
  })
}

shinyApp(ui = ui, server = server )
