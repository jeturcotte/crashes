
  # source of data at:
  # https://www.kaggle.com/datasets/benoit72/uk-accidents-10-years-history-with-many-variables

  # TODO - it seems that junction tpes are misclassified sometimes, re: A329's
  # intersection with u7201 is a roundabout but sometimes it's listed as a T
  # so may consider trying to identify any listed as a roundabout and then
  # consolidate any non-roundabouts with the same number-by-number AS a round
  # -about to reduce error somewhat????

library(readxl)
library(tidymodels)
library(ranger)
library(caret)
library(dplyr)
set.seed( 7041 )

  # for easier reading:
`%not in%` <- Negate(`%in%`)

  # core data
accident <- read.csv('accidents/Accidents0514.csv')
vehicle <- read.csv('accidents/Vehicles0514.csv')

  # NA ... 
  # after studying the xlxs (below), all NA are -1
  # so, for practical purposes I'm putting them back in

accident <- accident %>% mutate( across( where( is.integer ), ~na_if( ., -1 ) ) )
vehicle <- vehicle %>% mutate( across( where( is.integer ), ~na_if( ., -1 ) ) )

  # metadata / factor labels
meta <- 'accidents/Road-Accident-Safety-Data-Guide.xls'  

  # semigeneric
weekday <- read_excel( meta, sheet='Day of Week' )
officer <- read_excel( meta, sheet='Police Officer Attend' )
gender <- read_excel( meta, sheet='Sex of Driver' )
age.band <- read_excel( meta, sheet='Age Band' )

  # accident
accident.severity <- read_excel( meta, sheet='Accident Severity' )
accident.vehicle.motion <- read_excel( meta, sheet='Skidding and Overturning' )
accident.vehicle.leaving <- read_excel( meta, sheet='Veh Leaving Carriageway' )
accident.object.hit.on.road <- read_excel( meta, sheet='Hit Object in Carriageway' )
accident.object.hit.off.road <- read_excel( meta, sheet='Hit Object Off Carriageway' )
accident.impact.point <- read_excel( meta, sheet='1st Point of Impact' )
accident.travel.purpose <- read_excel( meta, sheet='Journey Purpose' )

  # location/road
road.class <- read_excel( meta, sheet='2nd Road Class' )
road.type <- read_excel( meta, sheet='Road Type' )
road.junction <- read_excel( meta, sheet='Junction Detail' )
road.junction.location <- read_excel( meta, sheet='Junction Location' )
road.control <- read_excel( meta, sheet='Junction Control' )
road.crossing.human <- read_excel( meta, sheet='Ped Cross - Human' )
road.crossing.physical <- read_excel( meta, sheet='Ped Cross - Physical' )

  # conditions
condition.light <- read_excel( meta, sheet='Light Conditions' )
condition.weather <- read_excel( meta, sheet='Weather' )
condition.surface <- read_excel( meta, sheet='Road Surface' )
condition.special <- read_excel( meta, sheet='Special Conditions at Site' )
condition.hazard <- read_excel( meta, sheet='Carriageway Hazards' )
condition.urbanity <- read_excel( meta, sheet='Urban Rural' )

  # vehicle
vehicle.type <- read_excel( meta, sheet='Vehicle Type' )
vehicle.towing <- read_excel( meta, sheet='Towing and Articulation' )
vehicle.action <- read_excel( meta, sheet='Vehicle Manoeuvre' )
vehicle.location <- read_excel( meta, sheet='Vehicle Location' )
vehicle.engine <- read_excel( meta, sheet='Vehicle Propulsion Code' )
vehicle.left.hand <- read_excel( meta, sheet='Was Vehicle Left Hand Drive' )

  # casualty
casualty.class <- read_excel( meta, sheet='Casualty Class' )
casualty.type <- read_excel( meta, sheet='Casualty Type' )
casualty.severity <- read_excel( meta, sheet='Casualty Severity' )

  # pedestrian
ped.location <- read_excel( meta, sheet='Ped Location' )
ped.motion <- read_excel( meta, sheet='Ped Movement' )
ped.is.maintenance <- read_excel( meta, sheet='Ped Road Maintenance Worker' )
    # close but not exactly the same as urban rural (double checked)
ped.home.type <- read_excel( meta, sheet='Home Area Type' )

  # passenger
pass.car.location <- read_excel( meta, sheet='Car Passenger' )
pass.bus.location <- read_excel( meta, sheet='Bus Passenger' )

  # http://mast.roadsafetyanalysis.org/wiki/index.php?title=Driver_IMD_Decile
  # appears to be an index of economy deprivation of an individual
imd.decile <- read_excel( meta, sheet='IMD Decile' )

  # for the sake of being able to read the data
  # ACCIDENTs
accident <- merge( accident, accident.severity, by.x = 'Accident_Severity', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "severity"
accident <- merge( accident, road.class, by.x = 'X1st_Road_Class', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "first_road_class"
accident <- merge( accident, road.type, by.x = 'Road_Type', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "first_road_type"
accident <- merge( accident, road.junction, by.x = 'Junction_Detail', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "junction"
accident <- merge( accident, road.control, by.x = 'Junction_Control', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "junction_control"
accident <- merge( accident, road.class, by.x = 'X2nd_Road_Class', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "second_road_class"
accident <- merge( accident, road.crossing.human, by.x = 'Pedestrian_Crossing.Human_Control', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "controlled_crossing"
accident <- merge( accident, road.crossing.physical, by.x = 'Pedestrian_Crossing.Physical_Facilities', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "uncontrolled_crossing"
accident <- merge( accident, condition.light, by.x = 'Light_Conditions', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "light_condition"
accident <- merge( accident, condition.weather, by.x = 'Weather_Conditions', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "weather_condition"
accident <- merge( accident, condition.surface, by.x = 'Road_Surface_Conditions', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "road_condition"
accident <- merge( accident, condition.special, by.x = 'Special_Conditions_at_Site', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "special_condition"
accident <- merge( accident, condition.hazard, by.x = 'Carriageway_Hazards', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "road_hazard"
accident <- merge( accident, condition.urbanity, by.x = 'Urban_or_Rural_Area', by.y = 'code', all.x = TRUE )
colnames( accident )[ ncol( accident ) ] <- "urbanity"
accident$when <- as.POSIXct( strptime( paste( accident$Date, accident$Time, sep=" " ), "%d/%m/%Y %H:%M"), tz = 'GMT' )

  # now for a lot of mostly-meaningful dummy variables instead of pure categories

  # now, before we clean it up, let's make some road class indices based on size
accident$first_road_class <- as.factor( accident$first_road_class )
levels( accident$first_road_class ) <- c(
  'a', 'a', 'b', 'c', 'm', 'u'
)

  # and the intervening road class (if any) againt increasing in size/import
accident$second_road_class <- as.factor( accident$second_road_class )
levels( accident$second_road_class ) <- c(
  'a', 'a', 'b', 'c', 'm', 'u'
)

  # and intersection junction, if any, numbered by perceived difficulty
  # [1] "Crossroads"                          "Mini-roundabout"                    
  # [3] "More than 4 arms (not roundabout)"   "Not at junction or within 20 metres"
  # [5] "Other junction"                      "Private drive or entrance"          
  # [7] "Roundabout"                          "Slip road"                          
  # [9] "T or staggered junction"            
accident$junction <- as.factor( accident$junction )
levels( accident$junction ) <- c(
  'crossroad',
  'roundabout',
  'complex',
  'none',
  'other',
  'private',
  'roundabout',
  'ramp',
  't-junction'
)

  # and intersection control levels (within reason)
  # [1] "Authorised person"  "Auto traffic signal"  "Give way or uncontrolled" 
  # [4] "Not at junction or within 20 metres" "Stop sign" 
accident$junction_control <- as.factor( accident$junction_control )
levels( accident$junction_control ) <- c(
  'conductor',
  'signal',
  'merge',
  'none',
  'sign'
)
  
  # various uncontrolled crossings may be a facto
  # [1] "Central refuge" "Footbridge or subway"                                                     
  # [3] "No physical crossing facilities within 50 metres" "Pedestrian phase at traffic signal junction"                              
  # [5] "Pelican, puffin, toucan or similar non-junction pedestrian light crossing" "Zebra"   
accident$uncontrolled_crossing <- as.factor( accident$uncontrolled_crossing )
levels( accident$uncontrolled_crossing ) <- c(
  'refuge',
  'passageway',
  'none',
  'signal',
  'signal',
  'crosswalk'
)

  # road condition
  # [1] "Dry"  "Flood over 3cm. deep" "Frost or ice"  "Snow"                
  # [5] "Wet or damp"  
accident$road_condition <- as.factor( accident$road_condition )
levels( accident$road_condition ) <- c(
  'dry',
  'flooded',
  'ice',
  'snow',
  'wet'
)

  # weather condition; splitting wind out of precipitation
  # [1] "Fine + high winds"  "Fine no high winds"  "Fog or mist"  "Other"                
  # [5] "Raining + high winds"  "Raining no high winds" "Snowing + high winds"
  # [8] "Snowing no high winds" "Unknown" 
accident$precipitation <- accident$wind <- as.factor( accident$weather_condition )
levels( accident$precipitation ) <- c( 'fine', 'fine', 'fog', 'other', 'rain', 'rain', 'snow', 'snow', 'other' )
levels( accident$wind ) <- c( 'windy', 'calm', 'calm', 'windy', 'windy', 'calm', 'windy', 'calm', 'calm' )
accident$precipitation <- as.factor( as.character( accident$precipitation ) )
accident$wind <- as.factor( as.character( accident$wind ) )

  # lighting!!!
  # [1] "Darkness - lighting unknown" "Darkness - lights lit" 
  # [3] "Darkness - lights unlit" "Darkness - no lighting" "Daylight" 
accident$light_condition <- as.factor( accident$light_condition )
levels( accident$light_condition ) <- c(
  'dark',
  'lights',
  'dark',
  'dark',
  'daylight'
)

  # urban?
  # [1] "Rural" "Unallocated" "Urban" 
accident$urbanity <- as.factor( accident$urbanity )
levels( accident$urbanity ) <- c(
  'rural',
  'other',
  'urban'
)

  # severity?
  # [1] "Rural" "Unallocated" "Urban" 
accident$severity <- as.factor( accident$severity )
levels( accident$severity ) <- c(
  'fatal',
  'serious',
  'slight'
)

# narrowed down and cleaned up
accident <- accident[ ,c( 
  'Accident_Index', 
  'severity',
  'Number_of_Vehicles',
  'Number_of_Casualties',
  'when', 
  'Speed_limit',
  'urbanity',
  'first_road_class', 
  'X1st_Road_Number',
  'second_road_class',
  'X2nd_Road_Number',
  'junction',
  'junction_control',
  'uncontrolled_crossing',
  'light_condition',
  'road_condition',
  'precipitation',
  'wind'
) ]
colnames( accident ) <- c( 
  'aID', 
  'hit.severity',
  'hit.vehicles',
  'hit.casualties',
  'hit.when', 
  'road.speed.limit',
  'road.urbanity',
  'road.first.class', 
  'road.first.number',
  'road.second.class',
  'road.second.number',
  'road.junction.type',
  'road.junction.control',
  'road.ped.crossing',
  'cond.light',
  'cond.road',
  'cond.precip',
  'cond.wind'
)


  # VEHICLES
vehicle <- merge( vehicle, vehicle.type, by.x = 'Vehicle_Type', by.y = 'code', all.x = TRUE )
colnames( vehicle )[ ncol( vehicle ) ] <- "type"
vehicle <- merge( vehicle, vehicle.towing, by.x = 'Towing_and_Articulation', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "towing"
vehicle <- merge( vehicle, vehicle.action, by.x = 'Vehicle_Manoeuvre', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "action"
  # i'm not confident in this one; I haven't seen an alternative, but there's a code (10) that never occurs
  # in the vehicle data 
vehicle <- merge( vehicle, vehicle.location, by.x = 'Vehicle_Location.Restricted_Lane', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "location"
vehicle <- merge( vehicle, road.junction.location, by.x = 'Junction_Location', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "junction"
vehicle <- merge( vehicle, accident.vehicle.motion, by.x = 'Skidding_and_Overturning', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "motion"
vehicle <- merge( vehicle, accident.object.hit.on.road, by.x = 'Hit_Object_in_Carriageway', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "hit_on_road"
vehicle <- merge( vehicle, accident.object.hit.off.road, by.x = 'Hit_Object_off_Carriageway', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "hit_off_road"
vehicle <- merge( vehicle, accident.vehicle.leaving, by.x = 'Vehicle_Leaving_Carriageway', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "left_road"
vehicle <- merge( vehicle, accident.impact.point, by.x = 'X1st_Point_of_Impact', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "impact_point"
vehicle <- merge( vehicle, vehicle.left.hand, by.x = 'Was_Vehicle_Left_Hand_Drive.', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "handed"
  # lets make handedness easier to read
vehicle$handed <- as.factor(vehicle$handed)
levels(vehicle$handed) <- c( 'right', 'left' )
vehicle <- merge( vehicle, accident.travel.purpose, by.x = 'Journey_Purpose_of_Driver', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "trip_purpose"
vehicle <- merge( vehicle, gender, by.x = 'Sex_of_Driver', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "driver_gender"
vehicle <- merge( vehicle, vehicle.engine, by.x = 'Propulsion_Code', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "engine"
vehicle <- merge( vehicle, imd.decile, by.x = 'Driver_IMD_Decile', by.y = 'code', all.x = TRUE  )
colnames( vehicle )[ ncol( vehicle ) ] <- "imd_decile"

  # here we want to know about the car
  # and not so much the details of the accident
vehicle <- vehicle[ ,c(
  'Accident_Index',
  'Age_of_Driver',
  'driver_gender',
  'Engine_Capacity_.CC.',
  'handed',
  'Age_of_Vehicle',
  'type',
  'towing',
  'action'
)]

colnames(vehicle) <- c(
  'aID',
  'drv.age',
  'drv.gender',
  'eng.size',
  'veh.handed',
  'veh.age',
  'veh.type',
  'veh.towing',
  'veh.action'
)

  # driver gender?
  # [1] "Female"    "Male"      "Not known"
vehicle$drv.gender <- as.factor( vehicle$drv.gender )
levels( vehicle$drv.gender ) <- c(
  'f', 'm', 'none'
)


  # lets simplify the types a bit
  # [1] "Agricultural vehicle" "Bus or coach (17 or more pass seats)"  "Car" "Electric motorcycle"                  
  # [5] "Goods 7.5 tonnes mgw and over" "Goods over 3.5t. and under 7.5t" "Goods vehicle - unknown weight"  "Minibus (8 - 16 passenger seats)"     
  # [9] "Mobility scooter" "Motorcycle - unknown cc" "Motorcycle 125cc and under" "Motorcycle 50cc and under"            
  # [13] "Motorcycle over 125cc and up to 500cc" "Motorcycle over 500cc" "Other vehicle" "Pedal cycle"                          
  # [17] "Ridden horse" "Taxi/Private hire car" "Tram" "Van / Goods 3.5 tonnes mgw or under"   
vehicle$veh.type <- as.factor( vehicle$veh.type )
levels( vehicle$veh.type ) <- c(
  'tractor',
  'bus',
  'car',
  'motorcycle',
  'semi',
  'cargo',
  'cargo',
  'minibus',
  'scooter',
  'motorcycle',
  'motorcycle',
  'motorcycle',
  'motorcycle',
  'motorcycle',
  'unspecified',
  'bicycle',
  'horse',
  'car',
  'tram',
  'cargo'
)

  # and now for towing, if any
  # [1] "Articulated vehicle" "Caravan" "Double or multiple trailer"
  # [4] "No tow/articulation" "Other tow" "Single trailer"    
vehicle$veh.towing <- as.factor( vehicle$veh.towing )
levels( vehicle$veh.towing ) <- c(
  'agricultural',
  'caravan',
  'trailer',
  'none',
  'other',
  'trailer'
)

  # and what it's doing
  # [1] "Changing lane to left" "Changing lane to right" "Going ahead left-hand bend" "Going ahead other"                  
  # [5] "Going ahead right-hand bend" "Moving off" "Overtaking - nearside" "Overtaking moving vehicle - offside"
  # [9] "Overtaking static vehicle - offside" "Parked" "Reversing" "Slowing or stopping"                
  # [13] "Turning left" "Turning right" "U-turn" "Waiting to go - held up"            
  # [17] "Waiting to turn left" "Waiting to turn right"              
vehicle$veh.action <- as.factor( vehicle$veh.action )
levels( vehicle$veh.action ) <- c(
  'lane change',
  'lane change',
  'going ahead',
  'going ahead',
  'going ahead',
  'moving off',
  'overtaking',
  'overtaking',
  'overtaking',
  'parked',
  'reversing',
  'stopping',
  'turning',
  'turning',
  'u-turn',
  'waiting',
  'waiting',
  'waiting'
)

  # okay... now let's merge these together
crash <- merge( accident, vehicle, on = 'aID' )

  # sorry.. I may be a tad OCD?  probably not, but i do like cleanliness
rm( 
  accident,
  vehicle,
  meta, 
  weekday, 
  officer, 
  gender, 
  age.band,
  accident.severity, 
  accident.vehicle.motion, 
  accident.vehicle.leaving,
  accident.object.hit.on.road,
  accident.object.hit.off.road,
  accident.impact.point,
  accident.travel.purpose,
  road.class,
  road.type,
  road.junction,
  road.junction.location,
  road.control,
  road.crossing.human,
  road.crossing.physical,
  condition.light,
  condition.weather,
  condition.surface,
  condition.special,
  condition.hazard,
  condition.urbanity,
  vehicle.type,
  vehicle.towing,
  vehicle.action,
  vehicle.location,
  vehicle.engine,
  vehicle.left.hand,
  casualty.class,
  casualty.type,
  casualty.severity,
  ped.location,
  ped.motion,
  ped.is.maintenance,
  ped.home.type,
  pass.car.location,
  pass.bus.location,
  imd.decile
)

  # lets reduce the number of intersections by detecting and treating an
  # intersection A+B (4 way crossroad between an A road and a B road)
  # as the same as a B+A
simplify_intersection <- function( ac, bc, j ) {
  
    # there will be NAs that shouldn't omit the observation
  levels( bc ) <- c( levels( bc ), '' )
  bc[ is.na(bc) ] <- ''
  levels( ac ) <- c( levels( ac ), '' )
  j[ is.na( j ) ] <- 'none'
  
    # some labels for easy of reading later
  jlab <- c( '+', 'O', '*', '-', 'T', 'T', 'V', 'T' )
  
    # in case there's no second road detected
  label <- ifelse(
    bc == '' | jlab[ as.numeric( j ) ] == '-',
    ifelse(
      bc == '',
      paste(
        ac,
        '-',
        sep = ''
      ),
      paste(
        ac,
        jlab[ as.numeric(j) ],
        sep = ''
      )
    ),
    ifelse( as.numeric( bc ) > as.numeric( ac ), 
            paste(
              bc,
              jlab[ as.numeric(j) ],
              ac,
              sep = ''
            ),
            paste(
              ac,
              jlab[ as.numeric(j) ],
              bc,
              sep = ''
            )
    )
  )
  
  label
  
}

  # right, now assemble a 'class' index
  # using the above function to narrow down
  # the options
crash$road.junction.class <- as.factor( 
  simplify_intersection( 
    crash$road.first.class, 
    crash$road.second.class, 
    crash$road.junction.type
  ) 
)

intersection_name <- function( ax, j, bx ) {
  
    # reduce the junction to something simple again
  j[ is.na( j ) ] <- 'none'
  jlab <- c( '+', 'O', '*', '-', 'T', 'T', 'V', 'T' )
  
    # to minimize, we'll always have the lower of the two
    # named roads first, except when 0 (which basically means
    # the road exists, but has no official number)
    # herein, most major roadways have lower numbers and will
    # take precedence over smaller roads with higher numbers
    # the M4 > C202, for example ... works in the uk, but not
    # as often in the states, so ugh either way
    # regless, the point is since we care about intersections
    # then M4+C202 is the same to us as C202+M4 and we'd rather
    # not have the distinct in this effort
  name <- ifelse(
    is.na(bx) | jlab[as.numeric(j)] == '-',
    ax,
    ifelse(
      ax > bx & bx != 0,
      paste(
        bx, jlab[as.numeric(j)], ax, sep = ' '
      ),
      paste(
        ax, jlab[as.numeric(j)], bx, sep = ' '
      )
    )
  )
  
  name
  
}

  # give the place a name
crash$road.junction.name <- as.factor(
  intersection_name(
    crash$road.first.number, 
    crash$road.junction.type, 
    crash$road.second.number
  )
)

  # what I will do now is eliminate things that aren't unique ACTUAL interesections
  # for example... theres no way to tell one 0T0 from anther... and all the A- and
  # C- roads are just literally an A or C road that has no intersection, which certainly
  # have a lot of accidents but can't be isolated to individual examinable circumstances
  # so yeah, let's just focus on knowable intersections
crash <- crash[ crash$road.junction.class %not in% c( 'a-', 'b-', 'c-', 'm-', 'u-' ), ]
crash <- crash[ crash$road.first.number != 0 & crash$road.second.number != 0, ]

  # further, roads can't generally junction themselves, so they are likely mis-
  # entries by the authorities in such cases ... best removed to prevent noise
crash <- crash[ crash$road.first.number != crash$road.second.number, ]

  # make sure it's in GMT, fool
crash$hit.when <- as.POSIXct( crash$hit.when, tz = 'GMT' )
  # and also the month of the yea
crash$when.month <- strftime( crash$hit.when, tz = 'GMT', format = "%B" )
  # and also the actually day, too

  # now lets get an index that represents this kind of intersection
#crash$place.index <- as.factor(
#  paste(
#    as.numeric(crash$road.first.class),
#    as.numeric(crash$road.junction.type),
#    as.numeric(crash$road.junction.control),
#    as.numeric(crash$road.second.class),
#    crash$road.speed.limit,
#    sep = '|'
#  )
#)

  # also an index name
crash$name.index <- as.factor(
  paste(
    crash$road.first.number,
    crash$road.second.number,
    sep = '|'
  )
)
  # and its inverse, in case that's useful later
crash$name.inverse <- as.factor(
  paste(
    crash$road.second.number,
    crash$road.first.number,
    sep = '|'
  )
)

  # oh look, a save point 
#write.csv( crash, 'cds.303.final.preprocessed.crash.data.csv' )
#crash <- read.csv( 'cds.303.final.preprocessed.crash.data.csv', stringsAsFactors = TRUE )

  # in both the above cases, the direction from which you approach a junction
  # may well mean a different kind of merge, and at a different seem
  # so previous attempts to whittle down the list really may have done more harm
  # than good... 

  # now we want some kind of tally of accidents for the intersection type that
  # we can tie back in for use in a regression predictor
accident <- as.data.frame(
  expand.grid(
    levels( crash$name.index ),
    c( 'slight', 'serious', 'fatal' ),
    c( 
      'January', 'February', 'March', 'April',
      'May', 'June', 'July', 'August',
      'September', 'October', 'November', 'December'
    )
  )
)
accident$vehicles <- 0
accident$join <- paste( accident$Var1, accident$Var2, accident$Var3, sep = ' ' )

  # bop 
junc.total <- aggregate(
  crash$name.index,
  by = list(
    crash$name.index,
    crash$hit.severity,
    crash$when.month
  ),
  FUN = length
)
junc.total$join <- paste( junc.total$Group.1, junc.total$Group.2, junc.total$Group.3, sep = ' ' )

  # now reassemble them so we know which pairings had no accidents at all
colnames(accident) <- c( 'index', 'severity', 'month', 'vehicles', 'join' )
colnames(junc.total) <- c( 'index', 'severity', 'month', 'vehicles', 'join' )
crash.totals <- merge( accident, junc.total, by='join', all = TRUE )
crash.totals[ !is.na(crash.totals$vehicles.y), 'vehicles.x' ] <- crash.totals[ !is.na(crash.totals$vehicles.y), 'vehicles.y' ]
crash.totals <- crash.totals[ ,c(
  'index.x',
  'severity.x',
  'month.x',
  'vehicles.x'
)]
colnames(crash.totals) <- c(
  'junc',
  'sev',
  'month',
  'cvpm'
)
  # recall that these counts occur over 10 years
crash.totals$cvpm <- crash.totals$cvpm / 10

  # save some memory
rm(accident,junc.total)

  # now let's pivot these a bit
crash.totals <- crash.totals %>% pivot_wider( names_from = sev, values_from = cvpm )

  # oh look, another save point 
#write.csv( crash.totals, 'cds.303.final.crash.cvpm.data.csv' )
#crash.totals <- read.csv( 'cds.303.final.crash.cvpm.data.csv', stringsAsFactors = TRUE )

  # so lets get this hooked back up
crash.totals$join <- paste( crash.totals$junc, crash.totals$month, sep = ' ' )
crash$join <- paste( crash$name.index, crash$when.month, sep = ' ' )
crashes <- merge(
  crash[ ,c(
    'name.index',
    'name.inverse',
    'hit.when',
    'when.month',
    'hit.severity',
    'road.speed.limit',
    'road.urbanity',
    'road.first.class',
    'road.second.class',
    'road.junction.type',
    'road.junction.control',
    'cond.light',
    'cond.precip',
    'cond.road',
    'veh.type',
    'veh.action',
    'join'
  )],
  crash.totals[ ,c(
    'fatal',
    'serious',
    'slight',
    'join'
  )],
  by = 'join'
)


  # narrow it a bit for cleanliness
crashes <- crashes[ ,c(
  "name.index",
  "hit.when",
  "when.month",
  "hit.severity",
  "road.speed.limit",
  "road.urbanity",
  "road.first.class",
  "road.second.class",
  "road.junction.type",
  "road.junction.control",
  "cond.light",
  "cond.precip",
  "cond.road",
  "veh.type",
  "veh.action",
  "fatal",
  "serious",
  "slight"
)]

  # ocd
colnames( crashes ) <- c(
  "index",
  "when",
  "month",
  "severity",
  "speed.limit",
  "urbanity",
  "first.road",
  "second.road",
  "junction",
  "control",
  "light",
  "precip",
  "surface",
  "vehicle",
  "action",
  "fatal.cpm",
  "serious.cpm",
  "slight.cpm"
)

  # lets reduce the outputs a bit
levels( crashes$junction ) <- c(
  "stop",
  "merge",
  "other",
  "other",
  "stop",
  "merge",
  "merge",
  "stop"
)
crashes$junction <- as.factor( as.character( crashes$junction ) )

  # let's also reduce the vehicle types (memory issues)
levels( crashes$vehicle ) <- c(
  "cycle",
  "cargo",
  "car",
  "cargo",
  "other",
  "car",
  "cycle",
  "cycle",
  "cargo",
  "other",
  "other",
  "other"
)
crashes$vehicle <- as.factor( as.character( crashes$vehicle ) )

  # and let's numericize the month
crashes$month <- as.numeric( strftime( crashes$when, '%m' ) )

  # oh look, ANOTHER SAVE POINT !!!!
write.csv( crashes, 'cds.303.final.processed.crash.data.csv' )
#crashes <- read.csv( 'cds.303.final.processed.crash.data.csv', stringsAsFactors = TRUE )
rm( accident, crash, crash.totals, junc.total )

  # move on to cds.303.jturcott.final.model.R
