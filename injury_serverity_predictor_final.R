library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Change This! 
setwd("C:\\Users\\kwame\\OneDrive - Microsoft\\Documents\\UW MSIS\\MSIS 510\\TrafficInjuries")

### Traffic Injuries Dataset
location = read.csv("caracteristiques-2019_01.csv")
vehicles = read.csv("vehicules-2019_01.csv")
road = read.csv("lieux-2019_01.csv")
injuries = read.csv("usagers-2019_01.csv")

# Identify nulls
sapply(injuries_filtered, function(x) 100*sum(is.na(x))/nrow(injuries_filtered))
table(road_filtered$lane_scheme)

# Filter for relevant columns
location_filtered = location %>% select("accidentID", "hr_min", "lighting", "location", 
                                        "intersection", "weather", "col_type", "deptID") %>% 
                                        drop_na()
location_filtered$lighting = ifelse(location_filtered$lighting == 1, 1, 0)

# Process Vehicles
vehicles_filtered = vehicles %>% select("accidentID", "vehicleID", "vehicle_cat", "obs_fixed", "obs_movable",
                                        "contact_point", "manv", "motor")

road$lane_scheme = ifelse(is.na(road$lane_scheme), 2, road$lane_scheme)
road_filtered = road %>% select(accidentID, road_cat, lane_scheme, 
                                number_lanes, reserved_lane, gradient,
                                road_condition, situation, max_speed) %>% 
                                filter(max_speed < 500 & max_speed >= 10) %>% drop_na()
road_filtered$road_condition = ifelse(road_filtered$road_condition == 1, 1, 0)

# Process and Visualize Injuries
injuries_filtered = injuries %>% select("accidentID", "vehicleID", "num_veh", 
                                        "userCat", "severity", "driver_gender",
                                        "driver_year_birth", "reason_for_travel",
                                        "safety1") %>% filter(driver_year_birth < 2004 & driver_year_birth > 1930)
injuries_filtered$professionalUse = ifelse(injuries_filtered$reason_for_travel == 4, 1, 0)

## Visualize Severity by Age
injuries_filtered$birthDecade = round(injuries_filtered$driver_year_birth,-1)
injury_severity = injuries_filtered %>% filter(userCat == 1) %>% 
  group_by(birthDecade) %>%  summarize(mean_severity = mean(severity))
#ggplot(data = injury_severity, aes(x=birthDecade, y=mean_severity)) + geom_bar(stat="identity", fill="steelblue") + coord_cartesian(ylim=c(1.5,2.7))

injuries_filtered$unequipped = (injuries_filtered$safety1 == 0) * 1
youngDrivers = injuries_filtered %>% filter(userCat == 1) %>% group_by(accidentID) %>% summarize(youngestDriver = max(driver_year_birth), noEquipment = max(unequipped), vehicle_count = n())
injuriesFinal = merge(injuries_filtered, youngDrivers, "accidentID") 
#injuriesFinal = injuries2 %>% group_by(accidentID, vehicleID, youngestDriver, noEquipment) 

trafficFinal = injuriesFinal %>% 
               merge(vehicles_filtered, by=c("accidentID", "vehicleID")) %>% 
               merge(road_filtered) %>%
               merge(location_filtered) 




# Data Visualization
ggplot(data=trafficFinal, aes(x=factor(professionalUse), y=severity)) + geom_bar(stat="summary", fun="mean")
ggplot(data=trafficFinal, aes(x=factor(vehicle_cat), y=severity)) + geom_bar(stat="summary", fun="mean")
ggplot(data=trafficFinal, aes(x=factor(road_cat), y=severity)) + geom_bar(stat="summary", fun="mean")
ggplot(data = trafficFinal, aes(x=factor(birthDecade), y=severity)) + geom_bar(stat="summary", fun="mean", fill="steelblue") + coord_cartesian(ylim=c(1.5,2.7))
ggplot(data=trafficFinal, aes(x=factor(max_speed), y=severity)) + geom_bar(stat="summary", fun="mean")
ggplot(data=trafficFinal, aes(x=factor(driver_year_birth), y=severity)) + geom_bar(stat="summary", fun="mean")

# Target Encoding
targetVehicleCat = trafficFinal %>% group_by(vehicle_cat) %>% summarize(vehicleCatSeverity = mean(severity))
trafficFinal = trafficFinal %>% merge(targetVehicleCat)

# Target Encoding on dept_id
targetDeptId = trafficFinal %>% group_by(deptID) %>% summarize(deptIdSeverity = mean(severity))
trafficFinal = trafficFinal %>% merge(targetDeptId)

# Modeling - logistic regression, random forest

trafficSimplified = trafficFinal %>% select(deptIdSeverity, birthDecade,professionalUse, noEquipment, youngestDriver, 
                                            vehicle_count, vehicleCatSeverity, road_condition, max_speed, lighting, severity)
head(trafficSimplified)

# Convert to binary
trafficSimplified$severity = ifelse((trafficSimplified$severity == 1 | trafficSimplified$severity == 4), 0, 1)
trafficSimplified$severity = factor(trafficSimplified$severity)

# Downsample majority class: https://topepo.github.io/caret/subsampling-for-class-imbalances.html
up_train <- upSample(x = trafficSimplified[, -ncol(trafficSimplified)], y = trafficSimplified$severity, yname = "severity")

library(ranger)
# Original
#train_index = sample(1:nrow(trafficSimplified), nrow(trafficSimplified)*0.7)
#train_df = trafficSimplified[train_index,]
#test_df = trafficSimplified[-train_index,]

# Upsampled 
train_index = sample(1:nrow(up_train), nrow(up_train)*0.7)
train_df = up_train[train_index,]
test_df = up_train[-train_index,]


# Ranger Package: https://arxiv.org/pdf/1508.04409.pdf 
ptm <- proc.time()
rf_importance = ranger(severity ~ ., data=train_df, importance = "permutation")
importance(rf_importance)
rf_Tree = ranger(severity ~ ., data=train_df, write.forest = TRUE, num.trees = 1000)
proc.time() - ptm

# Evaluation
pred = predict(rf_Tree, data=test_df) 
pred_df = pred$predictions %>% factor()
confusionMatrix(factor(test_df$severity), pred_df)