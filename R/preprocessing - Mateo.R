strategies$YearBuilt = list(breaks = c(-Inf, 2000, Inf))
strategies$YrSold = list(breaks = c(-Inf, 2011, 2014, Inf))

strategies$TimeToSubway = list("0-5min" = "0-5min",
                               "10min-15min" = "other",
                               "15min-20min" = "other",
                               "5min-10min" = "other",
                               "no_bus_stop_nearby" = "other")


strategies$N_manager = list(breaks = c(-Inf, 4, Inf))
strategies$SubwayStation = list("Chil-sung-market" = "low",
                                "Daegu" = "1ow",
                                "Bangoge" = 'medium',
                                "Myung-duk" = "medium",
                                "Banwoldang" = 'high',
                                "Kyungbuk_uni_hospital" = 'high',
                                "no_subway_nearby" = 'high',
                                "Sin-nam" = 'high')
strategies$N_FacilitiesNearBy.PublicOffice. = list(breaks = c(-Inf, 4, Inf))
strategies$N_SchoolNearBy.High. = list(breaks = c(-Inf, 2, Inf))
strategies$N_SchoolNearBy.University. = list(breaks = c(-Inf, 2, Inf))
strategies$N_FacilitiesInApt = list(breaks = c(-Inf, 3, 8, Inf))
cut(train$YrSold, breaks = strategies$YrSold$breaks, right = TRUE)

modify_categories(train, strategies)



modify_categories <- function(df, strategies){
  
  variables <- names(strategies)
  
  for(variable in variables) {
    print(variable)
    strategy <- strategies[[variable]]
    new_var = sprintf('%s_cat', variable)
    
    if(is.numeric(df[,variable]) || (is.factor(df[,variable]) && !is.null(strategy$breaks))) {
      
      if (is.character(df[,variable])) {
        df[,variable] <- as.numeric(as.factor(df[,variable]))
      }
      df[,new_var] <- cut(df[,variable], breaks = strategy$breaks, right = TRUE)
    } else {
      df[, new_var] = mapvalues(df[,variable], from = names(strategy), to = strategy)
    }
    print(df[, new_var])
  }
  return(df)
}



strategies$TimeToSubway = list("0-5min" = "0-5min",
                               "10min-15min" = "other",
                               "15min-20min" = "other",
                               "5min-10min" = "other",
                               "no_bus_stop_nearby" = "no_bus_stop_nearby")



strategies$YearBuilt = list(breaks = c(-Inf, 2010, 2013, Inf))



features_cat <- grep("_cat$", colnames(train), value = TRUE)

# Sélectionner les colonnes qui n'ont pas été modifiées
features_not_modified <- setdiff(colnames(train),features_cat)


# Réunir les colonnes qui ont le suffixe '_cat' et celles qui n'ont pas été modifiées
features_selected <- c(features_cat, features_not_modified)



strategies$YrSold = list(breaks = c(-Inf, 2011, 2014, Inf))

train_bis <-  train_bis[, -which(names(train_bis) == "MonthSold")]
train_bis <- train_bis[, -which(names(train_bis) == "N_APT")]

strategies$TimeToSubway = list("0-5min" = "0-5min",
                               "10min~15min" = "other",
                               "15min~20min" = "other",
                               "5min~10min" = "other",
                               "no_bus_stop_nearby" = "other")


strategies$N_manager = list(breaks = c(-Inf, 4, Inf))
strategies$SubwayStation = list("Chil-sung-market" = "low",
                                "Daegu" = "1ow",
                                "Bangoge" = 'medium',
                                "Myung-duk" = "medium",
                                "Banwoldang" = 'high',
                                "Kyungbuk_uni_hospital" = 'high',
                                "no_subway_nearby" = 'high',
                                "Sin-nam" = 'high')
strategies$N_FacilitiesNearBy.PublicOffice. = list(breaks = c(-Inf, 4, Inf))
strategies$N_SchoolNearBy.High. = list(breaks = c(-Inf, 2, Inf))
strategies$N_SchoolNearBy.University. = list(breaks = c(-Inf, 2, Inf))
strategies$N_FacilitiesInApt = list(breaks = c(-Inf, 2,  Inf))
strategies$N_FacilitiesInApt = list(breaks = c(-Inf, 3, 8, Inf))
cut(train$YrSold, breaks = strategies$YrSold$breaks, right = TRUE)