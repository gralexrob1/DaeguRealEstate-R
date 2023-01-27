preprocessing <- function(data){
  
  ##### YearBuilt #####
  # avant 2000, après 2010
  data <- data %>%
    mutate(YearBuilt_2000_2010 = if_else(YearBuilt<=2000, "ante2000", 
                                         ifelse(YearBuilt<=2010, "2000to2010", "post2010")
    )
    )
  data$YearBuilt_2000_2010 <- data$YearBuilt_2000_2010 %>%
    as.character()
  
  # avant après 2000
  data <- data %>%
    mutate(YearBuilt_2005 = if_else(YearBuilt<=2005, "ante2005", "post2005"))
  
  data$YearBuilt_2005 <- data$YearBuilt_2005 %>%
    as.character()
  
  
  ##### YrSold  ##### 
  # méthode des deux quantiles 33% et 66%
  data <- data %>%
    mutate(YrSold_2011_2014 = if_else(YrSold < 2011, "ante2011", 
                                      ifelse(YrSold < 2014, "2011to2014", "post2014")
    )
    )
  data$YrSold_2011_2014 <- data$YrSold_2011_2014 %>%
    as.factor()
  
  # méthode 50/50
  data <- data %>%
    mutate(YrSold_2013 = if_else(YrSold < 2013, "ante2013", "post2013"))
  data$YrSold_2013 <- data$YrSold_2013 %>%
    as.factor()
  
  ##### MonthSold #####
  # categorical or dropped
  data$MonthSold <- data$MonthSold %>% as.character
  
  ##### Size.sqf. #####
  
  ##### Floor #####
  # should be numeric
  
  ##### HallwayType #####
  # no need to modify
  
  ##### HeatingType #####
  # should be merged  or dropped (equivalent)
  
  ##### AptManageType #####
  # no need to modify
  
  ##### N_Parkinglot.Ground. ##### 
  # should be numeric, will probably get skipped
  
  ##### N_Parkinglot.Basement. #####
  # should be numeric
  
  ##### TimeToBusStop ##### 
  # no modification
  
  ##### TimeToSubway ##### 
  # 0-5 vs all
  group1 <- c('0-5min')
  data <- data %>%
    mutate(TimeToSubway_05vsOther = if_else(TimeToSubway %in% group1, "group1", "group2"))
  data$TimeToSubway_05vsOther <- data$TimeToSubway_05vsOther %>%
    as.character()
  
  # 0-5 vs No vs other
  group1 <- c('no_bus_stop_nearby')
  group2 <- c('0-5min')
  data <- data %>%
    mutate(TimeToSubway_Novs05vsOther = if_else(TimeToSubway %in% group1, "group1", 
                                         ifelse(TimeToSubway %in% group2, "group2", 
                                                "group3")
                                         )
           )
  data$TimeToSubway_Novs05vsOther <- data$TimeToSubway_Novs05vsOther %>%
    as.character()
  
  ##### N_APT #####
  # weak correlation, no modification
  
  ##### N_manager #####
  # coupure à 4
  group1 <- c(1,2,3,4)
  data <- data %>%
    mutate(N_manager_splitAt4 = if_else(N_manager %in% group1, 'lessThan4', "moreThan4"))
  
  ##### N_elevators #####
  # chaotic, not sure it can event be used
  
  ##### SubwayStation #####
  group1 <- c('Sin-nam','Bangoge','Myung-duk')
  group2 <- c('Daegu','Chil-sing-market')
  group3 <- c('Banwoldang', 'Kyungbuk-uni-hospital')
  data <- data %>% 
    mutate(SubwayStation_byProxy1 =
             if_else(SubwayStation %in% group1, 'group1',
                     if_else(SubwayStation %in% group2, 'group2',
                             if_else(SubwayStation %in% group3, 'group3', 'None')
                             )
                     )
           )
                                            
  group1 <- c('Bangoge','Myung-duk')
  group2 <- c('Daegu','Chil-sing-market')
  group3 <- c('Banwoldang','Kyungbuk-uni-hospital','Sin-nam')
  data <- data %>% 
    mutate(SubwayStation_byProxy2 =
             if_else(SubwayStation %in% group1, 'group1',
                     if_else(SubwayStation %in% group2, 'group2',
                             if_else(SubwayStation %in% group3, 'group3', 'None')
                     )
             )
    )
  
  ##### N_FacilitiesNearBy.PublicOffice. #####
  # coupure à 4
  group1 <- c(1,2,3,4)
  data <- data %>%
    mutate(N_FacilitiesNearBy.PublicOffice._splitAt4 = 
             if_else(N_FacilitiesNearBy.PublicOffice. %in% group1, 'few', "many"))
  
  # groupe stats
  group1 <- c(1,2,3)
  group2 <- c(5,6,7)
  group3 <- c(0,4)
  data <- data %>%
    mutate(N_FacilitiesNearBy.PublicOffice._gr_stat = 
             if_else(N_FacilitiesNearBy.PublicOffice. %in% group1, '123', 
                     if_else(N_FacilitiesNearBy.PublicOffice. %in% group2, "567", "04")
             )
    )

  
  ##### N_FacilitiesNearBy.Hospital. #####
  # 0 vs 1,2
  group1 <- c(0)
  group2 <- c(1,2)
  data <- data %>%
    mutate(N_FacilitiesNearBy.Hospital._0vs12 = 
             if_else(N_FacilitiesNearBy.Hospital. %in% group1, 'none', "some"))
  
  
  ##### N_FacilitiesNearBy.Dpartmentstore. #####
  # 0,1 vs 2
  group1 <- c(0,1)
  group2 <- c(2)
  data <- data %>%
    mutate(N_FacilitiesNearBy.Dpartmentstore._01vs2 = 
             if_else(N_FacilitiesNearBy.Dpartmentstore. %in% group1, '01', "2"))
  
  ##### N_FacilitiesNearBy.Mall. #####
  # no modification, weak correlation
  
  ##### N_FacilitiesNearBy.ETC. #####
  # 0 vs others
  group1 <- c(0)
  data <- data %>%
    mutate(N_FacilitiesNearBy.ETC._0vsOther = 
             if_else(N_FacilitiesNearBy.ETC. %in% group1, 'None', "Other"))
  
  
  ##### N_FacilitiesNearBy.Park. #####
  # 0,1 vs 2
  group1 <- c(0,1)
  group2 <- c(2)
  data <- data %>%
    mutate(N_FacilitiesNearBy.Park._01vs2 = 
             if_else(N_FacilitiesNearBy.Park. %in% group1, '01', "2"))
  
  ##### N_SchoolNearBy.Elementary. #####
  # try numeric and categorical
  data$N_SchoolNearBy.Elementary._cat <- data$N_SchoolNearBy.Elementary. %>%
    as.character
   
  ##### N_SchoolNearBy.Middle. #####
  # 0 vs 1,2,3 vs 4
  group1 <- c(0)
  group2 <- c(1,2,3)
  group3 <- c(4)
  data <- data %>%
    mutate(N_SchoolNearBy.Middle._0vs123vs4 = 
             if_else(N_SchoolNearBy.Middle. %in% group1, 'group1', 
                     if_else(N_SchoolNearBy.Middle. %in% group2, 'group2', 
                             'group3'))
           )
  
  # categorical
  data$N_SchoolNearBy.Middle._chr <- data$N_SchoolNearBy.Middle. %>% 
    as.character
  
  ##### N_SchoolNearBy.High. #####
  # 0 vs 1,2 vs 3,4,5
  group1 <- c(0)
  group2 <- c(1,2)
  group3 <- c(3,4,5)
  data <- data %>%
    mutate(N_SchoolNearBy.High._0vs12vs345 = 
             if_else(N_SchoolNearBy.High. %in% group1, 'group1', 
                     if_else(N_SchoolNearBy.High. %in% group2, 'group2', 
                             'group3'))
    )
  
  
  ##### N_SchoolNearBy.University. #####
  
  # 0,1,2 vs 3,4,5
  group1 <- c(0,1,2)
  group2 <- c(3,4,5)
  data <- data %>%
    mutate(N_SchoolNearBy.University._012vs345 = 
             if_else(N_SchoolNearBy.University. %in% group1, 'group1', 'group2')
    )
  
  # 0 vs 1,2 vs 3,4,5
  group1 <- c(0)
  group2 <- c(1,2)
  group3 <- c(3,4,5)
  data <- data %>%
    mutate(N_SchoolNearBy.University._0vs12vs345 = 
             if_else(N_SchoolNearBy.University. %in% group1, 'group1', 
                     if_else(N_SchoolNearBy.University. %in% group2, 'group2', 
                             'group3'))
    )
  
  ##### N_FacilitiesInApt #####
  # [1,2,3] - [4,5,7,8] - [9,10] 
  group1 <- c(1,2,3)
  group2 <- c(4,5,6,7,8)
  group3 <- c(9,10)
  data <- data %>%
    mutate(N_FacilitiesInApt_123vs45678vs910 = 
             if_else(N_FacilitiesInApt %in% group1, 'group1', 
                     if_else(N_FacilitiesInApt %in% group2, 'group2', 
                             'group3'))
    )
  
  ##### N_FacilitiesNearBy.Total. #####
  # idea: merge variables correlated to target with same sign
  

  
  # 0 vs 3à9 vs 11à16
  group1 <- c(0)
  group2 <- c(3,6,7,8,9)
  group3 <- c(11,12,13,14,16)
  data <- data %>% 
    mutate(N_FacilitiesNearBy.Total._cut = if_else(N_FacilitiesNearBy.Total. %in% group1, 'none',
                                                   if_else(N_FacilitiesNearBy.Total. %in% group2, 'some', 
                                                           'loads')))

  ##### N_SchoolNearBy.Total. #####
  # no modification. an idea is to only keep N_SchoolNearBy.Total.
  
  # OUTPUT
  return(data)
}


preprocess.SalePrice <- function(data){
  #' Preprocesses target
  #' @param data A data.frame
  
  data <- data %>%
    mutate(SalePrice = sqrt(SalePrice))
  return(data)
}

preprocess.N_Facilities_Nearby <- function(data, strat=NULL, drop=TRUE){
  #' Preprocesses cluster N_FacilitiesNearBy
  #'
  #' @param data A data.frame
  #' @param strat Corresponds to the preprocessing strategy
  #' @param drop Should obsolete columns be dropped
  
  if(is.null(strat))
    return(data)
  
  total <- c('N_FacilitiesNearBy.Total.')
  
  corr_pos <- c('N_FacilitiesNearBy.Dpartmentstore.',
                'N_FacilitiesNearBy.Mall.',
                'N_FacilitiesNearBy.Park.'
  )
  corr_neg <- c('N_FacilitiesNearBy.PublicOffice.',
                'N_FacilitiesNearBy.Hospital.',
                'N_FacilitiesNearBy.ETC.'
  )
  if(strat=='corr'){
    data$N_FacilitiesNearby.corr_pos <- data %>%
      dplyr::select(all_of(corr_pos)) %>%
      rowSums()
    data$N_FacilitiesNearby.corr_neg <- data %>%
      dplyr::select(all_of(corr_neg)) %>%
      rowSums()
    
    if(drop){
      data <- data %>%
        dplyr::select(-which(names(data) %in% c(corr_pos, corr_neg, total)))
    }
    return(data)
  }
}


preprocess.SubwayStation <- function(data, strategie=NULL){
  
  if(is.null(strat))
    return(data)
  
  if(strategie==1){
    group1 <- c('Sin-nam','Bangoge','Myung-duk')
    group2 <- c('Daegu','Chil-sung-market')
    group3 <- c('Banwoldang', 'Kyungbuk_uni_hospital')
    data <- data %>% 
      mutate(SubwayStation_byProxy1 =
               if_else(SubwayStation %in% group1, 'group1',
                       if_else(SubwayStation %in% group2, 'group2',
                               if_else(SubwayStation %in% group3, 'group3', 'None')
                       )
               )
      )
    return(data)
  }
  
  if(strategie==2){
    group1 <- c('Bangoge','Myung-duk')
    group2 <- c('Daegu','Chil-sung-market')
    group3 <- c('Banwoldang','Kyungbuk_uni_hospital','Sin-nam')
    data <- data %>% 
      mutate(SubwayStation_byProxy2 =
               if_else(SubwayStation %in% group1, 'group1',
                       if_else(SubwayStation %in% group2, 'group2',
                               if_else(SubwayStation %in% group3, 'group3', 'None')
                       )
               )
      )
    return(data)
  }
}



factorize <- function(data){
  data <- data %>%
    mutate_if(is.character, as.factor)
  return(data)
}


engineering <- function(data){
  cat_vars <- c('N_Parkinglot.Ground.', 'N_Parkinglot.Basement.',
               'N_APT', 'N_manager', 'N_elevators', 'N_FacilitiesNearBy.PublicOffice.',
               'N_FacilitiesNearBy.Dpartmentstore.', 'N_FacilitiesNearBy.Mall.',
               'N_FacilitiesNearBy.ETC.', 'N_FacilitiesNearBy.Park.',
               'N_SchoolNearBy.Elementary.', 'N_SchoolNearBy.Middle.',
               'N_SchoolNearBy.High.', 'N_SchoolNearBy.University.',
               'N_FacilitiesInApt', 'N_FacilitiesNearBy.Total.',
               'N_SchoolNearBy.Total.', 'HallwayType', 'HeatingType', 'AptManageType',
               'TimeToBusStop', 'TimeToSubway', 'SubwayStation', 'N_FacilitiesNearBy.Hospital.',
               'MonthSold', 'YrSold', "YearBuilt")
  int_vars = c("SalePrice", "Size.sqf.")
  data = data %>% mutate_at(vars(matches(int_vars)), as.integer)
  data = data %>% mutate_at(vars(matches(cat_vars)), as.factor)
  return(data)
}
