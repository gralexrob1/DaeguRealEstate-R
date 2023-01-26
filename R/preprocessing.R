preprocessing <- function(data){
  
  ##### YearBuilt #####
  
  # avant 2000, après 2010
  data <- data %>%
    mutate(YearBuilt_2000_2010 = if_else(YearBuilt<=2000, "ante2000", 
                                         ifelse(YearBuilt<=2010, "2000to2010", "post2010")
    )
    )
  data$YearBuilt_2000_2010 <- data$YearBuilt_2000_2010 %>%
    as.factor()
  
  # avant après 2000
  data <- data %>%
    mutate(YearBuilt_2005 = if_else(YearBuilt<=2005, "ante2005", "post2005"))
  
  data$YearBuilt_2005 <- data$YearBuilt_2005 %>%
    as.factor()
  
  
  ##### YrSale  ##### 
  
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
  
  
  ##### N_Parkinglot.Ground. ##### 
  ##### N_Parkinglot.Basement.   ##### 
  
  ##### TimeToBusStop ##### 
  
  ##### TimeToSubway ##### 
  
  # 0-5 vs all
  data <- data %>%
    mutate(TimeToSubway_0_5 = if_else(TimeToSubway=='0-5min', "0-5min", "other"))
  
  data$TimeToSubway_0_5 <- data$TimeToSubway_0_5 %>%
    as.factor()
  
  # 0-5 vs No vs other
  data <- data %>%
    mutate(TimeToSubway_0_5_No = if_else(TimeToSubway=='0-5min', "0-5min", 
                                         ifelse(TimeToSubway=="no_bus_stop_nearby", "No", "other")
    )
    )
  data$TimeToSubway_0_5_No <- data$TimeToSubway_0_5_No %>%
    as.factor()
  
  
  ##### N_APT #####
  ##### N_manager #####
  
  # coupure à 4
  data <- data %>%
    mutate(N_manager_4 = if_else(N_manager %in% c(1,2,3,4), 'few', "many"))
  
  data$N_manager_4 <- data$N_manager_4 %>%
    as.factor()
  
  
  ##### N_elevators #####
  
  
  ##### SubwayStation #####
  
  group1 <- c('Sin-nam','Bangoge','Myung-duk')
  group2 <- c('Daegu','Chil-sing-market')
  group3 <- c('Banwoldang', 'Kyungbuk-uni-hospital')
  data <- data %>% 
    mutate(SubwayStation_byProxy1 =
             if_else(SubwayStation %in% group1, 'group1',
                     if_else(SubwayStation %in% group2, 'group2',
                             if_else(Subway %in% group3, 'group3', 'None')
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
                             if_else(Subway %in% group3, 'group3', 'None')
                     )
             )
    )
  
  ##### N_FacilitiesNearBy.PublicOffice. #####
  
  # coupure à 4
  data <- data %>%
    mutate(N_FacilitiesNearBy.PublicOffice._4 = 
             if_else(N_FacilitiesNearBy.PublicOffice. %in% c(1,2,3,4), 'few', "many"))
  
  # groupe stats
  data <- data %>%
    mutate(N_FacilitiesNearBy.PublicOffice._gr_stat = 
             if_else(N_FacilitiesNearBy.PublicOffice. %in% c(1,2,3), '123', 
                     if_else(N_FacilitiesNearBy.PublicOffice. %in% c(5,6,7), "567", "04")
             )
    )

  
  ##### N_FacilitiesNearBy.Hospital. #####
  
  # 0 vs 1,2
  data <- data %>%
    mutate(N_FacilitiesNearBy.Hospital._0vsOther = 
             if_else(N_FacilitiesNearBy.Hospital. %in% c(1,2), 'some', "none"))
  
  
  ##### N_FacilitiesNearBy.Dpartmentstore. #####
  
  # 0,1 vs 2
  data <- data %>%
    mutate(N_FacilitiesNearBy.Dpartmentstore._2vsOther = 
             if_else(N_FacilitiesNearBy.Dpartmentstore. %in% c(0,1), '1orNone', "Two"))
  
  ##### N_FacilitiesNearBy.Mall. #####
  
  
  ##### N_FacilitiesNearBy.ETC. #####
  
  # 0 vs others
  data <- data %>%
    mutate(N_FacilitiesNearBy.ETC._0vsOther = 
             if_else(N_FacilitiesNearBy.ETC. %in% c(0), 'None', "Other"))
  
  
  ##### N_FacilitiesNearBy.Park. #####
  
  # 0,1 vs 2
  data <- data %>%
    mutate(N_FacilitiesNearBy.Park._2vsOther = 
             if_else(N_FacilitiesNearBy.Park. %in% c(0,1), '1orNone', "Two"))
  
  ##### N_SchoolNearBy.Elementary. #####
  
  # as factor
  data$N_SchoolNearBy.Elementary._chr <- data$N_SchoolNearBy.Elementary. %>%
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
  
  # as factor
  data$N_SchoolNearBy.Middle._chr <- data$N_SchoolNearBy.Middle. %>% 
    as.character
  
  
  
  ##### N_SchoolNearBy.High. #####
  
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
  
  corr_pos_Facilities <- c('N_FacilitiesNearBy.Dpartmentstore.',
                           'N_FacilitiesNearBy.Mall.',
                           'N_FacilitiesNearBy.Park.'
  )
  corr_neg_Facilities <- c('N_FacilitiesNearBy.PublicOffice.',
                           'N_FacilitiesNearBy.Hospital.',
                           'N_FacilitiesNearBy.ETC.'
  )
  data <- data %>% mutate(N_FacilitiesNearby.corr_pos = 
                              N_FacilitiesNearBy.Dpartmentstore. + 
                              N_FacilitiesNearBy.Mall.+
                              N_FacilitiesNearBy.Park.
  )  
  data <- data %>% mutate(N_FacilitiesNearby.corr_neg = 
                              N_FacilitiesNearBy.PublicOffice. + 
                              N_FacilitiesNearBy.Hospital.+
                              N_FacilitiesNearBy.Park.
  )  
  
  
  ##### N_SchoolNearBy.Total. #####

}
