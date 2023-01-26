set_numerics <- function(data){
  list(
    'SalePrice',
    'Size.sqf.',
    'YearBuilt',
    'YrSold',
    'MonthSold',
    'N_FacilitiesNearBy.PublicOffice.',
    'N_FacilitiesNearBy.Hospital.',
    'N_FacilitiesNearBy.Dpartmentstore.',
    'N_FacilitiesNearBy.Mall.',
    'N_FacilitiesNearBy.ETC.',
    'N_FacilitiesNearBy.Park.',
    'N_SchoolNearBy.Elementary.',
    'N_SchoolNearBy.Middle.',
    'N_SchoolNearBy.High.',
    'N_SchoolNearBy.University.',
    'N_FacilitiesInApt',
    'N_FacilitiesNearBy.Total.',
    'N_SchoolNearBy.Total.',
    'N_Parkinglot.Ground.',
    'N_Parkinglot.Basement.',
    'N_APT',
    'N_manager',
    'N_elevators',
    'Floor'
  ) -> quantitative_vars
  
  list(
    'HallwayType',
    'HeatingType',
    'AptManageType',
    'TimeToBusStop',
    'TimeToSubway',
    'SubwayStation'
  ) -> qualitative_vars
  
  stopifnot(length(quantitative_vars) + length(qualitative_vars) == length(data))
  
  for (var in quantitative_vars)
    data[,var] <- data[,var] %>% as.numeric()
  
  return(data)
}


boxplot_mano <- function(df, target, var){
  data <- df
  data[,var] <- data[,var] %>% as.character
  
  g <- ggplot(data, aes(data[,var], colour=data[,var], fill=data[,var])) +
    geom_bar() +
    scale_color_discrete(name=var) + scale_fill_discrete(name=var)+
    labs(x=var)
  print(g)
  
  g <- ggplot(data, aes(y=data[,target], x=data[,var], colour=data[,var], fill=data[,var])) +
    geom_boxplot(alpha=0.5, outlier.alpha=0)+geom_jitter(width=0.25)+
    stat_summary(fun=mean, colour="black", geom="point",shape=18, size=3)+
    scale_color_discrete(name=var) + scale_fill_discrete(name=var)+
    labs(x=var, y=target, title = paste(target, "vs", var))
  print(g)
}

hist_and_density <- function(data, var, binwidth=10){
  ggplot(data, aes(x=data[,var])) +
    geom_density() +
    geom_histogram(aes(y=..density..), 
                   colour=1, 
                   fill='white',
                   binwidth=binwidth) +
    labs(x=var)
}


cor_qual <- function(df, var_quant, var_qual){
  data <- df
  data[,var_qual] <- data[,var_qual] %>% as.character
  dummy_var <- as.data.frame(model.matrix(~ data[,var_qual] -1, data))
  return(cor(dummy_var, data[,var_quant]))
}


# ggplot(train, aes(x=YrSold, y=SalePrice)) +
#   geom_point() +
#   geom_smooth()

