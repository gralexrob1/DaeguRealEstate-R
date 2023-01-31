
##### ALEX #####

set_numerics <- function(data, vars){
  for(var in vars)
    data[,var] <- data[,var] %>% as.numeric()
  
  return(data)
}

get_correlation <- function(data){
  #' Return correlation matrix
  #' 
  #' @param data A data.frame
  
  correlation_matrix = data %>% 
    select_if(is.numeric) %>% 
    cor(method='spearman')
  
  corrplot(correlation_matrix, 
           order="hclust", 
           tl.cex = 0.4, 
           type='upper'
           )
  recordPlot()
}


boxplot_mano <- function(df, target, var, both=T){
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
    geom_histogram(aes(y=..density..), 
                   colour=1, 
                   fill='white',
                   binwidth=binwidth) +
    geom_density(alpha=.2, fill="#FF6666") +
    labs(x=var, title = paste(var, "distribution"))
  
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

delete_variables <- function(data, vars){
  for(var in vars) {
    data <-  data[, -which(names(data) == var)]
  }
  return(data)
}


##### MATTEO #####


data_dtypes = function(data){
  cat_vars = c('N_Parkinglot.Ground.', 'N_Parkinglot.Basement.',
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

correlation_cat_plot = function(data){
  
  vars_categorical = data %>% select_if(is.factor) %>% colnames()
  n_vars_categorical = length(vars_categorical)
  
  matrix_correlation = matrix(data = -2, 
                              nrow = n_vars_categorical,
                              ncol = n_vars_categorical)
  data_cat = data %>% select_if(is.factor)
  for (ii in 1:n_vars_categorical){
    for (jj in 1:n_vars_categorical){
      
      corr = vcd::cramer.v(table(data_cat[, ii], data_cat[, jj]))
      matrix_correlation[ii, jj] = corr
    }
  }
  colnames(matrix_correlation) = vars_categorical
  rownames(matrix_correlation) = vars_categorical
  corrplot(matrix_correlation, order="hclust",tl.cex = 0.4)
}

correlation_num_plot = function(data){
  
  data_num = data %>% select_if(is.numeric)
  matrix_correlation = cor(data_num)
  colnames(matrix_correlation) = colnames(data_num)
  rownames(matrix_correlation) = colnames(data_num)
  corrplot(matrix_correlation, order="hclust",tl.cex = 0.4)
}

outliers_remover = function(data, var){
  Q1 <- quantile(data[[var]], probs = 0.25)
  Q3 <- quantile(data[[var]], probs = 0.75)
  IQR <- Q3-Q1
  data = data %>% filter_at(vars(var),  ~(. > Q1 - 1.5 * IQR) & (. < Q3 + 1.5 * IQR))
  return(data)
}

