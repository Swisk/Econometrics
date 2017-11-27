
df <- read.csv("C:\\Users\\Shane\\Documents\\NUS Documents\\NUS Year 4\\DSC5101\\Pumpkin Data RevC.csv")

# correlation between low and high price almost 1
# can treat as the same variable - take average

# price per pound
# eliminate other rows except blank (first) robustness?
# allows us to test if there is quantity discounts (package inherent value beyond price per pound)

df$Bulk.Flag[df$Approx.Weight..lbs. > 500] <- 1
df$Bulk.Flag[df$Approx.Weight..lbs. < 500] <- 0



# essentially the uniform price model (cost based approach plus)
basic.model <- lm(Av.Price.lb ~  Variety + Item.Size + Color, data = df)
summary(basic.model)

basic.model1 <- lm(Av.Price.lb ~  Variety + Origin + Distance..km. + Item.Size + Color, data = df)
summary(basic.model1)

# some testing for distance 
basic.model <- lm(Av.Price.lb ~  Variety + Origin + Distance..km.+ I(Distance..km.^2) + Item.Size + Color, data = df)
summary(basic.model)

basic.model <- lm(Av.Price.lb ~  Variety + Origin + Distance..km.+ I(Distance..km.^1/2) + Item.Size + Color, data = df)
summary(basic.model)

basic.model <- lm(Av.Price.lb ~  Variety + Origin + I(log(Distance..km.)) + Item.Size + Color, data = df)
summary(basic.model)




# test 3rd degree
basic.third_degree <- lm(Av.Price.lb ~  City.Name, data = df)
summary(basic.third_degree)

basic.model_destination <- lm(Av.Price.lb ~  City.Name + Variety + Origin + Distance..km. + Item.Size + Color, data = df)
summary(basic.model_destination)

basic.model_percapita <- lm(Av.Price.lb ~  Per.Capita.Income + Variety + Origin + Distance..km. + Item.Size + Color, data = df)
summary(basic.model_percapita)


# add in package to see if the different packages yield different price per pound
# bulk discounts
basic.second_degree <- lm(Av.Price.lb ~ Package, data = df)
summary(basic.second_degree)

basic.second_degree2 <- lm(Av.Price.lb ~ Approx.Weight..lbs., data = df)
summary(basic.second_degree2)

basic.second_degree3 <- lm(Av.Price.lb ~ I(log(Approx.Weight..lbs.)), data = df)
summary(basic.second_degree3)


# test with bulk flag split
second_degree.model <- lm(Av.Price.lb ~ Approx.Weight..lbs.*Bulk.Flag, data = df)
summary(second_degree.model)

second_degree.model2 <- lm(Av.Price.lb ~ I(log(Approx.Weight..lbs.))*Bulk.Flag, data = df)
summary(second_degree.model2)




# adding covariates
# final model to test parameters against covariates
final.model <- lm(Av.Price.lb ~ Approx.Weight..lbs.*Bulk.Flag, data = df)
summary(final.model)
final.model1 <- lm(Av.Price.lb ~ Origin+ Distance..km. +Approx.Weight..lbs.*Bulk.Flag, data = df)
summary(final.model1)
final.model2 <- lm(Av.Price.lb ~ Variety+ Item.Size + Color+Origin +Distance..km. +Approx.Weight..lbs.*Bulk.Flag, data = df)
summary(final.model2)

final.model3 <- lm(Av.Price.lb ~ City.Name+Variety+ Item.Size + Color+Origin +Distance..km. +Approx.Weight..lbs.*Bulk.Flag, data = df)
summary(final.model3)



final.model6 <- lm(Av.Price.lb ~ Per.Capita.Income+Variety+ Item.Size + Color+Origin +Distance..km. +Approx.Weight..lbs.*Bulk.Flag, data = df)
summary(final.model6)


# robustness on approximate weights

validation.df <- df

results <- matrix(nrow = 1501, ncol=length(levels(df$Package)))
colnames(results) <- levels(df$Package)

# randomly vary weights for each package type
for (level in levels(df$Package)){
  
  # randomly assign weights to bins, as long as they are still 'bulk'
  if (grepl(level, 'bins')){
    for (i in 500:2000){
      validation.df$Approx.Weight..lbs.[validation.df$Package == level] <- i
      
      validation.model <- lm(Av.Price.lb ~ Per.Capita.Income+Variety+ Item.Size + Color+Origin +Distance..km. +Approx.Weight..lbs.*Bulk.Flag, data = validation.df)
      
      
      results[i-499, level] <- summary(validation.model)$adj.r.squared
    
    }
    
  }else
    # randomly assign weights , as long as they are still 'non-bulk'
    for (i in 1:500){
      validation.df$Approx.Weight..lbs.[validation.df$Package == level] <- i
      
      validation.model <- lm(Av.Price.lb ~ Per.Capita.Income+Variety+ Item.Size + Color+Origin +Distance..km. +Approx.Weight..lbs.*Bulk.Flag, data = validation.df)
      
      
      results[i, level] <- summary(validation.model)$adj.r.squared
    }
  
  # reset data frame
  validation.df <- df
}

# see the r squared results after randomly simulating
output <- apply(results, 2, min, na.rm = TRUE)
