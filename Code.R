
#import library
library(readr)
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
#load dataset
student_lifestyle_dataset <- read_csv("student_lifestyle_dataset.csv")
str(student_lifestyle_dataset)
summary(student_lifestyle_dataset)

#check missing value
colSums(is.na(student_lifestyle_dataset))

#Drop unneccesary column
student_lifestyle_dataset <- student_lifestyle_dataset %>% select(-Student_ID)

# Compute correlation matrix
numeric_data <- student_lifestyle_dataset[, sapply(student_lifestyle_dataset, is.numeric)]
cor_matrix <- cor(numeric_data)

# Correlation plot
corrplot(
  cor_matrix,
  method = "color",             
  addCoef.col = "black",         
  tl.col = "black",              
  tl.srt = 45,                   
  col = colorRampPalette(c("blue", "white", "red"))(200), 
  title = "Correlation Matrix",  
  mar = c(0, 0, 1, 0),           
  cl.cex = 0.7,                  
  tl.cex = 0.7,                 
  number.cex = 0.7              
)


# Create a boxplot for GPA across Stress Levels
ggplot(student_lifestyle_dataset, aes(x = Stress_Level, y = GPA)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red", outlier.size = 2) +
  theme_minimal() +
  labs(
    title = "GPA Differences Across Stress Levels",
    x = "Stress Level",
    y = "GPA"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Statistical Analysis
# ANOVA - Does Stress_Level affect GPA?
anova_result <- aov(GPA ~ Stress_Level, data = student_lifestyle_dataset)
summary(anova_result)
TukeyHSD(anova_result)

#Linear regression - Relationship between GPA and Hours of study, sleep and physical activity
model <- lm(GPA ~ Study_Hours_Per_Day + Sleep_Hours_Per_Day + Physical_Activity_Hours_Per_Day, data = student_lifestyle_dataset)
summary(model)
#CHecking multicollinearity
vif(model)  


#Linear Regression model to evaluate relationship between Stress level and number hour of sleep, socail, physical activity.
# Encode Stress Level as an ordinal factor (if necessary)
student_lifestyle_dataset$Stress_Level <- as.numeric(factor(student_lifestyle_dataset$Stress_Level, levels = c("Low", "Moderate", "High")))

# Linear regression model
stress_model <- lm(Stress_Level ~ Sleep_Hours_Per_Day + Social_Hours_Per_Day + Physical_Activity_Hours_Per_Day, data = student_lifestyle_dataset)

# Summary of the model
summary(stress_model)


# Reshape data to a long format
data_long <- student_lifestyle_dataset %>%
  select(Stress_Level, Study_Hours_Per_Day, Sleep_Hours_Per_Day, Social_Hours_Per_Day, Physical_Activity_Hours_Per_Day) %>%
  pivot_longer(
    cols = c(Study_Hours_Per_Day, Sleep_Hours_Per_Day, Social_Hours_Per_Day, Physical_Activity_Hours_Per_Day),
    names_to = "Activity_Type",
    values_to = "Hours"
  )

# Create scatter plot with facets
ggplot(data_long, aes(x = Hours, y = Stress_Level, color = Activity_Type)) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +  # Add regression line
  facet_wrap(~Activity_Type, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Relationship Between Stress Level and Activities",
    x = "Number of Hours",
    y = "Stress Level",
    color = "Activity Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )



