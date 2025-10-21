#i. Construct a scatter plot to illustrate the relationship between horsepower (hp) and miles per 
#gallon (mpg). Include a trend line to demonstrate the correlation between these two variables. 
#Label the axes and provide a title for the plot.
#Import dataset 
datasets::mtcars
mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
# Load the dataset
data(mtcars)

# Create the scatter plot with trend line
plot(mtcars$hp, mtcars$mpg,
     main = "Relationship Between Horsepower and Miles per Gallon",
     xlab = "Horsepower (hp)",
     ylab = "Miles per Gallon (mpg)",
     pch = 19,             # solid circle points
     col = "blue")

# Add a trend line (linear regression)
abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2)

# Add a grid for better readability
grid()


#ii. Develop a box plot to compare the distribution of miles per gallon (mpg) across different 
#numbers of cylinders (cyl). Ensure the plot is well-labeled with a clear title and axis labels to 
#facilitate understanding of the data.

library(ggplot2)

ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Distribution of Miles per Gallon by Number of Cylinders",
       x = "Number of Cylinders",
       y = "Miles per Gallon (mpg)") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon")) +
  theme_minimal()



#iii. Create a histogram to depict the distribution of car weights (wt). Customize the number of bins 
#to enhance the visualization and provide appropriate titles and axis labels


library(ggplot2)

ggplot(mtcars, aes(x = wt)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Car Weights",
       x = "Weight (in 1000 lbs)",
       y = "Frequency") +
  theme_minimal()



#Q#2
# i. Create the dataset as a data frame
data <- data.frame(
  Emp_ID = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"),
  Name = c("X", "Y", "Z", "X", "Y", "Z", "X", "Y"),
  Age = c(34, 29, 40, 30, 35, 27, 41, 30),
  Dept = c("HR", "IT", "Finance", "Marketing", "HR", "IT", "Finance", "Marketing"),
  Salary = c(50000, 60000, 70000, 80000, 50000, 65000, 45000, 60000),
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  Experience = c(5, 3, 10, 4, 2, 7, 9, 6)
)

 # View dataset
  print(data)
Emp_ID Name Age      Dept Salary Gender Experience
1     E1    X  34        HR  50000   Male          5
2     E2    Y  29        IT  60000 Female          3
3     E3    Z  40   Finance  70000   Male         10
4     E4    X  30 Marketing  80000 Female          4
5     E5    Y  35        HR  50000   Male          2
6     E6    Z  27        IT  65000 Female          7
7     E7    X  41   Finance  45000   Male          9
8     E8    Y  30 Marketing  60000 Female          6


#i. Extract the Salary column from 
#the dataset as a vector and calculate the average salary.

# Create the dataset
data <- data.frame(
  Emp_ID = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"),
  Name = c("X", "Y", "Z", "X", "Y", "Z", "X", "Y"),
  Age = c(34, 29, 40, 30, 35, 27, 41, 30),
  Dept = c("HR", "IT", "Finance", "Marketing", "HR", "IT", "Finance", "Marketing"),
  Salary = c(50000, 60000, 70000, 80000, 50000, 65000, 45000, 60000),
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  Experience = c(5, 3, 10, 4, 2, 7, 9, 6)
)

# Extract the Salary column as a vector
salary <- data$Salary

# Display the Salary vector
print(salary)

# Calculate the average (mean) salary
average_salary <- mean(salary)
print(average_salary)



# ii.Use a vector to store the ages of employees. 
#Find the minimum and maximum age among the employees
# Create a vector to store the ages of employees
age <- c(34, 29, 40, 30, 35, 27, 41, 30)

# Display the age vector
print(age)

# Find the minimum age
min_age <- min(age)
print(paste("Minimum Age:", min_age))

# Find the maximum age
max_age <- max(age)
print(paste("Maximum Age:", max_age))

#iii. Create a list to store the details of a single employee (e.g., Name, Department, Age, Salary). 
#Display each element of the list.

# Create a list for a single employee
employee <- list(
  Name = "X",
  Department = "HR",
  Age = 34,
  Salary = 50000
)

# Display the entire list
print(employee)

# Display each element individually
print(paste("Name:", employee$Name))
print(paste("Department:", employee$Department))
print(paste("Age:", employee$Age))
print(paste("Salary:", employee$Salary))




#iv. Explain how lists are advantageous over vectors when 
#storing multiple types of information.

#Vectors:
#  A vector can only store one type of data at a 
#time — either all numeric, all character, or all logical values.
#Example:
  v <- c(10, 20, 30)      # Numeric vector  
v2 <- c("HR", "IT", "Finance")   # Character vector
#📘 Lists:
  #A list can store different types of data
  #together — numeric, character, logical, and even other lists or vectors.
#Example:
  emp <- list(Name = "X", Age = 34, Salary = 50000, Department = "HR")
  
#🌟 Advantages of Lists over Vectors:
#    Heterogeneous Data Storage:
#   Lists can store multiple data types in a single structure — ideal for representing 
#  real-world entities like an employee record.
#  Named Elements:
#   Each element in a list can have a name, making it easier to access data (e.g., employee$Salary).
# Complex Data Organization:
#  Lists can contain nested elements, such as other lists or data frames, allowing for more complex data modeling.
# Flexibility in Structure:
#   You can easily modify, add, or remove elements from a list without worrying about data type restrictions.
  
  
  
#v. Using the above-mentioned dataset in R, implement R code to calculate the mean, standard 
#  deviation, and correlation between two variables.  
# Create the dataset
Emp_ID <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
Name <- c("X", "Y", "Z", "X", "Y", "Z", "X", "Y")
Age <- c(34, 29, 40, 30, 35, 27, 41, 30)
Dept <- c("HR", "IT", "Finance", "Marketing", "HR", "IT", "Finance", "Marketing")
Salary <- c(50000, 60000, 70000, 80000, 50000, 65000, 45000, 60000)
Gender <- c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female")
Experience <- c(5, 3, 10, 4, 2, 7, 9, 6)

# Combine into a data frame
dataset <- data.frame(Emp_ID, Name, Age, Dept, Salary, Gender, Experience)

# Display dataset
print(dataset)

# i. Calculate the mean of Salary
mean_salary <- mean(dataset$Salary)
print(paste("Mean Salary:", mean_salary))

# ii. Calculate the standard deviation of Salary
sd_salary <- sd(dataset$Salary)
print(paste("Standard Deviation of Salary:", sd_salary))

# iii. Calculate the correlation between Salary and Experience
correlation <- cor(dataset$Salary, dataset$Experience)
print(paste("Correlation between Salary and Experience:", correlation))
  