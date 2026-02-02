data(mtcars)

# 1. Column and row names
print("Column names of mtcars:")
print(colnames(mtcars))

print("Row names of mtcars:")
print(rownames(mtcars))

# 2. Mean and standard deviation
print("Mean of mpg:")
print(mean(mtcars$mpg))

print("Standard deviation of mpg:")
print(sd(mtcars$mpg))

print("Mean of cyl:")
print(mean(mtcars$cyl))

print("Standard deviation of cyl:")
print(sd(mtcars$cyl))

# 3. Column means
print("Mean values for all columns:")
print(colMeans(mtcars))

# 4. Mazda RX4 cylinders
print("Number of cylinders for Mazda RX4:")
print(mtcars["Mazda RX4", "cyl"])