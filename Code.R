jobs <- read.csv("jobs_in_data.csv")
head(jobs)
ncol(jobs)

print("Names of columns of this dataset")
colnames(jobs)


#scatter plot
plot(jobs$work_year,jobs$salary_in_usd,xlab="Work Year",ylab="Salary (USD)",main="Year vs salary", col = "red")

#bar plot
t <- table(jobs$experience_level)
barplot(t,xlab="Level of Experience",ylab="Number of people" ,main="Barplot for Experience Levels Distribution",border="black")


#bar plot of people who earn more than 150K
people <- subset(jobs, salary_in_usd > 150000)
p <- table(people$experience_level)
barplot(p, xlab="Level of Experience",ylab="Number of people" ,main="Barplot for Experience Levels vs Salary Distribution (People who earn more than 150K)",border="black")

#box plot
boxplot(jobs$salary_in_usd~jobs$experience_level,data=jobs,xlab="Experience Levels",ylab="Salary in USD", main="Boxplot of experience levels vs Salary",border="black")

#mosaic plot
mosaicplot(jobs$experience_level~jobs$work_setting,xlab = 'Experience Levels',ylab = 'Work Setting', main = "Mosiac of experience levels vs work setting",border="black")

#mean of senior dev's salary_in_usd_in_usd
print("Average of senior scientists's Salary")
seniordev <- subset(jobs, experience_level == 'Senior')
mean(seniordev$salary_in_usd)

#average senior scientists' salaries by years
boxplot(seniordev$salary_in_usd~seniordev$work_year, data = seniordev, xlab="Work Years",ylab="Salary in USD", main="Boxplot of work years vs Salary (Senior)",border="black")

executivedev <- subset(jobs, experience_level == 'Executive')
boxplot(executivedev$salary_in_usd~executivedev$work_year, data = executivedev, xlab="Work Years",ylab="Salary in USD", main="Boxplot of work years vs Salary (Executive)",border="black")


print("Number of senior scientists")
length(seniordev$job_title)

print("Lowest Salary among all scientists")
min(jobs$salary_in_usd)

print("Highest Salary among all scientist")
max(jobs$salary_in_usd)

print("Average senior's Salary by their job category")
tapply(jobs[jobs$experience_level == 'Senior',]$salary_in_usd, jobs[jobs$experience_level == 'Senior',]$job_category,mean)

print("Average mid-level's Salary by their job category")
tapply(jobs[jobs$experience_level == 'Mid-level',]$salary_in_usd, jobs[jobs$experience_level == 'Mid-level',]$job_category,mean)

print("Average Entry-level's Salary by their job category")
tapply(jobs[jobs$experience_level == 'Entry-level',]$salary_in_usd, jobs[jobs$experience_level == 'Entry-level',]$job_category,mean)

print("Average Executive's Salary by their job category")
tapply(jobs[jobs$experience_level == 'Executive',]$salary_in_usd, jobs[jobs$experience_level == 'Executive',]$job_category,mean)

print("Average Mid-level's Salary by their countries")
tapply(jobs[jobs$experience_level == 'Mid-level',]$salary_in_usd, jobs[jobs$experience_level == 'Mid-level',]$employee_residence,mean)

print("Average Senior's Salary by their countries")
tapply(jobs[jobs$experience_level == 'Senior',]$salary_in_usd, jobs[jobs$experience_level == 'Senior',]$employee_residence,mean)

print("Average Senior's Salary by their job title")
tapply(jobs[jobs$experience_level == 'Senior',]$salary_in_usd, jobs[jobs$experience_level == 'Senior',]$job_title,mean)

print("Average Senior's Salary by years (2020-2023)")
tapply(jobs[jobs$experience_level == 'Senior',]$salary_in_usd, jobs[jobs$experience_level == 'Senior',]$work_year,mean)

print("Average Mid-level's Salary by years (2020-2023)")
tapply(jobs[jobs$experience_level == 'Mid-level',]$salary_in_usd, jobs[jobs$experience_level == 'Mid-level',]$work_year,mean)

print("Average Executive's Salary by years (2020-2023)")
tapply(jobs[jobs$experience_level == 'Executive',]$salary_in_usd, jobs[jobs$experience_level == 'Executive',]$work_year,mean)

print("Average Entry-level's Salary by years (2020-2023)")
tapply(jobs[jobs$experience_level == 'Entry-level',]$salary_in_usd, jobs[jobs$experience_level == 'Entry-level',]$work_year,mean)


print("Average Senior scientists's Salary by company size")
tapply(jobs[jobs$experience_level == 'Senior',]$salary_in_usd, jobs[jobs$experience_level == 'Senior',]$company_size,mean)

#Average senior data scientists' salaries by company size
boxplot(seniordev$salary_in_usd~seniordev$company_size, data = seniordev, xlab="Company Size",ylab="Salary in USD", main="Boxplot of company size vs Salary (Senior)",border="black")


print("Average data scientist's Salary by experience level")
tapply(jobs$salary_in_usd, jobs$experience_level,mean)

#prediction of data scientist's salary according to their experience level
prediction <- rep('Senior', nrow(jobs))
prediction[jobs$salary_in_usd < 35000] <- 'Entry-level'
prediction[jobs$salary_in_usd > 34999 & jobs$salary_in_usd < 68200] <- 'Mid-level'
prediction[jobs$salary_in_usd > 319000] <- 'Executive'
mean(prediction == jobs$experience_level)

