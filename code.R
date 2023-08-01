library(readxl)
IntStudents <- read_excel("Census_Overall-Enrollment-1949-2021_OD22.xlsx")
View(IntStudents)

# Replace missing data with "NA"
IntStudents[IntStudents == "no data"] <- NA

# Summary statistics for each variable
summary(IntStudents$TotalIntStudents)
summary(IntStudents$AnnualPercentChange)
summary(IntStudents$TotalUSEnrollment)
summary(IntStudents$PercentInt)

# Create a vector of years
years <- c("1963/64", "1964/65", "1965/66", "1966/67", "1967/68", "1968/69", "1969/70", "1970/71", "1971/72", "1972/73", "1973/74", "1974/75", "1975/76", "1976/77", "1977/78", "1978/79", "1979/80", "1980/81", "1981/82", "1982/83", "1983/84", "1984/85", "1985/86", "1986/87", "1987/88", "1988/89", "1989/90", "1990/91", "1991/92", "1992/93", "1993/94", "1994/95", "1995/96", "1996/97", "1997/98", "1998/99", "1999/00", "2000/01", "2001/02", "2002/03", "2003/04", "2004/05", "2005/06", "2006/07", "2007/08", "2008/09", "2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")

Years <- as.factor(years)
TotalIntStudentsInThousands <- IntStudents$TotalIntStudents/1000

plot(Years, TotalIntStudentsInThousands, xlab="Years", ylab="Total International Students in thousands", main="Years vs. Total international students enrolled")

barplot(IntStudents$AnnualPercentChange, ylim = c(min(IntStudents$AnnualPercentChange), max(IntStudents$AnnualPercentChange)), xlab="Years", ylab="Annual Percent Change", main="Box Chart", names.arg = Years)

# Assuming your data is stored in a vector called "PercentInt"
PercentInt = IntStudents$PercentInt
bs_stat <- c(PercentInt)
bs_n <- 10
for (ii in 1:bs_n) {
  bs_spl_percentint <- sample(PercentInt, replace = TRUE)
  bs_stat <- append(bs_stat, mean(bs_spl_percentint))
}

hist(bs_stat)

spl_stat <- mean(PercentInt)
CL <- spl_stat + 2 * sd(bs_stat) * c(-1, 1)
print(CL)

# Convert the TotalIntStudents variable to numeric
IntStudents$TotalIntStudents <- as.numeric(IntStudents$TotalIntStudents)

# remove "/" and convert to numeric
years_numeric <- as.numeric(gsub("/", "", years))

# Compute the correlation
stat <- cor(years_numeric, IntStudents$TotalIntStudents)

# Continue with the rest of the code
rand_stat <- c()
rand_n <- 1000
for (ii in 1:rand_n) {
  TotalIntStudents_shuffle <- sample(IntStudents$TotalIntStudents)
  new_stat <- cor(years_numeric, TotalIntStudents_shuffle)
  rand_stat <- append(rand_stat, new_stat)
}

hist(rand_stat)

sum(rand_stat > stat) / rand_n

z <- (stat < 0) / sd(rand_stat)
if (z < 0) {
  2 * pnorm(z)
} else {
  2 * pnorm(z, lower.tail = TRUE)
}
