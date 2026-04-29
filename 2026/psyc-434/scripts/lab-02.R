# ============================================================================
# PSYC 434 — Lab 2: Install R and RStudio
# self-standing script — run from top to bottom
# ============================================================================

# --- assignment and basic operations ----------------------------------------

x <- 10
y <- 5

sum <- x + y
print(sum)

difference <- x - y
difference

product <- x * y
product # note that `#` is a comment. It is not read by R

quotient <- x / y
quotient

# --- vectors ----------------------------------------------------------------
numbers <- c(1, 2, 3, 4, 5)
print(numbers)

vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
vector_product <- vector1 * vector2
vector_product

vector_division <- vector1 / vector2
vector_division

# integer division and modulo
integer_division <- 10 %/% 3 # 3
integer_division

remainder <- 10 %% 3 # 1
remainder


# --- remove objects ---------------------------------------------------------
devil_number <- 666
devil_number
rm(devil_number)

# --- logic ------------------------------------------------------------------
x_not_y <- x != y # TRUE
x_equal_10 <- x == 10 # TRUE

print(x_not_y)
print(x_equal_10)


# element-wise OR
vector_or <- c(TRUE, FALSE) | c(FALSE, TRUE) # c(TRUE, TRUE)
vector_or


# single OR (first element only)
single_or <- TRUE || FALSE # TRUE
single_or

# element-wise AND
vector_and <- c(TRUE, FALSE) & c(FALSE, TRUE) # c(FALSE, FALSE)
vector_and


# single AND (first element only)
single_and <- TRUE && FALSE # FALSE

# --- data types -------------------------------------------------------------
# integers
x_int <- 42L
str(x_int)

y_num <- as.numeric(x_int)
str(y_num)

# characters
name <- "Alice"
name

# factors
colors <- factor(c("red", "blue", "green"))

# ordered factors
education_levels <- c("high school", "bachelor", "master", "ph.d.")
education_factor_no_order <- factor(education_levels, ordered = FALSE)
education_factor <- factor(education_levels, ordered = TRUE)
education_factor

edu1 <- ordered("bachelor", levels = education_levels)
edu2 <- ordered("master", levels = education_levels)
edu2 > edu1 # TRUE


# --- strings ----------------------------------------------------------------
you <- "world!"
greeting <- paste("hello,", you)
greeting

# --- vectors (continued) ---------------------------------------------------
numeric_vector <- c(1, 2, 3, 4, 5)
character_vector <- c("apple", "banana", "cherry")
logical_vector <- c(TRUE, FALSE, TRUE, FALSE)

vector_sum <- numeric_vector + 10
vector_multiplication <- numeric_vector * 2
vector_greater_than_three <- numeric_vector > 3

first_element <- numeric_vector[1]
some_elements <- numeric_vector[c(2, 4)]

mean(numeric_vector)
sum(numeric_vector)
sort(numeric_vector)
unique(character_vector)

# --- data frames ------------------------------------------------------------
df <- data.frame(
  name = c("alice", "bob", "charlie"),
  age = c(25, 30, 35),
  gender = c("female", "male", "male")
)

head(df)
str(df)

# accessing elements
names <- df$name
second_person <- df[2, ]
age_column <- df[, "age"]
very_old_people <- subset(df, age > 25)
mean(very_old_people$age)

# exploring
head(df)
tail(df)
str(df)
summary(df)

# manipulating
df$employed <- c(TRUE, TRUE, FALSE)
new_person <- data.frame(name = "diana", age = 28, gender = "female", employed = TRUE)
df <- rbind(df, new_person)
df[4, "age"] <- 26
df$employed <- NULL
df <- df[-4, ]

# data have changed
head(df)
tail(df)
str(df)

# --- summary statistics -----------------------------------------------------
set.seed(12345)
vector <- rnorm(n = 40, mean = 0, sd = 1)
mean(vector)
sd(vector)
min(vector)
max(vector)

# cross-tabulation
set.seed(12345)
gender <- sample(c("male", "female"), size = 100, replace = TRUE, prob = c(0.5, 0.5))
education_level <- sample(c("high school", "bachelor", "master"), size = 100, replace = TRUE, prob = c(0.4, 0.4, 0.2))
df_table <- data.frame(gender, education_level)
table(df_table)
table(df_table$gender, df_table$education_level)

# --- ggplot2 visualisation --------------------------------------------------
required_packages <- c("ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(ggplot2)
set.seed(12345)
student_data <- data.frame(
  name = c("alice", "bob", "charlie", "diana", "ethan", "fiona", "george", "hannah"),
  score = sample(80:100, 8, replace = TRUE),
  stringsAsFactors = FALSE
)
student_data$passed <- ifelse(student_data$score >= 90, "passed", "failed")
student_data$passed <- factor(student_data$passed, levels = c("failed", "passed"))
student_data$study_hours <- sample(5:15, 8, replace = TRUE)

# bar plot
ggplot(student_data, aes(x = name, y = score, fill = passed)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("failed" = "red", "passed" = "blue")) +
  labs(title = "student scores", x = "student name", y = "score") +
  theme_minimal()

# scatter plot
ggplot(student_data, aes(x = study_hours, y = score, color = passed)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("failed" = "red", "passed" = "blue")) +
  labs(title = "scores vs. study hours", x = "study hours", y = "score") +
  theme_minimal()

# box plot
ggplot(student_data, aes(x = passed, y = score, fill = passed)) +
  geom_boxplot() +
  scale_fill_manual(values = c("failed" = "red", "passed" = "blue")) +
  labs(title = "score distribution by pass/fail status", x = "status", y = "score") +
  theme_minimal()

# histogram
ggplot(student_data, aes(x = score, fill = passed)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("failed" = "red", "passed" = "blue")) +
  labs(title = "histogram of scores", x = "score", y = "count") +
  theme_minimal()

# line plot
months <- factor(month.abb[1:8], levels = month.abb[1:8])
study_hours <- c(0, 3, 15, 30, 35, 120, 18, 15)
study_data <- data.frame(month = months, study_hours = study_hours)

ggplot(study_data, aes(x = month, y = study_hours, group = 1)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(color = "red", size = 1) +
  labs(title = "monthly study hours", x = "month", y = "study hours") +
  theme_minimal()
