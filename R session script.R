# load packages
library(tidyverse)
library(gtsummary)
library(gt)
library(patchwork)
library(plotly)
library(modelsummary)

# create a vector called x
x <- c(1,2,3,4,5,6)

# create a character vector
attendees <- c("Aaron", "Sodam", "Michael")


# Aaron's example

## edit trial to include grade == "III"
trial2 <- trial |> 
  filter(grade == "III") |> 
  filter(marker >= 2) |> 
  mutate(age_cat = ifelse(age >= 50, "Above or 50", "Below 50"))

trial2

table_gt <- tbl_summary(
  data = trial, by = trt
)

table_gt |> 
  add_p()


# Run a regression
my_model <- glm(
  data = trial,
  death ~ age + stage,
  family = binomial(link = "logit")
)

my_model |> 
  tbl_regression(exponentiate = TRUE, include = age)

my_model |> modelsummary()

my_model |> 
  broom::tidy() |> 
  filter(term == "(Intercept)") |> 
  select(term, p.value)

# logical vector
logical_vector <- c(TRUE, FALSE)

faviour_language = "R"

faviour_language

logical_vector


# some comparisons
TRUE == 1
5 <= 2
5 != 3

attendees

# the concept of functions
class(attendees)
mean(x)
median(x)
range(x)
max()

class(x)

# Let's talk plots
p1 <- trial |> 
  ggplot(
    aes(x = age, y = marker)
  ) + geom_point()


p2 <- trial |> 
  ggplot(
    aes(x = age, y = marker, color = trt)
  ) + geom_point()
p1
p2


ggplotly(p1)
p3 <- p1 | p2
ggplotly(p3)
ggsave(plot = p3, "myplot3.png", width = 20)

attendees
1:10
# iterations
for (i in c("T1", "T2", "T3", "T4")) {
  x = trial |> 
    filter(stage == i)
  print(x)
}

# let's purrr

my_object <- c("T1", "T2", "T3", "T4") |> 
  map(
    ~ trial |> 
      filter(stage == .x) %>%
      glm(
        data = .,
        death ~ age,
        family = binomial(link = "logit")
      ) |> 
      tbl_regression(exponentiate = T)
  ) 


trial |> 
  filter(stage == "T1") %>%
  glm(
    data = .,
    death ~ age,
    family = binomial(link = "logit")
  ) |> 
  tbl_regression(exponentiate = T)


trial |> 
  filter(stage == "T2") %>%
  glm(
    data = .,
    death ~ age,
    family = binomial(link = "logit")
  ) |> 
  tbl_regression(exponentiate = T)

trial |> 
  filter(stage == "T3") %>%
  glm(
    data = .,
    death ~ age,
    family = binomial(link = "logit")
  ) |> 
  tbl_regression(exponentiate = T)

trial |> 
  filter(stage == "T4") %>%
  glm(
    data = .,
    death ~ age,
    family = binomial(link = "logit")
  ) |> 
  tbl_regression(exponentiate = T)

my_object |> 
  tbl_merge(tab_spanner = c("T1", "T2", "T3", "T4"))


# Create your own function

my_cool_function <- function(put_your_number){
  
  if (put_your_number < 0) {
    output = "It does not work!"
  } else{
    output = sqrt(put_your_number)
  }
  
  return(output)
}

my_cool_function(put_your_number = 4)


# Let's migrate from SAS
library(haven)
x <- read_sas("//pharm-psop/Truven Data/redbook.sas7bdat")


x |> 
  filter(
    str_detect(GENNME, "Omeprazole")
  ) |> 
  select(NDCNUM) |> 
  pull()

