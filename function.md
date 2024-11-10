function
================
Congyu Yang
2024-10-29

# Do something simple

``` r
x = rnorm(30,mean = 5, sd = 3)

# z score
(x - mean(x))/sd(x)
```

    ##  [1] -0.27850133 -0.49357478 -1.61138921  0.03619520  1.42294946 -0.47600795
    ##  [7] -1.63707895  0.42448116  0.64795399  1.36316493  1.15881032  0.36696362
    ## [13]  1.07794657  0.20516338 -1.45073545 -0.88523994  1.71336752  1.06829232
    ## [19] -0.46740358 -0.51876627 -0.08491948  0.42104862 -0.25690839  0.35638843
    ## [25] -0.97152604  1.50056692 -2.09664549 -0.60586392 -0.29480378  0.36607212

I want a function to compute z-score

``` r
z_scores <-  function(x) {
  
  # conditional exection
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  if(length(x) < 3){
    stop("Input must have at least three numbers.")
  }
  
  z = (x - mean(x))/sd(x)
  
  return(z)
}

z_scores(x)
```

    ##  [1] -0.27850133 -0.49357478 -1.61138921  0.03619520  1.42294946 -0.47600795
    ##  [7] -1.63707895  0.42448116  0.64795399  1.36316493  1.15881032  0.36696362
    ## [13]  1.07794657  0.20516338 -1.45073545 -0.88523994  1.71336752  1.06829232
    ## [19] -0.46740358 -0.51876627 -0.08491948  0.42104862 -0.25690839  0.35638843
    ## [25] -0.97152604  1.50056692 -2.09664549 -0.60586392 -0.29480378  0.36607212

Try my function on some other things.

``` r
z_scores(3)  # sd(3) = NA
```

    ## Error in z_scores(3): Input must have at least three numbers.

``` r
z_scores("my name is jeff") # non-numeric argument
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(c(T,T,F,T))
```

    ## Error in z_scores(c(T, T, F, T)): Input must be numeric

# Multiple outputs

``` r
mean_and_sd <-  function(x) {
  
  # conditional exection
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  if(length(x) < 3){
    stop("Input must have at least three numbers.")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )

}

mean_and_sd(x)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.42  2.97

# Multiple Inputs

``` r
sim_data <- tibble(
  x = rnorm(100,4,3)
)

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.89  3.37

``` r
sim_mean_sd <- function(n,mu,sigma){
  
    sim_data <- tibble(
    x = rnorm(n,mu,sigma)
  )
    
    sim_data %>% 
      summarize(
       mean = mean(x),
       sd = sd(x)
      )
}

sim_mean_sd(100,6,3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.23  3.05

# Revisit LoTR

``` r
fellowship_df <- 
  read_excel("data/LotR_Words.xlsx",range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring") %>% 
  janitor::clean_names()

tower_df <- 
  read_excel("data/LotR_Words.xlsx",range = "F3:H6") %>% 
  mutate(movie = "two_towers") %>% 
  janitor::clean_names()

return_df <- 
  read_excel("data/LotR_Words.xlsx",range = "J3:L6") %>% 
  mutate(movie = "return_king") %>% 
  janitor::clean_names()
```

``` r
readxl <- function(range,name){
   df<- 
    read_excel("data/LotR_Words.xlsx",range = range) %>%
    mutate(name = name) %>% 
    janitor::clean_names()
   
   return(df)
  
}

lotr_df <- 
  bind_rows (readxl(range = "B3:D6",name = "fellowship_ring"),
             readxl(range = "F3:H6",name = "two_towers"),
             readxl(range = "J3:L6",name = "return_king"))
```

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}
```
