iternation_and_list
================
Congyu Yang
2024-10-29

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8,nrow = 2, ncol =4,byrow = T)
)

l[["mat"]][1,2]
```

    ## [1] 2

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
list_norm = 
  list(
    a = rnorm(20,0,5),
    b = rnorm(20,4,5),
    c = rnorm(20,0,10),
    d = rnorm(20,4,10)
  )

list_norm[["b"]]
```

    ##  [1] -4.6310404  2.8788009 -3.5875456  2.7207490  0.1973369  0.9769680
    ##  [7] -2.8796165  5.3361143  7.2861543  4.0162184  0.8046382  8.7354983
    ## [13]  4.6696859  1.1428986 -0.8738047 -2.6415215  6.2398893  1.5257671
    ## [19]  1.4851431 -4.2903310

``` r
mean_and_sd = function(x){
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return(out_df)
}
```

Use funtion to take mean and sd of all samples

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.18  4.10

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.46  3.87

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.878  9.13

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.86  13.4

## Use a for loop

``` r
output = vector("list",length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Use `map` instead

``` r
(output_dbl = map(list_norm,mean_and_sd) %>% 
   bind_rows())
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -2.18   4.10
    ## 2  1.46   3.87
    ## 3  0.878  9.13
    ## 4  4.86  13.4

``` r
(output = map_dbl(list_norm,IQR))
```

    ##         a         b         c         d 
    ##  4.244493  5.495319  4.487362 17.988221

## LIST COLUMNS!!

``` r
listcol_df =
  tibble(
    name = c("a","b","c","d"),
    samp = list_norm
  )

listcol_df %>% 
  filter(name %in% c("a","b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df %>% 
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.18  4.10

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.46  3.87

``` r
map(listcol_df[["samp"]],mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.18  4.10
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.46  3.87
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.878  9.13
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.86  13.4

ADD a list column

``` r
listcol_df %>% 
  mutate(output = map(samp,mean_and_sd),
         iqr = map_dbl(samp,IQR)) %>% 
  select(-samp) %>% 
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -2.18   4.10  4.24
    ## 2 b      1.46   3.87  5.50
    ## 3 c      0.878  9.13  4.49
    ## 4 d      4.86  13.4  18.0

``` r
nsduh_table_format <- function(html, table_num) {
  
  out_table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value"))
  
  return(out_table)

  
}

nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html,table_num = 1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html,table_num = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html,table_num = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_df <- tibble(
   drug = c("marj","cocaine","herion"),
   table_n = c(1,4,5)) %>% 
   mutate(table = map(table_n,nsduh_table_format,html = nsduh_html)) %>%
  unnest()
```

    ## Warning: `cols` is now required when using `unnest()`.
    ## ℹ Please use `cols = c(table)`.

``` r
nsduh_df %>% filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug    table_n State    `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>     <dbl> <chr>    <chr>            <chr>            <chr>             
    ## 1 marj          1 New York 14.24b           15.04            13.94             
    ## 2 cocaine       4 New York 2.28             2.54             0.71              
    ## 3 herion        5 New York 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>
