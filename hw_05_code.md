P8105\_HW\_05
================
James Ng
11/7/2019

## Problem 1

  - Problem Setup

<!-- end list -->

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

  - function code for filling in missing values

<!-- end list -->

``` r
fill_in = function(x) {
  
  if (!is.numeric(x)) {
    replace_na(x,"virginica")
  } else if (is.numeric(x)) {
    replace_na(x,mean(x,na.rm=TRUE))
  }
  
}
```

  - map function to fill in the missing values

<!-- end list -->

``` r
iris_full=map(iris_with_missing,fill_in)
```

## Problem 2
