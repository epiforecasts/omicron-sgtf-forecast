growth_grid <- function(growth) {
  region_growth <- growth[age_group %in% "Overall"]
  region_growth <- region_growth[!(region %in% "England")]
  age_region_growth <- growth[!(age_group %in% "Overall")]
  age_region_growth <- age_region_growth[!(region %in% "England")]
  age_growth <- growth[region %in% "England"]
  age_growth <- age_growth[!(age_group %in% "Overall")]

growth <- data.table::data.table(
  stratification = c("region", "age", "age and region"),
  growth = list(
    region_growth, age_growth, age_region_growth
  ),
  by = c(list("region"), list("age_group"), list(c("age_group", "region")))
)

# Set up estimation grid
grid <- data.table::CJ(
  stratification = c("region", "age", "age and region"),
  gt_type = c("intrinsic", "household"),
  gt_diff = c(TRUE, FALSE)
)

grid[, gt_prior := purrr::map(
  gt_type, ~ gt_prior(source = "hart2021", type = .x))
]

grid <- merge(grid, growth, by = "stratification")
return(grid)
}
