## R - Basic Analysis Toolkit v1_0_0
By wisaimtiac

██████╗    /\   {}_{}   /\       
██╔══██╗  /  \__(ö ö)__/  \      
██████╔╝ /    ,_|\./|_,    \     
██╔══██╗ ~~~~'  |   |  '~~~~   
██║  ██║        \___/         
╚═╝  ╚═╝        /   \         

R template for cleaning, exploring, and analyzing tabular data.
Runs end-to-end on simulated data — or your own CSV-file.

Requires R ≥ 4.0. 
Missing packages are installed automatically.


## 
## What it does
1. Setup & Packages
2a. Simulation of example data or
2b. Importing your own data
3. Cleaning, recoding & calculating scores for list of items
4. Calculating reliability (Cronbach's α)
5. Descriptive statistics
6. Plots
7. Inferential statistics (correlation, t-test, ANOVA, regression)
8. Power-analysis

## 
## Using your own data
1. Comment out Section 2a (simulation).
2. Uncomment Section 2b and point it at your file.
3. Update column names in Section 3 to match your data.

## 
## Dependencies
`tidyverse`, `psych`, `afex`, `car`, `pwr`

## 
## Author
wisaimtiac

## License
MIT
