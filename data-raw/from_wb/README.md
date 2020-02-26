Attaching dataset. The bins into which we want to categorize the indicator dropdowns are given by variables “level1” and “level2”. We want three bins

             

Bin 1: “Financial Protection” if level1 = “fp”
Bin 2: “Healthcare Coverage” if level2 = “h_cov”
Bin 3: “Health Outcomes” if level2 = “h_out”
 

I also attach an indicator description Excel file which includes indicator names and descriptions and the corresponding variable names in the Stata dataset (content of str variable indic). Tony (cc-ed) used these to build the Tableau portal. Tony, if there is any additional info that could be useful for rebuilding the portal in R-shiny, could you kindly share with Joe?

 

One important feature of the current portal which we want to preserve is that indicator value and meta-data info is shown when hovering over a point/country – a key piece of information here is the “refernenceid_list” variable which displays the official code of the survey underlying the data point.