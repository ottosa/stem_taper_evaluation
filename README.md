# stem_taper_evaluation
PhD student Otto Saikkonen, Department of Forest Siences, University of Helsinki 
otto.saikkonen@helsinki.fi

## Disclaimer
This is a readme document for the stem taper evaluation workflow. All the code is created with R language. Workflow is created to work with the research article "Evaluating taper functions and stem volume in even aged and continuous cover Norway spruce stands" and created for scientific purposes. The code can be used free of charge under the [CC By 4.0 license](https://creativecommons.org/licenses/by/4.0/).

## Contents
The stem taper evaluation consists of a main script (Main.R) which goes through the workflow (steps 1-6) of data processing:
1. re_functions.R
   - Contains functions used in the data processing
     
3. Data_process.R
   - Reads, harmonize and prepare the data
   - Output data.frame (separately for both CCF and RF treatments):
     - id = tree id
     - stand = stand id
     - X = X coordinate (in local coordinate system)
     - Y = Y coordinate (in local coordinate system)
     - Z = height point of the measurement (in meter)
     - D = diameter measurement at Z (in decimetre)
     - CC = canopy class of the tree
     - dbh_c = diameter class of the tree
       
5. re-parametrize.R
   - Re-parametrize Laasasenaho (1982) stem curve function
   - Re-parametrization process is explained in the materials and methods of the article
     
7. volume_calculations.R
   - Volume calculations with each re-parametrization for each sample tree
   - Volume calculations by each individual sample tree parametrization
     
9. variable_calculations.R
   - Calculate attributes for each sample tree
   - Explanations of the attributes are in the materials and methods section of the article

This workflow returns tree volumes calculated with re-parametrized Laasasenaho (1982) stem curve function and tree attributes calculated with tree measurements and the calculated volumes.  

Repository contains also scripts:

pairedTest.R
  - statistical tests used in the article
  - Check the article materials and methods section for more information of this section

Plotting.R
  - Plots and graphics in the article result section


## Required R libraries 
- data.table
- dplyr
- gridExtra
- ggplot2
- tidyverse
- ggpubr
- scales
- ggpmisc
- reshape2
- Metrics
- lme4
- rsq
- agricolae


## References
###R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
###Laasasenaho J (1982) Taper curve and volume functions for pine, spruce and birch. Communicationes Instituti Forestalis Fenniae 108: 1-74. http://urn.fi/URN:ISBN:951-40-0589-9
