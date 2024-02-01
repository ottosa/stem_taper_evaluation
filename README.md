# stem_taper_evaluation
PhD student Otto Saikkonen, Department of forest siences, University of Helsinki 
otto.saikkonen@helsinki.fi

## Disclamer
This is a readme document for the stem taper evaluation workflow. All the code is created with R language. Workflow is created to work with the research article "Evaluating taper functions and stem volume in even aged and continuous cover Norway spruce stands" and created for scientific purposes. The code can be used free of charge under the [CC By 4.0 license](https://creativecommons.org/licenses/by/4.0/).

## Contents
The stem taper evaluation concist of a main script (Main.R) wich covers the data prosessing part and the functions used in the worflow:
1. functions used in the data prosessing (re_functions.R)
2. Read, syncronize and prepare the data (Data_process.R)
3. Re-parametrize Laasasenaho (1982) stem curve function (re-parametrize.R)
4. Calculate volumes with each parametrization for each sample tree (volume_calculations.R)
5. Calculate attributes for each sample tree (variable_calculations.R

This workflow returns tree volumes caluculated with re-parametrized Laasasenaho (1982) stem cureve funktion and tree attributes calculated with tree measurements and the calculated volumes.  
## Description of the contents

## Required R libraryes 


## References
###R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
