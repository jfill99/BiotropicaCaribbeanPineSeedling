# Files in this repository contain code and data associated with: Fill,J.M., F. Tricone, M. Muschamp, R.M. Crandall, and R. Anderson. Accepted. Landscape heterogeneity increases survival of Pinus caribaea var. hondurensis juveniles in a frequently burned, humid savanna. 

BiotropicaAnalyses.R contains the commented code for analyses and figures used in the paper.  This R script runs with the Plotdata_Biotropica.csv and Char_Biotropica.csv datasets.

The dataset Plotdata_Biotropica.csv contains data on pine survival and height collected during each of the census years. Columns of data are as follows:
	"Plot" is a unique identifier for each plot within which pines were tagged.
	"Environment" indicates the environment within which the plot was located.
	"EntryID" is a unique identifier for each seedling tagged during the study.
	"height_start" is the height of the seedling when first tagged, in centimeters.
	"surv" indicates whether the seedling survived to the following year (1) or not (0).  
	"fire_new" indicates whether the seedling experienced a fire during that interval (1) or not (0).

The dataset Char_Biotropica.csv contains data on the height of char visible on trees in the study plots during the census years. Columns of data are as follows:
	"Plot" is a unique identifier for each plot within which pines were tagged and char was assessed.
	"Char_height" indicates the char heights on trees within the plots, in meters.
	"Environment" indicates the environment within which the plot was located.
