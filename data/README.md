# Project 1: Unraveling the Stories Behind Women's Smiles
### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.


### cleaned_hm.csv

`cleaned_hm.csv` contains cleaned-up happy moments and some additional information in addition to original happy moments.

- **hmid (int)**: Happy moment ID
- **wid (int)**: Worker ID
- **reflection_period (str)**: Reflection period used in the instructions provided to the worker (3m or 24h)
- **original_hm (str)**: Original happy moment
- **cleaned_hm (str)**: Cleaned happy moment
- **modified (bool)**: If True, `original_hm` is "cleaned up" to generate `cleaned_hm` (True or False)
- **predicted_category (str)**: Happiness category label predicted by our classifier (7 categories. Please see the reference for details)
- **ground_truth_category (str)**: Ground truth category label. The value is `NaN` if the ground truth label is missing for the happy moment
- **num_sentence (int)**: Number of sentences in the happy moment


### original_hm.csv

`original_hm.csv` contains *unfiltered* version of happy moments. 

- **hmid (int)**: Happy moment ID
- **wid (int)**: Worker ID
- **hm (str)**: Original happy moment
- **reflection_period (str)**: Reflection period used in the instructions provided to the worker (3m or 24h)


### demographic.csv

`demographic.csv` contains demographic information of the workers who contributed to the happy moment collection.

- **wid (int)**: Worker ID
- **age (float)**: Age
- **country (str)**: Country of residence (follows the ISO 3166 Country Code)
- **gender (str)**: {Male (m), Female (f), Other (o)}
- **marital (str)**: Marital status {single, married, divorced, separated, or widowed}
- **parenthood (str)**: Parenthood status {yes (y) or no (n)}
