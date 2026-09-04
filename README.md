# Evaluating MiFI Vegetative Cover on MDNR-Administered Public Lands: A Wildlife Division Case Study

### [Steven M. Gurney](https://linktr.ee/stevenmgurney)

### Data: Because MiFI records may change through time, the data snapshots used in this study were retained to preserve the exact inventory evaluated. These large datasets were archived internally within the Wildlife Division Planning and Adaptation Section repository. Current MiFI data are available internally to MDNR staff through the MiFI enterprise system on the MDNR GIS Portal, while public MiFI inventory data are available through the Michigan GIS Open Data Portal.

#### Please contact the first author for questions about the code or data: Steven M. Gurney (gurneys5@michigan.gov)
__________________________________________________________________________________________________________________________________________

## Summary
Wildlife Division-administered public lands include areas of nonforested vegetative cover maintained to support wildlife and wildlife-related recreation objectives, including planted or maintained herbaceous openings and agricultural plantings. However, the extent and distribution of these cover types had not been consistently summarized across Michigan. This assessment used the Michigan Forest Inventory (MiFI) to develop a reproducible framework for evaluating inventoried Herbaceous Openland and Agriculture across Wildlife Division-administered lands. We found that Herbaceous Openland and Agriculture represented approximately 6% (20,579 ac) and 4% (14,232 ac) of inventoried terrestrial acreage, respectively, with distinct spatial patterns and substantial concentrations within a subset of management areas. Importantly, these patterns changed with administrative scale: management-area summaries revealed localized concentrations, while Unit-level summaries highlighted broader regional trends and reduced many of the extreme differences observed among individual areas. By establishing a standardized inventory baseline, this work provides a foundation for future evaluation of management and ecological context.

__________________________________________________________________________________________________________________________________________

## Repository Directory

### OpenlandAssessment_Phase1Step1_DataPrep_18Aug2026.R - Code used to read and prepare MiFI data.

OpenlandAssessment_Phase1Step2_EDA_18Aug2026.R - Code used for exploratory data analysis (EDA) and data quality control (QC).

OpenlandAssessment_Phase1Step3_Analysis_18Aug2026.R - Code used for analyses and creating visuals.



### [Code](./Code): Contains code for preparing study data and running study model.
*  [Data_Prep](./Code/Data_Prep) - Folder with code to prepare study data for use in the N-mixture model.
   * [Data_Processing.R](./Code/Data_Prep/Data_Processing.R) - Code to process wrangled data.
   * [Data_Wrangling.R](./Code/Data_Prep/Data_Wrangling.R) - Code to wrangle study data.
* [N_Mixture_Model_Supplement.R](./Code/N_Mixture_Model_Supplement.R) - Code to fit alternative model with year as a factor rather than linear effect
* [N_Mixture_Model.R](./Code/N_Mixture_Model.R) - Code to fit N-
