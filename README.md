# ExtractTMSMEM
Collate individ subj data across many days from MEM files (from QTracks) from TMS sessions

This script is part of an analysis pipeline for QuARTS and SNBR clinical trials at Sunnybrook Health Sciences Research Institute, ALS Research, 2024

Scripts "collate QUARTS for RMT50"and "collate all QTMS for SAS" will take the source data (MEM files created in QtRACKS when taking TMS measures of motor excitability like RMT50, RMT200, SICI, SICF, t-SICI, t-SICF, etc
and convert the data into excel or csv files for easy wrangling in the next step of the pipeline: "AnalyzeTMSData" or AnalyzeRMT50"etc.

Folder structire for input can be seen in Source Data
File structure for output can be seen in "copy_of_all subs SAS data"

MEM files are text files (csv) but in an annoying format, with different deliminators, different chunks for different measures (RMT, SICI, SICF) and not every file has the same data. Some are missing measures, some are in different order.

This script automates the process of opening 64+ files per subject and copy pasting several dozen rows and columns.
You're welcome.
xoxo
- Abs
- 
