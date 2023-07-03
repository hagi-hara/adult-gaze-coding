Exploration of Factors Affecting Webcam-Based Automated Gaze Coding 
====

## Overview
This repository includes the codes for pre-processing and analyses for the following paper:

Anonymized (2023). Exploration of factors affecting webcam-based automated gaze coding.


## Folder Structure
- Preprocessing
  - preprocessing.ipynb: Main codes for pre-processing the data
  - run_icatcher.py: For *iCatcher+*, see this link: [https://github.com/icatcherplus/icatcher_plus](https://github.com/icatcherplus/icatcher_plus)
  - ffprobe.py
- Analyses
  - adult_gaze_coding.Rproj: Open this file and run analysis_icatcher.R
  - analysis_icatcher.R
  - Data
    - RawData_Anonymized: CSV data of the experimental conditions and gaze direction estimations for each participant
    - RawData_NonAnonymized: CSV data of the experimental conditions and gaze direction estimations for each participant
    - SampledData: Datasets for analyses (**1** = dataset used in the main analysis, **2** = dataset extracted with another random seed)
    - reliability_check.csv
  - Figures


## Data Structure
We provide an explanation of the data structure using **Analyse/Data/SampledData/_data_sampled_NonAnonymized_1.csv** as an example. See also lines 38-47 in **Analyse/analysis_icatcher.R** for variable renamings.

| Column Name     | Variable              | Example             | Explanation                                             |
| ----            | ----                  |----                 |   ----                                                  |
| id              |qualitative            |c01                  | Participant ID. IDs starting with **"c"** indicate participants in Ireland and those starting with **"t"** indicate participants in Japan. Note that the ID "t13" does not exist due to data exclusion. | 
|lighting         |qualitative            | Lc, Ll, or Lr       | Experimental condition of Lighting source (**Lc** = Front, **Ll** = Left, **Lr** = Right). |
|distance         |qualitative            | D30, D60, or D90    | Experimental condition of Distance to the Camera (**D30** = Close, **D60** = Middle, **D90** = Far). |
|side             |qualitative            | Sc00, Sl24, or Sr24 | Experimental condition of Left-Right Offset (**Sc00** = Center, **Sl24** = Left, **Sr24** = Right). |
|rotation         |qualitative            | Rc, Rl, or Rr       | Experimental condition of Facial Rotation (**Rc** = Upright, **Rl** = Left, **Rr** = Right). |
|trial            |quantitative (integer) | 1 to 10             | The numbered disc at which the participants were asked to look. This provides the ground truth of gaze direction. |
|time             |quantitative (integer) | 1 or 2              | The order in which the number sequence is repeated under the same condition (first or second time). |
|frame            |quantitative (integer) | 23                  | The frame randomly extracted from annotated videos. |
|annotation       |qualitative            | left, right, away, or noface | Annotations provided by iCatcher+. Note that iCatcher+ returns the left-right annotations from the perspective of the webcam, not the participant. |
|confidence       |qualitative (continuous) | 0.34 | Confidence values provided by iCatcher+, ranging from 0 to 1. Note that when no face was detected (i.e., **noface**), this value becomes -1. |
|filename         |qualitative            | c01_Lr_D30_Sc00_Rc_01_1.txt | Raw file names. |


## Dataset Availability
The videos for which participants agreed to public availability (*n* = 47) are uploaded to [https://doi.org/10.17605/OSF.IO/48ZVH](https://doi.org/10.17605/OSF.IO/48ZVH).


## Authors of This Repository
If you have any questions, please email XXX.








