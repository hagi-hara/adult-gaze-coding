Exploration of Factors Affecting Webcam-Based Automated Gaze Coding 
====

## Overview
This repository includes the codes for pre-processing and analyses for the following paper:

Hagihara, H., Zaadnoordijk, L., Cusack, R., N., Kimura, & Tsuji, S. (2024). Exploration of factors affecting webcam-based automated gaze coding. _Behavior Research Methods_. [https://doi.org/10.3758/s13428-024-02424-1](https://doi.org/10.3758/s13428-024-02424-1)


![Examples](https://github.com/hagi-hara/adult-gaze-coding/assets/40618747/52eeb4e3-2f89-436b-8b27-4ef02f5bee6d)


## Folder Structure
- Preprocessing
  - preprocessing.ipynb: Main codes for pre-processing the data
  - run_icatcher.py: For *iCatcher+*, see this link: [https://github.com/icatcherplus/icatcher_plus](https://github.com/icatcherplus/icatcher_plus)
  - ffprobe.py
- Analyses
  - adult_gaze_coding.Rproj: Open this file and run R files accordingly
  - 1_DataShaping.R
  - 2_FaceDetection.R
  - 3_GazeDirection_iCatcher+.R
  - 4_GazeDirection_OWLET.R
  - NonAnonymization_vs_Anonymization.xlsx: Overview of the results across different datasets.
  - Data
    - RawData_Anonymized: CSV data of the experimental conditions and gaze direction estimations for each participant
    - RawData_NonAnonymized: CSV data of the experimental conditions and gaze direction estimations for each participant
    - _last_frame_Anonymized.csv: The concatenated dataset with only the last frame per video.
    - _last_frame_NonAnonymized.csv: The concatenated dataset with only the last frame per video.
    - reliability_check.csv
    - files_correspondence.csv
  - Figures


## Data Structure
We provide an explanation of the data structure using **Analyse/Data/SampledData/_data_sampled_NonAnonymized_1.csv** as an example. See also lines 94-103 in **Analyse/analysis_icatcher.R** for variable renamings.

| Column Name     | Variable              | Example             | Explanation                                             |
| ----            | ----                  |----                 |   ----                                                  |
| id              |qualitative            |c01                  | Participant ID. IDs starting with **"c"** indicate participants in Ireland and those starting with **"t"** indicate participants in Japan. Note that the ID "t13" does not exist due to data exclusion. | 
|lighting         |qualitative            | Lc, Ll, or Lr       | Experimental condition of Lighting source (**Lc** = Front, **Ll** = Left, **Lr** = Right). |
|distance         |qualitative            | D30, D60, or D90    | Experimental condition of Distance to the Camera (**D30** = Close, **D60** = Middle, **D90** = Far). |
|side             |qualitative            | Sc00, Sl24, or Sr24 | Experimental condition of Left-Right Offset (**Sc00** = Center, **Sl24** = Left, **Sr24** = Right). |
|rotation         |qualitative            | Rc, Rl, or Rr       | Experimental condition of Facial Rotation (**Rc** = Upright, **Rl** = Left, **Rr** = Right). |
|trial            |quantitative (integer) | 1 to 10             | The numbered disc at which the participants were asked to look. This provides the ground truth of gaze direction. |
|time             |quantitative (integer) | 1 or 2              | The order in which the number sequence is repeated under the same condition (first or second time). |
|frame            |quantitative (integer) | 23                  | The frame number. |
|annotation       |qualitative            | left, right, away, or noface | Annotations provided by iCatcher+. Note that iCatcher+ returns the left-right annotations from the perspective of the webcam, not the participant. |
|confidence       |qualitative (continuous) | 0.34 | Confidence values provided by iCatcher+, ranging from 0 to 1. Note that when no face was detected (i.e., **noface**), this value becomes -1. |
|filename         |qualitative            | c01_Lr_D30_Sc00_Rc_01_1.txt | Raw file names. |


## Dataset Availability
The videos for which participants agreed to public availability (*n* = 47) are uploaded to [https://doi.org/10.17605/OSF.IO/48ZVH](https://doi.org/10.17605/OSF.IO/48ZVH).


## Authors of This Repository
If you have any questions, please email at **hiromichi.h(AT)gmail.com** (please replace **(AT)** with **@**).	- [Hiromichi Hagihara](https://github.com/hagi-hara) 
