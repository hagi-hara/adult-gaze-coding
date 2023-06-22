import codecs
import json
import subprocess
import pandas as pd
from pandas.io.json import json_normalize
import glob

topDir = "/mnt/c/Users/intern_mbah/aws_video/_agc_analysis/data/_ForTest/"
inputfiles = glob.glob(topDir + "*.mp4")

for inputfile in inputfiles:
    #inputpath = topDir + "11.mp4"
    #jsonpath = topDir + "11.json"
    #csvpath =  topDir + "11.csv"
    jsonfile = inputfile.replace('.mp4', '.json')
    csvfile = inputfile.replace('.mp4', '.csv')
    command = 'ffprobe -show_frames -select_streams v -print_format json "{}" > {}'.format(inputfile, jsonfile)

    print(inputfile)
    print(jsonfile)
    print(csvfile)
    try:
        # Obtain results as CompletedProcess object
        response = subprocess.run(
            command,
            shell=True, 
            check=True, # Raise CalledProcessError if the final code is not zero
            stdout=subprocess.PIPE, # Pass the results to PIPE
        )

    except CalledProcessError as e:
        raise e


    # Read JSON file to be converted 
    df = pd.read_json(jsonfile)

    # Expand the results by json_normalize (this cannot be done by read_json)
    df_json = json_normalize(df['frames'])
    df_csv = df_json[["pkt_pts","pkt_pts_time","pkt_duration_time"]]
    df_csv.to_csv(csvfile, encoding='utf-8')