import subprocess
import os

path = '/mnt/c/Users/intern_mbah/aws_video/_agc_analysis'
input_folder_path = path+'/data/AnonymisedVideoDataForAnalyses' # OR VideoDataForAnalyses
output_folder_path = path+'/res_icatcher/AnonymisedVideoDataForAnalyses'  # OR VideoDataForAnalyses

input_folders = os.listdir(input_folder_path)
i=0
for target_folder in input_folders:
    input_path = input_folder_path+'/'+target_folder 
    output_video_path = output_folder_path+'/'+target_folder+'/video'
    output_annotation_path = output_folder_path+'/'+target_folder+'/text'
    command = 'python test.py '+input_path+' models/icatcher+_lookit.pth --output_video_path '+output_video_path+' --output_annotation '+output_annotation_path
    print(command)
    subprocess.run(command, shell=True)
    i=i+1
    print('Folder '+str(i)+': DONE!')

print('DONE!')

