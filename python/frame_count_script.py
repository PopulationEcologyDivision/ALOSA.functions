# importing the module
import glob
import os
import sys
import moviepy
from moviepy.editor import *
from datetime import timedelta
import csv

video_list = []

for f in glob.glob('C:\\Users\SILVERKAY\Documents\\**\\*.mp4', recursive=True):
            video_list.append(f)


#at this point you have a list called `video_list` that has filenames as entries. 

#loop through this list and add frame counts to a new list
framecounts = []
for filename in video_list:
    try:
        clip = VideoFileClip(filename)
        num_frames = len(list(clip.iter_frames()))
       
        #do your FPS calculations and capture results to a new list
        FPS = 20.0
        frame_count = num_frames
        td = timedelta(seconds=(frame_count / FPS))
        framecounts.append([os.path.basename(filename), frame_count, td])
    except Exception as err:
        print("Theres an issue with file " + filename)
        continue

with open('test.csv', 'w', newline='') as f:
    writer = csv.writer(f, delimiter=',')
    writer.writerow(['filename','frame_count','td'])
    writer.writerows(framecounts)
