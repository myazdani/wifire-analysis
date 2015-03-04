import os
from PIL import Image, ImageDraw
from numpy import *
from pylab import *
import pickle
import csv
import pca
from scipy.cluster.vq import *


featuresFilename = "../../processedData/features/output_HUE_ZONES_3__JUL_3_SD.csv"
outfile = "../../figures/pca_H_PC-1-2Jul_3_2k.jpg"

with open(featuresFilename, 'rU') as f:
  reader = csv.reader(f)
  all_rows = [row for row in reader]

rows = all_rows[170:-100]
feature_matrix = []
image_paths = []

for row in rows:
  image_paths.append(row[0])
  features = [float(r) for r in row[1:]]
  feature_matrix.append(features)

feature_matrix = array(feature_matrix)

V,S,immean = pca.pca(feature_matrix)
projected_features = array([dot(V[[0,1]],feature_matrix[i]-immean) for i in range(len(image_paths))])

h,w = 2000,2000
img = Image.new('RGB',(w,h),(255,255,255))
draw = ImageDraw.Draw(img)

scale = abs(projected_features).max(0)
scaled = floor(array([ (p / scale) * (w/2-20,h/2-20) + (w/2,h/2) for p in projected_features]))
print "number of images", len(image_paths)
for i in range(len(image_paths)):
  nodeim = Image.open(image_paths[i]) 
  nodeim = nodeim.resize((95,95))
  ns = nodeim.size 
  img.paste(nodeim,(int(scaled[i][0]-ns[0]//2),int(scaled[i][1]-ns[1]//2),int(scaled[i][0]+ns[0]//2+1),int(scaled[i][1]+ns[1]//2+1))) 

img.save(outfile)