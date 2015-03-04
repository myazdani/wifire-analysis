## getHPWRENimages.py
##
##

import urllib2
import urllib
import re
import os

outpath = "../../output"

f = open("dates.txt", 'ru')
dates = [line.rstrip() for line in  f]
f.close()

#date = "20140630"
Qs = ["/Q" + str(i) + "/" for i in range(1,9)]
for date in dates:
	for Q in Qs:
		print "working on", date, Q
		## find image urls
		page = "http://hpwren.ucsd.edu/anim-a4/vo-s-mobo-c/large/" + date + Q
		
		try:	
			response = urllib2.urlopen(page)
			page_source = response.read()
			matches = re.findall("[0-9]+\.jpg", page_source)
			image_urls = [page + match for match in matches]
		except:
			print page, "failed"
			continue 
		## download images
		print 'downloading images'
		for image_url in image_urls:
		  try:
		    filename = image_url.split("/")[-1]
		    write_path = outpath + "/" + image_url.split("/")[-3] + "/" + image_url.split("/")[-2] 
		    if not os.path.exists(write_path): os.makedirs(write_path)
		    urllib.urlretrieve(image_url, write_path + "/" +filename)
		  except:
		    print image_url, 'failed' 

