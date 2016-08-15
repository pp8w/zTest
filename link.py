from bs4 import BeautifulSoup
import requests
import csv



#Output variables which is written to file
output = []

url = " "
urllist = [
"http://www.boxofficemojo.com/yearly/chart/?yr=2015&p=.htm"];




#Beautiful Soup--------
#Obtains all links in a URL, loops through list of URLs

for i in range(len(urllist)):
	url = urllist[i]
	r  = requests.get(url)
	data = r.text
	soup = BeautifulSoup(data)
	for link in soup.find_all('a'):
	    output.append(link.get('href'))
		    
	print (output)   



# Writing output to file
file = open('test.csv', 'w')
for item in output:
  file.write("%s\n" % item)
file.close


