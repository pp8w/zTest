from bs4 import BeautifulSoup
import requests
import time
import csv  

#-----URL and URL List to be scrapped -----------------

urllist = [

"http://www.boxofficemojo.com/movies/?page=weekly&id=1952.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=ageofadaline.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=almanac.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=alvin4.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=antman.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=avengers2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=bestexotic2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=bigshort.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=blackorwhite.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=blumhouse2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=blumhousejuly2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=bond24.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=boynextdoor.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=brooklyn.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=chappie.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=cinderella2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=coldwar2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=concussion2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=coup.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=creed.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=crimsonpeak.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=crowe2014.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=cybernatural.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=daddyshome.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=dc2016.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=duff.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=entourage.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=everest2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=exmachina.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=fantasticfour15.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=fast7.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=fiftyshadesofgrey.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=focus2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=furyroad.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=gethard.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=goosebumps.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=happysmekday.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=heartofthesea.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=hitman47.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=hoteltransylvania2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=hungergames4.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=insidiouschapter3.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=insurgent.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=intern.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=joy.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=jupiterascending.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=jurassicpark4.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=kevinhart15.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=krampus.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=lastwitchhunter.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=lazarus.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=lovethecoopers.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=magicmike2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=max2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=mazerunner2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=mcfarland.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=mi5.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=minions.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=nest.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=newline15.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=newlinehorror2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=paddington.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=pan.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=papertowns.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=paulblart2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=peanuts2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=perfectguy.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=pitchperfect2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=pixar2013.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=pixar2014.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=pixels.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=pointbreak2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=poltergeist2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=revenant.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=rickiandtheflash.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=runallnight.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=sanandreas.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=scott2016.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=secretservice.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=sicario.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=sinister2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=southpaw2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=spongebob2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=spotlight.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=starwars7.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=straightouttacompton.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=susancooper.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=taken3.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=ted2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=terminator2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=thehatefuleight.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=thelongestride.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=trainwreck15.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=uncle.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=walkinthewoods.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=warroom2015.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=whiteybulger15.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=witherspoonvergara.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=womaninblack2.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=womaningold.htm",
"http://www.boxofficemojo.com/movies/?page=weekly&id=xmas2015.htm"
];



#-----Initializing key variables


date = ""
rank = ""
wGross = ""
pChange = ""
trs = ""
tChange = ""
tAvg = ""
cGross = ""
wNum = ""



#-----Parse rfl from &id

for i in range(len(urllist)):
    url = urllist[i]

    r  = requests.get(url)
    data = r.text
    soup = BeautifulSoup(data, "lxml")

    a = url.rfind("=") + 1
    b = url.rfind(".")
    rfl = url[a:b]+".csv"
    
    with open (rfl, 'w') as csvfile:
        fieldnames = ['week', 'wGross', 'cGross']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()

        print(rfl)

        for row in soup.findAll("tr"):
            cells = row.findAll("td")
            #For each "tr", assign each "td" to a variable.
            if len(cells) == 9:
                date = cells[0].find(text=True)
                wGross = cells[2].find(text=True)
                cGross = cells[7].find(text=True)
                wNum = cells[8].find(text=True)

                if wGross != "Distributor: ":
                    #eliminates an infrequent error
                
                    writer.writerow({'week':wNum, 'wGross':wGross.strip("$").strip(','), 'cGross':cGross.strip("$").strip(',')})
                    csvfile.close
    
    #   time.sleep(1)

    

 


      


        



