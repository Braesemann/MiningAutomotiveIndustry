import urllib
import requests
from bs4 import BeautifulSoup
from collections import deque

import json
import sys
import re

import networkx as nx
import matplotlib.pyplot as plt






def get_links(url):

    for i in url:
        r = requests.get(i)
        soup = BeautifulSoup(r.content, "lxml")

        hrefs = []

        for a in soup.find_all('a', href=True):
            hrefs.append(a['href'])

        links = []
        for href in hrefs:
            if (href.startswith("http")):
                links.append(href)
            else:
                pass

        return links







def breadth_search(domain_name, search_depth, url):
    queue = deque([url])
    level = 0

    while (len(queue) > 0) and level < search_depth:
        print (len(queue))
        url = queue.popleft()

        links = get_links(url)  # get links, maybe parse the result of last statement

        ## write to txt file
        for e in (links):

            if e != url:  # check if link directs back to parent

                file.write((str(url[0]) + ',' + str(e) + '\n'))

                if domain_name in e:  # check if link is still on main page of company

                    queue.append([e])

        level += 1





#___________________________________________________________________________________




url = "https://www.bwm.de"


domain_name = "bmw"

file = open(domain_name + '.txt', 'w')
search_depth = 4

breadth_search(domain_name, search_depth, [url])

#recursive_search(url, 0)


fh=open(domain_name + '.txt', 'rb')
G=nx.read_edgelist(fh, delimiter=',', nodetype = str)
fh.close()

nx.draw(G)
plt.show()