## WEB SCRAPING FOR ANCESTRY LIBRARY PORT DATA
import re
from urllib.request import Request, urlopen
import requests
headers={'User-Agent': 'Mozilla/6.0'}
response = requests.get('https://www.ancestrylibrary.com/search/collections/1263/', headers=headers)
response2 = requests.get('https://www.ancestrylibrary.com/search/collections/1263/', headers=headers)

print(response2.text)