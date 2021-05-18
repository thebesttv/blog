import requests
from bs4 import BeautifulSoup

try:
    with open('full-emoji-list.html', 'r') as f:
        content = f.read()
except:
    content = requests.get('https://unicode.org/emoji/charts/full-emoji-list.html').content.decode('utf-8')

soup = BeautifulSoup(content, 'html.parser')
table = soup.find(name='table')

cnt = 0

def find_td_with_class(tr, cls):
    return tr.find('td', attrs={'class': cls})

for tr in table.find_all('tr'):
    if len(tr.find_all('th')) == 1 and 'mediumhead' in tr.th['class']:
        print(tr.text)
    elif len(tr.find_all('td')) >= 3:
        idx = int(find_td_with_class(tr, 'rchars').text)
        emoji = find_td_with_class(tr, 'chars').text
        short_name = find_td_with_class(tr, 'name').text
        print(f'  {emoji} {short_name}')

        cnt += 1
        assert cnt == idx
