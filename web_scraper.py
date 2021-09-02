


'''
library(reticulate)
py_install("pandas")


Para instalar librerías: 
1) abrir consola de windows: Windows+R
2) cmd + Enter
3) py -m pip install -U matplotlib  

'''


import csv
import bs4
from bs4 import BeautifulSoup
from selenium import webdriver
import time
import requests
import lxml
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC

chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument("start-maximized")
chrome_options.add_argument("disable-infobars")
chrome_options.add_argument("--disable-extensions")
chrome_options.add_argument("--disable-gpu")
chrome_options.add_argument("--disable-dev-shm-usage")
chrome_options.add_argument("--no-sandbox")
chrome_options.add_argument('--disable-blink-features=AutomationControlled') 

driver = webdriver.Chrome(executable_path = 'C:/Users/omen03/Documents/Growshop/chromedriver_win32/chromedriver.exe',options = chrome_options)
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.keys import Keys

# grab Keywords
# search_term = input('Keywords: ')

# url generator
#driver.get('https://www.aliexpress.com')
driver.get('https://www.ine.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion')
driver.implicitly_wait(10)

driver.execute_script("document.getElementByClassName('navPrincipalDescargas navPrincipalDescargasActivo').click()")

# <a class="" href="https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2021/formato-csv/ene-2021-02-efm.csv?sfvrsn=6d3786e2_10&amp;download=true"> <span class="iconoDescarga iconoDescarga-csv"></span> <span class="tituloDescargaArchivos">ENE 2021 02 EFM<span class="pesoArchivoDescarga">CSV, 47.57 MB</span></span><span class="circuloDescarga"><i class="fas fa-download"></i></span></a>



# "https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2021/formato-csv/ene-2021-02-efm.csv?sfvrsn=6d3786e2_10&amp;download=true"
# 
# "https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2021/formato-csv/ene-2021-03-fma.csv?sfvrsn=f99721e_12&amp;download=true"
# 


# # p = driver.find_element_by_xpath('//*[@id="Content_C007_Col00"]/div/div/div[7]/em')
p = driver.find_element_by_class_name('navPrincipalDescargas navPrincipalDescargasActivo')
driver.execute_script("arguments[0].click();", p)
# print(p)
# #p.send_keys(search_term)
# p.send_keys(Keys.ENTER)

search_query = "widArchNavArchivoDescarga"

p.send_keys(search_query + Keys.RETURN)
    

select_element = WebDriverWait(driver, 10).until(EC.visibility_of_element_located((By.ID, "BASES DE DATOS")))

#Content_C007_Col00 > div > div > div.navPrincipalDescargas.navPrincipalDescargasActivo

//*[@id="Content_C007_Col00"]/div/div/div[7]/em



productlist = []
product = driver.find_element_by_xpath('//*[@id="root"]/div/div/div[2]/div[2]/div/div[2]/ul/div[1]/li[3]/div/div[2]/div/div[1]')

height = driver.execute_script("return document.body.scrollHeight")
for scrol in range(100,height-1800,100):
    driver.execute_script(f"window.scrollTo(0,{scrol})")
    time.sleep(0.5)
# driver.execute_script('window.scrollTo(0, document.body.scrollHeight);')
div = []
list_i = []
item_title = []
a = []
for z in range(1,16):                     
    div.append(product.find_element_by_xpath('//*[@id="root"]/div/div/div[2]/div[2]/div/div[2]/ul/div[1]/li[3]/div/div[2]/div/div[1]'+str([z])))
for pr in div:
    list_i.append(pr.find_elements_by_class_name('list-item'))
for pc in list_i:
    for p in pc:
        item_title.append(p.find_element_by_class_name('item-title-wrap'))
for pt in item_title:
    a.append(pt.find_element_by_tag_name('a'))
for prt in a:
    productlist.append(prt.text)



import requests
import json
import re

target = ["title", "itemDetailUrl", "imagePath"]

def main(url):
    r = requests.get(url)
    match = re.search(r'data: ({.+})', r.text).group(1)
    data = json.loads(match)
    goal = [data['pageModule'][x] for x in target] + \
        [data['priceModule']['formatedActivityPrice']]
    print(goal)
    
main("https://es.aliexpress.com/item/32601027783.html")    
    
    
    
    
import requests
import json
import re

target = ["item-title", "item-price-row"]


def main(url):
    r = requests.get(url)
    match = re.search(r'data: ({.+})', r.text)
    data = json.loads(match)
    goal = [data['pageModule'][x] for x in target] + \
        [data['priceModule']['formatedActivityPrice']]
    print(goal)

r = requests.get("https://es.aliexpress.com/wholesale?trafficChannel=main&d=y&CatId=204000014&SearchText=juegos&ltype=wholesale&SortType=default&page=1")
re.search(r'data: ({.+})', r.text).group(1)

main("https://es.aliexpress.com/wholesale?trafficChannel=main&d=y&CatId=204000014&SearchText=juegos&ltype=wholesale&SortType=default&page=1")    
    
