from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import csv
import re
import time

driver = webdriver.Chrome()
driver.get("https://www.rei.com/search?q=kayak")
#show all results
all_results_button = driver.find_element_by_xpath('//*[@id="app-main"]/div/div/div[1]/div/div[2]/div[2]/div[2]/div/ul/li[last()]')
all_results_button.click()

wait_products = WebDriverWait(driver, 10)
products = wait_products.until(EC.presence_of_all_elements_located((By.XPATH, '//*[@id="search-results"]/ul/li')))
#open two csv files
products_csv = open('products.csv', 'w', encoding='utf-8', newline='')
reviews_csv = open('reviews.csv', 'w', encoding='utf-8', newline='')
product_writer = csv.writer(products_csv)
reviews_writer = csv.writer(reviews_csv)

for i in range(1,len(products)+1):
    #each loop will choose the next product in the list
    button = driver.find_element_by_xpath('//*[@id="search-results"]/ul/li[{x}]/a'.format(x=i))
    button.click()

    #exceptions should only happen when a product redirects to REI Outlet (which has a different layout and since about 5 
    #products link to it was not deemed necessary to scrape)
    try:
        print('Begin scraping product {x}'.format(x = i))

        wait_name = WebDriverWait(driver, 10)
        name = wait_name.until(EC.presence_of_all_elements_located((By.XPATH,'//div[@class="product-title"]')))[0].text
        product_id = driver.find_element_by_xpath('//*[@id="product-container"]/div[2]/div/div[2]/div[3]/div[2]/span/span').text

        #products with 0 reviews and  0 ratings cause error
        try:
            total_reviews = driver.find_element_by_xpath('//*[@id="bv-rating-summary"]/div/div/div[3]/button').text
            total_reviews = "".join((re.findall("[0-9]", total_reviews)))
            rating = driver.find_element_by_xpath('//*[@id="bv-rating-summary"]/div/div/div[2]/button').text
        except:
            total_reviews = 0
            rating = 0

        #a few prices have a range and require further selection
        try:
            price = driver.find_element_by_xpath('//*[@id="js-product-information-price"]/div/span/span/span').text
        except:
            price = driver.find_element_by_xpath('//*[@id="js-product-information-price"]/div/span/div/span/span[1]').text
        

        #change price to float
        price = float(re.sub('[$,/,]', '', price))
        #create dictionary
        product_dict = {'product_id': product_id, 'name':name, 'total_reviews':total_reviews, 'rating': rating, 'price': price}

        #locate product details and split by names and values. Since different product pages don't all have the same 
        #technical details, a temporary dictionary is used to draw from. If the page is missing the value, 'None' is used
        #instead
        tech_specs = driver.find_element_by_xpath('//*[@id="product-wrapper"]/div[9]/div[1]/div[2]/div/table/tbody')
        spec_names = tech_specs.find_elements_by_tag_name('th')
        spec_values = tech_specs.find_elements_by_tag_name('td')

        temp = {}
        c = iter(spec_values)
        for name in spec_names:
            temp[name.text] = (next(c).text)

        product_dict['Best Use'] = temp.get('Best Use', None)
        product_dict['Material(s)'] = temp.get('Material(s)', None)
        product_dict['Length'] = temp.get('Length', None)
        product_dict['Width'] = temp.get('Width', None)
        product_dict['Depth'] = temp.get('Depth', None)
        product_dict['Weight'] = temp.get('Weight', None)
        product_dict['Cockpit Size'] = temp.get('Cockpit Size', None)
        product_dict['Seat Type'] = temp.get('Seat Type', None)
        product_dict['Number of Paddlers'] = temp.get('Number of Paddlers', None)
        product_dict['Hatch Capacity'] = temp.get('Hatch Capacity', None)
        product_dict['Weight Capacity (lbs)'] = temp.get('Weight Capacity (lbs)', None)
        product_dict['Tracking System'] = temp.get('Tracking System', None)
        product_dict['Foldable'] = temp.get('Foldable', None)
        product_dict['Packed Dimensions'] = temp.get('Packed Dimensions', None)
        product_dict['Sustainability'] = temp.get('Sustainability', None)

        #write complete product dictionary as a new row in csv
        product_writer.writerow(product_dict.values())
        
        ##scrape from reviews. If no reviews exists, add no reviews to dictionary and move on.
        #Reviews are maxed at 8. We used a try/except method to determine if a 'load more' button
        #exists and if it does, click it until we cannot locate it anymore (all reviews should be visible).
        reviews_dict = {'Product ID':product_id}
        
        if int(total_reviews) >= 1:
            try:
                load_more = driver.find_element_by_xpath('//*[@id="BVRRContainer"]/div/div/div/div/div[3]/div/button/span')
                load_more_exists = True
            except:
                load_more_exists = False

            while load_more_exists:
                try:
                    load_more.click()
                except:
                    load_more_exists = False
            #Now that all reviews are visible, locate all reviews.
            reviews = driver.find_elements_by_xpath('//*[@id="BVRRContainer"]/div/div/div/div/ol/li')
            #Loop through reviews and add each body text to dictionary
            index = 1
            for review in reviews:
                body = review.find_element_by_class_name('bv-content-summary-body-text').text
                REPLACE_NO_SPACE = re.compile("[.;:!\'?,\"()\[\]]")
				REPLACE_WITH_SPACE = re.compile("(<br\s*/><br\s*/>)|(\-)|(\/)")
				body = REPLACE_NO_SPACE.sub("", body.lower())
				body = REPLACE_WITH_SPACE.sub(" ", line)
                reviews_dict['review {x}'.format(x = index)] = body
                index += 1
        else:
            reviews_dict['Reviews'] = 'No reviews'

        #write complete dictionary as a new row in csv
        reviews_writer.writerow(reviews_dict.values())

        #sleeping time before returning to results page
        print('Finished scraping product {x}'.format(x = i))
        time.sleep(5)
        driver.back()
        time.sleep(2)

    except Exception as e:
        print(e)
        print("Unscraped REI Outlet page?")
        time.sleep(3)
        driver.back()
        time.sleep(1)

#close csv files
products_csv.close()
reviews_csv.close()