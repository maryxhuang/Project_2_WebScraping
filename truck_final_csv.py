from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import math
import time
import csv
import re

driver = webdriver.Chrome()
driver = webdriver.Chrome(r'E:\E-Documents\NYC_Data_Science_Academy\Project_2_WebScraping\v8_0802_all\chromedriver.exe')
links = ['ryder-truck-rental', 'enterprise-truck-rental', 'budget-truck-rental', 'penske-truck-rental', 'uhaul-trucks']
urls = ['https://moving101.hireahelper.com/transportation/moving-truck-rental/' + x for x in links]

csv_file = open('reviews.csv', 'w', newline = '')
writer = csv.writer(csv_file)
review_dict = {}
review_dict['company'] = 'Company'
review_dict['username'] = 'Username'
review_dict['city'] = 'City'
review_dict['state'] = 'State'
review_dict['publishdate'] = 'Date'
review_dict['rating'] = 'Rating'
review_dict['review_text'] = 'Review'
review_dict['distance'] = 'Distance'
review_dict['cost'] = 'Cost'
review_dict['permile'] = 'Per.Mile'
review_dict['sqft_moved'] = 'Sqft.Moved'
writer.writerow(review_dict.values())

for url in urls:
        driver.get(url)
        index = 1
        company = driver.find_element_by_xpath('//div[@class="small-12 columns"]/h3').text.split()[0]
        num_reviews = int(driver.find_element_by_xpath('//div[@class="rating-bar summary"]/a/span').text)
        num_pages = math.ceil(num_reviews/3)
        print("Total of", num_reviews, "in", num_pages, "pages")
        print('=' * 10)
        while True:
                try:
                        while index <= num_pages:
                                wait_button = WebDriverWait(driver, 10)
                                next_button = wait_button.until(EC.element_to_be_clickable((By.XPATH, '//div[@class="text-center"]/a[@class="button bigger js-loadMoreReviewsButton hahbutton"]')))
                                print('Page', str(index)+'/'+str(num_pages))
                                next_button.click()
                                time.sleep(2)
                                index += 1

                        wait_review = WebDriverWait(driver, 10)
                        reviews = wait_review.until(EC.presence_of_all_elements_located((By.XPATH, '//div[@itemprop="review"]')))
                        print(len(reviews))
                        print('=' * 10)
                        r = 1
                        for review in reviews:
                                print(company+':', str(r)+'/'+str(num_reviews))
                                review_dict = {}
                                username = review.find_element_by_xpath('.//div[@class="small-7 columns"]/h3/span/span').text
                                city, state = review.find_element_by_xpath('.//div[@class="small-7 columns"]/h3/small').text.split(', ')[:2]
                                publish_date = review.find_element_by_xpath('.//div[@class="small-5 columns date"]').text.split()[0]
                                rating = float(review.find_element_by_xpath('.//div[@class="progress"]/meta[@itemprop="ratingValue"]').get_attribute('content'))
                                review_text = review.find_element_by_xpath('.//p[@itemprop="reviewBody"]').text
                                distance = review.find_element_by_xpath('.//div[@class="row"]/div[1]/span[2]').text
                                cost = review.find_element_by_xpath('.//div[@class="row"]/div[2]/span[2]').text
                                per_mile = review.find_element_by_xpath('.//div[@class="row"]/div[3]/span[2]').text
                                sqft_moved = review.find_element_by_xpath('.//div[@class="row"]/div[4]/span[2]').text
                                review_dict['company'] = company
                                review_dict['username'] = username
                                review_dict['city'] = city
                                review_dict['state'] = state
                                review_dict['publishdate'] = publish_date
                                review_dict['rating'] = rating
                                review_dict['review_text'] = review_text
                                review_dict['distance'] = distance
                                review_dict['cost'] = cost
                                review_dict['permile'] = per_mile
                                review_dict['sqft_moved'] = sqft_moved
                                r += 1
                                writer.writerow(review_dict.values())

                        csv_file.close()
                        # driver.close()
                        # driver.close()
                        break

                except Exception as e:
                        print(e)
                        csv_file.close()
                        # driver.close()
                        # driver.close()
                        break
