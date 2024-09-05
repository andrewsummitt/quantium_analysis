# ANDREW SUMMITT
# QUANTIUM TASK 1


# Goals
# - Understand the current purchasing trends and behaviors
# - Analyze customer segments and purchasing behavior


# STEPS
# Start with objectives
# Break the problem down
# Write an outline
# Look at data



# 1.0 SET UP ----
library(tidyverse)
library(readxl)
library(skimr)
library(tidyquant)
library(rcartocolor)


## 1.1 READ IN DATA ----
behavior_tbl <- read_csv("00_data/QVI_purchase_behaviour.csv")
transaction_tbl <- read_xlsx("00_data/QVI_transaction_data.xlsx")


## 1.2 EXAMINE DATA ----
# Behavior(Segments) - Card #, Age/Demographic, Membership Level
behavior_tbl


# Date, Store #, Product, Qty, Sales
transaction_tbl


## 1.3 EXPLORE & UNDERSTAND DATA ----

# Behavior Info
behavior_tbl                  # 70K customers
behavior_tbl %>% glimpse()    # dbl, chr, chr
behavior_tbl %>% skim()       # no missing data

behavior_tbl %>%                  # not sure about factor
    select(LIFESTAGE) %>% 
    unique()

behavior_tbl %>%                  # could be an ordered factor
    select(PREMIUM_CUSTOMER) %>% 
    unique()


# Transaction Data
transaction_tbl %>% count(duplicated(.))   # 1 duplicated row
transaction_tbl %>% glimpse()     # 1 chr, 7 dbl
transaction_tbl %>% skim()        # no missing data


# How many unique stores, customers & products are there?

transaction_tbl %>% 
    select(STORE_NBR, LYLTY_CARD_NBR, TXN_ID, PROD_NBR) %>% 
    map(~ unique(.) %>% length()) %>% 
    as_tibble()


# Are there multiple transactions for the same customers? - Yes
transaction_tbl %>% 
    filter(LYLTY_CARD_NBR == 1307)


# Does TOT_SALES mean Price x Quantity? - Yes

transaction_tbl %>% 
    select(PROD_NBR, PROD_QTY, TOT_SALES) %>% 
    mutate(UNIT_PRICE = TOT_SALES/PROD_QTY) %>% 
    filter(PROD_NBR == 1)


# What format is the date in? - Occurs in Year 2088?
transaction_tbl %>% 
    select(DATE) %>% 
    mutate(DATE_FORMATTED = DATE %>% as_date()) %>% 
    arrange(DATE)

# What info does the PROD_NAME include? - Brand, Name, & Size
transaction_tbl %>% 
    select(PROD_NAME) %>% 
    unique() %>% 
    head(20)



# 2.0 DATA CLEANING ----

## 2.1 Adding Factors ----
# Clean Factors Low to High Levels
behavior_tbl_cleaned <- behavior_tbl %>% 
    mutate(PREMIUM_CUSTOMER = PREMIUM_CUSTOMER %>% as.factor()) %>% 
    mutate(LIFESTAGE        = LIFESTAGE %>% as_factor() %>% 
            fct_relevel("YOUNG SINGLES/COUPLES",
                        "YOUNG FAMILIES",
                        "MIDAGE SINGLES/COUPLES",
                        "NEW FAMILIES",
                        "OLDER SINGLES/COUPLES",
                        "OLDER FAMILIES",
                        "RETIREES"
            )
    )

behavior_tbl_cleaned


# Change Formats & Add Unit Price
transaction_cleaned_tbl <- transaction_tbl %>% 
    mutate(DATE = DATE %>% as_date()) %>% 
    mutate(UNIT_PRICE = TOT_SALES / PROD_QTY) %>% 
    arrange(DATE) %>%                              # sort chronological
    unique()                                       # remove one duplicated row


transaction_cleaned_tbl %>% head(10)


# 3.0 DATA PREPARATION ----
# Pack size, brand name, define metrics


transaction_prepared_tbl <- transaction_cleaned_tbl %>% 
    
    ## 3.1 EXTRACT PROD_WEIGHT ----
    # Creates New Numeric Column
    mutate(PROD_WEIGHT = parse_number(PROD_NAME)) %>% 
    
    ## Removes Weight Chr From Product Name
    mutate(PROD_NAME = PROD_NAME %>% 
               str_replace_all("[0-9]", "") %>% 
               str_replace(" g", "") %>% 
               str_replace(" G", "") %>% 
               str_trim("right")) %>% 
    
    ## 3.2 EXTRACT BRAND ----
    # Tokenize PROD_NAME
    separate(
        col = PROD_NAME,
        into = str_c("NAME_", 1:8),
        sep = " ",
        remove = FALSE,
        fill = "right"
    ) %>% 
    
    # Fix Brand Typos
    mutate(NAME_1 = case_when(
        NAME_1 == "Smith" ~ "Smiths",
        NAME_1 == "GrnWves" ~ "Grain",
        NAME_1 == "NCC" ~ "Natural",
        NAME_1 == "WW" ~ "Woolworths",
        NAME_1 == "RRD" ~ "Red",
        NAME_1 == "Snbts" ~ "Sunbites",
        NAME_1 == "Dorito" ~ "Doritos",
        NAME_1 == "Infzns" ~ "Infuzions",
        TRUE ~ NAME_1)
    ) %>% 
    
    # Set Brand to First Word Of PROD_NAME
    mutate(PROD_BRAND = case_when(
        
        # Adjust for Brands with Two Word Names
        str_detect(str_to_lower(NAME_1), "grain") ~ "Grain Waves",
        str_detect(str_to_lower(NAME_1), "natural") ~ "Natrual ChipCo",
        str_detect(str_to_lower(NAME_1), "french") ~ "French Fries",
        str_detect(str_to_lower(NAME_1), "burger") ~ "Burger Ring",
        
        # Adjust for Brands with Three Word Names
        str_detect(str_to_lower(NAME_1), "red") ~ "Red Rock Deli",
        str_detect(str_to_lower(NAME_1), "old") ~ "Old El Paso",
        TRUE ~ NAME_1
        )
    ) %>% 
    
    ## 3.3 UPDATE PRODUCT NAME ----
    # Removes Brand for PROD_NAME
    mutate(PROD_NAME = case_when(
        
        str_detect(PROD_NAME, pattern = "Smith")   ~ PROD_NAME %>% str_remove("Smiths") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "GrnWves") ~ PROD_NAME %>% str_remove("GrnWves") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "NCC")     ~ PROD_NAME %>% str_remove("NCC") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "WW")      ~ PROD_NAME %>% str_remove("WW") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "RRD")     ~ PROD_NAME %>% str_remove("RRD") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "Snbts")   ~ PROD_NAME %>% str_remove("Snbts") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "Dorito")  ~ PROD_NAME %>% str_remove("Doritos") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "Infzns")  ~ PROD_NAME %>% str_remove("Infzns") %>% str_trim("left"), 
        str_detect(PROD_NAME, pattern = "Rings")   ~ PROD_NAME %>% str_remove("Burger Rings") %>% str_trim("left"), 
               
       TRUE ~ PROD_NAME %>% str_remove(PROD_BRAND) %>% str_trim("left"))
    )%>% 
    
    # Removes Extra Spaces & Handles Cases of No Product Name
    mutate(PROD_NAME = PROD_NAME %>% str_replace_all(" +", " "),
           PROD_NAME = replace(PROD_NAME, PROD_NAME == "", "Not Specified")
    ) %>% 
    
    ## 3.4 REMOVE EXTRA COLUMNS ----
    select(DATE:PROD_NBR,PROD_BRAND, PROD_NAME, PROD_QTY:PROD_WEIGHT)



transaction_prepared_tbl %>% select(PROD_NAME) %>% print(n = 50)
transaction_prepared_tbl

# 4.0 JOINING DATA ----

behavior_transaction_tbl <- transaction_prepared_tbl %>% 
    left_join(behavior_tbl_cleaned, by = "LYLTY_CARD_NBR") %>% 
    glimpse()



# 5.0 DATA SUMMARIES ----
behavior_transaction_tbl


## 5.1 Sales over Time ----

revenue_by_month_tbl <- behavior_transaction_tbl %>% 
    
    # Aggregate Dates
    select(DATE, TOT_SALES) %>% 
    mutate(YEAR_MONTH = floor_date(DATE, "months")) %>% 
    
    # Summarize Total Sales
    group_by(YEAR_MONTH) %>% 
    summarize(SALES = sum(TOT_SALES)) %>% 
    ungroup() %>% 
    
    # Filter to Exclude Incomplete Months
    filter(YEAR_MONTH < "2089-07-01" %>% ymd())
    

revenue_by_month_tbl %>%   
    
    # Plot
    ggplot(aes(YEAR_MONTH, SALES)) +
    geom_point() +
    geom_line() +
    
    # Format
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    # theme(axis.text.x = element_text(angle = 45)) +
    labs(
        title    = "Revenue by Month",
        subtitle = "Jul 2088 - Jun 2089",
        x        = "",
        y        = "Sales"
    ) +
    theme_bw() +
    scale_color_tq()


behavior_transaction_tbl %>% glimpse()

## 5.2 Sales by Brand ----
brand_tbl <- behavior_transaction_tbl %>% 
    group_by(PROD_BRAND) %>% 
    summarize(SALES = sum(TOT_SALES)) %>% 
    ungroup() %>% 
    arrange(desc(SALES)) %>% 
    mutate(PROD_BRAND = PROD_BRAND %>% as_factor() %>% fct_rev())
  


brand_tbl %>% 
    
    # Plot
    ggplot(aes(SALES, PROD_BRAND, fill = PROD_BRAND)) +
    geom_col(aes(y = PROD_BRAND)) +
    
    # Format
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
        title = "Total Sales by Brand",
        x = "",
        y = "Brand"
    ) + 
    theme_bw() +
    # scale_fill_carto_d(direction = -1) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.position = "none")
    



## 5.3 Customers by Membership Tier ----
membership_tbl <- behavior_transaction_tbl %>% 
    
    # Aggregate by Tier
    group_by(PREMIUM_CUSTOMER) %>% 
    summarize(count = n()) %>% 
    ungroup()

    
membership_tbl %>% 
    
    # Plot
    ggplot(aes(count, PREMIUM_CUSTOMER)) +
    geom_col(aes(x = PREMIUM_CUSTOMER, y = count, fill = PREMIUM_CUSTOMER)) +
    
    # Format
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
        title = "Customers by Membership Tier",
        x = "Tier",
        y = ""
    ) +
    # scale_fill_tq(theme = "green") +
    theme_bw() +
    theme(legend.position = "none")
    


## 5.4 Total Sales By Membership Tier ----
membership_sales_tbl <- behavior_transaction_tbl %>% 
    group_by(PREMIUM_CUSTOMER) %>% 
    summarize(SALES = sum(TOT_SALES)) %>% 
    ungroup()

membership_sales_tbl %>% 
    
    # Plot
    ggplot(aes(PREMIUM_CUSTOMER, SALES, fill = PREMIUM_CUSTOMER)) +
    geom_col() +
    
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
        title = "Sales by Membership Tier",
        caption = "Q: What discounts are available to premium members?",
        x = "",
        y = ""
    ) +
    # scale_fill_tq(theme = "green") +
    theme_bw() +
    theme(legend.position = "none")
    


# Do Premium Members buy more? Which Kinds?

# Stacked Bar chart?
cust_brand_tbl <- behavior_transaction_tbl %>% 
    
    # Group & Summarize
    group_by(PREMIUM_CUSTOMER, PROD_BRAND) %>% 
    summarize(quantity = sum(PROD_QTY)) %>% 
    ungroup() %>% 
    
    # Arrange
    arrange(desc(quantity), PROD_BRAND) %>%
    mutate(PREMIUM_CUSTOMER = PREMIUM_CUSTOMER %>% as_factor() %>% fct_rev()) %>% 
    mutate(PROD_BRAND = PROD_BRAND %>% as_factor() %>% fct_rev())

cust_brand_tbl %>%     
    # Plot
    ggplot(aes(quantity, PROD_BRAND, fill = PREMIUM_CUSTOMER)) +
    geom_col() +
    
    # Format
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = "Brand by Customer Tier",
         x = "Sales", 
         y = "Brand")



# Which stores have the most premium members?
cust_store_tbl <- behavior_transaction_tbl %>% 
    group_by(STORE_NBR) %>% 
    summarize(total = n(),
              budget = sum(PREMIUM_CUSTOMER == "Budget"),
              mainstream = sum(PREMIUM_CUSTOMER == "Mainstream"),
              premium = sum(PREMIUM_CUSTOMER == "Premium")) %>% 
    ungroup() %>% 
    arrange(desc(total)) %>%
    mutate(STORE_NBR = STORE_NBR %>% as.character() %>% as_factor()) %>% 
    slice(1:20)


cust_store_tbl %>% 
    pivot_longer(cols = c(budget, mainstream, premium),
                 names_to = "tier",
                 values_to = "amount") %>% 
    
    mutate(tier = tier %>% as_factor() %>% fct_rev()) %>% 
    
    ggplot(aes(STORE_NBR, total)) +
    geom_col(aes(fill = tier)) +
    
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
        title = "Top Stores by Tier",
        x = "Store Number",
        y = "Customers",
        fill = "Tier",
        caption = "A: Indicates evenly distributed membership types"
    )


# Count of Premium Members over Time? - How is it changing?


