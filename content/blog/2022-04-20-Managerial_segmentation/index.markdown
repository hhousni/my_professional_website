---
title: Personalize your customer journey with the RFM segmentation, Part II
layout: single-sidebar
date: '2022-04-21'
categories:
   - Marketing
   - Business Analytics
   - Customer Insights
tags:
  - RFM segmentation
  - Customer Insights analysis
slug: managerial-segmentation
subtitle: A managerial approach
summary: In this post we explore how to implement the RFM segmentation in R programming using a managerial approach.
lastmod: 2021-05-04
featured: yes
draft: no
image:
  placement: 1
  caption: 
  focal_point: Center
  preview_only: no
output: 
  blogdown::html_page:
    toc: yes
    number_sections: no
    toc_depth: 1
  
links:
  - icon: github
    icon_pack: fab
    name: post materials
    url: https://github.com/hhousni/rfm_analysis
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />




This is the Part II of a two-part series named: "Personalise your customer Journey With RFM segmentation". 

An online retail wants to better understand their customers in order to perform targeted marketing actions. In others words, the eRetail business wants to know which customers are active and which ones are moving from active to inactive. Knowing that will help the online retail to create targeted marketing to keep customers active. To solve this business challenge, we will use a grouping technique called customer segmentation, and group customers by their purchase behaviors.

**Customer segmentation** is the process of dividing a group customers into subgroups using specific elements e.g. demographics, geographic, psychographic, etc. There are multitude of way to perform a customer segmentation. In this two-part series, we will focus on a popular, easy to use and effective segmentation method called **RFM segmentation** 

[The first article](https://www.housnihassani.com/blog/rfm-segmentation/) was focused on how to implement an RFM segmentation in R using a statistical method (hierarchical clustering). In this part II, we will focus on how to perform an RFM segmentation using a managerial approach.

## How should we split the customers? 

There are infinite criteria that can be used to segment customers . However, there is a popular, an effective, and an easy-to-use segmentation method named RFM segmentation that use customers purchase behavior to group customer. In the RFM segmentation: 

- R stands for Recency. When was the last time the customer - purchased something?
- F stand for Frequency. What is the number of total purchases made by the customer?
- M stands for Monetary. What is the average amount spent when purchasing something?

It’s an excellent tool for marketers to target specific customers with communications and actions that are relevant for their behavior. Most specifically, it can help companies to manage wisely their budget,to increase revenue and profit by increasing sales, to better understand their customers, to gain competitive advantage against competitors, etc.

## How can we perform the RFM segmentation in R ?  

**About the data** 

In this article, we will use the online store data set from the UCL repository available. [Here](https://archive.ics.uci.edu/ml/datasets/Online+Retail+II). The data set contains all the transactions occurring for a UK-based and registered, non-store online retail between 01/12/2009 and 09/12/2011.The company mainly sells unique all-occasion gift-ware. Many customers of the company are wholesalers."

We have done all the data exploration in the [Part I](https://www.housnihassani.com/blog/rfm-segmentation/). Please refer to it for further informations. 

**Data loading**


```r
# install and Load the relevant packages
if(!require("pacman")) install.packages("pacman")
p_load(readxl,tidyverse,kableExtra)
```



```r
# Data loading 
o.data <- read.csv("https://raw.githubusercontent.com/hhousni/rfm_analysis/main/Online%20Retail.csv")
# Data prep
data <- o.data %>% 
  select(CustomerID, Quantity, UnitPrice, InvoiceDate) %>%
  mutate(InvoiceDate=as.Date(InvoiceDate, "%d/%m/%Y")) %>%
  drop_na(CustomerID) %>% 
  filter(Quantity > 0, UnitPrice > 0 )

# create the RFM indicators 
customersQ42011 <- data%>% 
  mutate (purchase_amount = (Quantity * UnitPrice)) %>% 
  arrange(CustomerID) %>%
  group_by(CustomerID,InvoiceDate) %>%
  summarise(Sales = sum(purchase_amount)) %>%
  mutate(datediff =as.numeric(as.Date("2011-12-10") - InvoiceDate)) %>% 
  group_by(CustomerID) %>%
  summarise(monetary  = mean(Sales),
            recency   = min(datediff),
            frequency = n(),
            fisrt_purchase = max(datediff))

head(customersQ42011)
```

```
## # A tibble: 6 x 5
##   CustomerID monetary recency frequency fisrt_purchase
##        <int>    <dbl>   <dbl>     <int>          <dbl>
## 1      12346   77184.     326         1            326
## 2      12347     616.       3         7            368
## 3      12348     449.      76         4            359
## 4      12349    1758.      19         1             19
## 5      12350     334.     311         1            311
## 6      12352     358.      37         7            297
```


We have created and stored the RFM indicators and stored them in a new data frame called customerQ42011 which going to be used to proceed the segmentation. 

## Customer Segmentation 

The managerial method for segmentation requires marketers to set rules in order to split their customers. In this article, we choose firstly to split the online store customers by the recency indicator. In other words, we choose to slice customers by when was the last time the customer made a purchase. Then, w e choose secondly to create subgroups by adding the monetary value. We choose finally to add the first purchase date to create the new active subgroup.

We assume that this is a quarterly analysis, taking place one day after the last purchasing date i.e 2011-12-10. We consider that a customer is:

- **New active**, if he has made his first purchase in the last 30 days.
- **Active high value**, if he has made at least one purchase in the last 70 days and his average order value is equal or more than £458 
- **Active low value**, if he has made at least one purchase in the last 70 days and his average order value is less than £458  
- **Warm high value**, if he made at least one purchase between 70 days and 162 days and his average order value was equal or more than £458.
- **Warm low value**, if he made at least one purchase between 70 days and 162 days and his average order value was less than £458.
- **Cold**, if he made no order in the last 162 days with at least one order in the in the last 253 days.
- **Inactive**, if his last order was in more than 253 days.


```r
# Segment customers according to the rules define above. 
customersQ42011$segment <- "NA"
customersQ42011$segment[which(customersQ42011$recency <= 70)] = "Active"
customersQ42011$segment[which(customersQ42011$recency <= 70 & customersQ42011$fisrt_purchase <=30)] = "New active"
customersQ42011$segment[which(customersQ42011$segment == "Active" & customersQ42011$monetary >=458)] = "Active high value"
customersQ42011$segment[which(customersQ42011$segment == "Active" & customersQ42011$monetary < 458)] = "Active low value"
customersQ42011$segment[which(customersQ42011$recency <= 162 & customersQ42011$recency > 70)] = "Warm"
customersQ42011$segment[which(customersQ42011$segment == "Warm" & customersQ42011$monetary > 458)] = "Warm high value"
customersQ42011$segment[which(customersQ42011$segment == "Warm" & customersQ42011$monetary < 458)] = "Warm low value"
customersQ42011$segment[which(customersQ42011$recency <= 253 & customersQ42011$recency > 162)] = "Cold"
customersQ42011$segment[which(customersQ42011$recency > 253)] = "Inactive"

#create a factor to order the segment from new to inactive customers 
customersQ42011$segment = factor(customersQ42011$segment, levels = c("New active","Active high value","Active low value","Warm high value","Warm low value","Cold","Inactive"))

#create the average profile by segment.
customersQ42011 %>%
  group_by(segment) %>%
  summarise(NbCustomer = n(),
            Monetary   = paste0("£",round(mean(monetary))),
            Recency    = round(mean(recency)),
            Frequency  = round(mean(frequency))) %>%
  mutate("Percentage"  = NbCustomer/sum(NbCustomer)) %>%
  mutate(Percentage=paste0(round(Percentage*100),"%"))%>%
  select(segment,NbCustomer,Percentage,Monetary,Recency,Frequency)%>%
  kbl(caption = "Average profile by segment") %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-3"></span>Table 1: Average profile by segment</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> segment </th>
   <th style="text-align:right;"> NbCustomer </th>
   <th style="text-align:left;"> Percentage </th>
   <th style="text-align:left;"> Monetary </th>
   <th style="text-align:right;"> Recency </th>
   <th style="text-align:right;"> Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> New active </td>
   <td style="text-align:right;"> 252 </td>
   <td style="text-align:left;"> 6% </td>
   <td style="text-align:left;"> £385 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Active high value </td>
   <td style="text-align:right;"> 629 </td>
   <td style="text-align:left;"> 14% </td>
   <td style="text-align:left;"> £1038 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Active low value </td>
   <td style="text-align:right;"> 1679 </td>
   <td style="text-align:left;"> 39% </td>
   <td style="text-align:left;"> £263 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Warm high value </td>
   <td style="text-align:right;"> 218 </td>
   <td style="text-align:left;"> 5% </td>
   <td style="text-align:left;"> £880 </td>
   <td style="text-align:right;"> 103 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Warm low value </td>
   <td style="text-align:right;"> 588 </td>
   <td style="text-align:left;"> 14% </td>
   <td style="text-align:left;"> £246 </td>
   <td style="text-align:right;"> 107 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cold </td>
   <td style="text-align:right;"> 483 </td>
   <td style="text-align:left;"> 11% </td>
   <td style="text-align:left;"> £461 </td>
   <td style="text-align:right;"> 206 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inactive </td>
   <td style="text-align:right;"> 489 </td>
   <td style="text-align:left;"> 11% </td>
   <td style="text-align:left;"> £484 </td>
   <td style="text-align:right;"> 310 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

The table above show results and the average profile by segment. For example, on average, the active high-value customers have spent £1038 and have made 7 purchases. While, on average the inactive customer spent £489 and have made 1 purchase.   

From this, we can determine a certain amount of analysis to understand each segment behaviour. We can for instance interested to determine how much revenue is generated by cluster in the Q42011


```r
# How much revenue is generated by each cluster in Q42011
data %>%
  filter(InvoiceDate >= "2011-10-01") %>%
  group_by(CustomerID) %>%
  summarise(revQ42011=sum(Quantity*UnitPrice)) %>%
  right_join(customersQ42011, by="CustomerID") %>%
  group_by(segment) %>%
  summarise("revQ42011"=paste0("£ ",sum(revQ42011))) %>% replace(is.na(.), 0) %>%
  filter(segment %in% c("New active","Active high value","Active low value")) %>% kbl(caption = "Total Q42011 revenue generated by segment") %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-4"></span>Table 2: Total Q42011 revenue generated by segment</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> segment </th>
   <th style="text-align:left;"> revQ42011 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> New active </td>
   <td style="text-align:left;"> £ 115023.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Active high value </td>
   <td style="text-align:left;"> £ 1727384.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Active low value </td>
   <td style="text-align:left;"> £ 876921.180000001 </td>
  </tr>
</tbody>
</table>


The table above shows the total revenue generated by segment in the Q42011. We see that the active high value, the active low value and the new active have generated respectively £1,727,384, £876,921 and £115,023. 

We can go further by trying to know how much Q42011 revenue was generated by inactive, cold or warm Q32011 customers. Knowing that can help us to know how much revenue can be expected from inactive, cold and warm customers today going tomorrow.  

## Compare past and future periods

From Here, we consider that Q42011 didn't happen. We are at the end of Q32011. And, we are doing a segmentation using the same conditions as we did in Q42011.

**Data prep**


```r
# Split the data, keep only data before 2011-10-01
customersQ32011 <- data %>% 
  filter(InvoiceDate < "2011-10-01") %>%
  mutate (purchase_amount = (Quantity * UnitPrice)) %>% 
  arrange(CustomerID) %>%
  group_by(CustomerID,InvoiceDate) %>%
  summarise(Sales = sum(purchase_amount)) %>%
  mutate(datediff =as.numeric(as.Date("2011-10-02") - InvoiceDate)) %>% 
  group_by(CustomerID) %>%
  summarise(monetary  = mean(Sales),
            recency   = min(datediff),
            frequency = n(),
            fisrt_purchase = max(datediff))

# Perform segmentation in Q32011
customersQ32011$segment <- "NA"
customersQ32011$segment[which(customersQ32011$recency <= 70)] = "active"
customersQ32011$segment[which(customersQ32011$recency <= 70 & customersQ32011$fisrt_purchase <=30)] = "new active"
customersQ32011$segment[which(customersQ32011$segment == "active" & customersQ32011$monetary >=458)] = "active high value"
customersQ32011$segment[which(customersQ32011$segment == "active" & customersQ32011$monetary < 458)] = "active low value"
customersQ32011$segment[which(customersQ32011$recency <= 162 & customersQ32011$recency > 70)] = "warm"
customersQ32011$segment[which(customersQ32011$segment == "warm" & customersQ32011$monetary > 458)] = "warm high value"
customersQ32011$segment[which(customersQ32011$segment == "warm" & customersQ32011$monetary < 458)] = "warm low value"
customersQ32011$segment[which(customersQ32011$recency <= 253 & customersQ32011$recency > 162)] = "cold"
customersQ32011$segment[which(customersQ32011$recency > 251)] = "inactive"

customersQ32011$segment = factor(customersQ32011$segment, levels = c("new active","active high value","active low value","warm high value","warm low value","cold","inactive"))

customersQ32011 %>%
  group_by(segment) %>%
  summarise(NbCustomer = n(),
            Monetary   = paste0("£",round(mean(monetary))),
            Recency    = round(mean(recency)),
            Frequency  = round(mean(frequency))) %>%
  mutate("Percentage"  = NbCustomer/sum(NbCustomer)) %>%
  mutate(Percentage=paste0(round(Percentage*100),"%"))%>%
  select(segment,NbCustomer,Percentage,Monetary,Recency,Frequency)%>%
  kbl(caption = "Average profile by segment") %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-5"></span>Table 3: Average profile by segment</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> segment </th>
   <th style="text-align:right;"> NbCustomer </th>
   <th style="text-align:left;"> Percentage </th>
   <th style="text-align:left;"> Monetary </th>
   <th style="text-align:right;"> Recency </th>
   <th style="text-align:right;"> Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> new active </td>
   <td style="text-align:right;"> 292 </td>
   <td style="text-align:left;"> 8% </td>
   <td style="text-align:left;"> £492 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> active high value </td>
   <td style="text-align:right;"> 470 </td>
   <td style="text-align:left;"> 13% </td>
   <td style="text-align:left;"> £915 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> active low value </td>
   <td style="text-align:right;"> 1143 </td>
   <td style="text-align:left;"> 32% </td>
   <td style="text-align:left;"> £266 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> warm high value </td>
   <td style="text-align:right;"> 175 </td>
   <td style="text-align:left;"> 5% </td>
   <td style="text-align:left;"> £1147 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> warm low value </td>
   <td style="text-align:right;"> 695 </td>
   <td style="text-align:left;"> 19% </td>
   <td style="text-align:left;"> £231 </td>
   <td style="text-align:right;"> 112 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cold </td>
   <td style="text-align:right;"> 587 </td>
   <td style="text-align:left;"> 16% </td>
   <td style="text-align:left;"> £374 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> inactive </td>
   <td style="text-align:right;"> 254 </td>
   <td style="text-align:left;"> 7% </td>
   <td style="text-align:left;"> £596 </td>
   <td style="text-align:right;"> 287 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>




```r
data %>% 
  filter(InvoiceDate > "2011-10-01") %>% 
  mutate(revenueQ42011=Quantity * UnitPrice) %>%
  select(CustomerID,revenueQ42011) %>%
  right_join(customersQ32011, by="CustomerID") %>%
  group_by(segment) %>%
  summarise(rev=paste0("£",sum(revenueQ42011, na.rm = TRUE)))%>%
  kbl(caption = "Q42011 revenue generated by Q32011 segment") %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-6"></span>Table 4: Q42011 revenue generated by Q32011 segment</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> segment </th>
   <th style="text-align:left;"> rev </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> new active </td>
   <td style="text-align:left;"> £77145.3699999993 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> active high value </td>
   <td style="text-align:left;"> £1116832.21000001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> active low value </td>
   <td style="text-align:left;"> £558571.190000031 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> warm high value </td>
   <td style="text-align:left;"> £122281.839999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> warm low value </td>
   <td style="text-align:left;"> £313854.369999997 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cold </td>
   <td style="text-align:left;"> £92894.7099999994 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> inactive </td>
   <td style="text-align:left;"> £31443.5400000002 </td>
  </tr>
</tbody>
</table>

The table 4 shows the contribution of Q32011 segments in the Q42011 revenue. The Q32011 active high-value customers have contributed the most in the Q42011 revenue while the cold customers have contributed the less. The most important insight from this table is that Q32011 warm customers have contributed about twice more than Q32011 new customers. This insight can be used to prioritise and to target each segment. 

## Conclusion

In this post, we wanted to show how to perform an RFM segmentation by using a managerial approach. We used recency to slice the online store database. Then we added the monetary value to understand the customer’s behaviours in each segment. Then we compare the past and the actual revenue to know how much revenue can be expected from each cluster in the future periods. And we have found that the eRetail can expect about twice more revenue from warm customers compared to new customers.  
