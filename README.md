# Twitter-analytics

### For this project data was taken from twitter 
### More than 1 GB tweets were extracted where science related news from Los Angeles Times (LATimes) were posted.

## The goal of the analysis was to extract the original LAtimes links from the tweets

### After some exploratory analysis it seems to me that links are mainly given in three different formats
### The original link was given diretly or shorten url or quote/status url
### For example "http://www.latimes.com/science/sciencenow/la-sci-sn-galaxies-universe-hubble-20161013-snap-story.html" or 
### "http://bit.ly/2dQRft5" or "https://twitter.com/i/web/status/786575345310769152" format.
### All the urls were first extracted from "expanded_url" field from tweets are re_tweet section
### The shorten urls were expanded using "expand_urls" function in "longurl" package
### The original url from the quote/status urls were extracted by web scraping (tough part) 
