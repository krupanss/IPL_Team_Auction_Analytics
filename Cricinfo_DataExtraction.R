library(XML)
setwd("F:/Krupan/Documents/2_Freelance/3_Github_Projects/IPL_Team_Auction_Analytics/")
#***************************************Read All Matches Scorecard URLs****************************************************************
ipl2016url = "http://www.espncricinfo.com/indian-premier-league-2016/engine/series/968923.html"

urls = XML::getHTMLLinks(ipl2016url)
urls = unique(urls) 
scorecardUrls = as.character(urls[grep("/indian-premier-league-2016/engine/match/",urls)])
scorecardUrls = paste0("http://www.espncricinfo.com",scorecardUrls)
str(scorecardUrls)


library(XML)
library(RCurl)
xData <- getURL(ipl2016url)
doc <- xmlParse(xData)

doc <- htmlTreeParse(ipl2016url)
xmldoc = xmlRoot(xmlParse(doc[3]))
doc[3]
xmlAttrs(xmlRoot(doc$children[[4]]))
td <- xmlRoot(xmlParse(html))
xmlElementsByTagName(doc$children[[3]], "team-1-name")




# Encode to UTF-8 for proper display of text
Encoding(doc.text) = "UTF-8"
doc.text = as.character(doc.text)

fn_getScoreCard = function(url)
{
  tables=readHTMLTable(url,stringsAsFactors = F)  
  tables.length = length(tables)
  names(tables)[1:tables.length] = paste("Table",1:tables.length,sep="")
}

matchurl="http://www.espncricinfo.com/indian-premier-league-2016/engine/match/981019.html"
tables=readHTMLTable(matchurl,stringsAsFactors = F)
tables.length = length(tables)
names(tables)[1:tables.length] = paste("Table",1:tables.length,sep="")
names(tables)

# Table 1 - 1st Innings Batting
# Table 2 - 1st Innings Bowling
# Table 3 - 2nd Innings Batting
# Table 4 - 2nd Innings Bowling

doc.html = XML::htmlTreeParse(matchurl, useInternal = TRUE)
str(doc.html)
doc.text = unlist(XML::xpathApply(doc.html, '//a', xmlAttrs))



tables[1]
str(tables)
a = tables[2]
a = a$Table2
View(a)
is.na(a$`O`)
a = subset(a, !is.na(a$`O`))
a$` ` = NULL
a$` .1`= NULL
View(na.omit(a))

