## load the needed data
load("~/Projekte/arXiv_app2/arxiv_app_data_faster.RData")

###### This data is basically:
## Load papers and issn_data
# papers_complete = read.csv("~/Projekte/arXiv_data/papers_data.csv") %>% data.table
# issn_data = read.csv("~/Projekte/arXiv_data/issn_data.csv") %>% data.table
# papers$coverDate %<>% as.POSIXct
# papers$datestamp %<>% as.POSIXct
# papers$created %<>% as.POSIXct
## Load keywords
# load("~/Projekte/arXiv_data/keywords_final.RData")
## Format the data
# papers_complete$keywords = keywords
# papers_complete = left_join(papers_complete, issn_data, by = "issn")
# papers_complete$X.x = NULL
# papers_complete$X.y = NULL
# papers = papers_complete
# papers = papers[!is.na(SJR)]
# papers = papers[!is.na(IPP)]
# rm(keywords)
# rm(papers_complete)
# save.image("~/Projekte/arXiv_data/arxiv_app_data_faster.RData", compress = FALSE)

## Horváth Colours and custom Palette:
hpdarkblue  <- rgb(5,65,90,max=255)
hplightblue <- rgb(0,140,200, max=255)
hppalette = colorRampPalette(c("gray80", hplightblue, hpdarkblue))

## Extract list of categories and journals to use as choices in the menu
categories_all <- papers$categories %>% as.character %>% strsplit(split = " ") %>% 
  unlist %>% table %>% data.table %>% setnames(".", "category") %>% arrange(category)
count_per_year = papers %>% mutate(year = year(coverDate)) %>% .[year >= 2000] %>%
  getElement("year") %>% table %>% data.table %>% setnames(".", "year")
journals <- papers$journal_title %>% as.character %>%
  unlist %>% table %>% data.table %>% setnames(".", "journal") %>% arrange(desc(N), journal)
journals_all <- setNames(
  journals$journal, paste(journals$journal, "-", journals$N) )

# journals_menu <- papers$journal_title %>% as.character %>% table %>% 
#   data.table
# journals_menu <- paste(journals_menu$., "-", journals_menu$N)


